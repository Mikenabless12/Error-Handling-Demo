(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-BALANCE (err u101))
(define-constant ERR-INVALID-AMOUNT (err u102))
(define-constant ERR-USER-NOT-FOUND (err u103))
(define-constant ERR-ALREADY-EXISTS (err u104))
(define-constant ERR-INVALID-RECIPIENT (err u105))
(define-constant ERR-TRANSFER-FAILED (err u106))
(define-constant ERR-INVALID-PASSWORD (err u107))
(define-constant ERR-ACCOUNT-LOCKED (err u108))
(define-constant ERR-WITHDRAWAL-LIMIT-EXCEEDED (err u109))
(define-constant ERR-INVALID-PAGE (err u110))
(define-constant ERR-PENDING-TRANSACTION-NOT-FOUND (err u111))
(define-constant ERR-TRANSACTION-EXPIRED (err u112))
(define-constant ERR-APPROVAL-NOT-REQUIRED (err u113))
(define-constant ERR-RECOVERY-NOT-FOUND (err u114))
(define-constant ERR-RECOVERY-EXPIRED (err u115))
(define-constant ERR-RECOVERY-ACTIVE (err u116))
(define-constant ERR-INVALID-RECOVERY-CONTACT (err u117))
(define-constant ERR-INSUFFICIENT-VOTES (err u118))
(define-constant ERR-ALREADY-VOTED (err u119))
(define-constant ERR-SCHEDULED-PAYMENT-NOT-FOUND (err u120))
(define-constant ERR-PAYMENT-NOT-DUE (err u121))
(define-constant ERR-PAYMENT-ALREADY-EXECUTED (err u122))

(define-data-var contract-owner principal tx-sender)
(define-data-var total-supply uint u1000000)
(define-data-var daily-withdrawal-limit uint u10000)
(define-data-var transaction-counter uint u0)
(define-data-var pending-transaction-counter uint u0)
(define-data-var recovery-request-counter uint u0)
(define-data-var scheduled-payment-counter uint u0)

(define-map user-balances principal uint)
(define-map user-passwords principal (string-ascii 64))
(define-map user-locked principal bool)
(define-map daily-withdrawals principal uint)
(define-map withdrawal-timestamps principal uint)
(define-map transaction-history uint {user: principal, tx-type: (string-ascii 16), amount: uint, counterparty: (optional principal), block-height: uint})
(define-map user-transaction-count principal uint)
(define-map approval-thresholds principal uint)
(define-map pending-transactions uint {user: principal, tx-type: (string-ascii 16), amount: uint, counterparty: (optional principal), created-at: uint, expires-at: uint})
(define-map user-pending-count principal uint)
(define-map recovery-contacts {user: principal, contact: principal} bool)
(define-map user-recovery-contact-count principal uint)
(define-map recovery-requests uint {user: principal, new-password: (string-ascii 64), created-at: uint, expires-at: uint, votes: uint, required-votes: uint})
(define-map recovery-votes {request-id: uint, voter: principal} bool)
(define-map scheduled-payments uint {sender: principal, recipient: principal, amount: uint, interval-blocks: uint, next-payment-block: uint, total-payments: uint, executed-payments: uint, active: bool})
(define-map user-scheduled-payment-count principal uint)

(define-public (create-account (password (string-ascii 64)))
  (let ((caller tx-sender))
    (if (is-some (map-get? user-balances caller))
      ERR-ALREADY-EXISTS
      (begin
        (map-set user-balances caller u0)
        (map-set user-passwords caller password)
        (map-set user-locked caller false)
        (map-set daily-withdrawals caller u0)
        (map-set user-transaction-count caller u0)
        (map-set approval-thresholds caller u0)
        (map-set user-pending-count caller u0)
        (map-set user-recovery-contact-count caller u0)
        (map-set user-scheduled-payment-count caller u0)
        (ok true)))))

(define-public (deposit (amount uint))
  (let ((caller tx-sender)
        (current-balance (default-to u0 (map-get? user-balances caller))))
    (if (is-none (map-get? user-balances caller))
      ERR-USER-NOT-FOUND
      (if (<= amount u0)
        ERR-INVALID-AMOUNT
        (begin
          (map-set user-balances caller (+ current-balance amount))
          (unwrap-panic (log-transaction caller "deposit" amount none))
          (ok current-balance))))))

(define-public (withdraw (amount uint) (password (string-ascii 64)))
  (let ((caller tx-sender)
        (current-balance (default-to u0 (map-get? user-balances caller)))
        (stored-password (map-get? user-passwords caller))
        (is-locked (default-to false (map-get? user-locked caller)))
        (daily-withdrawn (default-to u0 (map-get? daily-withdrawals caller)))
        (last-withdrawal (default-to u0 (map-get? withdrawal-timestamps caller))))
    (if (is-none (map-get? user-balances caller))
      ERR-USER-NOT-FOUND
      (if is-locked
        ERR-ACCOUNT-LOCKED
        (if (is-none stored-password)
          ERR-INVALID-PASSWORD
          (if (not (is-eq (unwrap-panic stored-password) password))
            ERR-INVALID-PASSWORD
            (if (<= amount u0)
              ERR-INVALID-AMOUNT
              (if (> amount current-balance)
                ERR-INSUFFICIENT-BALANCE
                (if (> (+ daily-withdrawn amount) (var-get daily-withdrawal-limit))
                  ERR-WITHDRAWAL-LIMIT-EXCEEDED
                  (begin
                    (map-set user-balances caller (- current-balance amount))
                    (if (> (- stacks-block-height last-withdrawal) u144)
                      (map-set daily-withdrawals caller amount)
                      (map-set daily-withdrawals caller (+ daily-withdrawn amount)))
                    (map-set withdrawal-timestamps caller stacks-block-height)
                    (unwrap-panic (log-transaction caller "withdraw" amount none))
                    (ok (- current-balance amount))))))))))))

(define-public (transfer (recipient principal) (amount uint) (password (string-ascii 64)))
  (let ((caller tx-sender)
        (sender-balance (default-to u0 (map-get? user-balances caller)))
        (recipient-balance (default-to u0 (map-get? user-balances recipient)))
        (stored-password (map-get? user-passwords caller))
        (is-locked (default-to false (map-get? user-locked caller))))
    (if (is-none (map-get? user-balances caller))
      ERR-USER-NOT-FOUND
      (if (is-none (map-get? user-balances recipient))
        ERR-INVALID-RECIPIENT
        (if is-locked
          ERR-ACCOUNT-LOCKED
          (if (is-none stored-password)
            ERR-INVALID-PASSWORD
            (if (not (is-eq (unwrap-panic stored-password) password))
              ERR-INVALID-PASSWORD
              (if (<= amount u0)
                ERR-INVALID-AMOUNT
                (if (> amount sender-balance)
                  ERR-INSUFFICIENT-BALANCE
                  (if (is-eq caller recipient)
                    ERR-INVALID-RECIPIENT
                    (begin
                      (map-set user-balances caller (- sender-balance amount))
                      (map-set user-balances recipient (+ recipient-balance amount))
                      (unwrap-panic (log-transaction caller "transfer-out" amount (some recipient)))
                      (unwrap-panic (log-transaction recipient "transfer-in" amount (some caller)))
                      (ok true))))))))))))

(define-public (lock-account (user principal))
  (let ((caller tx-sender))
    (if (not (is-eq caller (var-get contract-owner)))
      ERR-NOT-AUTHORIZED
      (if (is-none (map-get? user-balances user))
        ERR-USER-NOT-FOUND
        (begin
          (map-set user-locked user true)
          (ok true))))))

(define-public (unlock-account (user principal))
  (let ((caller tx-sender))
    (if (not (is-eq caller (var-get contract-owner)))
      ERR-NOT-AUTHORIZED
      (if (is-none (map-get? user-balances user))
        ERR-USER-NOT-FOUND
        (begin
          (map-set user-locked user false)
          (ok true))))))

(define-public (change-password (old-password (string-ascii 64)) (new-password (string-ascii 64)))
  (let ((caller tx-sender)
        (stored-password (map-get? user-passwords caller))
        (is-locked (default-to false (map-get? user-locked caller))))
    (if (is-none (map-get? user-balances caller))
      ERR-USER-NOT-FOUND
      (if is-locked
        ERR-ACCOUNT-LOCKED
        (if (is-none stored-password)
          ERR-INVALID-PASSWORD
          (if (not (is-eq (unwrap-panic stored-password) old-password))
            ERR-INVALID-PASSWORD
            (begin
              (map-set user-passwords caller new-password)
              (ok true))))))))

(define-public (set-withdrawal-limit (new-limit uint))
  (let ((caller tx-sender))
    (if (not (is-eq caller (var-get contract-owner)))
      ERR-NOT-AUTHORIZED
      (if (<= new-limit u0)
        ERR-INVALID-AMOUNT
        (begin
          (var-set daily-withdrawal-limit new-limit)
          (ok new-limit))))))

(define-read-only (get-balance (user principal))
  (ok (default-to u0 (map-get? user-balances user))))

(define-read-only (get-daily-withdrawal-limit)
  (ok (var-get daily-withdrawal-limit)))

(define-read-only (get-daily-withdrawals (user principal))
  (ok (default-to u0 (map-get? daily-withdrawals user))))

(define-read-only (is-account-locked (user principal))
  (ok (default-to false (map-get? user-locked user))))

(define-read-only (account-exists (user principal))
  (ok (is-some (map-get? user-balances user))))

(define-read-only (get-contract-owner)
  (ok (var-get contract-owner)))

(define-read-only (get-total-supply)
  (ok (var-get total-supply)))

(define-private (reset-daily-withdrawals-if-needed (user principal))
  (let ((last-withdrawal (default-to u0 (map-get? withdrawal-timestamps user))))
    (if (> (- stacks-block-height last-withdrawal) u144)
      (map-set daily-withdrawals user u0)
      true)))

(define-private (validate-user-exists (user principal))
  (if (is-some (map-get? user-balances user))
    (ok true)
    ERR-USER-NOT-FOUND))

(define-private (validate-amount (amount uint))
  (if (> amount u0)
    (ok true)
    ERR-INVALID-AMOUNT))

(define-private (validate-sufficient-balance (user principal) (amount uint))
  (let ((balance (default-to u0 (map-get? user-balances user))))
    (if (>= balance amount)
      (ok true)
      ERR-INSUFFICIENT-BALANCE)))

(define-private (validate-password (user principal) (password (string-ascii 64)))
  (let ((stored-password (map-get? user-passwords user)))
    (if (is-some stored-password)
      (if (is-eq (unwrap-panic stored-password) password)
        (ok true)
        ERR-INVALID-PASSWORD)
      ERR-INVALID-PASSWORD)))

(define-private (validate-account-not-locked (user principal))
  (let ((is-locked (default-to false (map-get? user-locked user))))
    (if is-locked
      ERR-ACCOUNT-LOCKED
      (ok true))))

(define-private (log-transaction (user principal) (tx-type (string-ascii 16)) (amount uint) (counterparty (optional principal)))
  (let ((current-counter (var-get transaction-counter))
        (user-tx-count (default-to u0 (map-get? user-transaction-count user))))
    (begin
      (map-set transaction-history current-counter {
        user: user,
        tx-type: tx-type,
        amount: amount,
        counterparty: counterparty,
        block-height: stacks-block-height
      })
      (map-set user-transaction-count user (+ user-tx-count u1))
      (var-set transaction-counter (+ current-counter u1))
      (ok current-counter))))

(define-read-only (get-transaction (tx-id uint))
  (match (map-get? transaction-history tx-id)
    transaction (ok transaction)
    (err u404)))

(define-read-only (get-user-transactions (user principal) (page uint) (per-page uint))
  (if (or (is-eq page u0) (is-eq per-page u0) (> per-page u50))
    ERR-INVALID-PAGE
    (let ((user-tx-count (default-to u0 (map-get? user-transaction-count user)))
          (start-index (* (- page u1) per-page))
          (max-results (if (> per-page u10) u10 per-page)))
      (if (>= start-index user-tx-count)
        (ok (list))
        (ok (get results (get-user-transactions-range user start-index max-results)))))))

(define-read-only (get-user-transaction-count (user principal))
  (ok (default-to u0 (map-get? user-transaction-count user))))

(define-read-only (get-recent-transactions (limit uint))
  (if (or (is-eq limit u0) (> limit u20))
    (ok (list))
    (let ((current-counter (var-get transaction-counter))
          (start-id (if (>= current-counter limit) (- current-counter limit) u0)))
      (ok (get results (get-transactions-range start-id limit))))))

(define-private (get-user-transactions-range (user principal) (start uint) (count uint))
  (let ((current-counter (var-get transaction-counter)))
    (fold check-and-add-user-transaction 
          (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9)
          {user: user, start: start, count: count, found: u0, results: (list), current-id: u0})))

(define-private (get-transactions-range (start-id uint) (count uint))
  (let ((current-counter (var-get transaction-counter)))
    (fold check-and-add-transaction
          (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19)
          {start: start-id, count: count, found: u0, results: (list), current-id: start-id})))

(define-private (check-and-add-user-transaction (index uint) (state {user: principal, start: uint, count: uint, found: uint, results: (list 10 {user: principal, tx-type: (string-ascii 16), amount: uint, counterparty: (optional principal), block-height: uint}), current-id: uint}))
  (let ((user (get user state))
        (start (get start state))
        (count (get count state))
        (found (get found state))
        (results (get results state))
        (current-id (get current-id state))
        (current-counter (var-get transaction-counter)))
    (if (or (>= found count) (>= current-id current-counter))
      state
      (match (map-get? transaction-history current-id)
        transaction (if (is-eq (get user transaction) user)
                     {user: user, start: start, count: count, found: (+ found u1), results: (unwrap-panic (as-max-len? (append results transaction) u10)), current-id: (+ current-id u1)}
                     {user: user, start: start, count: count, found: found, results: results, current-id: (+ current-id u1)})
        {user: user, start: start, count: count, found: found, results: results, current-id: (+ current-id u1)}))))

(define-private (check-and-add-transaction (index uint) (state {start: uint, count: uint, found: uint, results: (list 20 {user: principal, tx-type: (string-ascii 16), amount: uint, counterparty: (optional principal), block-height: uint}), current-id: uint}))
  (let ((start (get start state))
        (count (get count state))
        (found (get found state))
        (results (get results state))
        (current-id (get current-id state)))
    (if (>= found count)
      state
      (match (map-get? transaction-history current-id)
        transaction {start: start, count: count, found: (+ found u1), results: (unwrap-panic (as-max-len? (append results transaction) u20)), current-id: (+ current-id u1)}
        {start: start, count: count, found: found, results: results, current-id: (+ current-id u1)}))))

(define-public (set-approval-threshold (threshold uint))
  (let ((caller tx-sender))
    (if (is-none (map-get? user-balances caller))
      ERR-USER-NOT-FOUND
      (begin
        (map-set approval-thresholds caller threshold)
        (ok threshold)))))

(define-public (request-withdrawal-approval (amount uint) (password (string-ascii 64)))
  (let ((caller tx-sender)
        (threshold (default-to u0 (map-get? approval-thresholds caller)))
        (current-balance (default-to u0 (map-get? user-balances caller)))
        (stored-password (map-get? user-passwords caller))
        (is-locked (default-to false (map-get? user-locked caller))))
    (if (is-none (map-get? user-balances caller))
      ERR-USER-NOT-FOUND
      (if is-locked
        ERR-ACCOUNT-LOCKED
        (if (<= amount threshold)
          ERR-APPROVAL-NOT-REQUIRED
          (if (is-none stored-password)
            ERR-INVALID-PASSWORD
            (if (not (is-eq (unwrap-panic stored-password) password))
              ERR-INVALID-PASSWORD
              (if (<= amount u0)
                ERR-INVALID-AMOUNT
                (if (> amount current-balance)
                  ERR-INSUFFICIENT-BALANCE
                  (create-pending-transaction caller "withdrawal" amount none))))))))))

(define-public (request-transfer-approval (recipient principal) (amount uint) (password (string-ascii 64)))
  (let ((caller tx-sender)
        (threshold (default-to u0 (map-get? approval-thresholds caller)))
        (sender-balance (default-to u0 (map-get? user-balances caller)))
        (stored-password (map-get? user-passwords caller))
        (is-locked (default-to false (map-get? user-locked caller))))
    (if (is-none (map-get? user-balances caller))
      ERR-USER-NOT-FOUND
      (if (is-none (map-get? user-balances recipient))
        ERR-INVALID-RECIPIENT
        (if is-locked
          ERR-ACCOUNT-LOCKED
          (if (<= amount threshold)
            ERR-APPROVAL-NOT-REQUIRED
            (if (is-none stored-password)
              ERR-INVALID-PASSWORD
              (if (not (is-eq (unwrap-panic stored-password) password))
                ERR-INVALID-PASSWORD
                (if (<= amount u0)
                  ERR-INVALID-AMOUNT
                  (if (> amount sender-balance)
                    ERR-INSUFFICIENT-BALANCE
                    (if (is-eq caller recipient)
                      ERR-INVALID-RECIPIENT
                      (create-pending-transaction caller "transfer" amount (some recipient)))))))))))))

(define-public (approve-transaction (pending-tx-id uint) (password (string-ascii 64)))
  (let ((caller tx-sender)
        (stored-password (map-get? user-passwords caller))
        (pending-tx (map-get? pending-transactions pending-tx-id)))
    (if (is-none pending-tx)
      ERR-PENDING-TRANSACTION-NOT-FOUND
      (let ((tx-data (unwrap-panic pending-tx))
            (tx-user (get user tx-data))
            (tx-type (get tx-type tx-data))
            (tx-amount (get amount tx-data))
            (tx-counterparty (get counterparty tx-data))
            (expires-at (get expires-at tx-data)))
        (if (not (is-eq caller tx-user))
          ERR-NOT-AUTHORIZED
          (if (> stacks-block-height expires-at)
            ERR-TRANSACTION-EXPIRED
            (if (is-none stored-password)
              ERR-INVALID-PASSWORD
              (if (not (is-eq (unwrap-panic stored-password) password))
                ERR-INVALID-PASSWORD
                (begin
                  (map-delete pending-transactions pending-tx-id)
                  (if (is-eq tx-type "withdrawal")
                    (begin
                      (unwrap-panic (execute-approved-withdrawal tx-user tx-amount))
                      (ok true))
                    (execute-approved-transfer tx-user (unwrap-panic tx-counterparty) tx-amount)))))))))))

(define-public (reject-transaction (pending-tx-id uint) (password (string-ascii 64)))
  (let ((caller tx-sender)
        (stored-password (map-get? user-passwords caller))
        (pending-tx (map-get? pending-transactions pending-tx-id)))
    (if (is-none pending-tx)
      ERR-PENDING-TRANSACTION-NOT-FOUND
      (let ((tx-data (unwrap-panic pending-tx))
            (tx-user (get user tx-data)))
        (if (not (is-eq caller tx-user))
          ERR-NOT-AUTHORIZED
          (if (is-none stored-password)
            ERR-INVALID-PASSWORD
            (if (not (is-eq (unwrap-panic stored-password) password))
              ERR-INVALID-PASSWORD
              (begin
                (map-delete pending-transactions pending-tx-id)
                (ok true)))))))))

(define-read-only (get-approval-threshold (user principal))
  (ok (default-to u0 (map-get? approval-thresholds user))))

(define-read-only (get-pending-transaction (pending-tx-id uint))
  (match (map-get? pending-transactions pending-tx-id)
    transaction (ok transaction)
    ERR-PENDING-TRANSACTION-NOT-FOUND))

(define-read-only (get-user-pending-count (user principal))
  (ok (default-to u0 (map-get? user-pending-count user))))

(define-private (create-pending-transaction (user principal) (tx-type (string-ascii 16)) (amount uint) (counterparty (optional principal)))
  (let ((pending-counter (var-get pending-transaction-counter))
        (user-pending (default-to u0 (map-get? user-pending-count user)))
        (expires-at (+ stacks-block-height u144)))
    (begin
      (map-set pending-transactions pending-counter {
        user: user,
        tx-type: tx-type,
        amount: amount,
        counterparty: counterparty,
        created-at: stacks-block-height,
        expires-at: expires-at
      })
      (map-set user-pending-count user (+ user-pending u1))
      (var-set pending-transaction-counter (+ pending-counter u1))
      (ok pending-counter))))

(define-private (execute-approved-withdrawal (user principal) (amount uint))
  (let ((current-balance (default-to u0 (map-get? user-balances user))))
    (if (> amount current-balance)
      ERR-INSUFFICIENT-BALANCE
      (begin
        (map-set user-balances user (- current-balance amount))
        (unwrap-panic (log-transaction user "withdrawal" amount none))
        (ok (- current-balance amount))))))

(define-private (execute-approved-transfer (sender principal) (recipient principal) (amount uint))
  (let ((sender-balance (default-to u0 (map-get? user-balances sender)))
        (recipient-balance (default-to u0 (map-get? user-balances recipient))))
    (if (> amount sender-balance)
      ERR-INSUFFICIENT-BALANCE
      (begin
        (map-set user-balances sender (- sender-balance amount))
        (map-set user-balances recipient (+ recipient-balance amount))
        (unwrap-panic (log-transaction sender "transfer-out" amount (some recipient)))
        (unwrap-panic (log-transaction recipient "transfer-in" amount (some sender)))
        (ok true)))))

(define-public (add-recovery-contact (contact principal))
  (let ((caller tx-sender)
        (contact-count (default-to u0 (map-get? user-recovery-contact-count caller))))
    (if (is-none (map-get? user-balances caller))
      ERR-USER-NOT-FOUND
      (if (is-eq caller contact)
        ERR-INVALID-RECOVERY-CONTACT
        (if (is-none (map-get? user-balances contact))
          ERR-INVALID-RECOVERY-CONTACT
          (if (>= contact-count u5)
            ERR-INVALID-AMOUNT
            (if (is-some (map-get? recovery-contacts {user: caller, contact: contact}))
              ERR-ALREADY-EXISTS
              (begin
                (map-set recovery-contacts {user: caller, contact: contact} true)
                (map-set user-recovery-contact-count caller (+ contact-count u1))
                (ok true)))))))))

(define-public (remove-recovery-contact (contact principal) (password (string-ascii 64)))
  (let ((caller tx-sender)
        (stored-password (map-get? user-passwords caller))
        (contact-count (default-to u0 (map-get? user-recovery-contact-count caller))))
    (if (is-none (map-get? user-balances caller))
      ERR-USER-NOT-FOUND
      (if (is-none stored-password)
        ERR-INVALID-PASSWORD
        (if (not (is-eq (unwrap-panic stored-password) password))
          ERR-INVALID-PASSWORD
          (if (is-none (map-get? recovery-contacts {user: caller, contact: contact}))
            ERR-INVALID-RECOVERY-CONTACT
            (begin
              (map-delete recovery-contacts {user: caller, contact: contact})
              (map-set user-recovery-contact-count caller (- contact-count u1))
              (ok true))))))))

(define-public (request-account-recovery (user principal) (new-password (string-ascii 64)))
  (let ((caller tx-sender)
        (contact-count (default-to u0 (map-get? user-recovery-contact-count user)))
        (recovery-counter (var-get recovery-request-counter))
        (expires-at (+ stacks-block-height u1008)))
    (if (is-none (map-get? user-balances user))
      ERR-USER-NOT-FOUND
      (if (not (is-some (map-get? recovery-contacts {user: user, contact: caller})))
        ERR-INVALID-RECOVERY-CONTACT
        (if (< contact-count u2)
          ERR-INSUFFICIENT-VOTES
          (if false
            ERR-RECOVERY-ACTIVE
            (let ((required-votes (calculate-required-votes contact-count)))
              (begin
                (map-set recovery-requests recovery-counter {
                  user: user,
                  new-password: new-password,
                  created-at: stacks-block-height,
                  expires-at: expires-at,
                  votes: u1,
                  required-votes: required-votes
                })
                (map-set recovery-votes {request-id: recovery-counter, voter: caller} true)
                (var-set recovery-request-counter (+ recovery-counter u1))
                (ok recovery-counter)))))))))

(define-public (vote-recovery (request-id uint))
  (let ((caller tx-sender)
        (request (map-get? recovery-requests request-id)))
    (if (is-none request)
      ERR-RECOVERY-NOT-FOUND
      (let ((request-data (unwrap-panic request))
            (user (get user request-data))
            (expires-at (get expires-at request-data))
            (current-votes (get votes request-data))
            (required-votes (get required-votes request-data)))
        (if (> stacks-block-height expires-at)
          ERR-RECOVERY-EXPIRED
          (if (not (is-some (map-get? recovery-contacts {user: user, contact: caller})))
            ERR-INVALID-RECOVERY-CONTACT
            (if (is-some (map-get? recovery-votes {request-id: request-id, voter: caller}))
              ERR-ALREADY-VOTED
              (let ((new-votes (+ current-votes u1)))
                (begin
                  (map-set recovery-votes {request-id: request-id, voter: caller} true)
                  (map-set recovery-requests request-id (merge request-data {votes: new-votes}))
                  (if (>= new-votes required-votes)
                    (execute-recovery request-id request-data)
                    (ok true)))))))))))

(define-public (cancel-recovery-request (request-id uint) (password (string-ascii 64)))
  (let ((caller tx-sender)
        (stored-password (map-get? user-passwords caller))
        (request (map-get? recovery-requests request-id)))
    (if (is-none request)
      ERR-RECOVERY-NOT-FOUND
      (let ((request-data (unwrap-panic request))
            (user (get user request-data)))
        (if (not (is-eq caller user))
          ERR-NOT-AUTHORIZED
          (if (is-none stored-password)
            ERR-INVALID-PASSWORD
            (if (not (is-eq (unwrap-panic stored-password) password))
              ERR-INVALID-PASSWORD
              (begin
                (map-delete recovery-requests request-id)
                (ok true)))))))))

(define-read-only (get-recovery-contact-count (user principal))
  (ok (default-to u0 (map-get? user-recovery-contact-count user))))

(define-read-only (is-recovery-contact (user principal) (contact principal))
  (ok (is-some (map-get? recovery-contacts {user: user, contact: contact}))))

(define-read-only (get-recovery-request (request-id uint))
  (match (map-get? recovery-requests request-id)
    request (ok request)
    ERR-RECOVERY-NOT-FOUND))

(define-read-only (has-voted-recovery (request-id uint) (voter principal))
  (ok (is-some (map-get? recovery-votes {request-id: request-id, voter: voter}))))



(define-private (calculate-required-votes (total-contacts uint))
  (if (<= total-contacts u2)
    u2
    (if (<= total-contacts u3)
      u2
      (/ (* total-contacts u2) u3))))

(define-private (execute-recovery (request-id uint) (request-data {user: principal, new-password: (string-ascii 64), created-at: uint, expires-at: uint, votes: uint, required-votes: uint}))
  (let ((user (get user request-data))
        (new-password (get new-password request-data)))
    (begin
      (map-set user-passwords user new-password)
      (map-set user-locked user false)
      (map-delete recovery-requests request-id)
      (ok true))))

(define-public (create-scheduled-payment (recipient principal) (amount uint) (interval-blocks uint) (total-payments uint) (password (string-ascii 64)))
  (let ((caller tx-sender)
        (stored-password (map-get? user-passwords caller))
        (current-balance (default-to u0 (map-get? user-balances caller)))
        (payment-counter (var-get scheduled-payment-counter))
        (user-payment-count (default-to u0 (map-get? user-scheduled-payment-count caller))))
    (if (is-none (map-get? user-balances caller))
      ERR-USER-NOT-FOUND
      (if (is-none (map-get? user-balances recipient))
        ERR-INVALID-RECIPIENT
        (if (is-none stored-password)
          ERR-INVALID-PASSWORD
          (if (not (is-eq (unwrap-panic stored-password) password))
            ERR-INVALID-PASSWORD
            (if (<= amount u0)
              ERR-INVALID-AMOUNT
              (if (<= interval-blocks u0)
                ERR-INVALID-AMOUNT
                (if (<= total-payments u0)
                  ERR-INVALID-AMOUNT
                  (if (is-eq caller recipient)
                    ERR-INVALID-RECIPIENT
                    (begin
                      (map-set scheduled-payments payment-counter {
                        sender: caller,
                        recipient: recipient,
                        amount: amount,
                        interval-blocks: interval-blocks,
                        next-payment-block: (+ stacks-block-height interval-blocks),
                        total-payments: total-payments,
                        executed-payments: u0,
                        active: true
                      })
                      (map-set user-scheduled-payment-count caller (+ user-payment-count u1))
                      (var-set scheduled-payment-counter (+ payment-counter u1))
                      (ok payment-counter))))))))))))

(define-public (execute-scheduled-payment (payment-id uint))
  (let ((payment (map-get? scheduled-payments payment-id)))
    (if (is-none payment)
      ERR-SCHEDULED-PAYMENT-NOT-FOUND
      (let ((payment-data (unwrap-panic payment))
            (sender (get sender payment-data))
            (recipient (get recipient payment-data))
            (amount (get amount payment-data))
            (next-payment-block (get next-payment-block payment-data))
            (interval-blocks (get interval-blocks payment-data))
            (total-payments (get total-payments payment-data))
            (executed-payments (get executed-payments payment-data))
            (active (get active payment-data))
            (sender-balance (default-to u0 (map-get? user-balances sender)))
            (recipient-balance (default-to u0 (map-get? user-balances recipient))))
        (if (not active)
          ERR-PAYMENT-ALREADY-EXECUTED
          (if (> next-payment-block stacks-block-height)
            ERR-PAYMENT-NOT-DUE
            (if (> amount sender-balance)
              ERR-INSUFFICIENT-BALANCE
              (let ((new-executed-payments (+ executed-payments u1))
                    (is-final-payment (>= new-executed-payments total-payments))
                    (new-next-payment-block (+ next-payment-block interval-blocks)))
                (begin
                  (map-set user-balances sender (- sender-balance amount))
                  (map-set user-balances recipient (+ recipient-balance amount))
                  (unwrap-panic (log-transaction sender "scheduled-pay" amount (some recipient)))
                  (unwrap-panic (log-transaction recipient "scheduled-rcv" amount (some sender)))
                  (if is-final-payment
                    (map-set scheduled-payments payment-id (merge payment-data {executed-payments: new-executed-payments, active: false}))
                    (map-set scheduled-payments payment-id (merge payment-data {executed-payments: new-executed-payments, next-payment-block: new-next-payment-block})))
                  (ok new-executed-payments))))))))))

(define-public (cancel-scheduled-payment (payment-id uint) (password (string-ascii 64)))
  (let ((caller tx-sender)
        (stored-password (map-get? user-passwords caller))
        (payment (map-get? scheduled-payments payment-id)))
    (if (is-none payment)
      ERR-SCHEDULED-PAYMENT-NOT-FOUND
      (let ((payment-data (unwrap-panic payment))
            (sender (get sender payment-data)))
        (if (not (is-eq caller sender))
          ERR-NOT-AUTHORIZED
          (if (is-none stored-password)
            ERR-INVALID-PASSWORD
            (if (not (is-eq (unwrap-panic stored-password) password))
              ERR-INVALID-PASSWORD
              (begin
                (map-set scheduled-payments payment-id (merge payment-data {active: false}))
                (ok true)))))))))

(define-read-only (get-scheduled-payment (payment-id uint))
  (match (map-get? scheduled-payments payment-id)
    payment (ok payment)
    ERR-SCHEDULED-PAYMENT-NOT-FOUND))

(define-read-only (get-user-scheduled-payment-count (user principal))
  (ok (default-to u0 (map-get? user-scheduled-payment-count user))))

(define-read-only (is-payment-due (payment-id uint))
  (match (map-get? scheduled-payments payment-id)
    payment (let ((next-payment-block (get next-payment-block payment))
                  (active (get active payment)))
              (ok (and active (<= next-payment-block stacks-block-height))))
    ERR-SCHEDULED-PAYMENT-NOT-FOUND))
