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

(define-data-var contract-owner principal tx-sender)
(define-data-var total-supply uint u1000000)
(define-data-var daily-withdrawal-limit uint u10000)
(define-data-var transaction-counter uint u0)

(define-map user-balances principal uint)
(define-map user-passwords principal (string-ascii 64))
(define-map user-locked principal bool)
(define-map daily-withdrawals principal uint)
(define-map withdrawal-timestamps principal uint)
(define-map transaction-history uint {user: principal, tx-type: (string-ascii 16), amount: uint, counterparty: (optional principal), block-height: uint})
(define-map user-transaction-count principal uint)

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
