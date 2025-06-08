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

(define-data-var contract-owner principal tx-sender)
(define-data-var total-supply uint u1000000)
(define-data-var daily-withdrawal-limit uint u10000)

(define-map user-balances principal uint)
(define-map user-passwords principal (string-ascii 64))
(define-map user-locked principal bool)
(define-map daily-withdrawals principal uint)
(define-map withdrawal-timestamps principal uint)

(define-public (create-account (password (string-ascii 64)))
  (let ((caller tx-sender))
    (if (is-some (map-get? user-balances caller))
      ERR-ALREADY-EXISTS
      (begin
        (map-set user-balances caller u0)
        (map-set user-passwords caller password)
        (map-set user-locked caller false)
        (map-set daily-withdrawals caller u0)
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
