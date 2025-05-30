;; wallet-contract
;; A comprehensive multi-signature wallet with advanced features for secure asset management

;; constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-NOT-AUTHORIZED (err u101))
(define-constant ERR-INVALID-AMOUNT (err u102))
(define-constant ERR-INSUFFICIENT-BALANCE (err u103))
(define-constant ERR-INVALID-RECIPIENT (err u104))
(define-constant ERR-ALREADY-EXISTS (err u105))
(define-constant ERR-NOT-FOUND (err u106))
(define-constant ERR-INVALID-THRESHOLD (err u107))
(define-constant ERR-ALREADY-SIGNED (err u108))
(define-constant ERR-TRANSACTION-EXPIRED (err u109))
(define-constant ERR-INSUFFICIENT-SIGNATURES (err u110))
(define-constant ERR-WALLET-FROZEN (err u111))
(define-constant ERR-DAILY-LIMIT-EXCEEDED (err u112))

(define-constant MAX-SIGNERS u10)
(define-constant TRANSACTION-EXPIRY-BLOCKS u144) ;; ~24 hours
(define-constant MIN-THRESHOLD u1)

;; data maps and vars
(define-map wallets 
  { wallet-id: uint }
  {
    name: (string-ascii 50),
    signers: (list 10 principal),
    threshold: uint,
    balance: uint,
    is-frozen: bool,
    daily-limit: uint,
    daily-spent: uint,
    last-reset-block: uint,
    created-at: uint
  }
)

(define-map wallet-permissions
  { wallet-id: uint, signer: principal }
  { is-active: bool, added-at: uint }
)

(define-map pending-transactions
  { tx-id: uint }
  {
    wallet-id: uint,
    recipient: principal,
    amount: uint,
    memo: (optional (string-ascii 200)),
    signatures: (list 10 principal),
    created-at: uint,
    expires-at: uint,
    is-executed: bool
  }
)

(define-map user-wallets
  { user: principal }
  { wallet-ids: (list 20 uint) }
)

(define-data-var next-wallet-id uint u1)
(define-data-var next-tx-id uint u1)
(define-data-var contract-paused bool false)

;; private functions
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT-OWNER)
)

(define-private (is-wallet-signer (wallet-id uint) (user principal))
  (match (map-get? wallet-permissions { wallet-id: wallet-id, signer: user })
    permission (get is-active permission)
    false
  )
)

(define-private (get-wallet-info (wallet-id uint))
  (map-get? wallets { wallet-id: wallet-id })
)

(define-private (is-wallet-frozen (wallet-id uint))
  (match (get-wallet-info wallet-id)
    wallet (get is-frozen wallet)
    true
  )
)

(define-private (update-daily-spending (wallet-id uint) (amount uint))
  (match (get-wallet-info wallet-id)
    wallet 
      (let 
        (
          (current-block block-height)
          (last-reset (get last-reset-block wallet))
          (daily-spent (get daily-spent wallet))
          (daily-limit (get daily-limit wallet))
          (blocks-since-reset (- current-block last-reset))
        )
        (if (>= blocks-since-reset u144) ;; Reset daily limit every ~24 hours
          (begin
            (map-set wallets 
              { wallet-id: wallet-id }
              (merge wallet { 
                daily-spent: amount, 
                last-reset-block: current-block 
              })
            )
            true
          )
          (if (<= (+ daily-spent amount) daily-limit)
            (begin
              (map-set wallets 
                { wallet-id: wallet-id }
                (merge wallet { daily-spent: (+ daily-spent amount) })
              )
              true
            )
            false
          )
        )
      )
    false
  )
)

(define-private (add-signature-to-tx (tx-id uint) (signer principal))
  (match (map-get? pending-transactions { tx-id: tx-id })
    tx-data
      (let ((current-signatures (get signatures tx-data)))
        (if (is-none (index-of current-signatures signer))
          (map-set pending-transactions 
            { tx-id: tx-id }
            (merge tx-data { 
              signatures: (unwrap! (as-max-len? (append current-signatures signer) u10) false)
            })
          )
          false
        )
      )
    false
  )
)

(define-private (has-sufficient-signatures (wallet-id uint) (tx-id uint))
  (match (get-wallet-info wallet-id)
    wallet
      (match (map-get? pending-transactions { tx-id: tx-id })
        tx-data
          (>= (len (get signatures tx-data)) (get threshold wallet))
        false
      )
    false
  )
)