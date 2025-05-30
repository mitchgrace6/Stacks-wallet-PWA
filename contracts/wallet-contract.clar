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