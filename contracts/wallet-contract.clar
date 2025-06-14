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

;; public functions

;; Create a new multi-signature wallet
(define-public (create-wallet 
  (name (string-ascii 50))
  (signers (list 10 principal))
  (threshold uint)
  (daily-limit uint)
)
  (let 
    (
      (wallet-id (var-get next-wallet-id))
      (signers-count (len signers))
    )
    (asserts! (not (var-get contract-paused)) ERR-NOT-AUTHORIZED)
    (asserts! (and (>= threshold MIN-THRESHOLD) (<= threshold signers-count)) ERR-INVALID-THRESHOLD)
    (asserts! (<= signers-count MAX-SIGNERS) ERR-INVALID-AMOUNT)
    (asserts! (> daily-limit u0) ERR-INVALID-AMOUNT)
    
    ;; Create wallet
    (map-set wallets
      { wallet-id: wallet-id }
      {
        name: name,
        signers: signers,
        threshold: threshold,
        balance: u0,
        is-frozen: false,
        daily-limit: daily-limit,
        daily-spent: u0,
        last-reset-block: block-height,
        created-at: block-height
      }
    )
    
    ;; Add permissions for each signer
    (fold add-wallet-permission-fold signers wallet-id)
    
    ;; Update user wallets mapping
    (fold add-to-user-wallets-fold signers wallet-id)
    
    ;; Increment wallet ID
    (var-set next-wallet-id (+ wallet-id u1))
    
    (ok wallet-id)
  )
)

;; Helper function to add wallet permission (for fold)
(define-private (add-wallet-permission-fold (signer principal) (wallet-id uint))
  (begin
    (map-set wallet-permissions
      { wallet-id: wallet-id, signer: signer }
      { is-active: true, added-at: block-height }
    )
    wallet-id
  )
)

;; Helper function to add wallet to user's list (for fold)
(define-private (add-to-user-wallets-fold (user principal) (wallet-id uint))
  (begin
    (match (map-get? user-wallets { user: user })
      existing-wallets
        (map-set user-wallets
          { user: user }
          { wallet-ids: (default-to (list wallet-id) (as-max-len? (append (get wallet-ids existing-wallets) wallet-id) u20)) }
        )
      (map-set user-wallets
        { user: user }
        { wallet-ids: (list wallet-id) }
      )
    )
    wallet-id
  )
)

;; Deposit STX to wallet
(define-public (deposit (wallet-id uint) (amount uint))
  (let ((wallet-info (unwrap! (get-wallet-info wallet-id) ERR-NOT-FOUND)))
    (asserts! (not (var-get contract-paused)) ERR-NOT-AUTHORIZED)
    (asserts! (not (is-wallet-frozen wallet-id)) ERR-WALLET-FROZEN)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    
    ;; Transfer STX from sender to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update wallet balance
    (map-set wallets
      { wallet-id: wallet-id }
      (merge wallet-info { balance: (+ (get balance wallet-info) amount) })
    )
    
    (ok amount)
  )
)

;; Create a pending transaction
(define-public (create-transaction 
  (wallet-id uint)
  (recipient principal)
  (amount uint)
  (memo (optional (string-ascii 200)))
)
  (let 
    (
      (tx-id (var-get next-tx-id))
      (wallet-info (unwrap! (get-wallet-info wallet-id) ERR-NOT-FOUND))
    )
    (asserts! (not (var-get contract-paused)) ERR-NOT-AUTHORIZED)
    (asserts! (not (is-wallet-frozen wallet-id)) ERR-WALLET-FROZEN)
    (asserts! (is-wallet-signer wallet-id tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (<= amount (get balance wallet-info)) ERR-INSUFFICIENT-BALANCE)
    
    ;; Create pending transaction
    (map-set pending-transactions
      { tx-id: tx-id }
      {
        wallet-id: wallet-id,
        recipient: recipient,
        amount: amount,
        memo: memo,
        signatures: (list tx-sender),
        created-at: block-height,
        expires-at: (+ block-height TRANSACTION-EXPIRY-BLOCKS),
        is-executed: false
      }
    )
    
    ;; Increment transaction ID
    (var-set next-tx-id (+ tx-id u1))
    
    (ok tx-id)
  )
)

;; Sign a pending transaction
(define-public (sign-transaction (tx-id uint))
  (let ((tx-data (unwrap! (map-get? pending-transactions { tx-id: tx-id }) ERR-NOT-FOUND)))
    (asserts! (not (var-get contract-paused)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get is-executed tx-data)) ERR-NOT-FOUND)
    (asserts! (< block-height (get expires-at tx-data)) ERR-TRANSACTION-EXPIRED)
    (asserts! (is-wallet-signer (get wallet-id tx-data) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (index-of (get signatures tx-data) tx-sender)) ERR-ALREADY-SIGNED)
    
    ;; Add signature
    (asserts! (add-signature-to-tx tx-id tx-sender) ERR-NOT-AUTHORIZED)
    
    (ok true)
  )
)

;; Execute a transaction with sufficient signatures
(define-public (execute-transaction (tx-id uint))
  (let 
    (
      (tx-data (unwrap! (map-get? pending-transactions { tx-id: tx-id }) ERR-NOT-FOUND))
      (wallet-id (get wallet-id tx-data))
      (wallet-info (unwrap! (get-wallet-info wallet-id) ERR-NOT-FOUND))
    )
    (asserts! (not (var-get contract-paused)) ERR-NOT-AUTHORIZED)
    (asserts! (not (is-wallet-frozen wallet-id)) ERR-WALLET-FROZEN)
    (asserts! (not (get is-executed tx-data)) ERR-NOT-FOUND)
    (asserts! (< block-height (get expires-at tx-data)) ERR-TRANSACTION-EXPIRED)
    (asserts! (has-sufficient-signatures wallet-id tx-id) ERR-INSUFFICIENT-SIGNATURES)
    (asserts! (update-daily-spending wallet-id (get amount tx-data)) ERR-DAILY-LIMIT-EXCEEDED)
    
    ;; Execute transfer
    (try! (as-contract (stx-transfer? (get amount tx-data) tx-sender (get recipient tx-data))))
    
    ;; Update wallet balance
    (map-set wallets
      { wallet-id: wallet-id }
      (merge wallet-info { balance: (- (get balance wallet-info) (get amount tx-data)) })
    )
    
    ;; Mark transaction as executed
    (map-set pending-transactions
      { tx-id: tx-id }
      (merge tx-data { is-executed: true })
    )
    
    (ok true)
  )
)

;; Freeze/unfreeze wallet (owner only)
(define-public (set-wallet-frozen (wallet-id uint) (frozen bool))
  (let ((wallet-info (unwrap! (get-wallet-info wallet-id) ERR-NOT-FOUND)))
    (asserts! (is-contract-owner) ERR-OWNER-ONLY)
    
    (map-set wallets
      { wallet-id: wallet-id }
      (merge wallet-info { is-frozen: frozen })
    )
    
    (ok frozen)
  )
)

;; Emergency pause contract (owner only)
(define-public (set-contract-paused (paused bool))
  (begin
    (asserts! (is-contract-owner) ERR-OWNER-ONLY)
    (var-set contract-paused paused)
    (ok paused)
  )
)

;; Read-only functions
(define-read-only (get-wallet-details (wallet-id uint))
  (map-get? wallets { wallet-id: wallet-id })
)

(define-read-only (get-transaction-details (tx-id uint))
  (map-get? pending-transactions { tx-id: tx-id })
)

(define-read-only (get-user-wallets (user principal))
  (default-to { wallet-ids: (list) } (map-get? user-wallets { user: user }))
)

(define-read-only (is-authorized-signer (wallet-id uint) (user principal))
  (is-wallet-signer wallet-id user)
)

(define-read-only (get-contract-info)
  {
    next-wallet-id: (var-get next-wallet-id),
    next-tx-id: (var-get next-tx-id),
    contract-paused: (var-get contract-paused),
    contract-owner: CONTRACT-OWNER
  }
)