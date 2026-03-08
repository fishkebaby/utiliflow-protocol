;; UtiliFlow Protocol - Decentralized Utilities Optimization

;; =============================================================
;; CONSTANTS
;; =============================================================

(define-constant CONTRACT-OWNER tx-sender)

(define-constant ERR-NOT-AUTHORIZED        (err u100))
(define-constant ERR-ALREADY-REGISTERED   (err u101))
(define-constant ERR-NOT-REGISTERED       (err u102))
(define-constant ERR-INSUFFICIENT-BALANCE (err u103))
(define-constant ERR-INVALID-AMOUNT       (err u104))
(define-constant ERR-POOL-NOT-FOUND       (err u105))
(define-constant ERR-TRADE-NOT-FOUND      (err u106))
(define-constant ERR-ORACLE-NOT-TRUSTED   (err u107))
(define-constant ERR-SELF-TRADE           (err u108))
(define-constant ERR-TRADE-CLOSED         (err u109))
(define-constant ERR-INVALID-RESOURCE     (err u110))
(define-constant ERR-INVALID-PRINCIPAL    (err u111))

;; Optimization fee: 2% of savings (200 basis points)
(define-constant OPTIMIZATION-FEE-BPS u200)
(define-constant BPS-DENOMINATOR u10000)

;; Resource type identifiers
(define-constant RESOURCE-ENERGY u1)
(define-constant RESOURCE-WATER  u2)
(define-constant RESOURCE-WASTE  u3)

;; =============================================================
;; FUNGIBLE TOKENS
;; =============================================================

;; FLOW: governance token with a fixed cap of 100 million
(define-fungible-token FLOW u100000000)

;; UTIL: utility credits, uncapped (minted on resource contribution)
(define-fungible-token UTIL)

;; =============================================================
;; DATA MAPS AND VARS
;; =============================================================

;; Trusted demand-response oracle addresses
(define-map trusted-oracles principal bool)

;; Property registry
;; property-id -> metadata
(define-map properties
  { property-id: uint }
  {
    owner:         principal,
    meter-hash:    (buff 32),  ;; hash of IoT meter device ID
    resource-type: uint,       ;; RESOURCE-ENERGY | RESOURCE-WATER | RESOURCE-WASTE
    active:        bool
  }
)

;; UTIL credit balances per property
(define-map property-util-balance
  { property-id: uint }
  { balance: uint }
)

;; Community resource pools
;; pool-id -> pool state
(define-map resource-pools
  { pool-id: uint }
  {
    resource-type:   uint,
    total-deposited: uint,
    total-withdrawn: uint,
    member-count:    uint,
    active:          bool
  }
)

;; Pool memberships
(define-map pool-members
  { pool-id: uint, property-id: uint }
  { contribution: uint }
)

;; Peer-to-peer trade offers
;; trade-id -> offer
(define-map trade-offers
  { trade-id: uint }
  {
    seller-property: uint,
    buyer-property:  uint,             ;; 0 = open offer
    offer-resource:  uint,
    offer-amount:    uint,             ;; in UTIL
    ask-resource:    uint,
    ask-amount:      uint,             ;; in UTIL
    status:          (string-ascii 10) ;; "open" | "filled" | "cancelled"
  }
)

;; Oracle demand-spike reports
;; (report-block, resource-type) -> predicted load factor (scaled x100)
;; Note: key uses report-block to avoid clash with reserved word at-block
(define-map demand-reports
  { report-block: uint, resource-type: uint }
  {
    oracle:       principal,
    load-factor:  uint,   ;; e.g. 150 = 1.5x normal load
    submitted-at: uint
  }
)

;; Protocol revenue accumulator (in UTIL)
(define-data-var protocol-revenue uint u0)

;; Auto-increment counters
(define-data-var next-property-id uint u1)
(define-data-var next-pool-id     uint u1)
(define-data-var next-trade-id    uint u1)

;; =============================================================
;; PRIVATE HELPERS
;; =============================================================

(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT-OWNER)
)

(define-private (is-trusted-oracle (who principal))
  (default-to false (map-get? trusted-oracles who))
)

(define-private (is-valid-resource (resource-type uint))
  (or (is-eq resource-type RESOURCE-ENERGY)
      (or (is-eq resource-type RESOURCE-WATER)
          (is-eq resource-type RESOURCE-WASTE)))
)

(define-private (get-property-owner (property-id uint))
  (match (map-get? properties { property-id: property-id })
    entry (some (get owner entry))
    none
  )
)

(define-private (is-property-owner (property-id uint) (who principal))
  (match (get-property-owner property-id)
    owner (is-eq owner who)
    false
  )
)

(define-private (get-util-balance (property-id uint))
  (default-to u0
    (get balance (map-get? property-util-balance { property-id: property-id }))
  )
)

(define-private (set-util-balance (property-id uint) (new-balance uint))
  (map-set property-util-balance
    { property-id: property-id }
    { balance: new-balance }
  )
)

;; Calculate optimization fee from gross savings amount
(define-private (calc-fee (savings uint))
  (/ (* savings OPTIMIZATION-FEE-BPS) BPS-DENOMINATOR)
)

;; =============================================================
;; ORACLE MANAGEMENT (owner only)
;; =============================================================

(define-public (add-oracle (oracle principal))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    ;; Guard: reject self-assignment and obvious invalid principals
    (asserts! (not (is-eq oracle CONTRACT-OWNER)) ERR-INVALID-PRINCIPAL)
    (map-set trusted-oracles oracle true)
    (ok true)
  )
)

(define-public (remove-oracle (oracle principal))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (asserts! (not (is-eq oracle CONTRACT-OWNER)) ERR-INVALID-PRINCIPAL)
    (map-delete trusted-oracles oracle)
    (ok true)
  )
)

;; =============================================================
;; FLOW TOKEN MANAGEMENT
;; =============================================================

;; Mint FLOW governance tokens (owner only, subject to cap)
(define-public (mint-flow (recipient principal) (amount uint))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    ;; Guard: recipient must differ from owner to prevent circular minting
    (asserts! (not (is-eq recipient CONTRACT-OWNER)) ERR-INVALID-PRINCIPAL)
    (ft-mint? FLOW amount recipient)
  )
)

;; Transfer FLOW tokens between principals
(define-public (transfer-flow (amount uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    ;; Guard: no self-transfer
    (asserts! (not (is-eq sender recipient)) ERR-SELF-TRADE)
    (ft-transfer? FLOW amount sender recipient)
  )
)

;; =============================================================
;; PROPERTY REGISTRATION
;; =============================================================

(define-public (register-property (meter-hash (buff 32)) (resource-type uint))
  (let
    (
      (property-id (var-get next-property-id))
    )
    ;; Guard: meter-hash must not be the zero buffer
    (asserts! (not (is-eq meter-hash 0x0000000000000000000000000000000000000000000000000000000000000000))
              ERR-INVALID-AMOUNT)
    ;; Guard: resource-type must be a recognized constant
    (asserts! (is-valid-resource resource-type) ERR-INVALID-RESOURCE)
    (map-set properties
      { property-id: property-id }
      {
        owner:         tx-sender,
        meter-hash:    meter-hash,
        resource-type: resource-type,
        active:        true
      }
    )
    (set-util-balance property-id u0)
    (var-set next-property-id (+ property-id u1))
    (ok property-id)
  )
)

(define-public (deactivate-property (property-id uint))
  (begin
    (asserts! (is-property-owner property-id tx-sender) ERR-NOT-AUTHORIZED)
    (match (map-get? properties { property-id: property-id })
      entry
        (begin
          (map-set properties { property-id: property-id }
            (merge entry { active: false }))
          (ok true)
        )
      ERR-NOT-REGISTERED
    )
  )
)

;; =============================================================
;; UTIL CREDIT MINTING (oracle-driven resource contribution)
;; =============================================================

;; Trusted oracles call this when a property contributes verified resources.
;; amount-util: gross UTIL to credit; savings-util: savings vs baseline for fee calc.
(define-public (record-contribution
    (property-id uint)
    (amount-util uint)
    (savings-util uint))
  (let
    (
      (prop     (unwrap! (map-get? properties { property-id: property-id }) ERR-NOT-REGISTERED))
      (owner    (get owner prop))
      (fee      (calc-fee savings-util))
      (net-util (- amount-util fee))
    )
    (asserts! (is-trusted-oracle tx-sender) ERR-ORACLE-NOT-TRUSTED)
    ;; Guard: property must be active
    (asserts! (get active prop) ERR-NOT-REGISTERED)
    (asserts! (> amount-util u0) ERR-INVALID-AMOUNT)
    (asserts! (>= amount-util fee) ERR-INVALID-AMOUNT)
    ;; Mint net UTIL to the verified property owner
    (try! (ft-mint? UTIL net-util owner))
    (set-util-balance property-id (+ (get-util-balance property-id) net-util))
    ;; Accumulate protocol fee
    (try! (ft-mint? UTIL fee CONTRACT-OWNER))
    (var-set protocol-revenue (+ (var-get protocol-revenue) fee))
    (ok net-util)
  )
)

;; =============================================================
;; DEMAND RESPONSE ORACLE SUBMISSIONS
;; =============================================================

(define-public (submit-demand-report
    (resource-type uint)
    (load-factor uint))     ;; scaled x100, e.g. 150 = 1.5x
  (begin
    (asserts! (is-trusted-oracle tx-sender) ERR-ORACLE-NOT-TRUSTED)
    ;; Guard: resource-type must be valid before writing to map
    (asserts! (is-valid-resource resource-type) ERR-INVALID-RESOURCE)
    (asserts! (> load-factor u0) ERR-INVALID-AMOUNT)
    (map-set demand-reports
      { report-block: block-height, resource-type: resource-type }
      {
        oracle:       tx-sender,
        load-factor:  load-factor,
        submitted-at: block-height
      }
    )
    (ok true)
  )
)

;; Query a demand report by block height and resource type.
;; Parameter named query-block to avoid reserved keyword at-block.
(define-read-only (get-demand-report (query-block uint) (resource-type uint))
  (map-get? demand-reports { report-block: query-block, resource-type: resource-type })
)

;; =============================================================
;; COMMUNITY RESOURCE POOLS
;; =============================================================

(define-public (create-pool (resource-type uint))
  (let
    (
      (pool-id (var-get next-pool-id))
    )
    ;; Guard: resource-type must be a recognized constant
    (asserts! (is-valid-resource resource-type) ERR-INVALID-RESOURCE)
    (map-set resource-pools
      { pool-id: pool-id }
      {
        resource-type:   resource-type,
        total-deposited: u0,
        total-withdrawn: u0,
        member-count:    u0,
        active:          true
      }
    )
    (var-set next-pool-id (+ pool-id u1))
    (ok pool-id)
  )
)

;; Deposit UTIL credits into a community pool
(define-public (deposit-to-pool
    (pool-id uint)
    (property-id uint)
    (amount uint))
  (let
    (
      (pool    (unwrap! (map-get? resource-pools { pool-id: pool-id }) ERR-POOL-NOT-FOUND))
      (cur-bal (get-util-balance property-id))
      (cur-mem (default-to u0
                  (get contribution
                    (map-get? pool-members { pool-id: pool-id, property-id: property-id }))))
    )
    (asserts! (is-property-owner property-id tx-sender) ERR-NOT-AUTHORIZED)
    ;; Guard: pool must be active
    (asserts! (get active pool) ERR-POOL-NOT-FOUND)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (>= cur-bal amount) ERR-INSUFFICIENT-BALANCE)
    ;; Deduct from property balance
    (set-util-balance property-id (- cur-bal amount))
    ;; Update pool totals
    (map-set resource-pools { pool-id: pool-id }
      (merge pool {
        total-deposited: (+ (get total-deposited pool) amount),
        member-count: (if (is-eq cur-mem u0)
                        (+ (get member-count pool) u1)
                        (get member-count pool))
      })
    )
    ;; Update membership contribution
    (map-set pool-members
      { pool-id: pool-id, property-id: property-id }
      { contribution: (+ cur-mem amount) }
    )
    (ok true)
  )
)

;; Withdraw UTIL credits from a community pool (capped at contributed amount)
(define-public (withdraw-from-pool
    (pool-id uint)
    (property-id uint)
    (amount uint))
  (let
    (
      (pool        (unwrap! (map-get? resource-pools { pool-id: pool-id }) ERR-POOL-NOT-FOUND))
      (membership  (unwrap! (map-get? pool-members { pool-id: pool-id, property-id: property-id })
                            ERR-NOT-REGISTERED))
      (contributed (get contribution membership))
      ;; Cap withdrawal at the member's contributed balance
      (available   (if (<= amount contributed) amount contributed))
    )
    (asserts! (is-property-owner property-id tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (> available u0) ERR-INSUFFICIENT-BALANCE)
    ;; Update membership
    (map-set pool-members
      { pool-id: pool-id, property-id: property-id }
      { contribution: (- contributed available) }
    )
    ;; Update pool totals
    (map-set resource-pools { pool-id: pool-id }
      (merge pool { total-withdrawn: (+ (get total-withdrawn pool) available) })
    )
    ;; Return UTIL to property
    (set-util-balance property-id (+ (get-util-balance property-id) available))
    (ok available)
  )
)

;; =============================================================
;; PEER-TO-PEER CROSS-UTILITY ARBITRAGE TRADES
;; =============================================================

;; Post an open trade offer (buyer-property = 0 means open market)
(define-public (post-trade-offer
    (seller-property uint)
    (offer-resource  uint)
    (offer-amount    uint)
    (ask-resource    uint)
    (ask-amount      uint))
  (let
    (
      (trade-id (var-get next-trade-id))
    )
    (asserts! (is-property-owner seller-property tx-sender) ERR-NOT-AUTHORIZED)
    ;; Guard: resource types must be valid constants before storing
    (asserts! (is-valid-resource offer-resource) ERR-INVALID-RESOURCE)
    (asserts! (is-valid-resource ask-resource)   ERR-INVALID-RESOURCE)
    (asserts! (> offer-amount u0) ERR-INVALID-AMOUNT)
    (asserts! (> ask-amount u0)   ERR-INVALID-AMOUNT)
    (asserts! (>= (get-util-balance seller-property) offer-amount) ERR-INSUFFICIENT-BALANCE)
    ;; Lock seller's UTIL for the duration of the offer
    (set-util-balance seller-property
      (- (get-util-balance seller-property) offer-amount))
    (map-set trade-offers
      { trade-id: trade-id }
      {
        seller-property: seller-property,
        buyer-property:  u0,
        offer-resource:  offer-resource,
        offer-amount:    offer-amount,
        ask-resource:    ask-resource,
        ask-amount:      ask-amount,
        status:          "open"
      }
    )
    (var-set next-trade-id (+ trade-id u1))
    (ok trade-id)
  )
)

;; Fill an open trade offer
(define-public (fill-trade-offer
    (trade-id       uint)
    (buyer-property uint))
  (let
    (
      (trade      (unwrap! (map-get? trade-offers { trade-id: trade-id }) ERR-TRADE-NOT-FOUND))
      (seller-id  (get seller-property trade))
      (ask-amount (get ask-amount trade))
      (offer-amt  (get offer-amount trade))
    )
    (asserts! (is-property-owner buyer-property tx-sender) ERR-NOT-AUTHORIZED)
    ;; Guard: trade must still be open
    (asserts! (is-eq (get status trade) "open") ERR-TRADE-CLOSED)
    ;; Guard: buyer cannot fill their own offer
    (asserts! (not (is-eq buyer-property seller-id)) ERR-SELF-TRADE)
    (asserts! (>= (get-util-balance buyer-property) ask-amount) ERR-INSUFFICIENT-BALANCE)
    ;; Transfer buyer's ask-amount to seller
    (set-util-balance buyer-property
      (- (get-util-balance buyer-property) ask-amount))
    (set-util-balance seller-id
      (+ (get-util-balance seller-id) ask-amount))
    ;; Release locked offer-amount to buyer
    (set-util-balance buyer-property
      (+ (get-util-balance buyer-property) offer-amt))
    ;; Mark trade filled
    (map-set trade-offers { trade-id: trade-id }
      (merge trade {
        buyer-property: buyer-property,
        status: "filled"
      })
    )
    (ok true)
  )
)

;; Cancel an open trade offer and unlock funds
(define-public (cancel-trade-offer
    (trade-id        uint)
    (seller-property uint))
  (let
    (
      (trade     (unwrap! (map-get? trade-offers { trade-id: trade-id }) ERR-TRADE-NOT-FOUND))
      (offer-amt (get offer-amount trade))
    )
    (asserts! (is-property-owner seller-property tx-sender) ERR-NOT-AUTHORIZED)
    ;; Guard: caller must be the original seller
    (asserts! (is-eq (get seller-property trade) seller-property) ERR-NOT-AUTHORIZED)
    ;; Guard: trade must still be open
    (asserts! (is-eq (get status trade) "open") ERR-TRADE-CLOSED)
    ;; Refund locked UTIL to seller
    (set-util-balance seller-property
      (+ (get-util-balance seller-property) offer-amt))
    (map-set trade-offers { trade-id: trade-id }
      (merge trade { status: "cancelled" })
    )
    (ok true)
  )
)

;; =============================================================
;; READ-ONLY QUERIES
;; =============================================================

(define-read-only (get-property (property-id uint))
  (map-get? properties { property-id: property-id })
)

(define-read-only (get-property-balance (property-id uint))
  (ok (get-util-balance property-id))
)

(define-read-only (get-pool (pool-id uint))
  (map-get? resource-pools { pool-id: pool-id })
)

(define-read-only (get-pool-membership (pool-id uint) (property-id uint))
  (map-get? pool-members { pool-id: pool-id, property-id: property-id })
)

(define-read-only (get-trade (trade-id uint))
  (map-get? trade-offers { trade-id: trade-id })
)

(define-read-only (get-protocol-revenue)
  (ok (var-get protocol-revenue))
)

(define-read-only (get-flow-balance (who principal))
  (ok (ft-get-balance FLOW who))
)

(define-read-only (get-util-total-supply)
  (ok (ft-get-supply UTIL))
)
