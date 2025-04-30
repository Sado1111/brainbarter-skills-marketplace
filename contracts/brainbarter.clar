;; BrainBarter - Skills Trading Network
;; Participants can register hours of expertise, offer them for exchange, and purchase expertise from others


;; =================================================================
;; SYSTEM PARAMETERS
;; =================================================================

;; Platform economics configuration
(define-data-var hourly-compensation uint u10) ;; Base compensation per hour (in microstacks)
(define-data-var platform-fee-percentage uint u10) ;; Platform commission (percentage)
(define-data-var expertise-capacity-global uint u1000) ;; Platform-wide limit on available expertise hours
(define-data-var expertise-allocation-per-user uint u100) ;; Maximum hours each user can offer

;; System state tracking
(define-data-var total-expertise-available uint u0) ;; Current amount of expertise hours in the system
(define-data-var collaboration-session-counter uint u0) ;; Counter for group collaboration sessions

;; =================================================================
;; CORE DATA STORAGE
;; =================================================================

;; User account data
(define-map user-expertise-holdings principal uint) ;; User's expertise balance in hours
(define-map user-financial-holdings principal uint) ;; User's cryptocurrency balance

;; Marketplace listings
(define-map expertise-marketplace {expert: principal} {hours-available: uint, compensation-rate: uint})

;; Premium and specialized offerings
(define-map certified-experts principal bool)
(define-map premium-expertise-marketplace {expert: principal} {hours-available: uint, compensation-rate: uint, certification-status: bool})

;; Bundled expertise packages
(define-map expertise-bundle-offers {expert: principal} {hours-available: uint, compensation-rate: uint, value-discount: uint})

;; Reputation system
(define-map expert-evaluations {expert: principal, evaluator: principal} uint)
(define-map expert-reputation principal {cumulative-score: uint, evaluation-count: uint})

;; Group collaboration framework
(define-map collaboration-sessions uint {organizer: principal, members: (list 10 principal), session-hours: uint, compensation-rate: uint, session-status: (string-ascii 20)})

;; =================================================================
;; GLOBAL CONFIGURATION AND INITIALIZATION
;; =================================================================

;; Platform administrator (set at deployment)
(define-constant admin-address tx-sender)

;; System error codes for validation and access control
(define-constant error-admin-restricted (err u200))
(define-constant error-funds-shortage (err u201))
(define-constant error-expertise-invalid (err u202))
(define-constant error-compensation-invalid (err u203))
(define-constant error-system-capacity-reached (err u204))
(define-constant error-operation-forbidden (err u205))
(define-constant error-parameter-invalid (err u206))
(define-constant error-fee-limit-exceeded (err u208))
(define-constant error-allocation-zero (err u209))
(define-constant error-capacity-reduction (err u210))
(define-constant error-verification-required (err u211))
(define-constant error-rating-too-low (err u212))
(define-constant error-rating-too-high (err u213))
(define-constant error-discount-too-low (err u214))
(define-constant error-discount-too-high (err u215))

;; =================================================================
;; INTERNAL HELPER FUNCTIONS
;; =================================================================

;; Calculate platform commission for a transaction
(define-private (compute-platform-commission (transaction-value uint))
  (/ (* transaction-value (var-get platform-fee-percentage)) u100))

;; Track system-wide expertise availability
(define-private (adjust-expertise-inventory (adjustment-amount int))
  (let (
    (current-inventory (var-get total-expertise-available))
    (adjusted-inventory (if (< adjustment-amount 0)
                     (if (>= current-inventory (to-uint (- 0 adjustment-amount)))
                         (- current-inventory (to-uint (- 0 adjustment-amount)))
                         u0)
                     (+ current-inventory (to-uint adjustment-amount))))
  )
    (asserts! (<= adjusted-inventory (var-get expertise-capacity-global)) error-system-capacity-reached)
    (var-set total-expertise-available adjusted-inventory)
    (ok true)))

;; =================================================================
;; CORE PLATFORM OPERATIONS
;; =================================================================

;; Register new expertise hours to user's account
(define-public (register-expertise-hours (hours uint))
  (let (
    (participant tx-sender)
    (current-expertise (default-to u0 (map-get? user-expertise-holdings participant)))
    (max-allocation (var-get expertise-allocation-per-user))
    (registration-cost (* hours (var-get hourly-compensation)))
    (participant-funds (default-to u0 (map-get? user-financial-holdings participant)))
  )
    (asserts! (> hours u0) error-expertise-invalid) ;; Hours must be positive
    (asserts! (<= (+ current-expertise hours) max-allocation) error-parameter-invalid) ;; Check allocation limit
    (asserts! (>= participant-funds registration-cost) error-funds-shortage) ;; Verify sufficient funds

    ;; Update participant balances
    (map-set user-expertise-holdings participant (+ current-expertise hours))
    (map-set user-financial-holdings participant (- participant-funds registration-cost))

    ;; Credit platform administrator
    (map-set user-financial-holdings admin-address (+ (default-to u0 (map-get? user-financial-holdings admin-address)) registration-cost))

    (ok true)))

;; List expertise hours for exchange
(define-public (list-expertise-for-exchange (hours uint) (compensation-rate uint))
  (let (
    (current-expertise (default-to u0 (map-get? user-expertise-holdings tx-sender)))
    (current-listing (get hours-available (default-to {hours-available: u0, compensation-rate: u0} (map-get? expertise-marketplace {expert: tx-sender}))))
    (updated-listing (+ hours current-listing))
  )
    (asserts! (> hours u0) error-expertise-invalid) ;; Hours must be positive
    (asserts! (> compensation-rate u0) error-compensation-invalid) ;; Rate must be positive
    (asserts! (>= current-expertise updated-listing) error-funds-shortage)
    (try! (adjust-expertise-inventory (to-int hours)))
    (map-set expertise-marketplace {expert: tx-sender} {hours-available: updated-listing, compensation-rate: compensation-rate})
    (ok true)))

;; Purchase expertise from another user
(define-public (purchase-expertise (provider principal) (hours uint))
  (let (
    (listing-data (default-to {hours-available: u0, compensation-rate: u0} (map-get? expertise-marketplace {expert: provider})))
    (transaction-value (* hours (get compensation-rate listing-data)))
    (platform-commission (compute-platform-commission transaction-value))
    (total-cost (+ transaction-value platform-commission))
    (provider-expertise (default-to u0 (map-get? user-expertise-holdings provider)))
    (buyer-balance (default-to u0 (map-get? user-financial-holdings tx-sender)))
    (provider-balance (default-to u0 (map-get? user-financial-holdings provider)))
  )
    (asserts! (not (is-eq tx-sender provider)) error-operation-forbidden)
    (asserts! (> hours u0) error-expertise-invalid) ;; Hours must be positive
    (asserts! (>= (get hours-available listing-data) hours) error-funds-shortage)
    (asserts! (>= provider-expertise hours) error-funds-shortage)
    (asserts! (>= buyer-balance total-cost) error-funds-shortage)

    ;; Update provider's expertise balance and listing
    (map-set user-expertise-holdings provider (- provider-expertise hours))
    (map-set expertise-marketplace {expert: provider} 
             {hours-available: (- (get hours-available listing-data) hours), compensation-rate: (get compensation-rate listing-data)})

    ;; Update buyer's funds and expertise
    (map-set user-financial-holdings tx-sender (- buyer-balance total-cost))
    (map-set user-expertise-holdings tx-sender (+ (default-to u0 (map-get? user-expertise-holdings tx-sender)) hours))

    ;; Credit provider and platform
    (map-set user-financial-holdings provider (+ provider-balance transaction-value))
    (map-set user-financial-holdings admin-address (+ (default-to u0 (map-get? user-financial-holdings admin-address)) platform-commission))

    (ok true)))

;; =================================================================
;; PREMIUM AND CERTIFIED EXPERTISE FEATURES
;; =================================================================

;; List premium certified expertise (requires verification)
(define-public (list-certified-expertise (hours uint) (compensation-rate uint))
  (let (
    (current-expertise (default-to u0 (map-get? user-expertise-holdings tx-sender)))
    (expert-certification (default-to false (map-get? certified-experts tx-sender)))
    (current-listing (get hours-available (default-to {hours-available: u0, compensation-rate: u0} (map-get? expertise-marketplace {expert: tx-sender}))))
    (updated-listing (+ hours current-listing))
  )
    (asserts! (> hours u0) error-expertise-invalid) ;; Hours must be positive
    (asserts! (> compensation-rate u0) error-compensation-invalid) ;; Rate must be positive
    (asserts! expert-certification error-verification-required) ;; Must be certified
    (asserts! (>= current-expertise updated-listing) error-funds-shortage)
    (try! (adjust-expertise-inventory (to-int hours)))
    ;; Update standard marketplace listing
    (map-set expertise-marketplace {expert: tx-sender} {hours-available: updated-listing, compensation-rate: compensation-rate})
    ;; Also add to premium marketplace
    (map-set premium-expertise-marketplace {expert: tx-sender} {hours-available: hours, compensation-rate: compensation-rate, certification-status: true})
    (ok true)))

;; Create bundled expertise package with discount
(define-public (create-expertise-bundle (hours uint) (compensation-rate uint) (discount-percentage uint))
  (let (
    (current-expertise (default-to u0 (map-get? user-expertise-holdings tx-sender)))
    (current-listing (get hours-available (default-to {hours-available: u0, compensation-rate: u0} (map-get? expertise-marketplace {expert: tx-sender}))))
    (current-bundle (default-to {hours-available: u0, compensation-rate: u0, value-discount: u0} (map-get? expertise-bundle-offers {expert: tx-sender})))
    (updated-listing (+ hours current-listing))
    (total-bundled-hours (+ hours (get hours-available current-bundle)))
  )
    (asserts! (> hours u0) error-expertise-invalid) ;; Hours must be positive
    (asserts! (> compensation-rate u0) error-compensation-invalid) ;; Rate must be positive
    (asserts! (> discount-percentage u0) error-discount-too-low) ;; Discount must be positive
    (asserts! (<= discount-percentage u50) error-discount-too-high) ;; Maximum 50% discount
    (asserts! (>= current-expertise updated-listing) error-funds-shortage)

    ;; Update inventory tracking
    (try! (adjust-expertise-inventory (to-int hours)))

    ;; Update standard listing
    (map-set expertise-marketplace {expert: tx-sender} {hours-available: updated-listing, compensation-rate: compensation-rate})

    ;; Create or update bundle
    (map-set expertise-bundle-offers {expert: tx-sender} 
             {hours-available: total-bundled-hours, compensation-rate: compensation-rate, value-discount: discount-percentage})

    (ok true)))

;; =================================================================
;; GROUP COLLABORATION FRAMEWORK
;; =================================================================

;; Create group collaboration session
(define-public (organize-collaboration-session (participants (list 10 principal)) (hours uint) (compensation-rate uint))
  (let (
    (organizer-expertise (default-to u0 (map-get? user-expertise-holdings tx-sender)))
    (session-id (var-get collaboration-session-counter))
    (participant-count (len participants))
    (total-session-hours (* hours participant-count))
  )
    (asserts! (> hours u0) error-expertise-invalid) ;; Hours must be positive
    (asserts! (> compensation-rate u0) error-compensation-invalid) ;; Rate must be positive
    (asserts! (>= organizer-expertise total-session-hours) error-funds-shortage) ;; Check sufficient expertise

    ;; Update system inventory
    (try! (adjust-expertise-inventory (to-int total-session-hours)))

    ;; Deduct from organizer's expertise balance
    (map-set user-expertise-holdings tx-sender (- organizer-expertise total-session-hours))

    ;; Increment session counter
    (var-set collaboration-session-counter (+ session-id u1))

    (ok session-id)))

;; =================================================================
;; REPUTATION AND EVALUATION SYSTEM
;; =================================================================

;; Evaluate expertise provider
(define-public (evaluate-expert (expert principal) (rating uint))
  (let (
    (expert-data (default-to {cumulative-score: u0, evaluation-count: u0} (map-get? expert-reputation expert)))
    (current-score (get cumulative-score expert-data))
    (current-count (get evaluation-count expert-data))
    (updated-score (+ current-score rating))
    (updated-count (+ current-count u1))
  )
    (asserts! (not (is-eq tx-sender expert)) error-operation-forbidden) ;; Cannot self-evaluate
    (asserts! (>= rating u1) error-rating-too-low) ;; Minimum rating = 1
    (asserts! (<= rating u5) error-rating-too-high) ;; Maximum rating = 5

    ;; Record evaluation
    (map-set expert-evaluations {expert: expert, evaluator: tx-sender} rating)
    (map-set expert-reputation expert {cumulative-score: updated-score, evaluation-count: updated-count})

    (ok true)))

;; =================================================================
;; FINANCIAL OPERATIONS
;; =================================================================

;; Deposit funds into platform account
(define-public (deposit-funds (amount uint))
  (let (
    (current-balance (default-to u0 (map-get? user-financial-holdings tx-sender)))
    (new-balance (+ current-balance amount))
  )
    (asserts! (> amount u0) error-parameter-invalid) ;; Amount must be positive
    ;; Transfer STX to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    ;; Update user's platform balance
    (map-set user-financial-holdings tx-sender new-balance)
    (ok true)))

;; Withdraw funds from platform account
(define-public (withdraw-funds (amount uint))
  (let (
    (current-balance (default-to u0 (map-get? user-financial-holdings tx-sender)))
    (contract-liquidity (as-contract (stx-get-balance tx-sender)))
  )
    (asserts! (> amount u0) error-parameter-invalid) ;; Amount must be positive
    (asserts! (>= current-balance amount) error-funds-shortage) ;; Check user balance
    (asserts! (>= contract-liquidity amount) error-funds-shortage) ;; Check contract liquidity

    ;; Transfer STX to user
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))

    ;; Update user's platform balance
    (map-set user-financial-holdings tx-sender (- current-balance amount))

    (ok true)))

;; =================================================================
;; PLATFORM ADMINISTRATION
;; =================================================================

;; Update platform configuration parameters
(define-public (configure-platform-parameters (new-compensation-rate uint) (new-fee-percentage uint) 
                                             (new-expertise-allocation uint) (new-capacity-limit uint))
  (begin
    (asserts! (is-eq tx-sender admin-address) error-admin-restricted)
    (asserts! (> new-compensation-rate u0) error-compensation-invalid) ;; Rate must be positive
    (asserts! (<= new-fee-percentage u30) error-fee-limit-exceeded) ;; Maximum fee = 30%
    (asserts! (> new-expertise-allocation u0) error-allocation-zero) ;; Allocation must be positive
    (asserts! (>= new-capacity-limit (var-get total-expertise-available)) error-capacity-reduction) ;; Cannot reduce below current usage

    ;; Update platform parameters
    (var-set hourly-compensation new-compensation-rate)
    (var-set platform-fee-percentage new-fee-percentage)
    (var-set expertise-allocation-per-user new-expertise-allocation)
    (var-set expertise-capacity-global new-capacity-limit)

    (ok true)))

;; =================================================================
;; CANCELLATION AND REFUND OPERATIONS
;; =================================================================

;; Cancel expertise listing and return hours to user balance
(define-public (cancel-expertise-listing (hours uint))
  (let (
    (listing-data (default-to {hours-available: u0, compensation-rate: u0} (map-get? expertise-marketplace {expert: tx-sender})))
    (available-hours (get hours-available listing-data))
    (user-expertise (default-to u0 (map-get? user-expertise-holdings tx-sender)))
  )
    (asserts! (> hours u0) error-expertise-invalid) ;; Hours must be positive
    (asserts! (>= available-hours hours) error-funds-shortage) ;; Check if enough hours are listed

    ;; Update user's expertise listing
    (map-set expertise-marketplace {expert: tx-sender} {
      hours-available: (- available-hours hours),
      compensation-rate: (get compensation-rate listing-data)
    })

    ;; Update user's expertise balance
    (map-set user-expertise-holdings tx-sender user-expertise)

    ;; Handle premium listing if applicable
    (if (is-some (map-get? premium-expertise-marketplace {expert: tx-sender}))
        (let (
          (premium-data (unwrap-panic (map-get? premium-expertise-marketplace {expert: tx-sender})))
          (premium-hours (get hours-available premium-data))
        )
          (if (>= premium-hours hours)
              (map-set premium-expertise-marketplace {expert: tx-sender} {
                hours-available: (- premium-hours hours),
                compensation-rate: (get compensation-rate premium-data),
                certification-status: (get certification-status premium-data)
              })
              (map-delete premium-expertise-marketplace {expert: tx-sender})
          )
        )
        true
    )

    (ok true)))

