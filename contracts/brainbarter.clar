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
