;; Mental Health Wellness Tracker Contract
;; Match patients with therapists anonymously, track session completion,
;; reward participation with tokens, maintain encrypted progress records

;; ===== CONSTANTS =====
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))
(define-constant ERR_ALREADY_EXISTS (err u103))
(define-constant ERR_INVALID_STATUS (err u104))
(define-constant ERR_SESSION_COMPLETED (err u105))
(define-constant ERR_INVALID_RATING (err u106))
(define-constant ERR_CRISIS_ACTIVE (err u107))

;; Token rewards
(define-constant SESSION_REWARD u100)
(define-constant MILESTONE_REWARD u50)
(define-constant PEER_SUPPORT_REWARD u25)
(define-constant CRISIS_SUPPORT_REWARD u75)

;; Session durations (in blocks)
(define-constant MIN_SESSION_DURATION u72) ;; ~12 hours
(define-constant CRISIS_RESPONSE_TIME u12) ;; ~2 hours

;; ===== DATA STRUCTURES =====

;; Anonymous patient profiles
(define-map patients
  { patient-id: uint }
  {
    anonymous-hash: (buff 32),
    total-sessions: uint,
    completed-sessions: uint,
    wellness-score: uint,
    token-balance: uint,
    crisis-status: bool,
    last-activity: uint,
    therapist-preference: (string-ascii 50)
  }
)

;; Verified therapist profiles
(define-map therapists
  { therapist-id: uint }
  {
    credential-hash: (buff 32),
    specialization: (string-ascii 100),
    total-sessions: uint,
    avg-rating: uint,
    rating-count: uint,
    is-available: bool,
    token-earnings: uint,
    verification-status: bool
  }
)

;; Therapy session tracking
(define-map sessions
  { session-id: uint }
  {
    patient-id: uint,
    therapist-id: uint,
    session-type: (string-ascii 20), ;; "individual", "group", "crisis"
    start-block: uint,
    end-block: (optional uint),
    status: (string-ascii 20), ;; "scheduled", "active", "completed", "cancelled"
    progress-notes: (buff 256), ;; Encrypted
    patient-rating: (optional uint),
    therapist-rating: (optional uint)
  }
)

;; Wellness milestones and goals
(define-map milestones
  { patient-id: uint, milestone-id: uint }
  {
    goal-description: (string-ascii 200),
    target-value: uint,
    current-value: uint,
    achieved: bool,
    reward-claimed: bool,
    created-block: uint,
    achieved-block: (optional uint)
  }
)

;; Crisis intervention records
(define-map crisis-interventions
  { crisis-id: uint }
  {
    patient-id: uint,
    severity-level: uint, ;; 1-5 scale
    response-time: uint,
    intervention-type: (string-ascii 50),
    resolved: bool,
    responder-id: (optional uint),
    resolution-block: (optional uint)
  }
)

;; ===== DATA VARIABLES =====
(define-data-var next-patient-id uint u1)
(define-data-var next-therapist-id uint u1)
(define-data-var next-session-id uint u1)
(define-data-var next-crisis-id uint u1)
(define-data-var total-tokens-distributed uint u0)
(define-data-var platform-wellness-score uint u0)

;; ===== PRIVATE FUNCTIONS =====

;; Generate anonymous patient ID
(define-private (generate-patient-hash (patient principal))
  (sha256 (concat (unwrap-panic (to-consensus-buff? patient)) (unwrap-panic (to-consensus-buff? stacks-block-height))))
)

;; Calculate wellness score based on session completion and goals
(define-private (calculate-wellness-score (completed-sessions uint) (total-sessions uint) (achieved-milestones uint))
  (let (
    (completion-rate (if (> total-sessions u0) (/ (* completed-sessions u100) total-sessions) u0))
    (milestone-bonus (* achieved-milestones u10))
  )
    (+ completion-rate milestone-bonus)
  )
)

;; Match patient with therapist based on preferences
(define-private (find-compatible-therapist (patient-id uint) (preference (string-ascii 50)))
  ;; Simplified matching - in production would use more sophisticated algorithm
  u1 ;; Returns therapist-id 1 for demo
)

;; ===== PUBLIC FUNCTIONS =====

;; Register as patient (anonymous)
(define-public (register-patient (preference (string-ascii 50)))
  (let (
    (patient-id (var-get next-patient-id))
    (anonymous-hash (generate-patient-hash tx-sender))
  )
    (map-set patients
      { patient-id: patient-id }
      {
        anonymous-hash: anonymous-hash,
        total-sessions: u0,
        completed-sessions: u0,
        wellness-score: u0,
        token-balance: u0,
        crisis-status: false,
        last-activity: stacks-block-height,
        therapist-preference: preference
      }
    )
    
    (var-set next-patient-id (+ patient-id u1))
    (ok patient-id)
  )
)

;; Register as therapist (verified)
(define-public (register-therapist (credential-hash (buff 32)) (specialization (string-ascii 100)))
  (let (
    (therapist-id (var-get next-therapist-id))
  )
    (map-set therapists
      { therapist-id: therapist-id }
      {
        credential-hash: credential-hash,
        specialization: specialization,
        total-sessions: u0,
        avg-rating: u0,
        rating-count: u0,
        is-available: true,
        token-earnings: u0,
        verification-status: false ;; Requires admin verification
      }
    )
    
    (var-set next-therapist-id (+ therapist-id u1))
    (ok therapist-id)
  )
)

;; Schedule therapy session
(define-public (schedule-session (patient-id uint) (session-type (string-ascii 20)))
  (let (
    (session-id (var-get next-session-id))
    (patient (unwrap! (map-get? patients { patient-id: patient-id }) ERR_NOT_FOUND))
    (therapist-id (find-compatible-therapist patient-id (get therapist-preference patient)))
  )
    (map-set sessions
      { session-id: session-id }
      {
        patient-id: patient-id,
        therapist-id: therapist-id,
        session-type: session-type,
        start-block: stacks-block-height,
        end-block: none,
        status: "scheduled",
        progress-notes: 0x00, ;; Empty encrypted notes
        patient-rating: none,
        therapist-rating: none
      }
    )
    
    ;; Update patient's session count
    (map-set patients
      { patient-id: patient-id }
      (merge patient {
        total-sessions: (+ (get total-sessions patient) u1),
        last-activity: stacks-block-height
      })
    )
    
    (var-set next-session-id (+ session-id u1))
    (ok session-id)
  )
)

;; Start therapy session
(define-public (start-session (session-id uint))
  (let (
    (session (unwrap! (map-get? sessions { session-id: session-id }) ERR_NOT_FOUND))
  )
    (asserts! (is-eq (get status session) "scheduled") ERR_INVALID_STATUS)
    
    (map-set sessions
      { session-id: session-id }
      (merge session {
        status: "active",
        start-block: stacks-block-height
      })
    )
    
    (ok "Session started")
  )
)

;; Complete therapy session and distribute tokens
(define-public (complete-session (session-id uint) (progress-notes (buff 256)))
  (let (
    (session (unwrap! (map-get? sessions { session-id: session-id }) ERR_NOT_FOUND))
    (patient-id (get patient-id session))
    (therapist-id (get therapist-id session))
    (patient (unwrap! (map-get? patients { patient-id: patient-id }) ERR_NOT_FOUND))
    (therapist (unwrap! (map-get? therapists { therapist-id: therapist-id }) ERR_NOT_FOUND))
  )
    (asserts! (is-eq (get status session) "active") ERR_INVALID_STATUS)
    (asserts! (>= (- stacks-block-height (get start-block session)) MIN_SESSION_DURATION) ERR_INVALID_AMOUNT)
    
    ;; Complete session
    (map-set sessions
      { session-id: session-id }
      (merge session {
        status: "completed",
        end-block: (some stacks-block-height),
        progress-notes: progress-notes
      })
    )
    
    ;; Reward patient with tokens
    (map-set patients
      { patient-id: patient-id }
      (merge patient {
        completed-sessions: (+ (get completed-sessions patient) u1),
        token-balance: (+ (get token-balance patient) SESSION_REWARD),
        last-activity: stacks-block-height
      })
    )
    
    ;; Reward therapist with tokens
    (map-set therapists
      { therapist-id: therapist-id }
      (merge therapist {
        total-sessions: (+ (get total-sessions therapist) u1),
        token-earnings: (+ (get token-earnings therapist) SESSION_REWARD)
      })
    )
    
    ;; Update global statistics
    (var-set total-tokens-distributed (+ (var-get total-tokens-distributed) (* SESSION_REWARD u2)))
    
    (ok SESSION_REWARD)
  )
)

;; Rate session (mutual rating system)
(define-public (rate-session (session-id uint) (rating uint) (is-patient bool))
  (let (
    (session (unwrap! (map-get? sessions { session-id: session-id }) ERR_NOT_FOUND))
  )
    (asserts! (and (>= rating u1) (<= rating u5)) ERR_INVALID_RATING)
    (asserts! (is-eq (get status session) "completed") ERR_INVALID_STATUS)
    
    (if is-patient
      ;; Patient rating therapist
      (let (
        (therapist-id (get therapist-id session))
        (therapist (unwrap! (map-get? therapists { therapist-id: therapist-id }) ERR_NOT_FOUND))
        (new-rating-count (+ (get rating-count therapist) u1))
        (new-total-rating (+ (* (get avg-rating therapist) (get rating-count therapist)) rating))
        (new-avg-rating (/ new-total-rating new-rating-count))
      )
        (map-set sessions
          { session-id: session-id }
          (merge session { patient-rating: (some rating) })
        )
        
        (map-set therapists
          { therapist-id: therapist-id }
          (merge therapist {
            avg-rating: new-avg-rating,
            rating-count: new-rating-count
          })
        )
        
        (ok "Therapist rated")
      )
      ;; Therapist rating patient progress
      (begin
        (map-set sessions
          { session-id: session-id }
          (merge session { therapist-rating: (some rating) })
        )
        (ok "Patient progress rated")
      )
    )
  )
)

;; Set wellness milestone
(define-public (set-milestone (patient-id uint) (description (string-ascii 200)) (target-value uint))
  (let (
    (patient (unwrap! (map-get? patients { patient-id: patient-id }) ERR_NOT_FOUND))
    (milestone-id (get total-sessions patient)) ;; Use session count as milestone ID
  )
    (map-set milestones
      { patient-id: patient-id, milestone-id: milestone-id }
      {
        goal-description: description,
        target-value: target-value,
        current-value: u0,
        achieved: false,
        reward-claimed: false,
        created-block: stacks-block-height,
        achieved-block: none
      }
    )
    
    (ok milestone-id)
  )
)

;; Update milestone progress
(define-public (update-milestone-progress (patient-id uint) (milestone-id uint) (current-value uint))
  (let (
    (milestone (unwrap! (map-get? milestones { patient-id: patient-id, milestone-id: milestone-id }) ERR_NOT_FOUND))
    (achieved (>= current-value (get target-value milestone)))
  )
    (map-set milestones
      { patient-id: patient-id, milestone-id: milestone-id }
      (merge milestone {
        current-value: current-value,
        achieved: achieved,
        achieved-block: (if achieved (some stacks-block-height) none)
      })
    )
    
    ;; Reward milestone achievement
    (if (and achieved (not (get reward-claimed milestone)))
      (let (
        (patient (unwrap! (map-get? patients { patient-id: patient-id }) ERR_NOT_FOUND))
      )
        (map-set patients
          { patient-id: patient-id }
          (merge patient {
            token-balance: (+ (get token-balance patient) MILESTONE_REWARD),
            wellness-score: (calculate-wellness-score 
                           (get completed-sessions patient)
                           (get total-sessions patient)
                           u1)
          })
        )
        
        (map-set milestones
          { patient-id: patient-id, milestone-id: milestone-id }
          (merge milestone { reward-claimed: true })
        )
        
        (ok MILESTONE_REWARD)
      )
      (ok u0)
    )
  )
)

;; Trigger crisis intervention
(define-public (trigger-crisis-support (patient-id uint) (severity-level uint))
  (let (
    (crisis-id (var-get next-crisis-id))
    (patient (unwrap! (map-get? patients { patient-id: patient-id }) ERR_NOT_FOUND))
  )
    (asserts! (and (>= severity-level u1) (<= severity-level u5)) ERR_INVALID_AMOUNT)
    
    ;; Create crisis intervention record
    (map-set crisis-interventions
      { crisis-id: crisis-id }
      {
        patient-id: patient-id,
        severity-level: severity-level,
        response-time: stacks-block-height,
        intervention-type: "immediate",
        resolved: false,
        responder-id: none,
        resolution-block: none
      }
    )
    
    ;; Update patient crisis status
    (map-set patients
      { patient-id: patient-id }
      (merge patient {
        crisis-status: true,
        last-activity: stacks-block-height
      })
    )
    
    (var-set next-crisis-id (+ crisis-id u1))
    (ok crisis-id)
  )
)

;; ===== ADMIN FUNCTIONS =====

;; Verify therapist credentials (admin only)
(define-public (verify-therapist (therapist-id uint))
  (let (
    (therapist (unwrap! (map-get? therapists { therapist-id: therapist-id }) ERR_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    
    (map-set therapists
      { therapist-id: therapist-id }
      (merge therapist { verification-status: true })
    )
    
    (ok "Therapist verified")
  )
)

;; ===== READ-ONLY FUNCTIONS =====

;; Get patient profile
(define-read-only (get-patient (patient-id uint))
  (map-get? patients { patient-id: patient-id })
)

;; Get therapist profile
(define-read-only (get-therapist (therapist-id uint))
  (map-get? therapists { therapist-id: therapist-id })
)

;; Get session details
(define-read-only (get-session (session-id uint))
  (map-get? sessions { session-id: session-id })
)

;; Get platform statistics
(define-read-only (get-platform-stats)
  (ok {
    total-patients: (var-get next-patient-id),
    total-therapists: (var-get next-therapist-id),
    total-sessions: (var-get next-session-id),
    tokens-distributed: (var-get total-tokens-distributed),
    platform-wellness-score: (var-get platform-wellness-score)
  })
)

;; title: wellness-tracker
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

