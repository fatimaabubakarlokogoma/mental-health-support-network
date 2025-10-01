;; Mental Health Support Utilities Contract
;; Peer networks, crisis support, wellness challenges, and community features

;; ===== CONSTANTS =====
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))
(define-constant ERR_ALREADY_EXISTS (err u103))

(define-constant PEER_SUPPORT_REWARD u25)
(define-constant WELLNESS_CHALLENGE_REWARD u50)
(define-constant CRISIS_RESPONSE_REWARD u75)

;; ===== DATA STRUCTURES =====

;; Peer support groups
(define-map support-groups
  { group-id: uint }
  {
    group-name: (string-ascii 100),
    group-type: (string-ascii 50), ;; "depression", "anxiety", "addiction", etc.
    member-count: uint,
    created-block: uint,
    is-active: bool
  }
)

;; Group memberships
(define-map group-memberships
  { group-id: uint, patient-id: uint }
  {
    join-block: uint,
    contribution-count: uint,
    tokens-earned: uint,
    is-moderator: bool
  }
)

;; Wellness challenges
(define-map wellness-challenges
  { challenge-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 300),
    duration-blocks: uint,
    reward-amount: uint,
    participant-count: uint,
    start-block: uint,
    end-block: uint,
    challenge-type: (string-ascii 50) ;; "daily", "weekly", "monthly"
  }
)

;; Challenge participations
(define-map challenge-participations
  { challenge-id: uint, patient-id: uint }
  {
    join-block: uint,
    progress: uint,
    completed: bool,
    reward-claimed: bool
  }
)

;; Crisis response network
(define-map crisis-responders
  { responder-id: uint }
  {
    responder-hash: (buff 32),
    specialization: (string-ascii 100),
    response-count: uint,
    avg-response-time: uint,
    is-available: bool,
    verification-level: uint ;; 1-3, higher is more qualified
  }
)

;; ===== DATA VARIABLES =====
(define-data-var next-group-id uint u1)
(define-data-var next-challenge-id uint u1)
(define-data-var next-responder-id uint u1)
(define-data-var total-peer-support-sessions uint u0)

;; ===== PUBLIC FUNCTIONS =====

;; Create peer support group
(define-public (create-support-group (group-name (string-ascii 100)) (group-type (string-ascii 50)))
  (let (
    (group-id (var-get next-group-id))
  )
    (map-set support-groups
      { group-id: group-id }
      {
        group-name: group-name,
        group-type: group-type,
        member-count: u0,
        created-block: stacks-block-height,
        is-active: true
      }
    )
    
    (var-set next-group-id (+ group-id u1))
    (ok group-id)
  )
)

;; Join support group
(define-public (join-support-group (group-id uint) (patient-id uint))
  (let (
    (group (unwrap! (map-get? support-groups { group-id: group-id }) ERR_NOT_FOUND))
  )
    (asserts! (get is-active group) ERR_UNAUTHORIZED)
    (asserts! (is-none (map-get? group-memberships { group-id: group-id, patient-id: patient-id })) ERR_ALREADY_EXISTS)
    
    ;; Add membership
    (map-set group-memberships
      { group-id: group-id, patient-id: patient-id }
      {
        join-block: stacks-block-height,
        contribution-count: u0,
        tokens-earned: u0,
        is-moderator: false
      }
    )
    
    ;; Update group member count
    (map-set support-groups
      { group-id: group-id }
      (merge group {
        member-count: (+ (get member-count group) u1)
      })
    )
    
    (ok "Joined support group")
  )
)

;; Contribute to peer support (earn tokens)
(define-public (contribute-peer-support (group-id uint) (patient-id uint))
  (let (
    (membership (unwrap! (map-get? group-memberships { group-id: group-id, patient-id: patient-id }) ERR_NOT_FOUND))
  )
    ;; Update contribution count and reward tokens
    (map-set group-memberships
      { group-id: group-id, patient-id: patient-id }
      (merge membership {
        contribution-count: (+ (get contribution-count membership) u1),
        tokens-earned: (+ (get tokens-earned membership) PEER_SUPPORT_REWARD)
      })
    )
    
    ;; Update global statistics
    (var-set total-peer-support-sessions (+ (var-get total-peer-support-sessions) u1))
    
    (ok PEER_SUPPORT_REWARD)
  )
)

;; Create wellness challenge
(define-public (create-wellness-challenge (title (string-ascii 100)) (description (string-ascii 300)) (duration-blocks uint) (reward-amount uint) (challenge-type (string-ascii 50)))
  (let (
    (challenge-id (var-get next-challenge-id))
    (start-block stacks-block-height)
  )
    (map-set wellness-challenges
      { challenge-id: challenge-id }
      {
        title: title,
        description: description,
        duration-blocks: duration-blocks,
        reward-amount: reward-amount,
        participant-count: u0,
        start-block: start-block,
        end-block: (+ start-block duration-blocks),
        challenge-type: challenge-type
      }
    )
    
    (var-set next-challenge-id (+ challenge-id u1))
    (ok challenge-id)
  )
)

;; Join wellness challenge
(define-public (join-wellness-challenge (challenge-id uint) (patient-id uint))
  (let (
    (challenge (unwrap! (map-get? wellness-challenges { challenge-id: challenge-id }) ERR_NOT_FOUND))
  )
    (asserts! (< stacks-block-height (get end-block challenge)) ERR_INVALID_AMOUNT)
    (asserts! (is-none (map-get? challenge-participations { challenge-id: challenge-id, patient-id: patient-id })) ERR_ALREADY_EXISTS)
    
    ;; Add participation
    (map-set challenge-participations
      { challenge-id: challenge-id, patient-id: patient-id }
      {
        join-block: stacks-block-height,
        progress: u0,
        completed: false,
        reward-claimed: false
      }
    )
    
    ;; Update challenge participant count
    (map-set wellness-challenges
      { challenge-id: challenge-id }
      (merge challenge {
        participant-count: (+ (get participant-count challenge) u1)
      })
    )
    
    (ok "Joined wellness challenge")
  )
)

;; Update challenge progress
(define-public (update-challenge-progress (challenge-id uint) (patient-id uint) (progress uint))
  (let (
    (participation (unwrap! (map-get? challenge-participations { challenge-id: challenge-id, patient-id: patient-id }) ERR_NOT_FOUND))
    (challenge (unwrap! (map-get? wellness-challenges { challenge-id: challenge-id }) ERR_NOT_FOUND))
    (completed (>= progress u100)) ;; 100% completion
  )
    (map-set challenge-participations
      { challenge-id: challenge-id, patient-id: patient-id }
      (merge participation {
        progress: progress,
        completed: completed
      })
    )
    
    (if (and completed (not (get reward-claimed participation)))
      (begin
        (map-set challenge-participations
          { challenge-id: challenge-id, patient-id: patient-id }
          (merge participation { reward-claimed: true })
        )
        (ok (get reward-amount challenge))
      )
      (ok u0)
    )
  )
)

;; Register as crisis responder
(define-public (register-crisis-responder (responder-hash (buff 32)) (specialization (string-ascii 100)) (verification-level uint))
  (let (
    (responder-id (var-get next-responder-id))
  )
    (asserts! (and (>= verification-level u1) (<= verification-level u3)) ERR_INVALID_AMOUNT)
    
    (map-set crisis-responders
      { responder-id: responder-id }
      {
        responder-hash: responder-hash,
        specialization: specialization,
        response-count: u0,
        avg-response-time: u0,
        is-available: true,
        verification-level: verification-level
      }
    )
    
    (var-set next-responder-id (+ responder-id u1))
    (ok responder-id)
  )
)

;; Respond to crisis (earn tokens)
(define-public (respond-to-crisis (responder-id uint) (response-time uint))
  (let (
    (responder (unwrap! (map-get? crisis-responders { responder-id: responder-id }) ERR_NOT_FOUND))
    (new-response-count (+ (get response-count responder) u1))
    (total-response-time (+ (* (get avg-response-time responder) (get response-count responder)) response-time))
    (new-avg-response-time (/ total-response-time new-response-count))
  )
    (map-set crisis-responders
      { responder-id: responder-id }
      (merge responder {
        response-count: new-response-count,
        avg-response-time: new-avg-response-time
      })
    )
    
    (ok CRISIS_RESPONSE_REWARD)
  )
)

;; ===== READ-ONLY FUNCTIONS =====

;; Get support group info
(define-read-only (get-support-group (group-id uint))
  (map-get? support-groups { group-id: group-id })
)

;; Get group membership
(define-read-only (get-group-membership (group-id uint) (patient-id uint))
  (map-get? group-memberships { group-id: group-id, patient-id: patient-id })
)

;; Get wellness challenge
(define-read-only (get-wellness-challenge (challenge-id uint))
  (map-get? wellness-challenges { challenge-id: challenge-id })
)

;; Get challenge participation
(define-read-only (get-challenge-participation (challenge-id uint) (patient-id uint))
  (map-get? challenge-participations { challenge-id: challenge-id, patient-id: patient-id })
)

;; Get crisis responder
(define-read-only (get-crisis-responder (responder-id uint))
  (map-get? crisis-responders { responder-id: responder-id })
)

;; Get platform statistics
(define-read-only (get-support-stats)
  (ok {
    total-groups: (var-get next-group-id),
    total-challenges: (var-get next-challenge-id),
    total-responders: (var-get next-responder-id),
    peer-support-sessions: (var-get total-peer-support-sessions)
  })
)

;; title: support-utilities
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

