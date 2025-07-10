;; =============================================================================
;; INTERGENERATIONAL SKILL EXCHANGE PLATFORM
;; =============================================================================
;; A comprehensive platform connecting seniors with younger community members
;; for bidirectional skill sharing, mentorship, and community collaboration.
;;
;; CONTRACT 1: skill-profiles.clar
;; Manages user profiles, skills, and experience documentation
;; =============================================================================

;; =============================================================================
;; CONSTANTS & ERRORS
;; =============================================================================

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROFILE-NOT-FOUND (err u101))
(define-constant ERR-SKILL-NOT-FOUND (err u102))
(define-constant ERR-INVALID-GENERATION (err u103))
(define-constant ERR-INVALID-SKILL-LEVEL (err u104))
(define-constant ERR-PROFILE-ALREADY-EXISTS (err u105))
(define-constant ERR-SKILL-ALREADY-EXISTS (err u106))
(define-constant ERR-INVALID-RATING (err u107))
(define-constant ERR-INVALID-EXPERIENCE-TYPE (err u108))

;; Generation categories
(define-constant GENERATION-SENIOR u1)
(define-constant GENERATION-MILLENNIAL u2)
(define-constant GENERATION-GENZ u3)

;; Skill proficiency levels
(define-constant SKILL-LEVEL-BEGINNER u1)
(define-constant SKILL-LEVEL-INTERMEDIATE u2)
(define-constant SKILL-LEVEL-ADVANCED u3)
(define-constant SKILL-LEVEL-EXPERT u4)

;; Experience types
(define-constant EXPERIENCE-TYPE-TEACHING u1)
(define-constant EXPERIENCE-TYPE-LEARNING u2)
(define-constant EXPERIENCE-TYPE-COLLABORATION u3)

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; =============================================================================
;; DATA STRUCTURES
;; =============================================================================

;; User profile structure
(define-map user-profiles
  principal
  {
    name: (string-ascii 50),
    age: uint,
    generation: uint,
    location: (string-ascii 100),
    bio: (string-ascii 500),
    interests: (list 10 (string-ascii 50)),
    created-at: uint,
    is-active: bool,
    total-connections: uint,
    reputation-score: uint
  })

;; User skills mapping
(define-map user-skills
  { user: principal, skill-id: uint }
  {
    skill-name: (string-ascii 50),
    category: (string-ascii 30),
    proficiency-level: uint,
    years-experience: uint,
    can-teach: bool,
    wants-to-learn: bool,
    description: (string-ascii 300),
    created-at: uint,
    endorsements: uint
  })

;; Experience documentation
(define-map experience-records
  { user: principal, experience-id: uint }
  {
    experience-type: uint,
    skill-involved: (string-ascii 50),
    partner-principal: (optional principal),
    duration-hours: uint,
    rating-received: uint,
    rating-given: uint,
    description: (string-ascii 500),
    achievements: (list 5 (string-ascii 100)),
    created-at: uint,
    is-verified: bool
  })

;; Skill categories registry
(define-map skill-categories
  (string-ascii 30)
  {
    description: (string-ascii 200),
    is-active: bool,
    skill-count: uint,
    created-at: uint
  })

;; Counter for unique IDs
(define-data-var next-skill-id uint u1)
(define-data-var next-experience-id uint u1)
(define-data-var total-users uint u0)
(define-data-var total-skills uint u0)

;; =============================================================================
;; READ-ONLY FUNCTIONS
;; =============================================================================

;; Get user profile
(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles user))

;; Get user skill
(define-read-only (get-user-skill (user principal) (skill-id uint))
  (map-get? user-skills { user: user, skill-id: skill-id }))

;; Get experience record
(define-read-only (get-experience-record (user principal) (experience-id uint))
  (map-get? experience-records { user: user, experience-id: experience-id }))

;; Get skill category
(define-read-only (get-skill-category (category (string-ascii 30)))
  (map-get? skill-categories category))

;; Get platform statistics
(define-read-only (get-platform-stats)
  {
    total-users: (var-get total-users),
    total-skills: (var-get total-skills),
    next-skill-id: (var-get next-skill-id),
    next-experience-id: (var-get next-experience-id)
  })

;; Check if user can teach a skill
(define-read-only (can-user-teach-skill (user principal) (skill-id uint))
  (match (get-user-skill user skill-id)
    skill-data (get can-teach skill-data)
    false))

;; Check if user wants to learn a skill
(define-read-only (does-user-want-to-learn-skill (user principal) (skill-id uint))
  (match (get-user-skill user skill-id)
    skill-data (get wants-to-learn skill-data)
    false))

;; Calculate user reputation score
(define-read-only (calculate-reputation-score (user principal))
  (match (get-user-profile user)
    profile (get reputation-score profile)
    u0))

;; =============================================================================
;; PRIVATE FUNCTIONS
;; =============================================================================

;; Validate generation
(define-private (is-valid-generation (generation uint))
  (or (is-eq generation GENERATION-SENIOR)
      (or (is-eq generation GENERATION-MILLENNIAL)
          (is-eq generation GENERATION-GENZ))))

;; Validate skill level
(define-private (is-valid-skill-level (level uint))
  (and (>= level SKILL-LEVEL-BEGINNER)
       (<= level SKILL-LEVEL-EXPERT)))

;; Validate experience type
(define-private (is-valid-experience-type (exp-type uint))
  (or (is-eq exp-type EXPERIENCE-TYPE-TEACHING)
      (or (is-eq exp-type EXPERIENCE-TYPE-LEARNING)
          (is-eq exp-type EXPERIENCE-TYPE-COLLABORATION))))

;; Validate rating (1-5 scale)
(define-private (is-valid-rating (rating uint))
  (and (>= rating u1) (<= rating u5)))

;; =============================================================================
;; PUBLIC FUNCTIONS
;; =============================================================================

;; Create user profile
(define-public (create-user-profile
  (name (string-ascii 50))
  (age uint)
  (generation uint)
  (location (string-ascii 100))
  (bio (string-ascii 500))
  (interests (list 10 (string-ascii 50))))
  (let ((caller tx-sender))
    (asserts! (is-none (get-user-profile caller)) ERR-PROFILE-ALREADY-EXISTS)
    (asserts! (is-valid-generation generation) ERR-INVALID-GENERATION)
    (asserts! (and (>= age u13) (<= age u120)) ERR-INVALID-GENERATION)
    (map-set user-profiles caller {
      name: name,
      age: age,
      generation: generation,
      location: location,
      bio: bio,
      interests: interests,
      created-at: stacks-block-height,
      is-active: true,
      total-connections: u0,
      reputation-score: u100
    })
    (var-set total-users (+ (var-get total-users) u1))
    (ok true)))

;; Update user profile
(define-public (update-user-profile
  (name (string-ascii 50))
  (location (string-ascii 100))
  (bio (string-ascii 500))
  (interests (list 10 (string-ascii 50))))
  (let ((caller tx-sender))
    (match (get-user-profile caller)
      profile (begin
        (map-set user-profiles caller (merge profile {
          name: name,
          location: location,
          bio: bio,
          interests: interests
        }))
        (ok true))
      ERR-PROFILE-NOT-FOUND)))

;; Add skill to user profile
(define-public (add-user-skill
  (skill-name (string-ascii 50))
  (category (string-ascii 30))
  (proficiency-level uint)
  (years-experience uint)
  (can-teach bool)
  (wants-to-learn bool)
  (description (string-ascii 300)))
  (let ((caller tx-sender)
        (skill-id (var-get next-skill-id)))
    (asserts! (is-some (get-user-profile caller)) ERR-PROFILE-NOT-FOUND)
    (asserts! (is-valid-skill-level proficiency-level) ERR-INVALID-SKILL-LEVEL)
    (asserts! (is-none (get-user-skill caller skill-id)) ERR-SKILL-ALREADY-EXISTS)
    (map-set user-skills { user: caller, skill-id: skill-id } {
      skill-name: skill-name,
      category: category,
      proficiency-level: proficiency-level,
      years-experience: years-experience,
      can-teach: can-teach,
      wants-to-learn: wants-to-learn,
      description: description,
      created-at: stacks-block-height,
      endorsements: u0
    })
    (var-set next-skill-id (+ skill-id u1))
    (var-set total-skills (+ (var-get total-skills) u1))
    (ok skill-id)))

;; Update skill information
(define-public (update-user-skill
  (skill-id uint)
  (proficiency-level uint)
  (years-experience uint)
  (can-teach bool)
  (wants-to-learn bool)
  (description (string-ascii 300)))
  (let ((caller tx-sender))
    (asserts! (is-valid-skill-level proficiency-level) ERR-INVALID-SKILL-LEVEL)
    (match (get-user-skill caller skill-id)
      skill-data (begin
        (map-set user-skills { user: caller, skill-id: skill-id } (merge skill-data {
          proficiency-level: proficiency-level,
          years-experience: years-experience,
          can-teach: can-teach,
          wants-to-learn: wants-to-learn,
          description: description
        }))
        (ok true))
      ERR-SKILL-NOT-FOUND)))

;; Add experience record
(define-public (add-experience-record
  (experience-type uint)
  (skill-involved (string-ascii 50))
  (partner-principal (optional principal))
  (duration-hours uint)
  (rating-received uint)
  (rating-given uint)
  (description (string-ascii 500))
  (achievements (list 5 (string-ascii 100))))
  (let ((caller tx-sender)
        (experience-id (var-get next-experience-id)))
    (asserts! (is-some (get-user-profile caller)) ERR-PROFILE-NOT-FOUND)
    (asserts! (is-valid-experience-type experience-type) ERR-INVALID-EXPERIENCE-TYPE)
    (asserts! (is-valid-rating rating-received) ERR-INVALID-RATING)
    (asserts! (is-valid-rating rating-given) ERR-INVALID-RATING)
    (map-set experience-records { user: caller, experience-id: experience-id } {
      experience-type: experience-type,
      skill-involved: skill-involved,
      partner-principal: partner-principal,
      duration-hours: duration-hours,
      rating-received: rating-received,
      rating-given: rating-given,
      description: description,
      achievements: achievements,
      created-at: stacks-block-height,
      is-verified: false
    })
    (var-set next-experience-id (+ experience-id u1))
    (ok experience-id)))

;; Endorse a user's skill
(define-public (endorse-user-skill (user principal) (skill-id uint))
  (let ((caller tx-sender))
    (asserts! (not (is-eq caller user)) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (get-user-profile caller)) ERR-PROFILE-NOT-FOUND)
    (match (get-user-skill user skill-id)
      skill-data (begin
        (map-set user-skills { user: user, skill-id: skill-id } (merge skill-data {
          endorsements: (+ (get endorsements skill-data) u1)
        }))
        (ok true))
      ERR-SKILL-NOT-FOUND)))

;; Create skill category
(define-public (create-skill-category
  (category (string-ascii 30))
  (description (string-ascii 200)))
  (let ((caller tx-sender))
    (asserts! (is-eq caller CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (get-skill-category category)) ERR-SKILL-ALREADY-EXISTS)
    (map-set skill-categories category {
      description: description,
      is-active: true,
      skill-count: u0,
      created-at: stacks-block-height
    })
    (ok true)))

;; Verify experience record (admin function)
(define-public (verify-experience-record (user principal) (experience-id uint))
  (let ((caller tx-sender))
    (asserts! (is-eq caller CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (match (get-experience-record user experience-id)
      experience-data (begin
        (map-set experience-records { user: user, experience-id: experience-id } (merge experience-data {
          is-verified: true
        }))
        (ok true))
      ERR-SKILL-NOT-FOUND)))

;; Update user reputation score
(define-public (update-reputation-score (user principal) (new-score uint))
  (let ((caller tx-sender))
    (asserts! (is-eq caller CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (match (get-user-profile user)
      profile (begin
        (map-set user-profiles user (merge profile {
          reputation-score: new-score
        }))
        (ok true))
      ERR-PROFILE-NOT-FOUND)))
