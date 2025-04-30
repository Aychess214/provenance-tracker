;; participant-registry
;; 
;; A contract that manages the registration and authentication of supply chain participants
;; in the provenance-tracker system. This registry serves as the foundation for establishing
;; trust in the supply chain by verifying identities, managing roles, and maintaining 
;; participant reputation scores.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-REGISTERED (err u102))
(define-constant ERR-INVALID-ROLE (err u103))
(define-constant ERR-INVALID-STATUS (err u104))
(define-constant ERR-INVALID-ARGUMENT (err u105))
(define-constant ERR-NO-VERIFIED-IDENTITY (err u106))
(define-constant ERR-REPUTATION-OUT-OF-RANGE (err u107))

;; Role constants
(define-constant ROLE-ADMIN u1)
(define-constant ROLE-MANUFACTURER u2) 
(define-constant ROLE-DISTRIBUTOR u3)
(define-constant ROLE-RETAILER u4)
(define-constant ROLE-INSPECTOR u5)
(define-constant ROLE-CONSUMER u6)

;; Status constants
(define-constant STATUS-PENDING u1)
(define-constant STATUS-ACTIVE u2)
(define-constant STATUS-SUSPENDED u3)
(define-constant STATUS-REVOKED u4)

;; Data maps
;; Stores participant details
(define-map participants
  { address: principal }
  {
    name: (string-ascii 100),
    role: uint,
    status: uint,
    identity-verified: bool,
    reputation-score: uint,
    registration-date: uint,
    metadata: (optional (string-ascii 256))
  }
)

;; Stores admins who have permission to manage participants
(define-map admins 
  { address: principal } 
  { 
    is-admin: bool,
    added-by: principal,
    added-at: uint
  }
)

;; Tracks participant roles to ensure only valid roles are assigned
(define-map valid-roles
  { role-id: uint }
  { 
    role-name: (string-ascii 50),
    is-active: bool
  }
)

;; Data variables
;; Contract owner - has admin privileges by default
(define-data-var contract-owner principal tx-sender)

;; Total number of registered participants
(define-data-var participant-count uint u0)

;; Private functions

;; Check if a principal is an admin
(define-private (is-admin (address principal))
  (default-to
    false
    (get is-admin (map-get? admins { address: address }))
  )
)

;; Check if a principal is the contract owner
(define-private (is-contract-owner (address principal))
  (is-eq address (var-get contract-owner))
)

;; Check if a principal is authorized (either admin or contract owner)
(define-private (is-authorized (address principal))
  (or
    (is-admin address)
    (is-contract-owner address)
  )
)

;; Initialize valid roles - called during contract deployment
(define-private (initialize-roles)
  (begin
    (map-set valid-roles { role-id: ROLE-ADMIN } { role-name: "Administrator", is-active: true })
    (map-set valid-roles { role-id: ROLE-MANUFACTURER } { role-name: "Manufacturer", is-active: true })
    (map-set valid-roles { role-id: ROLE-DISTRIBUTOR } { role-name: "Distributor", is-active: true })
    (map-set valid-roles { role-id: ROLE-RETAILER } { role-name: "Retailer", is-active: true })
    (map-set valid-roles { role-id: ROLE-INSPECTOR } { role-name: "Inspector", is-active: true })
    (map-set valid-roles { role-id: ROLE-CONSUMER } { role-name: "Consumer", is-active: true })
    (ok true)
  )
)

;; Checks if a role is valid
(define-private (is-valid-role (role-id uint))
  (and
    (is-some (map-get? valid-roles { role-id: role-id }))
    (get is-active (unwrap-panic (map-get? valid-roles { role-id: role-id })))
  )
)

;; Validates that a status code is valid
(define-private (is-valid-status (status uint))
  (or
    (is-eq status STATUS-PENDING)
    (is-eq status STATUS-ACTIVE)
    (is-eq status STATUS-SUSPENDED)
    (is-eq status STATUS-REVOKED)
  )
)

;; Validates that a reputation score is within acceptable range (0-100)
(define-private (is-valid-reputation (score uint))
  (and (>= score u0) (<= score u100))
)

;; Read-only functions

;; Get participant details
(define-read-only (get-participant (address principal))
  (let ((participant (map-get? participants { address: address })))
    (if (is-some participant)
      (ok (unwrap-panic participant))
      ERR-NOT-FOUND
    )
  )
)

;; Check if a participant is registered
(define-read-only (is-registered (address principal))
  (is-some (map-get? participants { address: address }))
)

;; Get participant status
(define-read-only (get-participant-status (address principal))
  (let ((participant (map-get? participants { address: address })))
    (if (is-some participant)
      (ok (get status (unwrap-panic participant)))
      ERR-NOT-FOUND
    )
  )
)

;; Get participant role
(define-read-only (get-participant-role (address principal))
  (let ((participant (map-get? participants { address: address })))
    (if (is-some participant)
      (ok (get role (unwrap-panic participant)))
      ERR-NOT-FOUND
    )
  )
)

;; Get role name from role ID
(define-read-only (get-role-name (role-id uint))
  (let ((role (map-get? valid-roles { role-id: role-id })))
    (if (is-some role)
      (ok (get role-name (unwrap-panic role)))
      ERR-INVALID-ROLE
    )
  )
)

;; Check if a participant's identity is verified
(define-read-only (is-identity-verified (address principal))
  (let ((participant (map-get? participants { address: address })))
    (if (is-some participant)
      (ok (get identity-verified (unwrap-panic participant)))
      ERR-NOT-FOUND
    )
  )
)

;; Get participant reputation score
(define-read-only (get-reputation-score (address principal))
  (let ((participant (map-get? participants { address: address })))
    (if (is-some participant)
      (ok (get reputation-score (unwrap-panic participant)))
      ERR-NOT-FOUND
    )
  )
)

;; Get total number of registered participants
(define-read-only (get-participant-count)
  (ok (var-get participant-count))
)

;; Public functions

;; Initialize the contract - called once during deployment
(define-public (initialize)
  (begin
    ;; Set contract owner as admin
    (map-set admins 
      { address: tx-sender } 
      { is-admin: true, added-by: tx-sender, added-at: block-height }
    )
    (initialize-roles)
    (ok true)
  )
)

;; Register a new participant
(define-public (register-participant 
    (name (string-ascii 100)) 
    (role uint)
    (metadata (optional (string-ascii 256)))
  )
  (let 
    (
      (caller tx-sender)
    )
    (asserts! (not (is-registered caller)) ERR-ALREADY-REGISTERED)
    (asserts! (is-valid-role role) ERR-INVALID-ROLE)
    
    ;; Non-admin roles need approval to become active
    (let 
      (
        (initial-status (if (is-eq role ROLE-ADMIN) STATUS-ACTIVE STATUS-PENDING))
      )
      (map-set participants
        { address: caller }
        {
          name: name,
          role: role,
          status: initial-status,
          identity-verified: false,
          reputation-score: u50, ;; Neutral starting reputation
          registration-date: block-height,
          metadata: metadata
        }
      )
      
      ;; If registering as admin, need to also add to admins map
      (if (is-eq role ROLE-ADMIN)
        (map-set admins 
          { address: caller } 
          { is-admin: true, added-by: caller, added-at: block-height }
        )
        true
      )
      
      ;; Increment participant count
      (var-set participant-count (+ (var-get participant-count) u1))
      
      (ok true)
    )
  )
)

;; Update participant status - admin only
(define-public (update-participant-status (participant principal) (new-status uint))
  (let 
    (
      (caller tx-sender)
    )
    (asserts! (is-authorized caller) ERR-NOT-AUTHORIZED)
    (asserts! (is-registered participant) ERR-NOT-FOUND)
    (asserts! (is-valid-status new-status) ERR-INVALID-STATUS)
    
    (let ((current-participant (unwrap-panic (map-get? participants { address: participant }))))
      (map-set participants
        { address: participant }
        (merge current-participant { status: new-status })
      )
      (ok true)
    )
  )
)

;; Verify participant identity - admin only
(define-public (verify-identity (participant principal) (verified bool))
  (let 
    (
      (caller tx-sender)
    )
    (asserts! (is-authorized caller) ERR-NOT-AUTHORIZED)
    (asserts! (is-registered participant) ERR-NOT-FOUND)
    
    (let ((current-participant (unwrap-panic (map-get? participants { address: participant }))))
      (map-set participants
        { address: participant }
        (merge current-participant { identity-verified: verified })
      )
      (ok true)
    )
  )
)

;; Update participant role - admin only
(define-public (update-participant-role (participant principal) (new-role uint))
  (let 
    (
      (caller tx-sender)
    )
    (asserts! (is-authorized caller) ERR-NOT-AUTHORIZED)
    (asserts! (is-registered participant) ERR-NOT-FOUND)
    (asserts! (is-valid-role new-role) ERR-INVALID-ROLE)
    
    (let ((current-participant (unwrap-panic (map-get? participants { address: participant }))))
      ;; If changing to/from admin role, update admins map accordingly
      (if (is-eq new-role ROLE-ADMIN)
        (map-set admins 
          { address: participant } 
          { is-admin: true, added-by: caller, added-at: block-height }
        )
        (if (is-eq (get role current-participant) ROLE-ADMIN)
          ;; Remove from admins if was previously an admin
          (map-delete admins { address: participant })
          true
        )
      )
      
      (map-set participants
        { address: participant }
        (merge current-participant { role: new-role })
      )
      (ok true)
    )
  )
)

;; Update reputation score - admin only
(define-public (update-reputation (participant principal) (score uint))
  (let 
    (
      (caller tx-sender)
    )
    (asserts! (is-authorized caller) ERR-NOT-AUTHORIZED)
    (asserts! (is-registered participant) ERR-NOT-FOUND)
    (asserts! (is-valid-reputation score) ERR-REPUTATION-OUT-OF-RANGE)
    
    (let ((current-participant (unwrap-panic (map-get? participants { address: participant }))))
      (map-set participants
        { address: participant }
        (merge current-participant { reputation-score: score })
      )
      (ok true)
    )
  )
)

;; Add new admin - only existing admins can add new ones
(define-public (add-admin (new-admin principal))
  (let 
    (
      (caller tx-sender)
    )
    (asserts! (is-authorized caller) ERR-NOT-AUTHORIZED)
    
    ;; If participant isn't registered, register them as an admin
    (if (not (is-registered new-admin))
      (begin
        (map-set participants
          { address: new-admin }
          {
            name: "",  ;; Empty name until they update it
            role: ROLE-ADMIN,
            status: STATUS-ACTIVE,
            identity-verified: true,  ;; Admins are considered verified
            reputation-score: u75,    ;; Higher starting reputation for admins
            registration-date: block-height,
            metadata: none
          }
        )
        (var-set participant-count (+ (var-get participant-count) u1))
      )
      ;; If already registered, update their role to admin
      (let ((current-participant (unwrap-panic (map-get? participants { address: new-admin }))))
        (map-set participants
          { address: new-admin }
          (merge 
            current-participant 
            { 
              role: ROLE-ADMIN,
              status: STATUS-ACTIVE,
              identity-verified: true
            }
          )
        )
      )
    )
    
    ;; Add to admins map
    (map-set admins 
      { address: new-admin } 
      { is-admin: true, added-by: caller, added-at: block-height }
    )
    
    (ok true)
  )
)

;; Remove admin privileges - only contract owner can do this
(define-public (remove-admin (admin principal))
  (let 
    (
      (caller tx-sender)
    )
    (asserts! (is-contract-owner caller) ERR-NOT-AUTHORIZED)
    (asserts! (is-registered admin) ERR-NOT-FOUND)
    (asserts! (not (is-eq admin caller)) ERR-NOT-AUTHORIZED) ;; Can't remove yourself
    
    ;; Remove from admins map
    (map-delete admins { address: admin })
    
    ;; Update their role to something other than admin (default to manufacturer)
    (let ((current-participant (unwrap-panic (map-get? participants { address: admin }))))
      (map-set participants
        { address: admin }
        (merge current-participant { role: ROLE-MANUFACTURER })
      )
    )
    
    (ok true)
  )
)

;; Update participant metadata
(define-public (update-metadata (metadata (optional (string-ascii 256))))
  (let 
    (
      (caller tx-sender)
    )
    (asserts! (is-registered caller) ERR-NOT-FOUND)
    
    (let ((current-participant (unwrap-panic (map-get? participants { address: caller }))))
      (map-set participants
        { address: caller }
        (merge current-participant { metadata: metadata })
      )
      (ok true)
    )
  )
)

;; Transfer contract ownership - only current owner can do this
(define-public (transfer-ownership (new-owner principal))
  (let 
    (
      (caller tx-sender)
    )
    (asserts! (is-contract-owner caller) ERR-NOT-AUTHORIZED)
    
    ;; Ensure new owner is registered
    (asserts! (is-registered new-owner) ERR-NOT-FOUND)
    
    ;; Ensure new owner's identity is verified
    (asserts! 
      (get identity-verified (unwrap-panic (map-get? participants { address: new-owner }))) 
      ERR-NO-VERIFIED-IDENTITY
    )
    
    ;; Set new owner
    (var-set contract-owner new-owner)
    
    ;; Ensure new owner is also an admin
    (map-set admins 
      { address: new-owner } 
      { is-admin: true, added-by: caller, added-at: block-height }
    )
    
    (ok true)
  )
)