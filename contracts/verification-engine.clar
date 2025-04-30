;; verification-engine
;; 
;; This smart contract provides mechanisms for verifying product authenticity
;; and provenance claims on the Stacks blockchain. It allows consumers and 
;; supply chain participants to verify product registration, current ownership,
;; and complete provenance history while maintaining appropriate privacy controls.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PRODUCT-NOT-FOUND (err u101))
(define-constant ERR-INVALID-PRODUCT-ID (err u102))
(define-constant ERR-INVALID-VERIFICATION-REQUEST (err u103))
(define-constant ERR-VERIFICATION-EXPIRED (err u104))
(define-constant ERR-ALREADY-VERIFIED (err u105))
(define-constant ERR-VERIFICATION-NOT-FOUND (err u106))
(define-constant ERR-INVALID-CREDENTIAL (err u107))

;; Data structures

;; Map to store product verification information
(define-map product-verifications
  { product-id: (buff 32) }
  {
    is-verified: bool,
    verified-at: uint,
    verified-by: principal,
    verification-expiry: uint,
    verification-level: uint
  }
)

;; Map to store verification requests
(define-map verification-requests
  { request-id: (buff 32) }
  {
    product-id: (buff 32),
    requester: principal,
    requested-at: uint,
    status: (string-utf8 20), ;; "pending", "approved", "rejected"
    review-notes: (optional (string-utf8 256))
  }
)

;; Map to store product credentials for public verification
(define-map product-credentials
  { credential-id: (buff 32) }
  {
    product-id: (buff 32),
    issuer: principal,
    issued-at: uint,
    expiry: uint,
    credential-hash: (buff 32), ;; Hash of the credential content
    revoked: bool
  }
)

;; Track master registry contract
(define-data-var registry-contract principal 'ST000000000000000000000000000000000000000)

;; Variable to store admin principal
(define-data-var contract-admin principal tx-sender)

;; Private functions

;; Internal function to validate product ID existence
;; Checks with the registry contract to confirm a product exists
(define-private (validate-product-exists (product-id (buff 32)))
  (match (contract-call? (var-get registry-contract) get-product-info product-id)
    success true
    error false
  )
)

;; Internal function to check if a verification has expired
(define-private (is-verification-expired (product-id (buff 32)))
  (match (map-get? product-verifications { product-id: product-id })
    verification-data (> block-height (get verification-expiry verification-data))
    false
  )
)

;; Internal function to generate a credential hash
(define-private (generate-credential-hash (product-id (buff 32)) (issuer principal) (timestamp uint))
  (sha256 (concat (concat product-id (principal->buff issuer)) (uint->buff timestamp)))
)

;; Internal function to check if caller is admin
(define-private (is-admin)
  (is-eq tx-sender (var-get contract-admin))
)

;; Public functions

;; Set the registry contract address - only callable by admin
(define-public (set-registry-contract (new-registry principal))
  (begin
    (asserts! (is-admin) ERR-NOT-AUTHORIZED)
    (ok (var-set registry-contract new-registry))
  )
)

;; Set a new admin - only callable by current admin
(define-public (set-admin (new-admin principal))
  (begin
    (asserts! (is-admin) ERR-NOT-AUTHORIZED)
    (ok (var-set contract-admin new-admin))
  )
)

;; Request verification for a product
(define-public (request-verification (product-id (buff 32)) (request-id (buff 32)))
  (begin
    ;; Check if product exists
    (asserts! (validate-product-exists product-id) ERR-PRODUCT-NOT-FOUND)
    
    ;; Create verification request
    (map-set verification-requests
      { request-id: request-id }
      {
        product-id: product-id,
        requester: tx-sender,
        requested-at: block-height,
        status: "pending",
        review-notes: none
      }
    )
    (ok true)
  )
)

;; Approve a verification request - only authorized verifiers can approve
(define-public (approve-verification-request 
                (request-id (buff 32)) 
                (verification-expiry uint) 
                (verification-level uint)
                (notes (optional (string-utf8 256))))
  (let (
    (request (unwrap! (map-get? verification-requests { request-id: request-id }) ERR-VERIFICATION-NOT-FOUND))
    (product-id (get product-id request))
  )
    ;; Check if caller is admin
    (asserts! (is-admin) ERR-NOT-AUTHORIZED)
    
    ;; Update request status
    (map-set verification-requests
      { request-id: request-id }
      (merge request {
        status: "approved",
        review-notes: notes
      })
    )
    
    ;; Set product verification
    (map-set product-verifications
      { product-id: product-id }
      {
        is-verified: true,
        verified-at: block-height,
        verified-by: tx-sender,
        verification-expiry: (+ block-height verification-expiry),
        verification-level: verification-level
      }
    )
    
    (ok true)
  )
)

;; Reject a verification request
(define-public (reject-verification-request (request-id (buff 32)) (notes (optional (string-utf8 256))))
  (let (
    (request (unwrap! (map-get? verification-requests { request-id: request-id }) ERR-VERIFICATION-NOT-FOUND))
  )
    ;; Check if caller is admin
    (asserts! (is-admin) ERR-NOT-AUTHORIZED)
    
    ;; Update request status
    (map-set verification-requests
      { request-id: request-id }
      (merge request {
        status: "rejected",
        review-notes: notes
      })
    )
    
    (ok true)
  )
)

;; Issue a verifiable credential for a product
(define-public (issue-credential (product-id (buff 32)) (credential-id (buff 32)) (expiry uint))
  (begin
    ;; Check if product exists and is verified
    (asserts! (validate-product-exists product-id) ERR-PRODUCT-NOT-FOUND)
    
    (let (
      (verification (unwrap! (map-get? product-verifications { product-id: product-id }) ERR-PRODUCT-NOT-FOUND))
      (credential-hash (generate-credential-hash product-id tx-sender block-height))
    )
      ;; Check that product is verified
      (asserts! (get is-verified verification) ERR-INVALID-VERIFICATION-REQUEST)
      ;; Check that verification hasn't expired
      (asserts! (not (is-verification-expired product-id)) ERR-VERIFICATION-EXPIRED)
      
      ;; Create credential
      (map-set product-credentials
        { credential-id: credential-id }
        {
          product-id: product-id,
          issuer: tx-sender,
          issued-at: block-height,
          expiry: (+ block-height expiry),
          credential-hash: credential-hash,
          revoked: false
        }
      )
      
      (ok credential-hash)
    )
  )
)

;; Revoke a previously issued credential
(define-public (revoke-credential (credential-id (buff 32)))
  (let (
    (credential (unwrap! (map-get? product-credentials { credential-id: credential-id }) ERR-INVALID-CREDENTIAL))
  )
    ;; Check if caller is the issuer or admin
    (asserts! (or (is-eq tx-sender (get issuer credential)) (is-admin)) ERR-NOT-AUTHORIZED)
    
    ;; Update credential to revoked status
    (map-set product-credentials
      { credential-id: credential-id }
      (merge credential { revoked: true })
    )
    
    (ok true)
  )
)

;; Read-only functions

;; Check if a product is verified
(define-read-only (is-product-verified (product-id (buff 32)))
  (match (map-get? product-verifications { product-id: product-id })
    verification-data (and 
                         (get is-verified verification-data)
                         (< block-height (get verification-expiry verification-data)))
    false
  )
)

;; Get product verification details
(define-read-only (get-verification-details (product-id (buff 32)))
  (map-get? product-verifications { product-id: product-id })
)

;; Get verification request details
(define-read-only (get-verification-request (request-id (buff 32)))
  (map-get? verification-requests { request-id: request-id })
)

;; Verify a credential's validity
(define-read-only (verify-credential (credential-id (buff 32)))
  (match (map-get? product-credentials { credential-id: credential-id })
    credential-data (if (and 
                          (not (get revoked credential-data))
                          (< block-height (get expiry credential-data)))
                        (ok credential-data)
                        (err ERR-INVALID-CREDENTIAL))
    (err ERR-INVALID-CREDENTIAL)
  )
)

;; Get all credentials for a product
(define-read-only (get-product-credentials (product-id (buff 32)))
  (filter 
    (compose not get revoked) ;; Filter out revoked credentials
    (map-get? product-credentials { credential-id: product-id })
  )
)

;; Check if caller has verification authority
(define-read-only (has-verification-authority (address principal))
  (is-eq address (var-get contract-admin))
)