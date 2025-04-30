;; ownership-transfer
;; A contract for securely transferring product ownership between supply chain participants

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-RECEIVER (err u101))
(define-constant ERR-ITEM-NOT-FOUND (err u102))
(define-constant ERR-INVALID-REQUEST (err u103))
(define-constant ERR-REQUEST-ALREADY-EXISTS (err u104))
(define-constant ERR-REQUEST-NOT-FOUND (err u105))
(define-constant ERR-NOT-CURRENT-OWNER (err u106))
(define-constant ERR-OWNER-RECEIVER-SAME (err u107))
(define-constant ERR-INVALID-SIGNATURE (err u108))

;; Data structures

;; Track current ownership of items
(define-map item-ownership
  {item-id: (string-utf8 36)}
  {
    owner: principal,
    timestamp: uint,
    location: (string-utf8 100)
  }
)

;; Store pending transfer requests
(define-map transfer-requests
  {item-id: (string-utf8 36), requester: principal, receiver: principal}
  {
    created-at: uint,
    status: (string-utf8 10), ;; "pending", "accepted", "rejected"
    location: (string-utf8 100)
  }
)

;; Maintain transfer history for all items
(define-map transfer-history
  {item-id: (string-utf8 36), transfer-id: uint}
  {
    from: principal,
    to: principal,
    timestamp: uint,
    location: (string-utf8 100)
  }
)

;; Counter to assign unique IDs to transfers
(define-data-var transfer-counter uint u0)

;; Private functions

;; Increments and returns a new transfer ID
(define-private (get-next-transfer-id)
  (let ((current-id (var-get transfer-counter)))
    (var-set transfer-counter (+ current-id u1))
    current-id
  )
)

;; Check if a principal is the current owner of an item
(define-private (is-item-owner (item-id (string-utf8 36)) (principal principal))
  (match (map-get? item-ownership {item-id: item-id})
    ownership (is-eq (get owner ownership) principal)
    false
  )
)

;; Records a completed transfer in the history
(define-private (record-transfer (item-id (string-utf8 36)) (from principal) (to principal) (location (string-utf8 100)))
  (let ((transfer-id (get-next-transfer-id))
        (timestamp (unwrap-panic (get-block-info? time (- block-height u1)))))
    
    ;; Update current ownership
    (map-set item-ownership 
      {item-id: item-id}
      {
        owner: to,
        timestamp: timestamp,
        location: location
      }
    )
    
    ;; Add to transfer history
    (map-set transfer-history
      {item-id: item-id, transfer-id: transfer-id}
      {
        from: from,
        to: to,
        timestamp: timestamp,
        location: location
      }
    )
    
    (ok transfer-id)
  )
)

;; Public functions

;; Initialize a new item in the system with its first owner
(define-public (register-item (item-id (string-utf8 36)) (location (string-utf8 100)))
  (let ((timestamp (unwrap-panic (get-block-info? time (- block-height u1)))))
    (match (map-get? item-ownership {item-id: item-id})
      existing-item ERR-REQUEST-ALREADY-EXISTS
      (begin
        ;; Set caller as the initial owner
        (map-set item-ownership
          {item-id: item-id}
          {
            owner: tx-sender,
            timestamp: timestamp,
            location: location
          }
        )
        
        ;; Record initial "transfer" from system to owner
        (map-set transfer-history
          {item-id: item-id, transfer-id: u0}
          {
            from: tx-sender, ;; Self-transfer for initialization
            to: tx-sender,
            timestamp: timestamp,
            location: location
          }
        )
        
        (ok u0)
      )
    )
  )
)

;; Create a transfer request from current owner to a new receiver
(define-public (request-transfer (item-id (string-utf8 36)) (receiver principal) (location (string-utf8 100)))
  (let ((request-key {item-id: item-id, requester: tx-sender, receiver: receiver}))
    (asserts! (not (is-eq tx-sender receiver)) ERR-OWNER-RECEIVER-SAME)
    (asserts! (is-item-owner item-id tx-sender) ERR-NOT-CURRENT-OWNER)
    
    ;; Check if request already exists
    (match (map-get? transfer-requests request-key)
      existing-request ERR-REQUEST-ALREADY-EXISTS
      (begin
        (map-set transfer-requests
          request-key
          {
            created-at: (unwrap-panic (get-block-info? time (- block-height u1))),
            status: "pending",
            location: location
          }
        )
        (ok true)
      )
    )
  )
)

;; Accept a transfer request (must be called by the receiver)
(define-public (accept-transfer (item-id (string-utf8 36)) (sender principal) (location (string-utf8 100)))
  (let ((request-key {item-id: item-id, requester: sender, receiver: tx-sender}))
    ;; Verify the request exists and is pending
    (match (map-get? transfer-requests request-key)
      request (if (is-eq (get status request) "pending")
                (begin
                  ;; Verify the sender is still the current owner
                  (asserts! (is-item-owner item-id sender) ERR-NOT-CURRENT-OWNER)
                  
                  ;; Update request status
                  (map-set transfer-requests
                    request-key
                    (merge request {status: "accepted"})
                  )
                  
                  ;; Complete the transfer and record it
                  (record-transfer item-id sender tx-sender location)
                )
                ERR-INVALID-REQUEST
              )
      ERR-REQUEST-NOT-FOUND
    )
  )
)

;; Reject a transfer request (must be called by the receiver)
(define-public (reject-transfer (item-id (string-utf8 36)) (sender principal))
  (let ((request-key {item-id: item-id, requester: sender, receiver: tx-sender}))
    (match (map-get? transfer-requests request-key)
      request (if (is-eq (get status request) "pending")
                (begin
                  (map-set transfer-requests
                    request-key
                    (merge request {status: "rejected"})
                  )
                  (ok true)
                )
                ERR-INVALID-REQUEST
              )
      ERR-REQUEST-NOT-FOUND
    )
  )
)

;; Cancel a pending transfer request (must be called by the sender/owner)
(define-public (cancel-transfer-request (item-id (string-utf8 36)) (receiver principal))
  (let ((request-key {item-id: item-id, requester: tx-sender, receiver: receiver}))
    (match (map-get? transfer-requests request-key)
      request (if (is-eq (get status request) "pending")
                (begin
                  (map-delete transfer-requests request-key)
                  (ok true)
                )
                ERR-INVALID-REQUEST
              )
      ERR-REQUEST-NOT-FOUND
    )
  )
)

;; Read-only functions

;; Get current owner of an item
(define-read-only (get-current-owner (item-id (string-utf8 36)))
  (match (map-get? item-ownership {item-id: item-id})
    ownership (ok (get owner ownership))
    ERR-ITEM-NOT-FOUND
  )
)

;; Get details of a specific transfer request
(define-read-only (get-transfer-request (item-id (string-utf8 36)) (sender principal) (receiver principal))
  (match (map-get? transfer-requests {item-id: item-id, requester: sender, receiver: receiver})
    request (ok request)
    ERR-REQUEST-NOT-FOUND
  )
)

;; Get details of a specific transfer from history
(define-read-only (get-transfer-details (item-id (string-utf8 36)) (transfer-id uint))
  (match (map-get? transfer-history {item-id: item-id, transfer-id: transfer-id})
    transfer (ok transfer)
    ERR-REQUEST-NOT-FOUND
  )
)

;; Get current ownership details including timestamp and location
(define-read-only (get-item-details (item-id (string-utf8 36)))
  (match (map-get? item-ownership {item-id: item-id})
    ownership (ok ownership)
    ERR-ITEM-NOT-FOUND
  )
)