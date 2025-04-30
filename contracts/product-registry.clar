;; product-registry
;; 
;; A smart contract for managing the registration and lifecycle tracking of products
;; in the supply chain. This contract enables manufacturers to register products,
;; tracks ownership transfers, and maintains a complete provenance history that
;; can be queried to verify authenticity and review chain of custody.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PRODUCT-EXISTS (err u101))
(define-constant ERR-PRODUCT-NOT-FOUND (err u102))
(define-constant ERR-NOT-OWNER (err u103))
(define-constant ERR-INVALID-INPUT (err u104))
(define-constant ERR-INVALID-TRANSFER (err u105))

;; Data Maps

;; Stores the primary product information
(define-map products
  { product-id: (buff 32) }
  {
    manufacturer: principal,
    current-owner: principal,
    origin: (string-ascii 64),
    manufacturing-date: uint,
    materials: (string-ascii 256),
    metadata: (string-utf8 1024),
    registered-at: uint
  }
)

;; Tracks the complete history of ownership transfers for each product
(define-map ownership-history
  { product-id: (buff 32), index: uint }
  {
    from: principal,
    to: principal,
    timestamp: uint,
    notes: (string-utf8 256)
  }
)

;; Counter for ownership history entries per product
(define-map history-counters
  { product-id: (buff 32) }
  { count: uint }
)

;; Maps manufacturers to their products
(define-map manufacturer-products
  { manufacturer: principal }
  { product-ids: (list 100 (buff 32)) }
)

;; Maps owners to their current products
(define-map owner-products
  { owner: principal }
  { product-ids: (list 100 (buff 32)) }
)

;; Private Functions

;; Generates a unique product ID based on manufacturer, timestamp, and product details
(define-private (generate-product-id (manufacturer principal) (origin (string-ascii 64)) (timestamp uint))
  (sha256 (concat (concat (principal->buff manufacturer) (string->buff origin)) (uint->buff timestamp)))
)

;; Adds a product ID to a principal's product list (either manufacturer or owner)
(define-private (add-product-to-principal-list (principal-key principal) (product-id (buff 32)) (map-name (string-ascii 20)))
  (let
    (
      (current-list
        (if (is-eq map-name "manufacturer")
          (default-to { product-ids: (list) } (map-get? manufacturer-products { manufacturer: principal-key }))
          (default-to { product-ids: (list) } (map-get? owner-products { owner: principal-key }))
        )
      )
      (updated-list (unwrap-panic (as-max-len? (append (get product-ids current-list) product-id) u100)))
    )
    (if (is-eq map-name "manufacturer")
      (map-set manufacturer-products { manufacturer: principal-key } { product-ids: updated-list })
      (map-set owner-products { owner: principal-key } { product-ids: updated-list })
    )
  )
)

;; Removes a product ID from a principal's product list
(define-private (remove-product-from-owner-list (owner principal) (product-id (buff 32)))
  (let
    (
      (current-list (default-to { product-ids: (list) } (map-get? owner-products { owner: owner })))
      (filtered-list 
        (filter 
          (lambda (id) (not (is-eq id product-id))) 
          (get product-ids current-list)
        )
      )
    )
    (map-set owner-products { owner: owner } { product-ids: filtered-list })
  )
)

;; Adds an entry to the ownership history of a product
(define-private (add-history-entry (product-id (buff 32)) (from principal) (to principal) (notes (string-utf8 256)))
  (let
    (
      (counter (default-to { count: u0 } (map-get? history-counters { product-id: product-id })))
      (index (get count counter))
      (next-index (+ index u1))
    )
    (begin
      (map-set ownership-history
        { product-id: product-id, index: index }
        {
          from: from,
          to: to,
          timestamp: block-height,
          notes: notes
        }
      )
      (map-set history-counters { product-id: product-id } { count: next-index })
      next-index
    )
  )
)

;; Read-only Functions

;; Retrieves product information by ID
(define-read-only (get-product (product-id (buff 32)))
  (map-get? products { product-id: product-id })
)

;; Checks if a product exists
(define-read-only (product-exists (product-id (buff 32)))
  (is-some (map-get? products { product-id: product-id }))
)

;; Gets all products registered by a specific manufacturer
(define-read-only (get-manufacturer-products (manufacturer principal))
  (default-to { product-ids: (list) } (map-get? manufacturer-products { manufacturer: manufacturer }))
)

;; Gets all products owned by a specific owner
(define-read-only (get-owner-products (owner principal))
  (default-to { product-ids: (list) } (map-get? owner-products { owner: owner }))
)

;; Gets the number of history entries for a product
(define-read-only (get-history-length (product-id (buff 32)))
  (get count (default-to { count: u0 } (map-get? history-counters { product-id: product-id })))
)

;; Gets a specific history entry for a product
(define-read-only (get-history-entry (product-id (buff 32)) (index uint))
  (map-get? ownership-history { product-id: product-id, index: index })
)

;; Returns the complete history of a product
(define-read-only (get-product-history (product-id (buff 32)))
  (let
    (
      (history-length (get-history-length product-id))
      (indices (list))
    )
    ;; Create a list from 0 to history-length-1
    (fold 
      (lambda (index acc)
        (unwrap-panic (as-max-len? (append acc index) u100))
      )
      indices
      (generate-indices history-length)
    )
  )
)

;; Helper function to generate list of indices for fold operation
(define-read-only (generate-indices (count uint))
  (if (<= count u0)
    (list)
    (generate-indices-iter count u0 (list))
  )
)

(define-read-only (generate-indices-iter (count uint) (current uint) (acc (list 100 uint)))
  (if (>= current count)
    acc
    (generate-indices-iter 
      count 
      (+ current u1) 
      (unwrap-panic (as-max-len? (append acc current) u100))
    )
  )
)

;; Checks if a principal is the current owner of a product
(define-read-only (is-product-owner (product-id (buff 32)) (owner principal))
  (match (map-get? products { product-id: product-id })
    product (is-eq (get current-owner product) owner)
    false
  )
)

;; Public Functions

;; Registers a new product in the system
(define-public (register-product 
  (origin (string-ascii 64))
  (manufacturing-date uint)
  (materials (string-ascii 256))
  (metadata (string-utf8 1024))
)
  (let
    (
      (manufacturer tx-sender)
      (timestamp block-height)
      (product-id (generate-product-id manufacturer origin timestamp))
    )
    ;; Check if product already exists
    (asserts! (not (product-exists product-id)) ERR-PRODUCT-EXISTS)
    
    ;; Register the product
    (map-set products
      { product-id: product-id }
      {
        manufacturer: manufacturer,
        current-owner: manufacturer,
        origin: origin,
        manufacturing-date: manufacturing-date,
        materials: materials,
        metadata: metadata,
        registered-at: timestamp
      }
    )
    
    ;; Add to manufacturer's product list
    (add-product-to-principal-list manufacturer product-id "manufacturer")
    
    ;; Add to owner's product list (initially also the manufacturer)
    (add-product-to-principal-list manufacturer product-id "owner")
    
    ;; Create initial history entry (from none to manufacturer)
    (add-history-entry product-id 'SP000000000000000000002Q6VF78 manufacturer "Product registered")
    
    (ok product-id)
  )
)

;; Transfers ownership of a product to a new owner
(define-public (transfer-ownership (product-id (buff 32)) (new-owner principal) (notes (string-utf8 256)))
  (let
    (
      (current-owner tx-sender)
    )
    ;; Check if product exists and sender is the current owner
    (asserts! (product-exists product-id) ERR-PRODUCT-NOT-FOUND)
    (asserts! (is-product-owner product-id current-owner) ERR-NOT-OWNER)
    (asserts! (not (is-eq current-owner new-owner)) ERR-INVALID-TRANSFER)
    
    ;; Update the product's current owner
    (match (map-get? products { product-id: product-id })
      product 
      (begin
        (map-set products
          { product-id: product-id }
          (merge product { current-owner: new-owner })
        )
        
        ;; Remove product from current owner's list
        (remove-product-from-owner-list current-owner product-id)
        
        ;; Add product to new owner's list
        (add-product-to-principal-list new-owner product-id "owner")
        
        ;; Add history entry
        (add-history-entry product-id current-owner new-owner notes)
        
        (ok true)
      )
      ERR-PRODUCT-NOT-FOUND
    )
  )
)

;; Updates the product metadata
(define-public (update-product-metadata (product-id (buff 32)) (new-metadata (string-utf8 1024)))
  (let
    (
      (current-owner tx-sender)
    )
    ;; Check if product exists and sender is the current owner
    (asserts! (product-exists product-id) ERR-PRODUCT-NOT-FOUND)
    (asserts! (is-product-owner product-id current-owner) ERR-NOT-OWNER)
    
    ;; Update the product metadata
    (match (map-get? products { product-id: product-id })
      product 
      (begin
        (map-set products
          { product-id: product-id }
          (merge product { metadata: new-metadata })
        )
        (ok true)
      )
      ERR-PRODUCT-NOT-FOUND
    )
  )
)

;; Makes a record of an event in the product's history without changing ownership
(define-public (record-product-event (product-id (buff 32)) (event-notes (string-utf8 256)))
  (let
    (
      (current-owner tx-sender)
    )
    ;; Check if product exists and sender is the current owner
    (asserts! (product-exists product-id) ERR-PRODUCT-NOT-FOUND)
    (asserts! (is-product-owner product-id current-owner) ERR-NOT-OWNER)
    
    ;; Add history entry with same sender and recipient to indicate an event
    (add-history-entry product-id current-owner current-owner event-notes)
    
    (ok true)
  )
)