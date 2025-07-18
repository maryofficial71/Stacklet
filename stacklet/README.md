;; NFT Marketplace Smart Contract
;; Implements SIP-009 NFT standard with marketplace functionality

;; Define the NFT token
(define-non-fungible-token stacks-nft uint)

;; Contract constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-insufficient-funds (err u102))
(define-constant err-listing-not-found (err u103))
(define-constant err-token-not-found (err u104))
(define-constant err-invalid-price (err u105))
(define-constant err-transfer-failed (err u106))

;; Data variables
(define-data-var last-token-id uint u0)
(define-data-var contract-uri (string-utf8 256) u"")

;; Data maps
(define-map token-metadata uint {
  name: (string-utf8 64),
  description: (string-utf8 256),
  image: (string-utf8 256),
  creator: principal
})

(define-map listings uint {
  owner: principal,
  price: uint,
  active: bool
})

(define-map royalties uint {
  creator: principal,
  percentage: uint
})

;; Get token URI (SIP-009 requirement)
(define-read-only (get-token-uri (token-id uint))
  (ok (some (var-get contract-uri)))
)

;; Get last token ID (SIP-009 requirement)
(define-read-only (get-last-token-id)
  (ok (var-get last-token-id))
)

;; Get token owner (SIP-009 requirement)
(define-read-only (get-owner (token-id uint))
  (ok (nft-get-owner? stacks-nft token-id))
)

;; Transfer function (SIP-009 requirement)
(define-public (transfer (token-id uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) err-not-token-owner)
    (nft-transfer? stacks-nft token-id sender recipient)
  )
)

;; Mint new NFT (admin only)
(define-public (mint (recipient principal) (name (string-utf8 64)) (description (string-utf8 256)) (image (string-utf8 256)))
  (let ((new-id (+ (var-get last-token-id) u1)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (try! (nft-mint? stacks-nft new-id recipient))
    (map-set token-metadata new-id {
      name: name,
      description: description,
      image: image,
      creator: tx-sender
    })
    (map-set royalties new-id {
      creator: tx-sender,
      percentage: u5 ;; 5% royalty
    })
    (var-set last-token-id new-id)
    (ok new-id)
  )
)

;; Get token metadata
(define-read-only (get-token-metadata (token-id uint))
  (map-get? token-metadata token-id)
)

;; List NFT for sale
(define-public (list-nft (token-id uint) (price uint))
  (let ((token-owner (unwrap! (nft-get-owner? stacks-nft token-id) err-token-not-found)))
    (asserts! (is-eq tx-sender token-owner) err-not-token-owner)
    (asserts! (> price u0) err-invalid-price)
    (try! (nft-transfer? stacks-nft token-id tx-sender (as-contract tx-sender)))
    (map-set listings token-id {
      owner: tx-sender,
      price: price,
      active: true
    })
    (ok true)
  )
)

;; Buy NFT from marketplace
(define-public (buy-nft (token-id uint))
  (let ((listing (unwrap! (map-get? listings token-id) err-listing-not-found)))
    (asserts! (get active listing) err-listing-not-found)
    (asserts! (>= (stx-get-balance tx-sender) (get price listing)) err-insufficient-funds)
    
    ;; Calculate royalty
    (let ((royalty-info (unwrap! (map-get? royalties token-id) err-token-not-found))
          (sale-price (get price listing))
          (royalty-amount (/ (* sale-price (get percentage royalty-info)) u100))
          (seller-amount (- sale-price royalty-amount)))
      
      ;; Transfer STX to seller
      (try! (stx-transfer? seller-amount tx-sender (get owner listing)))
      
      ;; Transfer royalty to creator
      (try! (stx-transfer? royalty-amount tx-sender (get creator royalty-info)))
      
      ;; Transfer NFT to buyer
      (try! (nft-transfer? stacks-nft token-id (as-contract tx-sender) tx-sender))
      
      ;; Remove listing
      (map-delete listings token-id)
      (ok true)
    )
  )
)

;; Cancel listing
(define-public (cancel-listing (token-id uint))
  (let ((listing (unwrap! (map-get? listings token-id) err-listing-not-found)))
    (asserts! (is-eq tx-sender (get owner listing)) err-not-token-owner)
    (asserts! (get active listing) err-listing-not-found)
    (try! (nft-transfer? stacks-nft token-id (as-contract tx-sender) tx-sender))
    (map-delete listings token-id)
    (ok true)
  )
)

;; Get listing details
(define-read-only (get-listing (token-id uint))
  (map-get? listings token-id)
)

;; Get all active listings (helper function)
(define-read-only (get-listing-info (token-id uint))
  (let ((listing (map-get? listings token-id))
        (metadata (map-get? token-metadata token-id)))
    {
      listing: listing,
      metadata: metadata
    }
  )
)

;; Update contract URI (admin only)
(define-public (set-contract-uri (new-uri (string-utf8 256)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set contract-uri new-uri)
    (ok true)
  )
)

;; Get contract URI
(define-read-only (get-contract-uri)
  (var-get contract-uri)
)

;; Emergency functions (admin only)
(define-public (emergency-withdraw (token-id uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (nft-transfer? stacks-nft token-id (as-contract tx-sender) tx-sender)
  )
)

;; Batch operations
(define-public (batch-mint (recipients (list 10 principal)) (names (list 10 (string-utf8 64))) (descriptions (list 10 (string-utf8 256))) (images (list 10 (string-utf8 256))))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-eq (len recipients) (len names)) err-invalid-price)
    (asserts! (is-eq (len names) (len descriptions)) err-invalid-price)
    (asserts! (is-eq (len descriptions) (len images)) err-invalid-price)
    (ok (map batch-mint-helper recipients names descriptions images))
  )
)

(define-private (batch-mint-helper (recipient principal) (name (string-utf8 64)) (description (string-utf8 256)) (image (string-utf8 256)))
  (mint recipient name description image)
)

;; Utility functions
(define-read-only (get-token-count)
  (var-get last-token-id)
)

(define-read-only (get-tokens-by-owner (owner principal))
  (let ((total-tokens (var-get last-token-id)))
    (filter is-owner-of-token (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10))
  )
)

(define-private (is-owner-of-token (token-id uint))
  (is-eq (some tx-sender) (nft-get-owner? stacks-nft token-id))
)