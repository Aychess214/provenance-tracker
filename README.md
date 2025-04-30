# provenance-tracker

A blockchain-based product origin tracking system using Clarity smart contracts on Stacks

## Overview

The provenance-tracker system enables comprehensive tracking of products throughout their lifecycle in the supply chain. It allows manufacturers, distributors, retailers, and consumers to verify the authenticity and history of products with cryptographic certainty.

## Features

- Product registration and lifecycle tracking
- Secure ownership transfers between supply chain participants  
- Product authenticity verification 
- Complete provenance history recording
- Participant identity and role management
- Credential issuance and verification

## Smart Contract Architecture

The system consists of four main smart contracts:

### Product Registry (`product-registry`)
- Manages product registration and lifecycle tracking
- Records product details and specifications
- Maintains complete ownership history
- Links products to manufacturers and current owners

### Ownership Transfer (`ownership-transfer`) 
- Handles secure transfer of product ownership
- Manages transfer requests and approvals
- Records transfer history and location data
- Validates transfer authenticity

### Verification Engine (`verification-engine`)
- Provides product authenticity verification
- Issues and validates verifiable credentials
- Manages verification requests and approvals
- Maintains verification status and expiry

### Participant Registry (`participant-registry`)
- Manages supply chain participant registration
- Controls role-based access and permissions
- Tracks participant reputation and status
- Verifies participant identities

## Key Functions

### Product Management
```clarity
;; Register a new product
(contract-call? .product-registry register-product 
    origin 
    manufacturing-date
    materials 
    metadata)

;; Transfer product ownership
(contract-call? .ownership-transfer request-transfer
    product-id
    receiver
    location)

;; Verify product authenticity
(contract-call? .verification-engine verify-credential
    credential-id)
```

### Participant Management
```clarity
;; Register as supply chain participant
(contract-call? .participant-registry register-participant
    name
    role
    metadata)

;; Update participant status
(contract-call? .participant-registry update-participant-status
    participant
    new-status)
```

## Security Model

The system implements several security measures:

- Role-based access control for all operations
- Cryptographic verification of ownership transfers
- Time-bound validity for verifications and credentials
- Participant reputation tracking
- Identity verification requirements
- Admin-controlled participant management

## Getting Started

1. Deploy the smart contracts to the Stacks blockchain
2. Register as a participant through the participant registry
3. Get verified by an admin to begin operations
4. Register products and manage ownership transfers
5. Issue and verify credentials as needed

## Contributing

Contributions are welcome! Please read the contributing guidelines before submitting pull requests.

## License

This project is licensed under the MIT License - see the LICENSE file for details.