# 🚨 Error Handling Demo Contract

A comprehensive Clarity smart contract demonstrating robust error handling patterns and defensive coding practices. This contract simulates a simple banking system with multiple failure points to showcase proper error management in blockchain development.

## 🎯 Features

- 👤 **Account Management**: Create accounts with password protection
- 💰 **Deposit/Withdrawal**: Handle funds with comprehensive validation
- 🔄 **Transfer System**: Secure peer-to-peer transfers
- 🔒 **Account Locking**: Administrative controls for security
- 📊 **Daily Limits**: Withdrawal limits with time-based resets
- 🛡️ **Defensive Coding**: Multiple validation layers and error handling

## 🏗️ Contract Structure

### Error Constants
- `ERR-NOT-AUTHORIZED` (100): Unauthorized access attempts
- `ERR-INSUFFICIENT-BALANCE` (101): Insufficient funds
- `ERR-INVALID-AMOUNT` (102): Invalid amount values
- `ERR-USER-NOT-FOUND` (103): Non-existent user operations
- `ERR-ALREADY-EXISTS` (104): Duplicate account creation
- `ERR-INVALID-RECIPIENT` (105): Invalid transfer recipient
- `ERR-INVALID-PASSWORD` (107): Password authentication failures
- `ERR-ACCOUNT-LOCKED` (108): Locked account operations
- `ERR-WITHDRAWAL-LIMIT-EXCEEDED` (109): Daily limit violations

## 🚀 Usage Examples

### Creating an Account
```clarity
(contract-call? .error-handling-demo create-account "my-secure-password")
```

### Making a Deposit
```clarity
(contract-call? .error-handling-demo deposit u1000)
```

### Withdrawing Funds
```clarity
(contract-call? .error-handling-demo withdraw u500 "my-secure-password")
```

### Transferring to Another User
```clarity
(contract-call? .error-handling-demo transfer 'ST1RECIPIENT123 u250 "my-secure-password")
```

### Checking Balance
```clarity
(contract-call? .error-handling-demo get-balance tx-sender)
```

## 🧪 Testing Scenarios

This contract is designed to test various error conditions:

1. **Authentication Errors**: Wrong passwords, missing accounts
2. **Balance Validation**: Insufficient funds, invalid amounts
3. **Authorization Checks**: Non-owner administrative actions
4. **Business Logic**: Daily limits, locked accounts
5. **Input Validation**: Zero amounts, self-transfers

## 📋 Public Functions

- `create-account(password)` - Create new user account
- `deposit(amount)` - Add funds to account
- `withdraw(amount, password)` - Remove funds with authentication
- `transfer(recipient, amount, password)` - Send funds to another user
- `lock-account(user)` - Admin function to lock accounts
- `unlock-account(user)` - Admin function to unlock accounts
- `change-password(old-password, new-password)` - Update account password
- `set-withdrawal-limit(new-limit)` - Admin function to set daily limits

## 📖 Read-Only Functions

- `get-balance(user)` - Check account balance
- `get-daily-withdrawal-limit()` - View current daily limit
- `get-daily-withdrawals(user)` - Check daily withdrawal amount
- `is-account-locked(user)` - Check if account is locked
- `account-exists(user)` - Verify account existence
- `get-contract-owner()` - View contract owner
- `get-total-supply()` - View total supply

## 🔧 Development Setup

1. Install Clarinet
2. Clone this repository
3. Run tests with `clarinet test`
4. Deploy with `clarinet deploy`

## 💡 Learning Objectives

- ✅ Proper error constant definitions
- ✅ Defensive input validation
- ✅ Nested error checking patterns
- ✅ State validation before mutations
- ✅ Authentication and authorization flows
- ✅ Business logic error handling

## 🎓 Educational Value

This contract demonstrates real-world error handling scenarios that developers encounter when building production smart contracts. Each function showcases different defensive coding patterns and error management strategies.
```

**Git Commit Message:**
```
feat: implement error handling demo contract with comprehensive validation
```

**GitHub Pull Request Title:**
```
🚨 Add Error Handling Demo Contract - Banking System with Defensive Coding
```

**GitHub Pull Request Description:**
```
## Summary
Added a comprehensive error handling demonstration contract that simulates a banking system with multiple failure points and defensive coding patterns.

## What's Added
- Complete banking contract with account management, deposits, withdrawals, and transfers
- 10 different error types covering authentication, validation, and business logic failures
- Defensive coding patterns with nested validation checks
- Administrative functions for account management and system configuration
- Comprehensive read-only functions for state inspection
- Educational README with usage examples and testing scenarios

## Key Features
- Password-protected accounts with authentication
- Daily withdrawal limits with time-based resets
- Account locking/unlocking capabilities
- Robust input validation and error handling
- Multiple validation layers demonstrating defensive coding

## Educational Value
Perfect for learning proper error handling in Clarity smart contracts, showcasing real-world validation patterns and defensive programming techniques.


