# CQRS Architecture Guide: Command Query Responsibility Segregation with Goja-Watermill

## Table of Contents

1. [Introduction to CQRS](#introduction-to-cqrs)
2. [CQRS Architecture Fundamentals](#cqrs-architecture-fundamentals)
3. [Core Patterns and Principles](#core-patterns-and-principles)
4. [Implementation with Watermill](#implementation-with-watermill)
5. [JavaScript Integration](#javascript-integration)
6. [Detailed Examples Walkthrough](#detailed-examples-walkthrough)
7. [Advanced Patterns](#advanced-patterns)
8. [Performance and Scalability](#performance-and-scalability)
9. [Best Practices](#best-practices)
10. [Troubleshooting](#troubleshooting)

## Introduction to CQRS

**Command Query Responsibility Segregation (CQRS)** is an architectural pattern that separates read and write operations into distinct models. This separation allows for optimized data structures, improved scalability, and clearer business logic organization.

### Why CQRS?

**Traditional Approach Problems:**
- Single model serves both reads and writes
- Complex queries impact write performance
- Difficult to scale reads and writes independently
- Business logic scattered across the application

**CQRS Benefits:**
- **Separation of Concerns**: Clear distinction between commands (writes) and queries (reads)
- **Independent Scaling**: Scale read and write sides independently
- **Optimized Models**: Different data structures for different use cases
- **Event-Driven Architecture**: Natural fit with event sourcing and messaging
- **Business Logic Clarity**: Commands represent business intentions clearly

### When to Use CQRS

**Good Candidates:**
- Complex business domains with different read/write patterns
- High-traffic applications requiring independent scaling
- Event-driven systems with complex workflows
- Applications with sophisticated reporting requirements
- Microservices architectures

**Avoid CQRS When:**
- Simple CRUD applications
- Small applications with minimal complexity
- Teams unfamiliar with event-driven patterns
- Tight coupling between reads and writes is acceptable

## CQRS Architecture Fundamentals

### Core Components

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Client App    │    │  Command Side   │    │   Event Store   │
│                 │    │                 │    │                 │
│  ┌───────────┐  │    │  ┌───────────┐  │    │  ┌───────────┐  │
│  │ Commands  │──┼────┼─→│  Handlers │──┼────┼─→│  Events   │  │
│  └───────────┘  │    │  └───────────┘  │    │  └───────────┘  │
│                 │    │                 │    │                 │
│  ┌───────────┐  │    │                 │    │                 │
│  │ Queries   │──┼────┼─────────────────┼────┼─────────────────┼──┐
│  └───────────┘  │    │                 │    │                 │  │
└─────────────────┘    └─────────────────┘    └─────────────────┘  │
                                                                   │
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐  │
│   Query Side    │    │  Read Models    │    │   Projections   │  │
│                 │    │                 │    │                 │  │
│  ┌───────────┐  │    │  ┌───────────┐  │    │  ┌───────────┐  │  │
│  │ Handlers  │←─┼────┼──│   Views   │←─┼────┼──│ Builders  │←─┼──┘
│  └───────────┘  │    │  └───────────┘  │    │  └───────────┘  │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

### 1. Command Side (Write Model)

**Commands** represent business intentions:
- `CreateUser`
- `UpdateUserProfile`
- `ProcessPayment`
- `CancelOrder`

**Command Handlers** process business logic:
- Validate business rules
- Execute domain operations
- Publish events for side effects

**Events** represent what happened:
- `UserCreated`
- `UserProfileUpdated`
- `PaymentProcessed`
- `OrderCancelled`

### 2. Query Side (Read Model)

**Queries** request data:
- `GetUserProfile`
- `ListActiveOrders`
- `GenerateReport`

**Query Handlers** return optimized views:
- Pre-computed aggregations
- Denormalized data structures
- Cached results

**Read Models** store query-optimized data:
- Different schema than write model
- Optimized for specific use cases
- Built from events via projections

## Core Patterns and Principles

### 1. Command Pattern

Commands are immutable data structures representing user intentions:

```javascript
// Command Structure
const createUserCommand = {
    name: "CreateUser",
    payload: {
        email: "john.doe@example.com",
        name: "John Doe",
        role: "customer"
    },
    metadata: {
        userId: "admin-123",
        timestamp: "2025-01-01T10:00:00Z",
        correlationId: "req-456"
    }
};
```

**Command Characteristics:**
- **Imperative**: Express what should happen
- **Immutable**: Cannot be changed after creation
- **Validated**: Business rules enforced before processing
- **Traceable**: Include metadata for auditing

### 2. Event Pattern

Events are immutable facts about what happened:

```javascript
// Event Structure
const userCreatedEvent = {
    name: "UserCreated",
    payload: {
        userId: "user-789",
        email: "john.doe@example.com",
        name: "John Doe",
        role: "customer",
        createdAt: "2025-01-01T10:00:01Z"
    },
    metadata: {
        commandId: "cmd-123",
        version: 1,
        timestamp: "2025-01-01T10:00:01Z"
    }
};
```

**Event Characteristics:**
- **Past Tense**: Describe what happened
- **Immutable**: Historical facts that cannot change
- **Causally Ordered**: Maintain sequence of operations
- **Replayable**: Can reconstruct state from events

### 3. Handler Pattern

#### Command Handlers (One Command → One Handler)

```javascript
// Command Handler - Business Logic
cqrs.addCommandHandler("CreateUser", function(command) {
    // 1. Validate Command
    if (!command.payload.email || !command.payload.name) {
        throw new Error("Email and name are required");
    }
    
    // 2. Check Business Rules
    if (!isValidEmail(command.payload.email)) {
        throw new Error("Invalid email format");
    }
    
    // 3. Execute Business Logic
    const userId = generateUserId();
    
    // 4. Publish Events
    cqrs.publishEvent({
        name: "UserCreated",
        payload: {
            userId: userId,
            email: command.payload.email,
            name: command.payload.name,
            role: command.payload.role || "customer"
        },
        metadata: {
            source: "CreateUserHandler",
            timestamp: new Date().toISOString()
        }
    });
});
```

#### Event Handlers (One Event → Multiple Handlers)

```javascript
// Event Handler 1 - Send Welcome Email
cqrs.addEventHandler("UserCreated", "WelcomeEmailHandler", function(event) {
    console.log("📧 Sending welcome email to:", event.payload.email);
    
    // Send email logic here
    sendWelcomeEmail(event.payload.email, event.payload.name);
    
    // Publish follow-up event
    cqrs.publishEvent({
        name: "WelcomeEmailSent",
        payload: {
            userId: event.payload.userId,
            email: event.payload.email,
            sentAt: new Date().toISOString()
        }
    });
});

// Event Handler 2 - Update Analytics
cqrs.addEventHandler("UserCreated", "AnalyticsHandler", function(event) {
    console.log("📊 Recording user creation analytics");
    
    // Analytics logic here
    recordUserCreation(event.payload.userId, event.payload.role);
});

// Event Handler 3 - Create User Profile
cqrs.addEventHandler("UserCreated", "ProfileHandler", function(event) {
    console.log("👤 Creating user profile");
    
    // Profile creation logic here
    createUserProfile(event.payload);
});
```

## Implementation with Watermill

### Architecture Overview

```
JavaScript Layer (Goja VM)
├── CQRS Facade
│   ├── sendCommand()
│   ├── publishEvent()
│   ├── addCommandHandler()
│   └── addEventHandler()
│
Go Layer (Watermill)
├── Command Bus ──→ Command Processor ──→ Command Handlers
├── Event Bus ──→ Event Processor ──→ Event Handlers
├── Message Router (coordinates all message flow)
└── PubSub (Memory/Redis/etc.)
```

### Component Responsibilities

#### 1. Command Bus
- **Purpose**: Publishes commands to appropriate topics
- **Topic Generation**: `commands.{CommandName}`
- **Marshaling**: JSON serialization with metadata
- **Routing**: Ensures commands reach correct handlers

#### 2. Command Processor
- **Purpose**: Subscribes to command topics and routes to handlers
- **Rule**: One command type → One handler (CQRS principle)
- **Error Handling**: Failed commands can be retried or dead-lettered
- **Concurrency**: Processes commands concurrently when safe

#### 3. Event Bus
- **Purpose**: Publishes events to appropriate topics
- **Topic Generation**: `events.{EventName}`
- **Fan-out**: Single event can reach multiple handlers
- **Ordering**: Maintains causal ordering when needed

#### 4. Event Processor
- **Purpose**: Subscribes to event topics and routes to handlers
- **Rule**: One event type → Multiple handlers allowed
- **Idempotency**: Handlers should be idempotent
- **Scaling**: Can scale handlers independently

### Topic Generation Strategy

```javascript
// Default Topic Generation
const commandTopic = `commands.${commandName}`;  // commands.CreateUser
const eventTopic = `events.${eventName}`;        // events.UserCreated

// Custom Topic Generation
const cqrs = watermill.createCQRS("memory", {
    commandsTopic: "app.commands",  // app.commands.CreateUser
    eventsTopic: "app.events"       // app.events.UserCreated
});
```

## JavaScript Integration

### Setting Up CQRS

```javascript
// 1. Create CQRS Facade
const cqrs = watermill.createCQRS("memory");

// 2. Add Command Handlers
cqrs.addCommandHandler("CreateUser", function(command) {
    // Business logic here
});

// 3. Add Event Handlers
cqrs.addEventHandler("UserCreated", "EmailHandler", function(event) {
    // Side effect logic here
});

// 4. Start Watermill
watermill.start();

// 5. Send Commands
cqrs.sendCommand({
    name: "CreateUser",
    payload: { email: "user@example.com", name: "User" }
});
```

### Error Handling

```javascript
// Command Handler with Error Handling
cqrs.addCommandHandler("ProcessPayment", function(command) {
    try {
        // Validate payment
        if (command.payload.amount <= 0) {
            throw new Error("Amount must be positive");
        }
        
        // Process payment
        const result = processPayment(command.payload);
        
        // Publish success event
        cqrs.publishEvent({
            name: "PaymentProcessed",
            payload: {
                paymentId: result.id,
                amount: command.payload.amount,
                status: "completed"
            }
        });
        
    } catch (error) {
        // Publish failure event
        cqrs.publishEvent({
            name: "PaymentFailed",
            payload: {
                amount: command.payload.amount,
                error: error.message,
                status: "failed"
            }
        });
        
        // Re-throw to trigger retry mechanism
        throw error;
    }
});
```

### Metadata and Correlation

```javascript
// Command with Rich Metadata
cqrs.sendCommand({
    name: "CreateOrder",
    payload: {
        customerId: "cust-123",
        items: [
            { productId: "prod-456", quantity: 2 },
            { productId: "prod-789", quantity: 1 }
        ]
    },
    metadata: {
        correlationId: "order-flow-001",
        causationId: "user-action-123",
        userId: "user-456",
        sessionId: "session-789",
        timestamp: new Date().toISOString(),
        source: "WebUI"
    }
});

// Event Handler Using Metadata
cqrs.addEventHandler("OrderCreated", "InventoryHandler", function(event) {
    const correlationId = event.metadata.correlationId;
    
    console.log(`Processing inventory for order ${correlationId}`);
    
    // Use correlation ID for tracking
    updateInventory(event.payload.items, correlationId);
});
```

## Detailed Examples Walkthrough

### Example 1: E-commerce Order Processing

This example demonstrates a complete order processing workflow with multiple bounded contexts.

#### Domain Model

```
Order Aggregate:
├── CreateOrder (Command)
├── CancelOrder (Command)
├── OrderCreated (Event)
├── OrderCancelled (Event)

Payment Aggregate:
├── ProcessPayment (Command)
├── RefundPayment (Command)
├── PaymentProcessed (Event)
├── PaymentFailed (Event)

Inventory Aggregate:
├── ReserveItems (Command)
├── ReleaseItems (Command)
├── ItemsReserved (Event)
├── ItemsReleased (Event)
```

#### Implementation

```javascript
// === ORDER AGGREGATE ===

// Create Order Command Handler
cqrs.addCommandHandler("CreateOrder", function(command) {
    console.log("🛒 Creating order for customer:", command.payload.customerId);
    
    // Validate order
    if (!command.payload.items || command.payload.items.length === 0) {
        throw new Error("Order must contain at least one item");
    }
    
    // Calculate total
    const total = command.payload.items.reduce((sum, item) => 
        sum + (item.price * item.quantity), 0);
    
    // Generate order ID
    const orderId = `order-${Date.now()}`;
    
    console.log(`✅ Order created: ${orderId}, Total: $${total}`);
    
    // Publish OrderCreated event
    cqrs.publishEvent({
        name: "OrderCreated",
        payload: {
            orderId: orderId,
            customerId: command.payload.customerId,
            items: command.payload.items,
            total: total,
            status: "pending",
            createdAt: new Date().toISOString()
        },
        metadata: {
            correlationId: command.metadata.correlationId,
            source: "OrderHandler"
        }
    });
});

// === INVENTORY AGGREGATE ===

// Reserve Items when Order Created
cqrs.addEventHandler("OrderCreated", "InventoryReservationHandler", function(event) {
    console.log("📦 Reserving inventory for order:", event.payload.orderId);
    
    // Check inventory availability
    const availableItems = checkInventoryAvailability(event.payload.items);
    
    if (availableItems.allAvailable) {
        // Reserve items
        reserveInventoryItems(event.payload.items);
        
        console.log("✅ Inventory reserved for order:", event.payload.orderId);
        
        // Publish ItemsReserved event
        cqrs.publishEvent({
            name: "ItemsReserved",
            payload: {
                orderId: event.payload.orderId,
                items: event.payload.items,
                reservedAt: new Date().toISOString()
            },
            metadata: {
                correlationId: event.metadata.correlationId,
                source: "InventoryReservationHandler"
            }
        });
    } else {
        console.log("❌ Insufficient inventory for order:", event.payload.orderId);
        
        // Publish ItemsNotAvailable event
        cqrs.publishEvent({
            name: "ItemsNotAvailable",
            payload: {
                orderId: event.payload.orderId,
                unavailableItems: availableItems.unavailable
            },
            metadata: {
                correlationId: event.metadata.correlationId,
                source: "InventoryReservationHandler"
            }
        });
    }
});

// === PAYMENT AGGREGATE ===

// Process Payment when Items Reserved
cqrs.addEventHandler("ItemsReserved", "PaymentHandler", function(event) {
    console.log("💳 Processing payment for order:", event.payload.orderId);
    
    // Send ProcessPayment command
    cqrs.sendCommand({
        name: "ProcessPayment",
        payload: {
            orderId: event.payload.orderId,
            amount: calculateOrderTotal(event.payload.items),
            paymentMethod: "credit_card"
        },
        metadata: {
            correlationId: event.metadata.correlationId,
            causationId: event.payload.orderId,
            source: "PaymentHandler"
        }
    });
});

// Process Payment Command Handler
cqrs.addCommandHandler("ProcessPayment", function(command) {
    console.log("💰 Processing payment:", command.payload.amount);
    
    try {
        // Simulate payment processing
        const paymentResult = processPaymentWithGateway(command.payload);
        
        if (paymentResult.success) {
            console.log("✅ Payment processed successfully");
            
            // Publish PaymentProcessed event
            cqrs.publishEvent({
                name: "PaymentProcessed",
                payload: {
                    orderId: command.payload.orderId,
                    paymentId: paymentResult.paymentId,
                    amount: command.payload.amount,
                    processedAt: new Date().toISOString()
                },
                metadata: {
                    correlationId: command.metadata.correlationId,
                    source: "PaymentHandler"
                }
            });
        } else {
            throw new Error(paymentResult.error);
        }
    } catch (error) {
        console.log("❌ Payment failed:", error.message);
        
        // Publish PaymentFailed event
        cqrs.publishEvent({
            name: "PaymentFailed",
            payload: {
                orderId: command.payload.orderId,
                amount: command.payload.amount,
                error: error.message,
                failedAt: new Date().toISOString()
            },
            metadata: {
                correlationId: command.metadata.correlationId,
                source: "PaymentHandler"
            }
        });
    }
});

// === ORDER COMPLETION ===

// Complete Order when Payment Processed
cqrs.addEventHandler("PaymentProcessed", "OrderCompletionHandler", function(event) {
    console.log("🎉 Completing order:", event.payload.orderId);
    
    // Update order status
    updateOrderStatus(event.payload.orderId, "completed");
    
    // Publish OrderCompleted event
    cqrs.publishEvent({
        name: "OrderCompleted",
        payload: {
            orderId: event.payload.orderId,
            paymentId: event.payload.paymentId,
            completedAt: new Date().toISOString()
        },
        metadata: {
            correlationId: event.metadata.correlationId,
            source: "OrderCompletionHandler"
        }
    });
});

// === NOTIFICATION SERVICES ===

// Send Order Confirmation Email
cqrs.addEventHandler("OrderCompleted", "EmailNotificationHandler", function(event) {
    console.log("📧 Sending order confirmation email");
    
    sendOrderConfirmationEmail(event.payload.orderId);
    
    console.log("✅ Order confirmation email sent");
});

// Update Customer Dashboard
cqrs.addEventHandler("OrderCompleted", "DashboardUpdateHandler", function(event) {
    console.log("📊 Updating customer dashboard");
    
    updateCustomerDashboard(event.payload.orderId);
    
    console.log("✅ Customer dashboard updated");
});

// === ERROR HANDLING ===

// Handle Payment Failures
cqrs.addEventHandler("PaymentFailed", "PaymentFailureHandler", function(event) {
    console.log("🔄 Handling payment failure for order:", event.payload.orderId);
    
    // Release reserved inventory
    cqrs.sendCommand({
        name: "ReleaseItems",
        payload: {
            orderId: event.payload.orderId,
            reason: "payment_failed"
        },
        metadata: {
            correlationId: event.metadata.correlationId,
            source: "PaymentFailureHandler"
        }
    });
    
    // Update order status
    updateOrderStatus(event.payload.orderId, "payment_failed");
    
    // Send failure notification
    sendPaymentFailureNotification(event.payload.orderId);
});

// Handle Inventory Unavailability
cqrs.addEventHandler("ItemsNotAvailable", "InventoryFailureHandler", function(event) {
    console.log("📦 Handling inventory unavailability for order:", event.payload.orderId);
    
    // Update order status
    updateOrderStatus(event.payload.orderId, "inventory_unavailable");
    
    // Send notification to customer
    sendInventoryUnavailableNotification(event.payload.orderId, event.payload.unavailableItems);
});
```

#### Usage Example

```javascript
// Start the order processing workflow
cqrs.sendCommand({
    name: "CreateOrder",
    payload: {
        customerId: "customer-123",
        items: [
            { productId: "laptop-001", quantity: 1, price: 999.99 },
            { productId: "mouse-002", quantity: 2, price: 29.99 }
        ]
    },
    metadata: {
        correlationId: "order-flow-" + Date.now(),
        userId: "customer-123",
        source: "WebStore"
    }
});
```

#### Expected Flow

```
1. CreateOrder → OrderCreated
2. OrderCreated → InventoryReservationHandler → ItemsReserved
3. ItemsReserved → PaymentHandler → ProcessPayment → PaymentProcessed
4. PaymentProcessed → OrderCompletionHandler → OrderCompleted
5. OrderCompleted → EmailNotificationHandler (send email)
6. OrderCompleted → DashboardUpdateHandler (update UI)
```

### Example 2: User Management System

This example shows user lifecycle management with role-based access control.

```javascript
// === USER CREATION ===

cqrs.addCommandHandler("CreateUser", function(command) {
    console.log("👤 Creating user:", command.payload.email);
    
    // Validate user data
    if (!isValidEmail(command.payload.email)) {
        throw new Error("Invalid email format");
    }
    
    if (userExists(command.payload.email)) {
        throw new Error("User already exists");
    }
    
    // Generate user ID and hash password
    const userId = generateUserId();
    const hashedPassword = hashPassword(command.payload.password);
    
    // Create user record
    const user = {
        userId: userId,
        email: command.payload.email,
        name: command.payload.name,
        role: command.payload.role || "user",
        hashedPassword: hashedPassword,
        createdAt: new Date().toISOString(),
        status: "active"
    };
    
    // Store user (this would be in your write database)
    storeUser(user);
    
    console.log("✅ User created:", userId);
    
    // Publish UserCreated event
    cqrs.publishEvent({
        name: "UserCreated",
        payload: {
            userId: userId,
            email: user.email,
            name: user.name,
            role: user.role,
            createdAt: user.createdAt
        },
        metadata: {
            correlationId: command.metadata.correlationId,
            source: "UserCreationHandler"
        }
    });
});

// === WELCOME WORKFLOW ===

// Send Welcome Email
cqrs.addEventHandler("UserCreated", "WelcomeEmailHandler", function(event) {
    console.log("📧 Sending welcome email to:", event.payload.email);
    
    const emailContent = generateWelcomeEmail(event.payload.name, event.payload.role);
    sendEmail(event.payload.email, "Welcome!", emailContent);
    
    console.log("✅ Welcome email sent");
    
    // Publish WelcomeEmailSent event
    cqrs.publishEvent({
        name: "WelcomeEmailSent",
        payload: {
            userId: event.payload.userId,
            email: event.payload.email,
            sentAt: new Date().toISOString()
        },
        metadata: {
            correlationId: event.metadata.correlationId,
            source: "WelcomeEmailHandler"
        }
    });
});

// Create User Profile
cqrs.addEventHandler("UserCreated", "ProfileCreationHandler", function(event) {
    console.log("👤 Creating user profile for:", event.payload.userId);
    
    const profile = {
        userId: event.payload.userId,
        displayName: event.payload.name,
        email: event.payload.email,
        role: event.payload.role,
        preferences: getDefaultPreferences(event.payload.role),
        createdAt: event.payload.createdAt
    };
    
    // Store in read model (optimized for queries)
    storeUserProfile(profile);
    
    console.log("✅ User profile created");
});

// Setup Role-Based Permissions
cqrs.addEventHandler("UserCreated", "PermissionHandler", function(event) {
    console.log("🔐 Setting up permissions for:", event.payload.userId);
    
    const permissions = getRolePermissions(event.payload.role);
    assignUserPermissions(event.payload.userId, permissions);
    
    console.log("✅ Permissions assigned");
});

// === USER UPDATES ===

cqrs.addCommandHandler("UpdateUserProfile", function(command) {
    console.log("🔄 Updating user profile:", command.payload.userId);
    
    // Validate user exists
    const user = getUser(command.payload.userId);
    if (!user) {
        throw new Error("User not found");
    }
    
    // Validate changes
    const changes = command.payload.changes;
    if (changes.email && !isValidEmail(changes.email)) {
        throw new Error("Invalid email format");
    }
    
    // Apply changes
    const updatedUser = { ...user, ...changes, updatedAt: new Date().toISOString() };
    updateUser(updatedUser);
    
    console.log("✅ User profile updated");
    
    // Publish UserProfileUpdated event
    cqrs.publishEvent({
        name: "UserProfileUpdated",
        payload: {
            userId: command.payload.userId,
            changes: changes,
            updatedAt: updatedUser.updatedAt
        },
        metadata: {
            correlationId: command.metadata.correlationId,
            source: "ProfileUpdateHandler"
        }
    });
});

// === ROLE CHANGES ===

cqrs.addCommandHandler("ChangeUserRole", function(command) {
    console.log("🔄 Changing user role:", command.payload.userId);
    
    const user = getUser(command.payload.userId);
    if (!user) {
        throw new Error("User not found");
    }
    
    const oldRole = user.role;
    const newRole = command.payload.newRole;
    
    // Validate role change
    if (!isValidRole(newRole)) {
        throw new Error("Invalid role");
    }
    
    // Update user role
    user.role = newRole;
    user.updatedAt = new Date().toISOString();
    updateUser(user);
    
    console.log(`✅ User role changed from ${oldRole} to ${newRole}`);
    
    // Publish UserRoleChanged event
    cqrs.publishEvent({
        name: "UserRoleChanged",
        payload: {
            userId: command.payload.userId,
            oldRole: oldRole,
            newRole: newRole,
            changedBy: command.metadata.userId,
            changedAt: user.updatedAt
        },
        metadata: {
            correlationId: command.metadata.correlationId,
            source: "RoleChangeHandler"
        }
    });
});

// Update Permissions on Role Change
cqrs.addEventHandler("UserRoleChanged", "PermissionUpdateHandler", function(event) {
    console.log("🔐 Updating permissions for role change:", event.payload.userId);
    
    // Remove old permissions
    removeUserPermissions(event.payload.userId);
    
    // Assign new permissions
    const newPermissions = getRolePermissions(event.payload.newRole);
    assignUserPermissions(event.payload.userId, newPermissions);
    
    console.log("✅ Permissions updated for new role");
});

// Audit Role Changes
cqrs.addEventHandler("UserRoleChanged", "AuditHandler", function(event) {
    console.log("📝 Auditing role change:", event.payload.userId);
    
    const auditEntry = {
        userId: event.payload.userId,
        action: "role_changed",
        oldValue: event.payload.oldRole,
        newValue: event.payload.newRole,
        changedBy: event.payload.changedBy,
        timestamp: event.payload.changedAt
    };
    
    storeAuditEntry(auditEntry);
    
    console.log("✅ Role change audited");
});
```

## Advanced Patterns

### 1. Saga Pattern for Long-Running Processes

Sagas coordinate multiple aggregates in complex business processes:

```javascript
// Order Processing Saga
class OrderProcessingSaga {
    constructor(cqrs) {
        this.cqrs = cqrs;
        this.setupHandlers();
    }
    
    setupHandlers() {
        // Start saga when order is created
        this.cqrs.addEventHandler("OrderCreated", "OrderSaga", (event) => {
            this.handleOrderCreated(event);
        });
        
        // Handle inventory reservation result
        this.cqrs.addEventHandler("ItemsReserved", "OrderSaga", (event) => {
            this.handleItemsReserved(event);
        });
        
        this.cqrs.addEventHandler("ItemsNotAvailable", "OrderSaga", (event) => {
            this.handleItemsNotAvailable(event);
        });
        
        // Handle payment result
        this.cqrs.addEventHandler("PaymentProcessed", "OrderSaga", (event) => {
            this.handlePaymentProcessed(event);
        });
        
        this.cqrs.addEventHandler("PaymentFailed", "OrderSaga", (event) => {
            this.handlePaymentFailed(event);
        });
    }
    
    handleOrderCreated(event) {
        console.log("🎬 Starting order processing saga:", event.payload.orderId);
        
        // Store saga state
        this.storeSagaState(event.payload.orderId, {
            step: "inventory_reservation",
            orderId: event.payload.orderId,
            customerId: event.payload.customerId,
            items: event.payload.items,
            total: event.payload.total,
            startedAt: new Date().toISOString()
        });
        
        // Send reserve inventory command
        this.cqrs.sendCommand({
            name: "ReserveItems",
            payload: {
                orderId: event.payload.orderId,
                items: event.payload.items
            },
            metadata: {
                correlationId: event.metadata.correlationId,
                sagaId: event.payload.orderId
            }
        });
    }
    
    handleItemsReserved(event) {
        console.log("📦 Saga: Items reserved, processing payment");
        
        const sagaState = this.getSagaState(event.payload.orderId);
        sagaState.step = "payment_processing";
        sagaState.inventoryReservedAt = new Date().toISOString();
        this.storeSagaState(event.payload.orderId, sagaState);
        
        // Send process payment command
        this.cqrs.sendCommand({
            name: "ProcessPayment",
            payload: {
                orderId: event.payload.orderId,
                amount: sagaState.total
            },
            metadata: {
                correlationId: event.metadata.correlationId,
                sagaId: event.payload.orderId
            }
        });
    }
    
    handlePaymentProcessed(event) {
        console.log("💳 Saga: Payment processed, completing order");
        
        const sagaState = this.getSagaState(event.payload.orderId);
        sagaState.step = "completed";
        sagaState.paymentProcessedAt = new Date().toISOString();
        sagaState.completedAt = new Date().toISOString();
        this.storeSagaState(event.payload.orderId, sagaState);
        
        // Send complete order command
        this.cqrs.sendCommand({
            name: "CompleteOrder",
            payload: {
                orderId: event.payload.orderId,
                paymentId: event.payload.paymentId
            },
            metadata: {
                correlationId: event.metadata.correlationId,
                sagaId: event.payload.orderId
            }
        });
    }
    
    handleItemsNotAvailable(event) {
        console.log("❌ Saga: Items not available, cancelling order");
        
        const sagaState = this.getSagaState(event.payload.orderId);
        sagaState.step = "cancelled";
        sagaState.cancellationReason = "items_not_available";
        sagaState.cancelledAt = new Date().toISOString();
        this.storeSagaState(event.payload.orderId, sagaState);
        
        // Send cancel order command
        this.cqrs.sendCommand({
            name: "CancelOrder",
            payload: {
                orderId: event.payload.orderId,
                reason: "items_not_available"
            },
            metadata: {
                correlationId: event.metadata.correlationId,
                sagaId: event.payload.orderId
            }
        });
    }
    
    handlePaymentFailed(event) {
        console.log("💳 Saga: Payment failed, compensating");
        
        const sagaState = this.getSagaState(event.payload.orderId);
        sagaState.step = "compensating";
        sagaState.paymentFailedAt = new Date().toISOString();
        this.storeSagaState(event.payload.orderId, sagaState);
        
        // Compensate: Release reserved items
        this.cqrs.sendCommand({
            name: "ReleaseItems",
            payload: {
                orderId: event.payload.orderId,
                reason: "payment_failed"
            },
            metadata: {
                correlationId: event.metadata.correlationId,
                sagaId: event.payload.orderId
            }
        });
        
        // Cancel order
        this.cqrs.sendCommand({
            name: "CancelOrder",
            payload: {
                orderId: event.payload.orderId,
                reason: "payment_failed"
            },
            metadata: {
                correlationId: event.metadata.correlationId,
                sagaId: event.payload.orderId
            }
        });
    }
    
    storeSagaState(sagaId, state) {
        // Store saga state for recovery and monitoring
        console.log(`💾 Storing saga state: ${sagaId} -> ${state.step}`);
        // Implementation would store in database
    }
    
    getSagaState(sagaId) {
        // Retrieve saga state
        console.log(`📖 Getting saga state: ${sagaId}`);
        // Implementation would retrieve from database
        return {};
    }
}

// Initialize saga
const orderSaga = new OrderProcessingSaga(cqrs);
```

### 2. Event Sourcing Integration

Store all events for complete audit trail and state reconstruction:

```javascript
// Event Store Integration
class EventStore {
    constructor(cqrs) {
        this.cqrs = cqrs;
        this.events = []; // In production, this would be a database
        this.setupEventCapture();
    }
    
    setupEventCapture() {
        // Capture all events for storage
        const originalPublishEvent = this.cqrs.publishEvent;
        
        this.cqrs.publishEvent = (event) => {
            // Store event before publishing
            this.storeEvent(event);
            
            // Continue with normal publishing
            return originalPublishEvent.call(this.cqrs, event);
        };
    }
    
    storeEvent(event) {
        const eventRecord = {
            id: generateEventId(),
            streamId: this.getStreamId(event),
            eventType: event.name,
            eventData: event.payload,
            metadata: event.metadata,
            version: this.getNextVersion(this.getStreamId(event)),
            timestamp: new Date().toISOString()
        };
        
        this.events.push(eventRecord);
        console.log(`📚 Event stored: ${event.name} (${eventRecord.id})`);
    }
    
    getStreamId(event) {
        // Extract stream ID from event (e.g., userId, orderId)
        return event.payload.userId || event.payload.orderId || 'global';
    }
    
    getNextVersion(streamId) {
        const streamEvents = this.events.filter(e => e.streamId === streamId);
        return streamEvents.length + 1;
    }
    
    getEventStream(streamId, fromVersion = 0) {
        return this.events
            .filter(e => e.streamId === streamId && e.version > fromVersion)
            .sort((a, b) => a.version - b.version);
    }
    
    replayEvents(streamId, fromVersion = 0) {
        console.log(`🔄 Replaying events for stream: ${streamId}`);
        
        const events = this.getEventStream(streamId, fromVersion);
        
        events.forEach(eventRecord => {
            console.log(`▶️ Replaying: ${eventRecord.eventType} (v${eventRecord.version})`);
            
            // Reconstruct event and replay
            const event = {
                name: eventRecord.eventType,
                payload: eventRecord.eventData,
                metadata: {
                    ...eventRecord.metadata,
                    isReplay: true,
                    originalTimestamp: eventRecord.timestamp
                }
            };
            
            // Replay through event handlers
            this.cqrs.publishEvent(event);
        });
    }
}

// Initialize event store
const eventStore = new EventStore(cqrs);
```

### 3. Read Model Projections

Build optimized read models from events:

```javascript
// User Profile Projection
class UserProfileProjection {
    constructor(cqrs) {
        this.cqrs = cqrs;
        this.profiles = new Map(); // In production: database
        this.setupProjections();
    }
    
    setupProjections() {
        // Build profile from UserCreated
        this.cqrs.addEventHandler("UserCreated", "UserProfileProjection", (event) => {
            this.handleUserCreated(event);
        });
        
        // Update profile from UserProfileUpdated
        this.cqrs.addEventHandler("UserProfileUpdated", "UserProfileProjection", (event) => {
            this.handleUserProfileUpdated(event);
        });
        
        // Update role in profile
        this.cqrs.addEventHandler("UserRoleChanged", "UserProfileProjection", (event) => {
            this.handleUserRoleChanged(event);
        });
        
        // Track login activity
        this.cqrs.addEventHandler("UserLoggedIn", "UserProfileProjection", (event) => {
            this.handleUserLoggedIn(event);
        });
    }
    
    handleUserCreated(event) {
        const profile = {
            userId: event.payload.userId,
            email: event.payload.email,
            name: event.payload.name,
            role: event.payload.role,
            createdAt: event.payload.createdAt,
            lastLoginAt: null,
            loginCount: 0,
            status: 'active'
        };
        
        this.profiles.set(event.payload.userId, profile);
        console.log(`👤 Profile projection created: ${event.payload.userId}`);
    }
    
    handleUserProfileUpdated(event) {
        const profile = this.profiles.get(event.payload.userId);
        if (profile) {
            Object.assign(profile, event.payload.changes);
            profile.updatedAt = event.payload.updatedAt;
            
            console.log(`🔄 Profile projection updated: ${event.payload.userId}`);
        }
    }
    
    handleUserRoleChanged(event) {
        const profile = this.profiles.get(event.payload.userId);
        if (profile) {
            profile.role = event.payload.newRole;
            profile.roleChangedAt = event.payload.changedAt;
            
            console.log(`🔐 Profile role updated: ${event.payload.userId}`);
        }
    }
    
    handleUserLoggedIn(event) {
        const profile = this.profiles.get(event.payload.userId);
        if (profile) {
            profile.lastLoginAt = event.payload.loginAt;
            profile.loginCount = (profile.loginCount || 0) + 1;
            
            console.log(`🔑 Profile login tracked: ${event.payload.userId}`);
        }
    }
    
    // Query methods for read model
    getUserProfile(userId) {
        return this.profiles.get(userId);
    }
    
    getUsersByRole(role) {
        return Array.from(this.profiles.values())
            .filter(profile => profile.role === role);
    }
    
    getActiveUsers() {
        return Array.from(this.profiles.values())
            .filter(profile => profile.status === 'active');
    }
    
    getRecentlyActiveUsers(hours = 24) {
        const cutoff = new Date(Date.now() - hours * 60 * 60 * 1000);
        return Array.from(this.profiles.values())
            .filter(profile => profile.lastLoginAt && new Date(profile.lastLoginAt) > cutoff);
    }
}

// Order Summary Projection
class OrderSummaryProjection {
    constructor(cqrs) {
        this.cqrs = cqrs;
        this.orders = new Map();
        this.customerOrders = new Map(); // customerId -> [orderIds]
        this.setupProjections();
    }
    
    setupProjections() {
        this.cqrs.addEventHandler("OrderCreated", "OrderSummaryProjection", (event) => {
            this.handleOrderCreated(event);
        });
        
        this.cqrs.addEventHandler("OrderCompleted", "OrderSummaryProjection", (event) => {
            this.handleOrderCompleted(event);
        });
        
        this.cqrs.addEventHandler("OrderCancelled", "OrderSummaryProjection", (event) => {
            this.handleOrderCancelled(event);
        });
        
        this.cqrs.addEventHandler("PaymentProcessed", "OrderSummaryProjection", (event) => {
            this.handlePaymentProcessed(event);
        });
    }
    
    handleOrderCreated(event) {
        const order = {
            orderId: event.payload.orderId,
            customerId: event.payload.customerId,
            items: event.payload.items,
            total: event.payload.total,
            status: 'pending',
            createdAt: event.payload.createdAt,
            itemCount: event.payload.items.length
        };
        
        this.orders.set(event.payload.orderId, order);
        
        // Update customer orders index
        if (!this.customerOrders.has(event.payload.customerId)) {
            this.customerOrders.set(event.payload.customerId, []);
        }
        this.customerOrders.get(event.payload.customerId).push(event.payload.orderId);
        
        console.log(`📋 Order summary created: ${event.payload.orderId}`);
    }
    
    handleOrderCompleted(event) {
        const order = this.orders.get(event.payload.orderId);
        if (order) {
            order.status = 'completed';
            order.completedAt = event.payload.completedAt;
            order.paymentId = event.payload.paymentId;
            
            console.log(`✅ Order summary completed: ${event.payload.orderId}`);
        }
    }
    
    handleOrderCancelled(event) {
        const order = this.orders.get(event.payload.orderId);
        if (order) {
            order.status = 'cancelled';
            order.cancelledAt = event.payload.cancelledAt;
            order.cancellationReason = event.payload.reason;
            
            console.log(`❌ Order summary cancelled: ${event.payload.orderId}`);
        }
    }
    
    handlePaymentProcessed(event) {
        const order = this.orders.get(event.payload.orderId);
        if (order) {
            order.paymentId = event.payload.paymentId;
            order.paidAt = event.payload.processedAt;
            
            console.log(`💳 Order payment recorded: ${event.payload.orderId}`);
        }
    }
    
    // Query methods
    getOrder(orderId) {
        return this.orders.get(orderId);
    }
    
    getCustomerOrders(customerId) {
        const orderIds = this.customerOrders.get(customerId) || [];
        return orderIds.map(id => this.orders.get(id)).filter(Boolean);
    }
    
    getOrdersByStatus(status) {
        return Array.from(this.orders.values())
            .filter(order => order.status === status);
    }
    
    getOrderStats() {
        const orders = Array.from(this.orders.values());
        return {
            total: orders.length,
            pending: orders.filter(o => o.status === 'pending').length,
            completed: orders.filter(o => o.status === 'completed').length,
            cancelled: orders.filter(o => o.status === 'cancelled').length,
            totalRevenue: orders
                .filter(o => o.status === 'completed')
                .reduce((sum, o) => sum + o.total, 0)
        };
    }
}

// Initialize projections
const userProfileProjection = new UserProfileProjection(cqrs);
const orderSummaryProjection = new OrderSummaryProjection(cqrs);
```

## Performance and Scalability

### 1. Message Throughput Optimization

```javascript
// Batch Processing for High Throughput
class BatchProcessor {
    constructor(cqrs, batchSize = 100, flushInterval = 1000) {
        this.cqrs = cqrs;
        this.batchSize = batchSize;
        this.flushInterval = flushInterval;
        this.commandBatch = [];
        this.eventBatch = [];
        
        this.setupBatching();
        this.startFlushTimer();
    }
    
    setupBatching() {
        // Override sendCommand for batching
        const originalSendCommand = this.cqrs.sendCommand;
        this.cqrs.sendCommand = (command) => {
            this.addCommandToBatch(command);
        };
        
        // Override publishEvent for batching
        const originalPublishEvent = this.cqrs.publishEvent;
        this.cqrs.publishEvent = (event) => {
            this.addEventToBatch(event);
        };
    }
    
    addCommandToBatch(command) {
        this.commandBatch.push(command);
        
        if (this.commandBatch.length >= this.batchSize) {
            this.flushCommands();
        }
    }
    
    addEventToBatch(event) {
        this.eventBatch.push(event);
        
        if (this.eventBatch.length >= this.batchSize) {
            this.flushEvents();
        }
    }
    
    flushCommands() {
        if (this.commandBatch.length === 0) return;
        
        console.log(`🚀 Flushing ${this.commandBatch.length} commands`);
        
        // Process batch
        this.commandBatch.forEach(command => {
            // Send individual command (original method)
            this.sendCommandDirect(command);
        });
        
        this.commandBatch = [];
    }
    
    flushEvents() {
        if (this.eventBatch.length === 0) return;
        
        console.log(`📤 Flushing ${this.eventBatch.length} events`);
        
        // Process batch
        this.eventBatch.forEach(event => {
            // Publish individual event (original method)
            this.publishEventDirect(event);
        });
        
        this.eventBatch = [];
    }
    
    startFlushTimer() {
        setInterval(() => {
            this.flushCommands();
            this.flushEvents();
        }, this.flushInterval);
    }
}
```

### 2. Horizontal Scaling Patterns

```javascript
// Partitioned Event Processing
class PartitionedEventProcessor {
    constructor(cqrs, partitionCount = 4) {
        this.cqrs = cqrs;
        this.partitionCount = partitionCount;
        this.setupPartitioning();
    }
    
    setupPartitioning() {
        // Override event handler registration
        const originalAddEventHandler = this.cqrs.addEventHandler;
        
        this.cqrs.addEventHandler = (eventName, handlerName, handlerFunc) => {
            // Create partitioned handlers
            for (let i = 0; i < this.partitionCount; i++) {
                const partitionedHandlerName = `${handlerName}_partition_${i}`;
                
                originalAddEventHandler.call(this.cqrs, eventName, partitionedHandlerName, (event) => {
                    // Check if this partition should handle this event
                    if (this.shouldHandleEvent(event, i)) {
                        handlerFunc(event);
                    }
                });
            }
        };
    }
    
    shouldHandleEvent(event, partition) {
        // Partition based on event payload (e.g., userId, orderId)
        const partitionKey = event.payload.userId || event.payload.orderId || event.payload.customerId;
        
        if (!partitionKey) {
            // If no partition key, handle in partition 0
            return partition === 0;
        }
        
        // Hash partition key to determine partition
        const hash = this.hashString(partitionKey);
        return hash % this.partitionCount === partition;
    }
    
    hashString(str) {
        let hash = 0;
        for (let i = 0; i < str.length; i++) {
            const char = str.charCodeAt(i);
            hash = ((hash << 5) - hash) + char;
            hash = hash & hash; // Convert to 32-bit integer
        }
        return Math.abs(hash);
    }
}
```

### 3. Caching and Read Model Optimization

```javascript
// Cached Read Model
class CachedReadModel {
    constructor(cqrs, cacheSize = 1000, ttl = 300000) { // 5 minutes TTL
        this.cqrs = cqrs;
        this.cache = new Map();
        this.cacheSize = cacheSize;
        this.ttl = ttl;
        this.accessTimes = new Map();
        
        this.setupCacheInvalidation();
        this.startCacheCleanup();
    }
    
    setupCacheInvalidation() {
        // Invalidate cache on relevant events
        this.cqrs.addEventHandler("UserProfileUpdated", "CacheInvalidator", (event) => {
            this.invalidateUserCache(event.payload.userId);
        });
        
        this.cqrs.addEventHandler("UserRoleChanged", "CacheInvalidator", (event) => {
            this.invalidateUserCache(event.payload.userId);
        });
        
        this.cqrs.addEventHandler("OrderCompleted", "CacheInvalidator", (event) => {
            this.invalidateOrderCache(event.payload.orderId);
            this.invalidateCustomerOrdersCache(event.payload.customerId);
        });
    }
    
    get(key, fetchFunction) {
        const cached = this.cache.get(key);
        const now = Date.now();
        
        if (cached && (now - cached.timestamp) < this.ttl) {
            // Update access time for LRU
            this.accessTimes.set(key, now);
            console.log(`💾 Cache hit: ${key}`);
            return cached.value;
        }
        
        // Cache miss - fetch data
        console.log(`🔍 Cache miss: ${key}`);
        const value = fetchFunction();
        
        this.set(key, value);
        return value;
    }
    
    set(key, value) {
        const now = Date.now();
        
        // Evict if cache is full
        if (this.cache.size >= this.cacheSize) {
            this.evictLRU();
        }
        
        this.cache.set(key, {
            value: value,
            timestamp: now
        });
        this.accessTimes.set(key, now);
        
        console.log(`💾 Cached: ${key}`);
    }
    
    invalidate(key) {
        this.cache.delete(key);
        this.accessTimes.delete(key);
        console.log(`🗑️ Cache invalidated: ${key}`);
    }
    
    invalidateUserCache(userId) {
        this.invalidate(`user:${userId}`);
        this.invalidate(`user:profile:${userId}`);
    }
    
    invalidateOrderCache(orderId) {
        this.invalidate(`order:${orderId}`);
    }
    
    invalidateCustomerOrdersCache(customerId) {
        this.invalidate(`customer:orders:${customerId}`);
    }
    
    evictLRU() {
        let oldestKey = null;
        let oldestTime = Date.now();
        
        for (const [key, time] of this.accessTimes) {
            if (time < oldestTime) {
                oldestTime = time;
                oldestKey = key;
            }
        }
        
        if (oldestKey) {
            this.invalidate(oldestKey);
        }
    }
    
    startCacheCleanup() {
        setInterval(() => {
            this.cleanupExpired();
        }, 60000); // Cleanup every minute
    }
    
    cleanupExpired() {
        const now = Date.now();
        const expiredKeys = [];
        
        for (const [key, cached] of this.cache) {
            if ((now - cached.timestamp) > this.ttl) {
                expiredKeys.push(key);
            }
        }
        
        expiredKeys.forEach(key => this.invalidate(key));
        
        if (expiredKeys.length > 0) {
            console.log(`🧹 Cleaned up ${expiredKeys.length} expired cache entries`);
        }
    }
}

// Usage with read models
const cache = new CachedReadModel(cqrs);

function getUserProfile(userId) {
    return cache.get(`user:profile:${userId}`, () => {
        // Expensive operation to build user profile
        return buildUserProfileFromEvents(userId);
    });
}

function getCustomerOrders(customerId) {
    return cache.get(`customer:orders:${customerId}`, () => {
        // Expensive operation to get customer orders
        return buildCustomerOrdersFromEvents(customerId);
    });
}
```

## Best Practices

### 1. Command Design

**DO:**
```javascript
// Good: Specific, intention-revealing commands
cqrs.addCommandHandler("CancelOrder", function(command) {
    // Clear business intention
});

cqrs.addCommandHandler("ApproveLeaveRequest", function(command) {
    // Specific business action
});
```

**DON'T:**
```javascript
// Bad: Generic, unclear commands
cqrs.addCommandHandler("UpdateOrder", function(command) {
    // What kind of update? Too generic
});

cqrs.addCommandHandler("ProcessData", function(command) {
    // What data? What processing?
});
```

### 2. Event Design

**DO:**
```javascript
// Good: Past tense, specific events
cqrs.publishEvent({
    name: "OrderCancelled",
    payload: {
        orderId: "order-123",
        reason: "customer_request",
        cancelledBy: "customer-456",
        refundAmount: 99.99
    }
});

cqrs.publishEvent({
    name: "LeaveRequestApproved",
    payload: {
        requestId: "req-789",
        employeeId: "emp-123",
        approvedBy: "mgr-456",
        startDate: "2025-02-01",
        endDate: "2025-02-07"
    }
});
```

**DON'T:**
```javascript
// Bad: Present tense, vague events
cqrs.publishEvent({
    name: "OrderUpdate", // Not past tense
    payload: {
        orderId: "order-123",
        status: "cancelled" // Unclear what changed
    }
});
```

### 3. Handler Responsibilities

**Command Handlers:**
- Validate business rules
- Execute business logic
- Publish events
- Should be deterministic
- Should not have side effects beyond event publishing

**Event Handlers:**
- Handle side effects
- Update read models
- Send notifications
- Integrate with external systems
- Should be idempotent

### 4. Error Handling Strategy

```javascript
// Comprehensive Error Handling
cqrs.addCommandHandler("ProcessPayment", function(command) {
    try {
        // Validate command
        validatePaymentCommand(command);
        
        // Execute business logic
        const result = processPayment(command.payload);
        
        // Publish success event
        cqrs.publishEvent({
            name: "PaymentProcessed",
            payload: result
        });
        
    } catch (error) {
        // Log error with context
        console.error("Payment processing failed:", {
            command: command,
            error: error.message,
            stack: error.stack
        });
        
        // Publish failure event
        cqrs.publishEvent({
            name: "PaymentFailed",
            payload: {
                orderId: command.payload.orderId,
                error: error.message,
                errorCode: error.code,
                retryable: isRetryableError(error)
            }
        });
        
        // Re-throw for retry mechanism
        if (isRetryableError(error)) {
            throw error;
        }
    }
});

// Idempotent Event Handler
cqrs.addEventHandler("PaymentProcessed", "OrderCompletionHandler", function(event) {
    const orderId = event.payload.orderId;
    
    // Check if already processed (idempotency)
    if (isOrderAlreadyCompleted(orderId)) {
        console.log(`Order ${orderId} already completed, skipping`);
        return;
    }
    
    try {
        // Complete order
        completeOrder(orderId);
        
        // Mark as processed
        markOrderAsCompleted(orderId);
        
    } catch (error) {
        console.error(`Failed to complete order ${orderId}:`, error);
        
        // Don't re-throw in event handlers unless you want retry
        // Instead, publish a failure event
        cqrs.publishEvent({
            name: "OrderCompletionFailed",
            payload: {
                orderId: orderId,
                error: error.message
            }
        });
    }
});
```

### 5. Testing Strategies

```javascript
// Command Handler Testing
function testCreateUserCommand() {
    console.log("🧪 Testing CreateUser command handler");
    
    // Arrange
    const command = {
        name: "CreateUser",
        payload: {
            email: "test@example.com",
            name: "Test User"
        },
        metadata: {
            correlationId: "test-123"
        }
    };
    
    const publishedEvents = [];
    
    // Mock event publishing
    const originalPublishEvent = cqrs.publishEvent;
    cqrs.publishEvent = (event) => {
        publishedEvents.push(event);
    };
    
    try {
        // Act
        cqrs.sendCommand(command);
        
        // Assert
        console.assert(publishedEvents.length === 1, "Should publish one event");
        console.assert(publishedEvents[0].name === "UserCreated", "Should publish UserCreated event");
        console.assert(publishedEvents[0].payload.email === "test@example.com", "Should include email");
        
        console.log("✅ CreateUser command test passed");
        
    } finally {
        // Restore original function
        cqrs.publishEvent = originalPublishEvent;
    }
}

// Event Handler Testing
function testWelcomeEmailHandler() {
    console.log("🧪 Testing WelcomeEmail event handler");
    
    // Arrange
    const event = {
        name: "UserCreated",
        payload: {
            userId: "user-123",
            email: "test@example.com",
            name: "Test User"
        },
        metadata: {
            correlationId: "test-123"
        }
    };
    
    const sentEmails = [];
    
    // Mock email sending
    const originalSendEmail = sendEmail;
    sendEmail = (to, subject, content) => {
        sentEmails.push({ to, subject, content });
    };
    
    try {
        // Act
        // Trigger event handler directly or through event publishing
        
        // Assert
        console.assert(sentEmails.length === 1, "Should send one email");
        console.assert(sentEmails[0].to === "test@example.com", "Should send to correct email");
        
        console.log("✅ WelcomeEmail handler test passed");
        
    } finally {
        // Restore original function
        sendEmail = originalSendEmail;
    }
}

// Integration Testing
function testOrderProcessingFlow() {
    console.log("🧪 Testing complete order processing flow");
    
    const events = [];
    
    // Capture all events
    const originalPublishEvent = cqrs.publishEvent;
    cqrs.publishEvent = (event) => {
        events.push(event);
        return originalPublishEvent.call(cqrs, event);
    };
    
    try {
        // Act - Start the flow
        cqrs.sendCommand({
            name: "CreateOrder",
            payload: {
                customerId: "customer-123",
                items: [{ productId: "prod-1", quantity: 1, price: 99.99 }]
            },
            metadata: { correlationId: "integration-test-123" }
        });
        
        // Wait for async processing
        setTimeout(() => {
            // Assert - Check event sequence
            const eventNames = events.map(e => e.name);
            
            console.assert(eventNames.includes("OrderCreated"), "Should create order");
            console.assert(eventNames.includes("ItemsReserved"), "Should reserve items");
            console.assert(eventNames.includes("PaymentProcessed"), "Should process payment");
            console.assert(eventNames.includes("OrderCompleted"), "Should complete order");
            
            console.log("✅ Order processing flow test passed");
            
        }, 1000);
        
    } finally {
        cqrs.publishEvent = originalPublishEvent;
    }
}

// Run tests
testCreateUserCommand();
testWelcomeEmailHandler();
testOrderProcessingFlow();
```

## Troubleshooting

### Common Issues and Solutions

#### 1. Commands Not Reaching Handlers

**Symptoms:**
- Commands sent but handlers not executed
- No error messages

**Diagnosis:**
```javascript
// Add debug logging to command sending
const originalSendCommand = cqrs.sendCommand;
cqrs.sendCommand = (command) => {
    console.log("🔍 Sending command:", command.name, command.payload);
    return originalSendCommand.call(cqrs, command);
};

// Add debug logging to command handlers
cqrs.addCommandHandler("CreateUser", function(command) {
    console.log("📥 Command handler received:", command.name);
    // ... handler logic
});
```

**Solutions:**
- Check if Watermill router is started: `watermill.start()`
- Verify command name matches handler registration
- Check topic generation consistency
- Ensure no exceptions in handler registration

#### 2. Events Not Reaching All Handlers

**Symptoms:**
- Some event handlers execute, others don't
- Inconsistent event processing

**Diagnosis:**
```javascript
// Track event publishing and handling
const eventTracker = {
    published: [],
    handled: []
};

// Track publishing
const originalPublishEvent = cqrs.publishEvent;
cqrs.publishEvent = (event) => {
    eventTracker.published.push({
        name: event.name,
        timestamp: new Date().toISOString()
    });
    console.log("📤 Event published:", event.name);
    return originalPublishEvent.call(cqrs, event);
};

// Track handling
cqrs.addEventHandler("UserCreated", "DebugHandler", function(event) {
    eventTracker.handled.push({
        name: event.name,
        handler: "DebugHandler",
        timestamp: new Date().toISOString()
    });
    console.log("📥 Event handled by DebugHandler:", event.name);
});

// Check tracking periodically
setInterval(() => {
    console.log("📊 Event tracking:", eventTracker);
}, 5000);
```

**Solutions:**
- Verify all event handlers are registered before starting
- Check for exceptions in event handlers (they may stop processing)
- Ensure event names match exactly
- Check if handlers are registered with unique names

#### 3. Memory Leaks in Long-Running Processes

**Symptoms:**
- Increasing memory usage over time
- Performance degradation

**Diagnosis:**
```javascript
// Monitor memory usage
function monitorMemory() {
    const usage = process.memoryUsage();
    console.log("💾 Memory usage:", {
        rss: Math.round(usage.rss / 1024 / 1024) + " MB",
        heapUsed: Math.round(usage.heapUsed / 1024 / 1024) + " MB",
        heapTotal: Math.round(usage.heapTotal / 1024 / 1024) + " MB"
    });
}

setInterval(monitorMemory, 10000); // Every 10 seconds
```

**Solutions:**
- Implement proper cleanup in handlers
- Use weak references for caches
- Implement TTL for stored data
- Monitor and limit batch sizes

#### 4. Message Ordering Issues

**Symptoms:**
- Events processed out of order
- Inconsistent state

**Diagnosis:**
```javascript
// Add sequence tracking
let messageSequence = 0;

const originalPublishEvent = cqrs.publishEvent;
cqrs.publishEvent = (event) => {
    event.metadata = event.metadata || {};
    event.metadata.sequence = ++messageSequence;
    event.metadata.publishedAt = new Date().toISOString();
    
    console.log(`📤 Publishing event ${event.metadata.sequence}: ${event.name}`);
    return originalPublishEvent.call(cqrs, event);
};

// Track processing order
cqrs.addEventHandler("*", "OrderTracker", function(event) {
    console.log(`📥 Processing event ${event.metadata.sequence}: ${event.name} at ${new Date().toISOString()}`);
});
```

**Solutions:**
- Use correlation IDs to track related messages
- Implement proper partitioning for related events
- Consider using single-threaded processing for critical sequences
- Implement idempotent handlers

### Performance Monitoring

```javascript
// Performance Monitoring
class PerformanceMonitor {
    constructor(cqrs) {
        this.cqrs = cqrs;
        this.metrics = {
            commandsProcessed: 0,
            eventsPublished: 0,
            eventsHandled: 0,
            averageCommandTime: 0,
            averageEventTime: 0,
            errors: 0
        };
        
        this.setupMonitoring();
        this.startReporting();
    }
    
    setupMonitoring() {
        // Monitor command processing time
        const originalSendCommand = this.cqrs.sendCommand;
        this.cqrs.sendCommand = (command) => {
            const startTime = Date.now();
            
            try {
                const result = originalSendCommand.call(this.cqrs, command);
                
                const duration = Date.now() - startTime;
                this.updateCommandMetrics(duration);
                
                return result;
            } catch (error) {
                this.metrics.errors++;
                throw error;
            }
        };
        
        // Monitor event publishing
        const originalPublishEvent = this.cqrs.publishEvent;
        this.cqrs.publishEvent = (event) => {
            const startTime = Date.now();
            
            try {
                const result = originalPublishEvent.call(this.cqrs, event);
                
                const duration = Date.now() - startTime;
                this.updateEventMetrics(duration);
                
                return result;
            } catch (error) {
                this.metrics.errors++;
                throw error;
            }
        };
    }
    
    updateCommandMetrics(duration) {
        this.metrics.commandsProcessed++;
        this.metrics.averageCommandTime = 
            (this.metrics.averageCommandTime * (this.metrics.commandsProcessed - 1) + duration) / 
            this.metrics.commandsProcessed;
    }
    
    updateEventMetrics(duration) {
        this.metrics.eventsPublished++;
        this.metrics.averageEventTime = 
            (this.metrics.averageEventTime * (this.metrics.eventsPublished - 1) + duration) / 
            this.metrics.eventsPublished;
    }
    
    startReporting() {
        setInterval(() => {
            console.log("📊 Performance Metrics:", {
                ...this.metrics,
                averageCommandTime: Math.round(this.metrics.averageCommandTime * 100) / 100 + "ms",
                averageEventTime: Math.round(this.metrics.averageEventTime * 100) / 100 + "ms",
                throughput: {
                    commandsPerSecond: Math.round(this.metrics.commandsProcessed / 30), // 30-second window
                    eventsPerSecond: Math.round(this.metrics.eventsPublished / 30)
                }
            });
            
            // Reset counters for next window
            this.metrics.commandsProcessed = 0;
            this.metrics.eventsPublished = 0;
            this.metrics.averageCommandTime = 0;
            this.metrics.averageEventTime = 0;
            this.metrics.errors = 0;
            
        }, 30000); // Report every 30 seconds
    }
}

// Initialize monitoring
const monitor = new PerformanceMonitor(cqrs);
```

## Conclusion

CQRS with Goja-Watermill provides a powerful foundation for building scalable, maintainable event-driven applications. The separation of commands and queries, combined with the flexibility of JavaScript business logic and the robustness of Go infrastructure, creates an ideal platform for complex business domains.

**Key Takeaways:**

1. **Clear Separation**: Commands represent intentions, events represent facts
2. **One-to-One**: Commands have single handlers, events can have multiple handlers
3. **Event-Driven**: Use events to coordinate between bounded contexts
4. **Scalability**: Independent scaling of read and write sides
5. **Maintainability**: Clear business logic organization and testing strategies
6. **Monitoring**: Comprehensive logging and performance tracking

**Next Steps:**

1. Start with simple CRUD operations converted to CQRS
2. Add event sourcing for complete audit trails
3. Implement read model projections for optimized queries
4. Add saga patterns for complex business processes
5. Scale horizontally with partitioning and caching

The patterns and examples in this guide provide a solid foundation for implementing CQRS in your applications. Remember to start simple and evolve your architecture as your understanding and requirements grow.

