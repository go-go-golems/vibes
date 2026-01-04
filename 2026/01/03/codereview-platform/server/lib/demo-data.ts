// Bundled demo repository data for deployment
// Contains sample source files, reviews, quizzes, and guides

export const DEMO_REPO_NAME = 'demo-codebase';

export const demoSourceFiles: Record<string, string> = {
  'README.md': `# Demo Codebase

A sample codebase for learning through code reviews.

## Features
- Authentication system with JWT
- Database layer with connection pooling
- API routes with validation
- Error handling middleware

## Getting Started
\`\`\`bash
npm install
npm run dev
\`\`\`
`,

  'package.json': `{
  "name": "demo-codebase",
  "version": "1.0.0",
  "main": "src/index.js",
  "scripts": {
    "dev": "node src/index.js",
    "test": "jest"
  },
  "dependencies": {
    "express": "^4.18.0",
    "bcrypt": "^5.1.0",
    "jsonwebtoken": "^9.0.0",
    "mysql2": "^3.6.0",
    "zod": "^3.22.0"
  }
}
`,

  'src/index.js': `const express = require('express');
const { authRouter } = require('./routes/auth');
const { userRouter } = require('./routes/users');
const { errorHandler } = require('./middleware/errorHandler');
const { requestLogger } = require('./middleware/logging');
const { connectDatabase } = require('./db/connection');

const app = express();
const PORT = process.env.PORT || 3000;

// Middleware
app.use(express.json());
app.use(requestLogger);

// Routes
app.use('/api/auth', authRouter);
app.use('/api/users', userRouter);

// Error handling (must be last)
app.use(errorHandler);

// Start server
async function start() {
  await connectDatabase();
  app.listen(PORT, () => {
    console.log(\`Server running on port \${PORT}\`);
  });
}

start().catch(console.error);
`,

  'src/routes/auth.js': `const express = require('express');
const bcrypt = require('bcrypt');
const jwt = require('jsonwebtoken');
const { z } = require('zod');
const { findUserByEmail, createUser } = require('../db/users');

const router = express.Router();
const JWT_SECRET = process.env.JWT_SECRET || 'dev-secret';
const SALT_ROUNDS = 10;

// Validation schemas
const loginSchema = z.object({
  email: z.string().email(),
  password: z.string().min(8)
});

const registerSchema = z.object({
  email: z.string().email(),
  password: z.string().min(8),
  name: z.string().min(2)
});

/**
 * POST /login
 * Authenticate user and return JWT token
 */
router.post('/login', async (req, res, next) => {
  try {
    // Validate input first
    const { email, password } = loginSchema.parse(req.body);
    
    // Find user by email (normalize to lowercase)
    const user = await findUserByEmail(email.toLowerCase().trim());
    
    if (!user) {
      // Use generic message to prevent user enumeration
      return res.status(401).json({ error: 'Invalid credentials' });
    }
    
    // Verify password using bcrypt
    const isValid = await bcrypt.compare(password, user.passwordHash);
    
    if (!isValid) {
      return res.status(401).json({ error: 'Invalid credentials' });
    }
    
    // Generate JWT token
    const token = jwt.sign(
      { userId: user.id, email: user.email },
      JWT_SECRET,
      { expiresIn: '24h' }
    );
    
    // Return token and user info (without sensitive data)
    res.json({
      token,
      user: {
        id: user.id,
        email: user.email,
        name: user.name
      },
      expiresIn: 86400
    });
  } catch (error) {
    next(error);
  }
});

/**
 * POST /register
 * Create new user account
 */
router.post('/register', async (req, res, next) => {
  try {
    const { email, password, name } = registerSchema.parse(req.body);
    
    // Check if user already exists
    const existing = await findUserByEmail(email.toLowerCase());
    if (existing) {
      return res.status(409).json({ error: 'Email already registered' });
    }
    
    // Hash password with bcrypt
    const passwordHash = await bcrypt.hash(password, SALT_ROUNDS);
    
    // Create user
    const user = await createUser({
      email: email.toLowerCase().trim(),
      passwordHash,
      name: name.trim()
    });
    
    res.status(201).json({
      message: 'User created successfully',
      user: { id: user.id, email: user.email, name: user.name }
    });
  } catch (error) {
    next(error);
  }
});

module.exports = { authRouter: router };
`,

  'src/routes/users.js': `const express = require('express');
const { z } = require('zod');
const { authenticate } = require('../middleware/auth');
const { getUserById, updateUser, deleteUser } = require('../db/users');

const router = express.Router();

// All routes require authentication
router.use(authenticate);

const updateSchema = z.object({
  name: z.string().min(2).optional(),
  email: z.string().email().optional()
});

/**
 * GET /me
 * Get current user profile
 */
router.get('/me', async (req, res, next) => {
  try {
    const user = await getUserById(req.user.userId);
    
    if (!user) {
      return res.status(404).json({ error: 'User not found' });
    }
    
    res.json({
      id: user.id,
      email: user.email,
      name: user.name,
      createdAt: user.createdAt
    });
  } catch (error) {
    next(error);
  }
});

/**
 * PATCH /me
 * Update current user profile
 */
router.patch('/me', async (req, res, next) => {
  try {
    const updates = updateSchema.parse(req.body);
    
    if (Object.keys(updates).length === 0) {
      return res.status(400).json({ error: 'No valid updates provided' });
    }
    
    const user = await updateUser(req.user.userId, updates);
    
    res.json({
      message: 'Profile updated',
      user: { id: user.id, email: user.email, name: user.name }
    });
  } catch (error) {
    next(error);
  }
});

/**
 * DELETE /me
 * Delete current user account
 */
router.delete('/me', async (req, res, next) => {
  try {
    await deleteUser(req.user.userId);
    res.json({ message: 'Account deleted successfully' });
  } catch (error) {
    next(error);
  }
});

module.exports = { userRouter: router };
`,

  'src/middleware/auth.js': `const jwt = require('jsonwebtoken');

const JWT_SECRET = process.env.JWT_SECRET || 'dev-secret';

/**
 * Authentication middleware
 * Extracts and verifies JWT from Authorization header
 */
function authenticate(req, res, next) {
  // Get token from Authorization header
  const authHeader = req.headers.authorization;
  
  if (!authHeader) {
    return res.status(401).json({ error: 'No authorization header' });
  }
  
  // Expect format: "Bearer <token>"
  const parts = authHeader.split(' ');
  
  if (parts.length !== 2 || parts[0] !== 'Bearer') {
    return res.status(401).json({ error: 'Invalid authorization format' });
  }
  
  const token = parts[1];
  
  try {
    // Verify token
    const decoded = jwt.verify(token, JWT_SECRET);
    
    // Attach user info to request
    req.user = {
      userId: decoded.userId,
      email: decoded.email
    };
    
    next();
  } catch (error) {
    if (error.name === 'TokenExpiredError') {
      return res.status(401).json({ error: 'Token expired' });
    }
    return res.status(401).json({ error: 'Invalid token' });
  }
}

/**
 * Optional authentication middleware
 * Attaches user if token present, but doesn't require it
 */
function optionalAuth(req, res, next) {
  const authHeader = req.headers.authorization;
  
  if (!authHeader) {
    return next();
  }
  
  const parts = authHeader.split(' ');
  if (parts.length !== 2 || parts[0] !== 'Bearer') {
    return next();
  }
  
  try {
    const decoded = jwt.verify(parts[1], JWT_SECRET);
    req.user = { userId: decoded.userId, email: decoded.email };
  } catch {
    // Token invalid, but that's okay for optional auth
  }
  
  next();
}

module.exports = { authenticate, optionalAuth };
`,

  'src/middleware/errorHandler.js': `const { ZodError } = require('zod');

/**
 * Global error handling middleware
 * Must be registered last in the middleware chain
 */
function errorHandler(err, req, res, next) {
  // Log error for debugging (in production, use proper logging)
  console.error('Error:', err);
  
  // Handle Zod validation errors
  if (err instanceof ZodError) {
    return res.status(400).json({
      error: 'Validation failed',
      details: err.errors.map(e => ({
        field: e.path.join('.'),
        message: e.message
      }))
    });
  }
  
  // Handle JWT errors
  if (err.name === 'JsonWebTokenError') {
    return res.status(401).json({ error: 'Invalid token' });
  }
  
  // Handle database errors
  if (err.code === 'ER_DUP_ENTRY') {
    return res.status(409).json({ error: 'Duplicate entry' });
  }
  
  if (err.code === 'ECONNREFUSED') {
    return res.status(503).json({ error: 'Database unavailable' });
  }
  
  // Handle known HTTP errors
  if (err.status && err.message) {
    return res.status(err.status).json({ error: err.message });
  }
  
  // Default to 500 Internal Server Error
  // Don't expose internal error details in production
  const isDev = process.env.NODE_ENV === 'development';
  
  res.status(500).json({
    error: 'Internal server error',
    ...(isDev && { details: err.message, stack: err.stack })
  });
}

/**
 * Not found handler for undefined routes
 */
function notFoundHandler(req, res) {
  res.status(404).json({
    error: 'Not found',
    path: req.path
  });
}

module.exports = { errorHandler, notFoundHandler };
`,

  'src/middleware/logging.js': `/**
 * Request logging middleware
 * Logs incoming requests with timing information
 */
function requestLogger(req, res, next) {
  const start = Date.now();
  const requestId = generateRequestId();
  
  // Attach request ID for tracing
  req.requestId = requestId;
  res.setHeader('X-Request-ID', requestId);
  
  // Log request start
  console.log(JSON.stringify({
    type: 'request',
    requestId,
    method: req.method,
    path: req.path,
    query: req.query,
    ip: req.ip,
    userAgent: req.get('User-Agent')
  }));
  
  // Log response when finished
  res.on('finish', () => {
    const duration = Date.now() - start;
    
    console.log(JSON.stringify({
      type: 'response',
      requestId,
      method: req.method,
      path: req.path,
      status: res.statusCode,
      duration: \`\${duration}ms\`
    }));
  });
  
  next();
}

/**
 * Generate unique request ID
 */
function generateRequestId() {
  return \`req_\${Date.now().toString(36)}_\${Math.random().toString(36).slice(2, 8)}\`;
}

module.exports = { requestLogger };
`,

  'src/db/connection.js': `const mysql = require('mysql2/promise');

let pool = null;

/**
 * Database configuration
 */
const dbConfig = {
  host: process.env.DB_HOST || 'localhost',
  port: parseInt(process.env.DB_PORT || '3306'),
  user: process.env.DB_USER || 'root',
  password: process.env.DB_PASSWORD || '',
  database: process.env.DB_NAME || 'demo',
  
  // Connection pool settings
  waitForConnections: true,
  connectionLimit: 10,
  queueLimit: 0,
  
  // Enable prepared statements for security
  namedPlaceholders: true
};

/**
 * Initialize database connection pool
 */
async function connectDatabase() {
  if (pool) {
    return pool;
  }
  
  try {
    pool = mysql.createPool(dbConfig);
    
    // Test connection
    const connection = await pool.getConnection();
    console.log('Database connected successfully');
    connection.release();
    
    return pool;
  } catch (error) {
    console.error('Database connection failed:', error.message);
    throw error;
  }
}

/**
 * Get database pool instance
 */
function getPool() {
  if (!pool) {
    throw new Error('Database not connected. Call connectDatabase() first.');
  }
  return pool;
}

/**
 * Execute a query with automatic connection handling
 */
async function query(sql, params = []) {
  const pool = getPool();
  const [rows] = await pool.execute(sql, params);
  return rows;
}

/**
 * Execute a transaction
 */
async function transaction(callback) {
  const pool = getPool();
  const connection = await pool.getConnection();
  
  try {
    await connection.beginTransaction();
    const result = await callback(connection);
    await connection.commit();
    return result;
  } catch (error) {
    await connection.rollback();
    throw error;
  } finally {
    connection.release();
  }
}

/**
 * Close all connections (for graceful shutdown)
 */
async function closeDatabase() {
  if (pool) {
    await pool.end();
    pool = null;
    console.log('Database connections closed');
  }
}

module.exports = {
  connectDatabase,
  getPool,
  query,
  transaction,
  closeDatabase
};
`,

  'src/db/users.js': `const { query, transaction } = require('./connection');

/**
 * Find user by email address
 */
async function findUserByEmail(email) {
  const rows = await query(
    'SELECT id, email, name, passwordHash, createdAt FROM users WHERE email = ?',
    [email]
  );
  return rows[0] || null;
}

/**
 * Find user by ID
 */
async function getUserById(id) {
  const rows = await query(
    'SELECT id, email, name, createdAt FROM users WHERE id = ?',
    [id]
  );
  return rows[0] || null;
}

/**
 * Create new user
 */
async function createUser({ email, passwordHash, name }) {
  const result = await query(
    'INSERT INTO users (email, passwordHash, name) VALUES (?, ?, ?)',
    [email, passwordHash, name]
  );
  
  return {
    id: result.insertId,
    email,
    name
  };
}

/**
 * Update user profile
 */
async function updateUser(id, updates) {
  const fields = [];
  const values = [];
  
  if (updates.name !== undefined) {
    fields.push('name = ?');
    values.push(updates.name);
  }
  
  if (updates.email !== undefined) {
    fields.push('email = ?');
    values.push(updates.email);
  }
  
  if (fields.length === 0) {
    return getUserById(id);
  }
  
  values.push(id);
  
  await query(
    \`UPDATE users SET \${fields.join(', ')} WHERE id = ?\`,
    values
  );
  
  return getUserById(id);
}

/**
 * Delete user account
 */
async function deleteUser(id) {
  await query('DELETE FROM users WHERE id = ?', [id]);
}

/**
 * List all users (admin only)
 */
async function listUsers({ limit = 50, offset = 0 } = {}) {
  return query(
    'SELECT id, email, name, createdAt FROM users ORDER BY createdAt DESC LIMIT ? OFFSET ?',
    [limit, offset]
  );
}

module.exports = {
  findUserByEmail,
  getUserById,
  createUser,
  updateUser,
  deleteUser,
  listUsers
};
`,

  'src/utils/validation.js': `const { z } = require('zod');

/**
 * Common validation schemas
 */
const schemas = {
  email: z.string().email('Invalid email format'),
  
  password: z.string()
    .min(8, 'Password must be at least 8 characters')
    .regex(/[A-Z]/, 'Password must contain uppercase letter')
    .regex(/[a-z]/, 'Password must contain lowercase letter')
    .regex(/[0-9]/, 'Password must contain a number'),
  
  name: z.string()
    .min(2, 'Name must be at least 2 characters')
    .max(100, 'Name must be at most 100 characters'),
  
  id: z.number().int().positive(),
  
  uuid: z.string().uuid(),
  
  pagination: z.object({
    page: z.number().int().min(1).default(1),
    limit: z.number().int().min(1).max(100).default(20)
  })
};

/**
 * Validate data against a schema
 * Returns { success, data, errors }
 */
function validate(schema, data) {
  const result = schema.safeParse(data);
  
  if (result.success) {
    return { success: true, data: result.data, errors: null };
  }
  
  return {
    success: false,
    data: null,
    errors: result.error.errors.map(e => ({
      field: e.path.join('.'),
      message: e.message
    }))
  };
}

/**
 * Sanitize string input
 */
function sanitize(input) {
  if (typeof input !== 'string') return input;
  return input.trim().replace(/[<>]/g, '');
}

module.exports = { schemas, validate, sanitize };
`,

  'tests/auth.test.js': `const request = require('supertest');
const app = require('../src/index');

describe('Authentication', () => {
  describe('POST /api/auth/login', () => {
    it('should return 401 for invalid credentials', async () => {
      const res = await request(app)
        .post('/api/auth/login')
        .send({ email: 'test@example.com', password: 'wrongpassword' });
      
      expect(res.status).toBe(401);
      expect(res.body.error).toBe('Invalid credentials');
    });
    
    it('should return 400 for invalid email format', async () => {
      const res = await request(app)
        .post('/api/auth/login')
        .send({ email: 'not-an-email', password: 'password123' });
      
      expect(res.status).toBe(400);
      expect(res.body.error).toBe('Validation failed');
    });
    
    it('should return token for valid credentials', async () => {
      // Assuming test user exists
      const res = await request(app)
        .post('/api/auth/login')
        .send({ email: 'test@example.com', password: 'TestPass123' });
      
      expect(res.status).toBe(200);
      expect(res.body.token).toBeDefined();
      expect(res.body.user.email).toBe('test@example.com');
    });
  });
  
  describe('POST /api/auth/register', () => {
    it('should create new user', async () => {
      const res = await request(app)
        .post('/api/auth/register')
        .send({
          email: 'newuser@example.com',
          password: 'SecurePass123',
          name: 'New User'
        });
      
      expect(res.status).toBe(201);
      expect(res.body.user.email).toBe('newuser@example.com');
    });
    
    it('should reject duplicate email', async () => {
      const res = await request(app)
        .post('/api/auth/register')
        .send({
          email: 'test@example.com',
          password: 'SecurePass123',
          name: 'Duplicate User'
        });
      
      expect(res.status).toBe(409);
    });
  });
});
`
};

// Code Reviews - 2 examples with different annotation types
export const demoReviews = [
  {
    id: 'auth-security-review',
    pr: 1,
    title: 'Add authentication middleware',
    description: 'This PR introduces bcrypt-based password hashing and JWT authentication with proper security practices.',
    baseBranch: 'main',
    headBranch: 'feature/auth',
    annotations: [
      {
        file: 'src/routes/auth.js',
        line: 35,
        type: 'educational',
        title: 'Why bcrypt for password hashing?',
        content: `Bcrypt is preferred over simpler hashing algorithms because:
- **Adaptive**: cost factor increases with hardware improvements
- **Built-in salting**: prevents rainbow table attacks
- **Deliberately slow**: ~100ms to prevent brute force

The SALT_ROUNDS=10 means 2^10 iterations, balancing security with performance.`,
        tags: ['security', 'hashing', 'best-practices'],
        quiz: {
          id: 'bcrypt-quiz',
          title: 'Understanding Password Hashing',
          questions: [
            {
              type: 'multiple_choice',
              question: 'Why is saltRounds set to 10?',
              options: [
                "It's the minimum allowed",
                "It's a balance between security and performance",
                "It matches the password length requirement",
                "It's required by the bcrypt library"
              ],
              correct: 1,
              explanation: 'SaltRounds=10 means 2^10 iterations, balancing security with ~100ms hash time'
            }
          ]
        }
      },
      {
        file: 'src/routes/auth.js',
        line: 42,
        type: 'gotcha',
        title: 'Timing Attack Prevention',
        content: `Notice how we use the same error message for both "user not found" and "wrong password".

This prevents **timing attacks** where an attacker could determine valid emails by measuring response times.

**Bad**: Different messages reveal information
**Good**: Generic "Invalid credentials" for all auth failures`,
        tags: ['security', 'vulnerability', 'timing-attack']
      },
      {
        file: 'src/middleware/auth.js',
        line: 15,
        type: 'educational',
        title: 'JWT Token Extraction',
        content: `The Authorization header follows the Bearer token scheme (RFC 6750):
\`\`\`
Authorization: Bearer <token>
\`\`\`

We split the header and verify:
1. Exactly 2 parts
2. First part is "Bearer"
3. Second part is the actual token`,
        tags: ['jwt', 'authentication', 'http-headers']
      }
    ]
  },
  {
    id: 'database-layer-review',
    pr: 2,
    title: 'Implement database connection pooling',
    description: 'Adds MySQL connection pooling with proper error handling and transaction support.',
    baseBranch: 'main',
    headBranch: 'feature/database',
    annotations: [
      {
        file: 'src/db/connection.js',
        line: 20,
        type: 'educational',
        title: 'Connection Pool Configuration',
        content: `Connection pooling is essential for production applications:

- **connectionLimit: 10**: Maximum concurrent connections
- **waitForConnections: true**: Queue requests when pool is full
- **queueLimit: 0**: Unlimited queue (be careful in production)

Without pooling, each request creates a new connection (~50ms overhead).`,
        tags: ['database', 'performance', 'mysql'],
        quiz: {
          id: 'db-pool-quiz',
          title: 'Database Connection Pooling',
          questions: [
            {
              type: 'multiple_choice',
              question: 'What happens when connectionLimit is reached?',
              options: [
                'New connections are rejected immediately',
                'Requests wait in queue (if waitForConnections is true)',
                'The oldest connection is closed',
                'The server crashes'
              ],
              correct: 1,
              explanation: 'With waitForConnections: true, requests queue until a connection is available'
            }
          ]
        }
      },
      {
        file: 'src/db/connection.js',
        line: 65,
        type: 'best-practice',
        title: 'Transaction Pattern',
        content: `The transaction function implements the **Unit of Work** pattern:

1. Get connection from pool
2. Begin transaction
3. Execute callback with connection
4. Commit on success, rollback on error
5. **Always** release connection (finally block)

This prevents connection leaks and ensures data consistency.`,
        tags: ['database', 'transactions', 'patterns']
      },
      {
        file: 'src/db/users.js',
        line: 25,
        type: 'gotcha',
        title: 'SQL Injection Prevention',
        content: `Always use parameterized queries:

**Vulnerable**:
\`\`\`js
query(\`SELECT * FROM users WHERE email = '\${email}'\`)
\`\`\`

**Safe**:
\`\`\`js
query('SELECT * FROM users WHERE email = ?', [email])
\`\`\`

The ? placeholder ensures proper escaping.`,
        tags: ['security', 'sql-injection', 'database']
      },
      {
        file: 'src/db/connection.js',
        line: 85,
        type: 'educational',
        title: 'Graceful Shutdown',
        content: `The closeDatabase() function is crucial for graceful shutdown:

- Closes all pool connections cleanly
- Prevents "connection reset" errors
- Should be called on SIGTERM/SIGINT

\`\`\`js
process.on('SIGTERM', async () => {
  await closeDatabase();
  process.exit(0);
});
\`\`\``,
        tags: ['devops', 'best-practices', 'nodejs']
      }
    ]
  },
  {
    id: 'error-handling-review',
    pr: 3,
    title: 'Add comprehensive error handling',
    description: 'Implements centralized error handling with proper logging and user-friendly messages.',
    baseBranch: 'main',
    headBranch: 'feature/error-handling',
    annotations: [
      {
        file: 'src/middleware/errorHandler.js',
        line: 10,
        type: 'educational',
        title: 'Centralized Error Handling',
        content: `Express error middleware has 4 parameters: (err, req, res, next)

This pattern provides:
- **Consistent error responses** across all routes
- **Single place** for error logging
- **Security**: hide internal details in production
- **Type-specific handling** for different error classes`,
        tags: ['express', 'error-handling', 'architecture']
      },
      {
        file: 'src/middleware/errorHandler.js',
        line: 35,
        type: 'gotcha',
        title: 'Information Disclosure',
        content: `Never expose internal error details in production!

**Bad** (leaks info):
\`\`\`json
{ "error": "ECONNREFUSED 127.0.0.1:3306" }
\`\`\`

**Good** (user-friendly):
\`\`\`json
{ "error": "Database unavailable" }
\`\`\`

Stack traces and internal messages help attackers.`,
        tags: ['security', 'error-handling', 'best-practices']
      },
      {
        file: 'src/middleware/logging.js',
        line: 8,
        type: 'best-practice',
        title: 'Request Tracing',
        content: `Each request gets a unique ID for tracing:

1. Generate ID at request start
2. Attach to request object
3. Include in response header
4. Log with all related events

This enables **distributed tracing** across services and helps debug issues in production.`,
        tags: ['logging', 'observability', 'debugging']
      }
    ]
  }
];

// Quizzes - 2+ examples with different question types
export const demoQuizzes = [
  {
    id: 'security-fundamentals',
    title: 'Security Fundamentals Quiz',
    description: 'Test your understanding of web security concepts including authentication, password hashing, and common vulnerabilities.',
    context: 'Based on the authentication module in src/routes/auth.js',
    difficulty: 'intermediate',
    estimatedTime: '15min',
    questions: [
      {
        type: 'multiple_choice',
        question: "What's the benefit of bcrypt over SHA256 for password hashing?",
        options: [
          'Bcrypt is faster',
          'Bcrypt includes built-in salting and is deliberately slow',
          'SHA256 is deprecated',
          'Bcrypt produces shorter hashes'
        ],
        correct: 1,
        explanation: "Bcrypt's slowness is a feature - it makes brute force attacks impractical"
      },
      {
        type: 'multiple_choice',
        question: 'Why should you use constant-time comparison for passwords?',
        options: [
          "It's faster",
          'It prevents timing attacks that could reveal password length',
          'It uses less memory',
          "It's required by OWASP"
        ],
        correct: 1,
        explanation: 'Timing attacks measure response time differences to guess password characters'
      },
      {
        type: 'code_completion',
        question: 'Complete the secure password comparison:',
        codeContext: `const crypto = require('crypto');
function secureCompare(a, b) {
  return crypto.________(a, b);
}`,
        answerPattern: 'timingSafeEqual',
        incorrectPatterns: [
          { pattern: '===', feedback: 'Direct comparison is vulnerable to timing attacks' },
          { pattern: 'equals', feedback: 'Use crypto.timingSafeEqual for constant-time comparison' }
        ]
      },
      {
        type: 'scenario',
        question: "A user reports they can log in with 'password' and 'password ' (with trailing space). What's the likely issue?",
        options: [
          'The database is trimming passwords',
          "Passwords aren't being trimmed before hashing",
          'Bcrypt ignores trailing spaces',
          'This is expected behavior'
        ],
        correct: 1,
        explanation: 'Always normalize input (trim whitespace) before hashing to ensure consistent behavior'
      }
    ]
  },
  {
    id: 'database-patterns',
    title: 'Database Patterns Quiz',
    description: 'Test your knowledge of database connection management, transactions, and query optimization.',
    context: 'Based on the database layer in src/db/',
    difficulty: 'intermediate',
    estimatedTime: '12min',
    questions: [
      {
        type: 'multiple_choice',
        question: 'Why use connection pooling instead of creating new connections per request?',
        options: [
          'Pools use less memory',
          'Creating connections has ~50ms overhead',
          'Pools are required by MySQL',
          'Single connections are deprecated'
        ],
        correct: 1,
        explanation: 'Connection establishment involves TCP handshake, authentication, and setup - pooling amortizes this cost'
      },
      {
        type: 'multiple_choice',
        question: 'What should happen in a transaction if an error occurs?',
        options: [
          'Commit partial changes',
          'Rollback all changes',
          'Retry the failed operation',
          'Log and continue'
        ],
        correct: 1,
        explanation: 'Transactions ensure atomicity - all changes succeed or all are rolled back'
      },
      {
        type: 'code_completion',
        question: 'Complete the SQL injection prevention:',
        codeContext: `// Safe query execution
const user = await query(
  'SELECT * FROM users WHERE id = ____',
  [userId]
);`,
        answerPattern: '?',
        incorrectPatterns: [
          { pattern: '${', feedback: 'Template literals allow SQL injection - use ? placeholders' },
          { pattern: 'userId', feedback: 'Direct interpolation is vulnerable - use parameterized queries' }
        ]
      },
      {
        type: 'scenario',
        question: 'Your app crashes and connections are not released. What happens to the pool?',
        options: [
          'Connections are automatically recovered',
          'Pool becomes exhausted, new requests fail',
          'MySQL kills idle connections',
          'Nothing, pools are unlimited'
        ],
        correct: 1,
        explanation: 'Always release connections in finally blocks to prevent pool exhaustion'
      }
    ]
  },
  {
    id: 'api-design',
    title: 'REST API Design Quiz',
    description: 'Test your understanding of RESTful API design principles and Express.js patterns.',
    context: 'Based on the routes in src/routes/',
    difficulty: 'beginner',
    estimatedTime: '10min',
    questions: [
      {
        type: 'multiple_choice',
        question: 'Which HTTP status code should be returned for a successful resource creation?',
        options: [
          '200 OK',
          '201 Created',
          '204 No Content',
          '202 Accepted'
        ],
        correct: 1,
        explanation: '201 Created indicates the request succeeded and a new resource was created'
      },
      {
        type: 'multiple_choice',
        question: 'Why register error handling middleware last in Express?',
        options: [
          "It's faster that way",
          'Express requires this order',
          'It catches errors from all previous middleware',
          'Error handlers must be synchronous'
        ],
        correct: 2,
        explanation: 'Error middleware catches errors thrown by all middleware registered before it'
      },
      {
        type: 'code_completion',
        question: 'Complete the Express error handler signature:',
        codeContext: `function errorHandler(____, req, res, next) {
  // Handle error
}`,
        answerPattern: 'err',
        incorrectPatterns: [
          { pattern: 'error', feedback: 'Convention uses "err" as the parameter name' },
          { pattern: 'e', feedback: 'Use descriptive parameter name "err"' }
        ]
      },
      {
        type: 'scenario',
        question: 'A client sends invalid JSON in the request body. What status code should be returned?',
        options: [
          '500 Internal Server Error',
          '400 Bad Request',
          '422 Unprocessable Entity',
          '415 Unsupported Media Type'
        ],
        correct: 1,
        explanation: '400 Bad Request indicates the client sent malformed data'
      }
    ]
  }
];

// Guided Tours - 2+ examples covering different code flows
export const demoGuides = [
  {
    id: 'auth-flow-guide',
    title: 'Authentication Flow Walkthrough',
    description: 'Follow a login request through the system from entry to database',
    difficulty: 'intermediate',
    estimatedTime: '20min',
    prerequisites: [
      'Understanding of Express middleware',
      'Basic cryptography knowledge',
      'Familiarity with JWT tokens'
    ],
    stops: [
      {
        id: 'entry-point',
        file: 'src/routes/auth.js',
        line: 28,
        title: 'Entry Point',
        content: `This is where login requests enter our system.
Notice how we validate input before anything else.

Key points:
- Request body validation using Zod schema
- Early return on validation failure
- Sanitization of email input`,
        questions: [
          'What happens if email is missing?',
          'Why validate before authentication?'
        ]
      },
      {
        id: 'user-lookup',
        file: 'src/db/users.js',
        line: 8,
        title: 'User Lookup',
        content: `The findUserByEmail function queries the database.

Important security considerations:
- Email is normalized (lowercase, trimmed)
- Returns null if not found (don't reveal which failed)
- Only fetches needed fields`,
        questions: [
          'Why normalize the email?',
          'What fields should NOT be returned?'
        ]
      },
      {
        id: 'password-verification',
        file: 'src/routes/auth.js',
        line: 38,
        title: 'Password Verification',
        content: `Here we use bcrypt to verify the password.

Important security considerations:
- Never store plain text passwords
- bcrypt.compare handles salt extraction
- Same error message for all failures`,
        questions: [
          'Why use bcrypt instead of SHA256?',
          'What is a timing attack?'
        ]
      },
      {
        id: 'token-generation',
        file: 'src/routes/auth.js',
        line: 45,
        title: 'JWT Token Generation',
        content: `After successful authentication, we generate a JWT token.

Token contains:
- User ID
- Email
- Issued at timestamp (iat)
- Expiration time (24 hours)

The token is signed with our secret key.`,
        questions: [
          'What should NOT be stored in a JWT?',
          'Why set an expiration time?'
        ]
      },
      {
        id: 'response-handling',
        file: 'src/routes/auth.js',
        line: 52,
        title: 'Response Handling',
        content: `Finally, we send the response back to the client.

The response includes:
- JWT token
- User profile (without sensitive data)
- Token expiration info

Note how we exclude the password hash from the response.`,
        questions: [
          'What data should never be in the response?',
          'How should the client store the token?'
        ]
      }
    ]
  },
  {
    id: 'request-lifecycle',
    title: 'Request Lifecycle Tour',
    description: 'Understand how a request flows through Express middleware',
    difficulty: 'beginner',
    estimatedTime: '15min',
    prerequisites: [
      'Basic Express.js knowledge',
      'Understanding of HTTP'
    ],
    stops: [
      {
        id: 'server-entry',
        file: 'src/index.js',
        line: 12,
        title: 'Server Entry Point',
        content: `Every request starts here at the Express app.

The middleware chain is registered in order:
1. Body parsing (express.json)
2. Request logging
3. Route handlers
4. Error handling (last!)

Order matters - middleware runs top to bottom.`,
        questions: [
          'Why is error handling registered last?',
          'What does express.json() do?'
        ]
      },
      {
        id: 'logging-middleware',
        file: 'src/middleware/logging.js',
        line: 6,
        title: 'Request Logging',
        content: `The logging middleware runs first for every request.

It captures:
- Request ID (for tracing)
- HTTP method and path
- Client IP and User-Agent
- Response time (on finish)

This data is essential for debugging and monitoring.`,
        questions: [
          'Why generate a request ID?',
          'When is the response logged?'
        ]
      },
      {
        id: 'auth-middleware',
        file: 'src/middleware/auth.js',
        line: 10,
        title: 'Authentication Check',
        content: `Protected routes use the authenticate middleware.

The flow:
1. Extract token from Authorization header
2. Verify token signature and expiration
3. Attach user info to request object
4. Call next() to continue

If auth fails, the request stops here.`,
        questions: [
          'What header format is expected?',
          'What happens if the token is expired?'
        ]
      },
      {
        id: 'route-handler',
        file: 'src/routes/users.js',
        line: 18,
        title: 'Route Handler',
        content: `The route handler processes the actual request.

It has access to:
- req.user (from auth middleware)
- req.body (parsed JSON)
- req.params (URL parameters)
- req.query (query string)

Errors are passed to next() for centralized handling.`,
        questions: [
          'How is the user ID accessed?',
          'Why use next(error) instead of res.status(500)?'
        ]
      },
      {
        id: 'error-handling',
        file: 'src/middleware/errorHandler.js',
        line: 8,
        title: 'Error Handling',
        content: `All errors flow through this middleware.

It handles:
- Validation errors (Zod)
- Authentication errors (JWT)
- Database errors
- Unknown errors (500)

Different error types get appropriate status codes.`,
        questions: [
          'Why hide error details in production?',
          'How are Zod errors formatted?'
        ]
      }
    ]
  },
  {
    id: 'database-operations',
    title: 'Database Operations Guide',
    description: 'Learn how the database layer handles queries and transactions',
    difficulty: 'intermediate',
    estimatedTime: '18min',
    prerequisites: [
      'SQL basics',
      'Understanding of connection pooling',
      'Async/await in JavaScript'
    ],
    stops: [
      {
        id: 'pool-init',
        file: 'src/db/connection.js',
        line: 25,
        title: 'Connection Pool Setup',
        content: `The connection pool is initialized once at startup.

Configuration options:
- connectionLimit: max concurrent connections
- waitForConnections: queue when pool is full
- namedPlaceholders: use :name instead of ?

The pool manages connection lifecycle automatically.`,
        questions: [
          'What happens if connectionLimit is reached?',
          'Why use a pool instead of single connection?'
        ]
      },
      {
        id: 'query-execution',
        file: 'src/db/connection.js',
        line: 55,
        title: 'Query Execution',
        content: `The query() helper simplifies database access.

It handles:
- Getting connection from pool
- Executing parameterized query
- Returning results
- Releasing connection automatically

Always use ? placeholders for security.`,
        questions: [
          'Why use parameterized queries?',
          'What does pool.execute return?'
        ]
      },
      {
        id: 'transaction-pattern',
        file: 'src/db/connection.js',
        line: 65,
        title: 'Transaction Handling',
        content: `Transactions ensure multiple operations succeed or fail together.

The pattern:
1. Get dedicated connection
2. BEGIN transaction
3. Execute operations
4. COMMIT on success
5. ROLLBACK on error
6. ALWAYS release connection

The finally block prevents connection leaks.`,
        questions: [
          'When should you use a transaction?',
          'What happens if you forget to release?'
        ]
      },
      {
        id: 'user-queries',
        file: 'src/db/users.js',
        line: 30,
        title: 'User CRUD Operations',
        content: `The users module provides data access functions.

Best practices shown:
- Single responsibility (one function per operation)
- Parameterized queries (SQL injection prevention)
- Selective field returns (don't leak passwordHash)
- Consistent error handling`,
        questions: [
          'Why not return passwordHash?',
          'How is SQL injection prevented?'
        ]
      }
    ]
  }
];
