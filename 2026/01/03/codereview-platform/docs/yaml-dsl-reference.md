# YAML DSL Reference Documentation

**Code Review Knowledge Platform**

*Version 1.0 | January 2025*

---

## Table of Contents

1. [Introduction](#introduction)
2. [Storage Architecture](#storage-architecture)
3. [Code Review DSL](#code-review-dsl)
4. [Quiz DSL](#quiz-dsl)
5. [Guided Tour DSL](#guided-tour-dsl)
6. [Git Notes Integration](#git-notes-integration)
7. [Complete Examples](#complete-examples)

---

## Introduction

The Code Review Knowledge Platform uses a YAML-based Domain Specific Language (DSL) to define code reviews, educational quizzes, and guided tours through codebases. This document provides a comprehensive reference for all DSL constructs, their properties, and usage patterns.

The DSL was designed with several goals in mind. First, it needed to be human-readable and easy to author without specialized tooling. Second, it needed to integrate seamlessly with git workflows by leveraging git notes for storage. Third, it needed to support rich educational content including embedded quizzes, code snippets, and narrative explanations. Finally, it needed to be extensible for future enhancements without breaking existing definitions.

All DSL definitions are stored as YAML documents within git notes, which means they are version-controlled alongside the code they describe. This approach ensures that educational content stays synchronized with the codebase as it evolves.

---

## Storage Architecture

### Git Notes Overview

Git notes provide a mechanism to attach arbitrary data to commits without modifying the commits themselves. The platform uses three separate note namespaces to organize different types of content:

| Namespace | Purpose | Content Type |
|-----------|---------|--------------|
| `refs/notes/reviews` | Code review definitions | Array of ReviewDefinition |
| `refs/notes/quizzes` | Quiz definitions | Array of QuizDefinition |
| `refs/notes/guides` | Guided tour definitions | Array of GuideDefinition |

Each namespace stores an array of definitions as a single YAML document attached to a specific commit. This design allows multiple reviews, quizzes, or guides to be associated with the same commit while maintaining clear separation between content types.

### Data Flow

When content is created or updated, the platform serializes the definitions to YAML and writes them to the appropriate git notes namespace using the `isomorphic-git` library. This pure JavaScript implementation ensures the platform works in environments where the git CLI is not available, such as serverless deployments.

Reading content follows the reverse path: the platform reads the raw note content, parses it as YAML, validates the structure against the expected schema, and returns typed objects for use in the application.

---

## Code Review DSL

Code reviews are the primary content type in the platform. Each review represents a pull request or code change that contains educational annotations explaining the code.

### ReviewDefinition Schema

The top-level structure for a code review definition contains metadata about the review and a collection of annotations:

```yaml
id: string                    # Unique identifier for the review
title: string                 # Display title for the review
description: string           # Detailed description of what the review covers
pr_number: number             # Associated pull request number (optional)
base_branch: string           # Target branch for the changes
head_branch: string           # Source branch containing the changes
status: string                # Review status: open, merged, or closed
files: string[]               # List of files modified in this review
annotations: Annotation[]     # Array of annotation objects
```

### Annotation Schema

Annotations are the educational content attached to specific locations in the code. Each annotation has a type that determines how it is displayed and what additional properties it supports:

```yaml
id: string                    # Unique identifier for the annotation
type: string                  # One of: educational, gotcha, best-practice, question
file: string                  # Path to the file being annotated
line_start: number            # Starting line number (1-indexed)
line_end: number              # Ending line number (optional, defaults to line_start)
title: string                 # Short title for the annotation
content: string               # Markdown content with the explanation
tags: string[]                # Categorization tags for filtering
quiz: EmbeddedQuiz            # Optional embedded quiz for this annotation
```

### Annotation Types

The platform supports four annotation types, each serving a distinct educational purpose:

**Educational annotations** provide explanatory content about how code works, why certain patterns are used, or background information that helps readers understand the implementation. These are the most common annotation type and form the foundation of the learning experience.

**Gotcha annotations** highlight potential pitfalls, common mistakes, or non-obvious behavior that could trip up developers. These annotations often include warnings about security vulnerabilities, performance issues, or subtle bugs that are easy to introduce.

**Best-practice annotations** recommend preferred approaches and explain why certain patterns should be followed. These annotations help establish coding standards and share institutional knowledge about effective development practices.

**Question annotations** pose thought-provoking questions to the reader, encouraging active engagement with the material. These can be used to prompt reflection before revealing an answer or to check understanding of the preceding content.

### Embedded Quiz Schema

Annotations can include embedded quizzes to test understanding of the annotated code:

```yaml
quiz:
  question: string            # The question text
  type: string                # Question type: multiple_choice, code_completion, scenario
  options: string[]           # Answer options (for multiple_choice)
  correct_answer: string      # The correct answer
  explanation: string         # Explanation shown after answering
```

---

## Quiz DSL

Standalone quizzes provide comprehensive assessments that can cover multiple topics and files. Unlike embedded quizzes in annotations, standalone quizzes are designed for focused learning sessions.

### QuizDefinition Schema

```yaml
id: string                    # Unique identifier for the quiz
title: string                 # Display title
description: string           # What the quiz covers
difficulty: string            # beginner, intermediate, or advanced
estimated_time: number        # Expected completion time in minutes
tags: string[]                # Categorization tags
prerequisites: string[]       # Recommended knowledge before taking
related_files: string[]       # Files covered by this quiz
questions: Question[]         # Array of question objects
```

### Question Schema

Each question in a quiz follows a consistent structure with type-specific properties:

```yaml
id: string                    # Unique identifier for the question
type: string                  # Question type (see below)
question: string              # The question text (supports Markdown)
points: number                # Point value for scoring
options: string[]             # Answer options (for multiple_choice)
correct_answer: string        # The correct answer
explanation: string           # Detailed explanation of the answer
code_context: CodeContext     # Optional code snippet for context
hints: string[]               # Optional hints for the question
```

### Question Types

The platform supports three question types, each designed for different assessment scenarios:

**Multiple Choice Questions** present a question with several possible answers, only one of which is correct. These are effective for testing factual knowledge, recognition of patterns, and understanding of concepts. The `options` array contains all possible answers, and `correct_answer` specifies which option is correct.

**Code Completion Questions** present a code snippet with a blank that the learner must fill in. These questions test practical coding knowledge and familiarity with APIs, syntax, and idioms. The `code_context` property provides the surrounding code, and the learner types their answer in a text field.

**Scenario Questions** present a realistic situation and ask the learner to identify the best course of action or diagnose a problem. These questions test higher-order thinking skills like analysis, evaluation, and application of knowledge to novel situations.

### CodeContext Schema

Code context provides syntax-highlighted code snippets that accompany questions:

```yaml
code_context:
  file: string                # Source file path
  language: string            # Programming language for highlighting
  code: string                # The code snippet
  highlight_lines: number[]   # Lines to emphasize (optional)
```

---

## Guided Tour DSL

Guided tours provide narrative walkthroughs through a codebase, leading learners through a sequence of stops that tell a coherent story about how the code works.

### GuideDefinition Schema

```yaml
id: string                    # Unique identifier for the guide
title: string                 # Display title
description: string           # Overview of what the guide covers
difficulty: string            # beginner, intermediate, or advanced
estimated_time: number        # Expected completion time in minutes
prerequisites: string[]       # Recommended knowledge before starting
tags: string[]                # Categorization tags
stops: Stop[]                 # Ordered array of tour stops
```

### Stop Schema

Each stop in a guided tour represents a location in the code with accompanying educational content:

```yaml
id: string                    # Unique identifier for the stop
title: string                 # Short title for the stop
file: string                  # Path to the file
line_start: number            # Starting line number
line_end: number              # Ending line number (optional)
content: string               # Markdown content explaining this stop
key_points: string[]          # Bullet points summarizing key takeaways
questions_to_consider: string[] # Thought-provoking questions
next_stop_hint: string        # Preview of what comes next (optional)
```

### Tour Navigation

Tours are designed to be followed in sequence, with each stop building on the previous ones. The platform tracks which stops have been visited and highlights the current position in the tour. Learners can navigate forward and backward through stops, or jump directly to any stop using the stop list.

The `next_stop_hint` property allows tour authors to create narrative continuity between stops, previewing what the learner will explore next and why it matters.

---

## Git Notes Integration

### Writing Notes with isomorphic-git

The platform uses `isomorphic-git` for all git operations, ensuring compatibility with environments where the git CLI is not available. Writing a note involves several steps:

1. **Serialize the content** to YAML format using the `yaml` library
2. **Create a blob** containing the YAML content
3. **Update the notes ref** to point to the new blob for the target commit

The implementation handles the complexity of git's object model, including creating tree objects and updating references atomically.

### Reading Notes

Reading notes reverses the write process:

1. **Resolve the notes ref** to find the tree containing note mappings
2. **Look up the blob** for the target commit
3. **Read and parse** the blob content as YAML
4. **Validate and type** the parsed data against the expected schema

### Note Namespaces

Using separate namespaces for reviews, quizzes, and guides provides several benefits. It allows independent versioning of different content types, simplifies queries by content type, and prevents naming collisions between different definition types.

---

## Complete Examples

### Complete Code Review Example

```yaml
id: auth-middleware-review
title: Add authentication middleware
description: >
  This PR introduces bcrypt-based password hashing and JWT authentication
  with proper security practices. The implementation follows OWASP guidelines
  for secure authentication.
pr_number: 1
base_branch: main
head_branch: feature/auth
status: merged
files:
  - src/routes/auth.js
  - src/middleware/auth.js
annotations:
  - id: bcrypt-explanation
    type: educational
    file: src/routes/auth.js
    line_start: 35
    line_end: 42
    title: Why bcrypt for password hashing?
    content: |
      Bcrypt is specifically designed for password hashing with several
      important properties:
      
      1. **Adaptive cost factor**: The work factor can be increased as
         hardware improves, keeping the hash computation slow enough to
         deter brute-force attacks.
      
      2. **Built-in salting**: Each password gets a unique random salt,
         preventing rainbow table attacks.
      
      3. **Constant-time comparison**: The bcrypt library includes
         timing-safe comparison functions to prevent timing attacks.
      
      Never use fast hashes like MD5 or SHA256 for passwords—they can
      be computed billions of times per second on modern GPUs.
    tags:
      - security
      - hashing
      - best-practices
    quiz:
      question: Why is bcrypt preferred over SHA256 for password hashing?
      type: multiple_choice
      options:
        - SHA256 is cryptographically broken
        - Bcrypt is faster to compute
        - Bcrypt includes built-in salting and adaptive cost
        - SHA256 produces shorter hashes
      correct_answer: Bcrypt includes built-in salting and adaptive cost
      explanation: >
        Bcrypt's slowness is a feature, not a bug. The adaptive cost factor
        means computation time can be increased as hardware improves, while
        built-in salting prevents precomputation attacks.

  - id: timing-attack-prevention
    type: gotcha
    file: src/routes/auth.js
    line_start: 48
    line_end: 52
    title: Timing Attack Prevention
    content: |
      This code uses `crypto.timingSafeEqual()` for comparing tokens.
      
      **Why this matters**: Regular string comparison (`===`) returns as soon
      as it finds a mismatched character. An attacker can measure response
      times to gradually discover valid tokens character by character.
      
      Timing-safe comparison always takes the same amount of time regardless
      of where (or if) the strings differ, eliminating this side channel.
    tags:
      - security
      - vulnerability
      - timing-attack
```

### Complete Quiz Example

```yaml
id: security-fundamentals
title: Security Fundamentals Quiz
description: >
  Test your understanding of web security concepts including authentication,
  password hashing, and common vulnerabilities.
difficulty: intermediate
estimated_time: 15
tags:
  - security
  - authentication
  - best-practices
prerequisites:
  - Basic JavaScript knowledge
  - Understanding of HTTP
  - Familiarity with Node.js
related_files:
  - src/routes/auth.js
  - src/middleware/auth.js
questions:
  - id: q1-bcrypt
    type: multiple_choice
    question: |
      Why is bcrypt preferred over SHA256 for password hashing?
    points: 10
    options:
      - SHA256 is cryptographically broken
      - Bcrypt is faster to compute
      - Bcrypt includes built-in salting and adaptive cost
      - SHA256 produces shorter hashes
    correct_answer: Bcrypt includes built-in salting and adaptive cost
    explanation: |
      Bcrypt's slowness is intentional—it makes brute-force attacks
      impractical. The adaptive cost factor allows increasing computation
      time as hardware improves, while built-in salting prevents
      precomputation attacks like rainbow tables.

  - id: q2-timing
    type: multiple_choice
    question: |
      What security vulnerability does `crypto.timingSafeEqual()` prevent?
    points: 10
    options:
      - SQL injection attacks
      - Cross-site scripting (XSS)
      - Timing attacks on token comparison
      - Buffer overflow attacks
    correct_answer: Timing attacks on token comparison
    explanation: |
      Regular string comparison returns early when characters don't match.
      Attackers can measure response times to gradually discover valid
      tokens. Timing-safe comparison always takes the same time regardless
      of input, eliminating this side channel.

  - id: q3-code-completion
    type: code_completion
    question: |
      Complete the code to perform a timing-safe comparison of two buffers:
      
      ```javascript
      const crypto = require('crypto');
      const isValid = crypto.____________(
        Buffer.from(providedToken),
        Buffer.from(expectedToken)
      );
      ```
    points: 10
    correct_answer: timingSafeEqual
    explanation: |
      The `crypto.timingSafeEqual()` function compares two buffers in
      constant time, preventing timing-based side-channel attacks.
    code_context:
      file: src/routes/auth.js
      language: javascript
      code: |
        const crypto = require('crypto');
        
        function validateToken(providedToken, expectedToken) {
          // Timing-safe comparison prevents timing attacks
          return crypto.____________(
            Buffer.from(providedToken),
            Buffer.from(expectedToken)
          );
        }
      highlight_lines: [5, 6, 7]

  - id: q4-scenario
    type: scenario
    question: |
      A user reports they can't log in even though they're certain their
      password is correct. You check the logs and see the password hash
      comparison is failing. The user mentions they copy-paste their
      password from a password manager.
      
      What is the most likely cause?
    points: 10
    options:
      - The bcrypt library is corrupted
      - The password contains unicode characters
      - Trailing whitespace is being included in the paste
      - The database connection is timing out
    correct_answer: Trailing whitespace is being included in the paste
    explanation: |
      Password managers and copy-paste operations often include trailing
      spaces or newlines. The stored hash was created from the trimmed
      password, but the login attempt includes the whitespace.
      
      Best practice: Always trim passwords before hashing and comparison.
```

### Complete Guided Tour Example

```yaml
id: auth-flow-walkthrough
title: Authentication Flow Walkthrough
description: >
  Follow a login request through the system from entry to database,
  understanding each step of the authentication process.
difficulty: intermediate
estimated_time: 20
prerequisites:
  - Understanding of Express middleware
  - Basic cryptography knowledge
  - Familiarity with JWT tokens
tags:
  - authentication
  - security
  - express
stops:
  - id: stop-1-entry
    title: Entry Point
    file: src/routes/auth.js
    line_start: 28
    line_end: 35
    content: |
      Our journey begins at the `/login` endpoint. When a user submits
      their credentials, this route handler receives the request.
      
      Notice how we immediately destructure `email` and `password` from
      the request body. Express's JSON middleware has already parsed the
      incoming JSON payload for us.
      
      **Security consideration**: We don't log the password or include it
      in any error messages—sensitive data should never appear in logs.
    key_points:
      - Login requests arrive at POST /login
      - Request body is parsed as JSON by middleware
      - Credentials are extracted but never logged
    questions_to_consider:
      - What happens if the request body is malformed?
      - How would you rate-limit login attempts?
    next_stop_hint: >
      Next, we'll see how the system looks up the user in the database.

  - id: stop-2-user-lookup
    title: User Lookup
    file: src/db/users.js
    line_start: 15
    line_end: 28
    content: |
      The `findByEmail` function queries the database for a user with
      the provided email address.
      
      This query uses a parameterized statement to prevent SQL injection.
      The `?` placeholder is replaced with the email value by the database
      driver, ensuring special characters are properly escaped.
      
      **Important**: We retrieve the full user record including the hashed
      password. The hash will be compared against the provided password
      in the next step.
    key_points:
      - Parameterized queries prevent SQL injection
      - User lookup is case-sensitive by default
      - The password hash is retrieved for comparison
    questions_to_consider:
      - Should email lookup be case-insensitive?
      - What index would optimize this query?
    next_stop_hint: >
      With the user record in hand, we can now verify the password.

  - id: stop-3-password-verification
    title: Password Verification
    file: src/routes/auth.js
    line_start: 45
    line_end: 55
    content: |
      Here's where the actual authentication happens. The `bcrypt.compare`
      function takes the plaintext password and the stored hash, then
      determines if they match.
      
      Internally, bcrypt extracts the salt and cost factor from the stored
      hash, applies them to the provided password, and compares the results.
      This is why we don't need to store the salt separately—it's embedded
      in the hash string itself.
      
      The comparison is performed in constant time to prevent timing attacks.
    key_points:
      - bcrypt.compare handles salt extraction automatically
      - Comparison is timing-safe by design
      - Invalid passwords return false, not an error
    questions_to_consider:
      - Why doesn't bcrypt throw an error for wrong passwords?
      - How would you implement account lockout after failed attempts?
    next_stop_hint: >
      Once verified, we need to create a session token for the user.

  - id: stop-4-token-generation
    title: JWT Token Generation
    file: src/routes/auth.js
    line_start: 60
    line_end: 75
    content: |
      After successful authentication, we generate a JSON Web Token (JWT)
      that the client will use for subsequent requests.
      
      The token payload includes the user's ID and role—enough information
      to authorize requests without hitting the database every time.
      
      **Security considerations**:
      - The token is signed with a secret key to prevent tampering
      - We set an expiration time to limit the window of vulnerability
      - Sensitive data like passwords is never included in the token
    key_points:
      - JWTs contain claims about the user
      - Tokens are signed to prevent tampering
      - Expiration limits security exposure
    questions_to_consider:
      - What should the token expiration time be?
      - How would you implement token refresh?
    next_stop_hint: >
      Finally, we'll see how the token is validated on subsequent requests.

  - id: stop-5-token-validation
    title: Token Validation Middleware
    file: src/middleware/auth.js
    line_start: 10
    line_end: 35
    content: |
      This middleware runs before protected routes, validating the JWT
      from the Authorization header.
      
      The `jwt.verify` function checks both the signature and expiration.
      If valid, the decoded payload is attached to `req.user`, making
      user information available to route handlers.
      
      **Error handling**: Invalid or expired tokens result in a 401
      response, prompting the client to re-authenticate.
    key_points:
      - Middleware validates tokens before protected routes
      - Decoded user info is attached to the request
      - Invalid tokens return 401 Unauthorized
    questions_to_consider:
      - How would you handle token refresh transparently?
      - What additional claims might be useful in the token?
```

---

## Validation and Error Handling

The platform validates all DSL definitions when they are loaded, providing clear error messages for malformed content. Common validation checks include:

| Check | Error Message |
|-------|---------------|
| Missing required field | "Required field 'title' is missing" |
| Invalid type value | "Invalid annotation type 'warning', expected one of: educational, gotcha, best-practice, question" |
| Invalid line numbers | "line_end (10) must be greater than or equal to line_start (15)" |
| Duplicate IDs | "Duplicate annotation ID 'intro' found" |
| Invalid difficulty | "Invalid difficulty 'expert', expected one of: beginner, intermediate, advanced" |

Validation errors include the path to the problematic field and suggestions for correction when possible.

---

## Best Practices

When authoring DSL content, consider the following recommendations:

**Use descriptive IDs** that indicate the content's purpose. IDs like `bcrypt-explanation` or `timing-attack-prevention` are more maintainable than `annotation-1` or `q3`.

**Write content in Markdown** to take advantage of formatting capabilities. Use code blocks for inline examples, bold text for emphasis, and lists for enumerating points.

**Tag content consistently** to enable effective filtering and discovery. Establish a tagging taxonomy and document it for content authors.

**Keep annotations focused** on a single concept. If an annotation covers multiple topics, consider splitting it into separate annotations.

**Order quiz questions by difficulty** within a quiz, starting with easier questions to build confidence before tackling harder ones.

**Create narrative flow in tours** by using `next_stop_hint` to preview upcoming content and explain why the tour progresses in a particular order.

---

*This document is part of the Code Review Knowledge Platform documentation.*
