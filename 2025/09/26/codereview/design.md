# Code Review System Design

## System Architecture

### Overview
The system consists of three main components:
1. **Go CLI Backend** - Command-line interface with embedded web server
2. **SQLite Database** - Local storage for review data
3. **React Frontend** - Web-based user interface

### Component Interaction Flow
```
User → Go CLI → SQLite Database
  ↓
React Frontend ← HTTP API ← Go Web Server
```

## Database Schema

### SQLite Tables

```sql
-- Reviews table - stores review metadata
CREATE TABLE reviews (
    id TEXT PRIMARY KEY,                    -- Unique review identifier (rev-001, etc.)
    title TEXT NOT NULL,                    -- Human-readable title
    branch TEXT,                            -- Git branch being reviewed
    commit TEXT,                            -- Specific commit hash
    base_commit TEXT,                       -- Base commit for diff comparison
    reviewer TEXT,                          -- Email/username of reviewer
    created DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated DATETIME DEFAULT CURRENT_TIMESTAMP,
    status TEXT DEFAULT 'pending',          -- pending, approved, changes_requested, draft
    files_changed INTEGER DEFAULT 0,        -- Summary statistics
    lines_added INTEGER DEFAULT 0,
    lines_removed INTEGER DEFAULT 0,
    tags TEXT                               -- JSON array of tags
);

-- Annotations table - stores individual review comments
CREATE TABLE annotations (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    review_id TEXT NOT NULL,               -- Foreign key to reviews
    file TEXT NOT NULL,                    -- File path relative to repo root
    line INTEGER,                          -- Single line number (null for file-level)
    line_start INTEGER,                    -- Start of line range (null for single line)
    line_end INTEGER,                      -- End of line range (null for single line)
    type TEXT NOT NULL,                    -- issue, suggestion, praise, question
    severity TEXT DEFAULT 'minor',         -- minor, major, critical
    message TEXT NOT NULL,                 -- Main comment text
    suggestion TEXT,                       -- Code suggestion (for type=suggestion)
    status TEXT DEFAULT 'open',            -- open, resolved, acknowledged
    created DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (review_id) REFERENCES reviews(id) ON DELETE CASCADE
);

-- Threads table - stores discussion threads for annotations
CREATE TABLE threads (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    annotation_id INTEGER NOT NULL,        -- Foreign key to annotations
    author TEXT NOT NULL,                  -- Author of the thread message
    message TEXT NOT NULL,                 -- Thread message content
    timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (annotation_id) REFERENCES annotations(id) ON DELETE CASCADE
);

-- Config table - stores application configuration
CREATE TABLE config (
    key TEXT PRIMARY KEY,
    value TEXT NOT NULL,
    updated DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- Indexes for performance
CREATE INDEX idx_annotations_review_id ON annotations(review_id);
CREATE INDEX idx_annotations_file ON annotations(file);
CREATE INDEX idx_threads_annotation_id ON threads(annotation_id);
CREATE INDEX idx_reviews_status ON reviews(status);
CREATE INDEX idx_reviews_created ON reviews(created);
```

## Go CLI Architecture

### Project Structure
```
codereview/
├── cmd/
│   ├── root.go              # Main CLI setup with glaze
│   ├── init.go              # Initialize .codereview directory
│   ├── create.go            # Create new review
│   ├── list.go              # List reviews
│   ├── show.go              # Show specific review
│   ├── annotate.go          # Add annotations via CLI
│   ├── serve.go             # Start web server
│   ├── export.go            # Export to YAML/other formats
│   └── import.go            # Import from YAML
├── internal/
│   ├── database/
│   │   ├── db.go            # Database connection and setup
│   │   ├── reviews.go       # Review CRUD operations
│   │   ├── annotations.go   # Annotation CRUD operations
│   │   ├── threads.go       # Thread CRUD operations
│   │   └── migrations.go    # Database migrations
│   ├── git/
│   │   ├── diff.go          # Git diff parsing
│   │   ├── files.go         # File operations
│   │   └── repo.go          # Repository operations
│   ├── server/
│   │   ├── server.go        # HTTP server setup
│   │   ├── handlers.go      # API handlers
│   │   ├── middleware.go    # HTTP middleware
│   │   └── static.go        # Static file serving
│   ├── models/
│   │   ├── review.go        # Review data structures
│   │   ├── annotation.go    # Annotation data structures
│   │   └── thread.go        # Thread data structures
│   └── config/
│       └── config.go        # Configuration management
├── web/                     # React frontend build output
├── go.mod
├── go.sum
├── Makefile
└── README.md
```

### CLI Commands Design

#### Root Command
```bash
codereview --help
```

#### Initialize Repository
```bash
codereview init [--reviewer email] [--config-file path]
```
- Creates `.codereview/` directory
- Initializes SQLite database
- Sets up default configuration

#### Create Review
```bash
codereview create [--title "Review Title"] [--branch branch-name] [--commit hash]
```
- Creates new review entry
- Auto-detects current branch/commit if not specified
- Generates unique review ID

#### List Reviews
```bash
codereview list [--status pending|approved|changes_requested] [--format table|json|yaml]
```
- Lists all reviews with filtering options
- Supports multiple output formats via glaze

#### Show Review
```bash
codereview show <review-id> [--format table|json|yaml]
```
- Shows detailed review information
- Includes annotations and statistics

#### Add Annotation
```bash
codereview annotate <review-id> <file> [--line N] [--type issue|suggestion|praise|question] [--severity minor|major|critical] [--message "text"] [--suggestion "code"]
```
- Adds annotation to specific file/line
- Interactive mode if message not provided

#### Start Web Server
```bash
codereview serve [--port 8080] [--host localhost] [--open]
```
- Starts HTTP server
- Serves React frontend
- Provides REST API
- Optionally opens browser

#### Export/Import
```bash
codereview export <review-id> [--format yaml|json] [--output file]
codereview import <file> [--format yaml|json]
```
- Export review to various formats
- Import reviews from external sources

## REST API Design

### Base URL: `/api/v1`

#### Reviews Endpoints
```
GET    /reviews                    # List all reviews
POST   /reviews                    # Create new review
GET    /reviews/{id}               # Get specific review
PUT    /reviews/{id}               # Update review
DELETE /reviews/{id}               # Delete review
GET    /reviews/{id}/stats         # Get review statistics
```

#### Annotations Endpoints
```
GET    /reviews/{id}/annotations   # Get annotations for review
POST   /reviews/{id}/annotations   # Add annotation to review
GET    /annotations/{id}           # Get specific annotation
PUT    /annotations/{id}           # Update annotation
DELETE /annotations/{id}           # Delete annotation
```

#### Threads Endpoints
```
GET    /annotations/{id}/threads   # Get threads for annotation
POST   /annotations/{id}/threads   # Add thread message
PUT    /threads/{id}               # Update thread message
DELETE /threads/{id}               # Delete thread message
```

#### Git Integration Endpoints
```
GET    /git/diff/{commit}          # Get git diff for commit
GET    /git/diff/{from}..{to}      # Get diff between commits
GET    /git/files/{commit}         # Get file list for commit
GET    /git/file/{commit}/{path}   # Get file content
GET    /git/branches               # List branches
GET    /git/commits/{branch}       # List commits for branch
```

#### Utility Endpoints
```
GET    /config                     # Get configuration
PUT    /config                     # Update configuration
GET    /health                     # Health check
GET    /version                    # Version information
```

## React Frontend Architecture

### Component Hierarchy
```
App
├── Router
├── Header
├── Sidebar
└── Main Content
    ├── ReviewList
    ├── ReviewDetail
    │   ├── DiffViewer
    │   │   ├── FileHeader
    │   │   ├── DiffLine
    │   │   └── InlineAnnotation
    │   ├── AnnotationPanel
    │   │   ├── AnnotationForm
    │   │   ├── AnnotationList
    │   │   └── AnnotationItem
    │   └── ReviewSummary
    └── Settings
```

### Redux Store Structure
```typescript
interface RootState {
  reviews: {
    list: Review[];
    current: Review | null;
    loading: boolean;
    error: string | null;
  };
  annotations: {
    byReview: Record<string, Annotation[]>;
    loading: boolean;
    error: string | null;
  };
  git: {
    diff: GitDiff | null;
    files: string[];
    branches: string[];
    loading: boolean;
    error: string | null;
  };
  ui: {
    selectedLine: number | null;
    showAnnotationForm: boolean;
    sidebarOpen: boolean;
  };
}
```

### TypeScript Interfaces
```typescript
interface Review {
  id: string;
  title: string;
  branch?: string;
  commit?: string;
  baseCommit?: string;
  reviewer: string;
  created: string;
  updated: string;
  status: 'pending' | 'approved' | 'changes_requested' | 'draft';
  filesChanged: number;
  linesAdded: number;
  linesRemoved: number;
  tags: string[];
}

interface Annotation {
  id: number;
  reviewId: string;
  file: string;
  line?: number;
  lineStart?: number;
  lineEnd?: number;
  type: 'issue' | 'suggestion' | 'praise' | 'question';
  severity: 'minor' | 'major' | 'critical';
  message: string;
  suggestion?: string;
  status: 'open' | 'resolved' | 'acknowledged';
  created: string;
  updated: string;
  threads?: Thread[];
}

interface Thread {
  id: number;
  annotationId: number;
  author: string;
  message: string;
  timestamp: string;
}

interface GitDiff {
  file: string;
  changes: DiffLine[];
}

interface DiffLine {
  type: 'context' | 'added' | 'removed';
  oldLine?: number;
  newLine?: number;
  content: string;
}
```

## Configuration Management

### Default Configuration
```yaml
# .codereview/config.yml
settings:
  default_reviewer: ""
  require_approval: false
  auto_assign: false
  database_path: ".codereview/reviews.db"
  
server:
  port: 8080
  host: "localhost"
  
git:
  default_base: "main"
  ignore_patterns:
    - "*.log"
    - "node_modules/"
    - ".git/"
    
templates:
  security:
    tags: ["security"]
    required_checks:
      - "Input validation"
      - "Authentication"
      - "Authorization"
      
  performance:
    tags: ["performance"]
    required_checks:
      - "Time complexity"
      - "Memory usage"
```

This design provides a comprehensive foundation for implementing the code review system with clear separation of concerns, scalable architecture, and robust data management.
