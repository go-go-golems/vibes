# Code Review System Analysis

## Requirements Summary

Based on the user request and provided examples, I need to implement:

1. **Go CLI Backend**
   - Command-line interface for code review operations
   - SQLite database for storing review data
   - `serve` command to host the React frontend
   - Integration with git repositories

2. **React Frontend**
   - Interactive code review interface
   - Diff viewer with annotation capabilities
   - Review management and export functionality
   - Integration with backend API

3. **Integration**
   - Makefile for building and bundling
   - Go CLI serves the React app
   - Data persistence in SQLite

## Provided Examples Analysis

### YAML DSL Specification
The first example provides a comprehensive YAML schema for code reviews:

**Key Components:**
- Review metadata (id, title, branch, commit, reviewer, status)
- Annotations with types (issue, suggestion, praise, question)
- Severity levels (minor, major, critical)
- Threading support for discussions
- File and line-specific comments
- Summary statistics

**Directory Structure:**
```
.codereview/
├── config.yml
├── reviews/
│   ├── rev-001.yml
│   └── ...
└── templates/
    └── default.yml
```

### React Component Example
The second example shows a functional React component with:

**Features:**
- Diff viewer with line-by-line display
- Interactive annotation system
- Form for adding new annotations
- Review summary panel
- Export functionality
- Mock git diff data structure

**UI Components:**
- Line-based diff display with old/new line numbers
- Color-coded additions/removals/context
- Inline annotation display
- Annotation form with type/severity selection
- Summary statistics

## System Architecture Design

### Go CLI Structure
```
cmd/
├── root.go          # Main CLI setup
├── init.go          # Initialize review repository
├── review.go        # Create/manage reviews
├── annotate.go      # Add annotations
├── list.go          # List reviews
├── serve.go         # Web server
└── export.go        # Export functionality
```

### Database Schema (SQLite)
```sql
-- Reviews table
CREATE TABLE reviews (
    id TEXT PRIMARY KEY,
    title TEXT NOT NULL,
    branch TEXT,
    commit TEXT,
    reviewer TEXT,
    created DATETIME,
    status TEXT DEFAULT 'pending'
);

-- Annotations table
CREATE TABLE annotations (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    review_id TEXT,
    file TEXT NOT NULL,
    line INTEGER,
    line_end INTEGER,
    type TEXT NOT NULL,
    severity TEXT,
    message TEXT NOT NULL,
    suggestion TEXT,
    status TEXT DEFAULT 'open',
    created DATETIME,
    FOREIGN KEY (review_id) REFERENCES reviews(id)
);

-- Threads table for discussions
CREATE TABLE threads (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    annotation_id INTEGER,
    author TEXT,
    message TEXT,
    timestamp DATETIME,
    FOREIGN KEY (annotation_id) REFERENCES annotations(id)
);
```

### API Endpoints
```
GET    /api/reviews              # List all reviews
POST   /api/reviews              # Create new review
GET    /api/reviews/{id}         # Get specific review
PUT    /api/reviews/{id}         # Update review
DELETE /api/reviews/{id}         # Delete review

GET    /api/reviews/{id}/annotations    # Get annotations for review
POST   /api/reviews/{id}/annotations    # Add annotation
PUT    /api/annotations/{id}            # Update annotation
DELETE /api/annotations/{id}            # Delete annotation

GET    /api/git/diff/{commit}           # Get git diff data
GET    /api/git/files/{commit}          # Get file list
```

### React Application Structure
```
src/
├── components/
│   ├── DiffViewer.tsx
│   ├── AnnotationForm.tsx
│   ├── AnnotationList.tsx
│   ├── ReviewSummary.tsx
│   └── ReviewList.tsx
├── store/
│   ├── reviewSlice.ts
│   ├── annotationSlice.ts
│   └── store.ts
├── services/
│   └── api.ts
├── types/
│   └── index.ts
└── App.tsx
```

## Implementation Plan

1. **Phase 1: Go CLI Backend**
   - Set up Go project with glaze framework
   - Implement SQLite database layer
   - Create CLI commands (init, review, annotate, list, serve)
   - Add git integration for diff parsing

2. **Phase 2: Testing**
   - Create test repository
   - Test CLI commands
   - Verify database operations

3. **Phase 3: React Frontend**
   - Set up TypeScript React project with Redux
   - Implement diff viewer component
   - Create annotation system
   - Add API integration

4. **Phase 4: Integration**
   - Create Makefile for building
   - Integrate React build with Go serve command
   - Test complete system

This analysis provides the foundation for implementing a comprehensive code review system that combines the YAML DSL specification with an interactive web interface.
