# Code Review Knowledge Platform - TODO

## CLI Testing Tools
- [x] cli/git-notes.ts - Test git notes read/write operations
- [x] cli/yaml-parser.ts - Test YAML DSL parsing
- [x] cli/api-test.ts - Test API endpoints
- [x] cli/repo-init.ts - Initialize test repositories (using git-notes.ts)

## Core Infrastructure
- [x] Database schema for repositories, reviews, quizzes, and user progress
- [x] Git notes storage layer for code review metadata
- [x] Git notes storage layer for quiz submissions
- [x] YAML DSL parser for review annotations
- [x] YAML DSL parser for quiz definitions
- [x] YAML DSL parser for guided tour definitions

## Codebase Navigation
- [x] Repository management (add/list repositories)
- [x] File browser with directory tree navigation
- [x] Syntax highlighting for multiple languages
- [x] Branch selector and navigation
- [ ] Branch comparison view

## Code Review System
- [x] PR metadata display (title, description, status)
- [x] Diff view with annotations
- [x] Inline annotation display
- [x] File-level PR association view
- [ ] Review comments and discussions

## Educational Quiz System
- [x] Multiple choice questions
- [x] Code completion questions
- [x] Interactive scenario questions
- [x] Quiz submission tracking via git notes
- [x] Progress monitoring and statistics
- [x] File-level quiz association view

## Review Guide System
- [x] Guided tour navigation
- [x] Sequential stop progression
- [x] Progress tracking (visited/current/locked)
- [x] Prerequisites display
- [x] Difficulty levels and estimated time

## Test Repository
- [x] Initialize test repository with sample code
- [x] Create sample code reviews with YAML annotations
- [x] Create sample quizzes with different question types
- [x] Create sample guided tours
- [x] Browser testing of all features

## UI/UX
- [x] Clean developer-focused design
- [x] Responsive layout
- [x] Dark/light theme support
- [x] Navigation and breadcrumbs
- [x] Loading states and error handling


## Demo Repository Bundling
- [x] Create bundled demo repository with multiple source files
- [x] Create 3 code reviews with different annotation types
- [x] Create 3 quizzes with different question types
- [x] Create 3 guided tours covering different code flows
- [x] Add server endpoint to initialize demo repository on first load
- [x] Store demo repo in persistent location (/tmp/code-review-demo)
- [x] Test demo data loads correctly after deployment


## Bug Fix: Git CLI Not Available in Production
- [x] Install isomorphic-git package for pure JavaScript git operations
- [x] Refactor demo-init.ts to use isomorphic-git instead of CLI
- [x] Refactor git-notes.ts to use isomorphic-git instead of CLI
- [x] Test all features work with isomorphic-git


## Documentation
- [x] YAML DSL reference documentation
- [x] Quiz system implementation walkthrough
- [x] Technical code architecture documentation
- [ ] Create zip package of the project
