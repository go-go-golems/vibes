---
Title: 'Deep Codebase Analysis: Architecture, Components, and APIs'
Ticket: ZINE-LAYOUT-CLEANUP
Status: active
Topics:
    - architecture
    - analysis
    - cleanup
DocType: design-doc
Intent: long-term
Owners: []
RelatedFiles:
    - Path: ../../../../../../../../../../zine-layout/cmd/zine-layout/main.go
      Note: Main entry point
    - Path: ../../../../../../../../../../zine-layout/pkg/imagelayout/engine/engine.go
      Note: Core image layout placement algorithm
    - Path: ../../../../../../../../../../zine-layout/pkg/pagelayout/renderer/renderer.go
      Note: Page renderer producing multiple variants
    - Path: ../../../../../../../../../../zine-layout/pkg/repo/types.go
      Note: Core domain entities and repository interfaces
    - Path: ../../../../../../../../../../zine-layout/pkg/serve/server.go
      Note: HTTP server initialization and route configuration
    - Path: ../../../../../../../../../../zine-layout/pkg/services/imposition.go
      Note: ImpositionService assembles pages into print sheets
    - Path: ../../../../../../../../../../zine-layout/pkg/services/layout.go
      Note: LayoutService orchestrates template application
    - Path: ../../../../../../../../../../zine-layout/pkg/services/pages.go
      Note: PagesService creates print-ready pages
    - Path: ../../../../../../../../../../zine-layout/pkg/services/zines.go
      Note: ZinesService manages zines and ordered pages
    - Path: ../../../../../../../../../../zine-layout/pkg/zinelayout/parser/parser.go
      Note: YAML DSL parser for imposition presets
    - Path: ../../../../../../../../../../zine-layout/web/src/api.ts
      Note: RTK Query API definitions for frontend
    - Path: ../../../../../../../../../../zine-layout/web/src/routes/App.tsx
      Note: Main React routing component
ExternalSources: []
Summary: 'Comprehensive analysis of zine-layout codebase: architecture, components, entry points, APIs, files, and symbols'
LastUpdated: 2025-11-30T18:31:50.528356143-05:00
---


# Deep Codebase Analysis: Architecture, Components, and APIs

## Executive Summary

This document provides a comprehensive analysis of the zine-layout codebase, covering its architecture, components, entry points, APIs, files, and symbols. The zine-layout platform is a photo/zine layout system that takes uploaded images, computes layout placements, renders print pages (including spreads), and imposes them onto printable sheets based on YAML presets, with export to PDF. It consists of a Go backend (repositories, services, REST server, renderers), a React frontend, and CLI tools.

## Architecture Overview

### High-Level Architecture

The zine-layout platform follows a layered architecture:

1. **CLI Layer** (`cmd/zine-layout/`): Command-line interface with multiple command groups
2. **Service Layer** (`pkg/services/`): Business logic orchestration
3. **Repository Layer** (`pkg/repo/`): Data persistence abstraction
4. **Storage Layer** (`pkg/repo/sqlite/`): SQLite database implementation
5. **Rendering Layer** (`pkg/pagelayout/`, `pkg/imagelayout/`, `pkg/zinelayout/`): Image processing and layout computation
6. **HTTP Layer** (`pkg/serve/`): REST API server
7. **Frontend Layer** (`web/`): React + RTK Query UI

### Data Flow

1. **Asset Upload** → imagelayout (crop/scale math) → laid-out images metadata
2. **Page Renderer** → takes laid-out image + page template → PNG variants (thumbnail/full/combined/left/right) + metadata
3. **Imposition Engine** → arranges rendered pages into sheet images per YAML preset → export to PDF (one sheet per PDF page)

### Data Storage

- **Database**: SQLite (`zine-layout.db`) with WAL mode
- **File Storage**: 
  - `data/projects/` - Project files and images
  - `data/uploads/` - Uploaded assets
  - `data/presets/` - YAML imposition presets

## Components Breakdown

### 1. CLI Commands (`cmd/zine-layout/cmds/`)

#### 1.1 API Commands (`cmd/zine-layout/cmds/api/`)
HTTP client verbs that require a running server:
- **image_layout_templates/**: create, get, list, update, delete
- **image_sequences/**: create, get, list, update, delete, add_item, delete_item, reorder
- **laid_out_images/**: create, get, list, update, delete, preview
- **layout_sequences/**: create, get, list, update, delete, add_item, delete_item, reorder
- **projects**: create, get, list, delete
- **images**: upload, upload-dir, list

#### 1.2 Workflow Commands (`cmd/zine-layout/cmds/workflow/`)
Direct DB/service verbs (no server required):
- **assets/**: create, delete, list
- **image_layout_templates/**: create, get, list, update, delete
- **image_sequences/**: create, get, list, update, delete, add_item, delete_item, reorder
- **laid_out_images/**: create, get, list, update, delete
- **laid_out_pages/**: create, get, list, update, delete, render, update_image
- **layout_sequences/**: create, get, list, update, delete, add_item, delete_item, reorder
- **page_templates/**: create, get, list, delete
- **projects/**: create, get, list, update, delete
- **zines/**: create, get, list, delete, export, set_pages

#### 1.3 Standalone Commands
- **imagelayout** (`cmd/zine-layout/cmds/imagelayout/`): Compute image layout placements
- **pagelayout** (`cmd/zine-layout/cmds/pagelayout/`): Compute page layouts
- **render** (`cmd/zine-layout/cmds/render/`): Standalone YAML imposition renderer
- **serve** (`cmd/zine-layout/cmds/serve/`): HTTP server launcher

### 2. Service Layer (`pkg/services/`)

#### 2.1 LayoutService (`pkg/services/layout.go`)
- **Purpose**: Orchestrates template application and laid-out image persistence
- **Key Methods**:
  - `CreateLaidOutImage()`: Renders placement metadata for an asset/template pair
- **Dependencies**: `repo.Repositories`, `imagelayout/engine`

#### 2.2 PagesService (`pkg/services/pages.go`)
- **Purpose**: Creates and manages laid-out pages
- **Key Methods**:
  - `CreatePage()`: Creates a print-ready page by placing one laid-out image on a physical page
- **Dependencies**: `repo.Repositories`, `pagelayout/renderer`
- **Data Root**: Requires filesystem access for rendering

#### 2.3 ZinesService (`pkg/services/zines.go`)
- **Purpose**: Manages zines and their ordered pages
- **Key Methods**:
  - `CreateZine()`: Constructs a zine and optionally seeds it with laid-out pages
  - `GetZineWithPages()`: Retrieves zine with its pages
- **Dependencies**: `repo.Repositories`

#### 2.4 ImpositionService (`pkg/services/imposition.go`)
- **Purpose**: Assembles rendered laid-out pages into print sheets based on YAML presets
- **Key Methods**:
  - `ImposeZine()`: Loads a preset by ID and imposes the given zine's laid-out pages into output sheets
- **Dependencies**: `repo.Repositories`, `zinelayout` parser
- **Data Root**: Requires filesystem access for preset files

### 3. Repository Layer (`pkg/repo/`)

#### 3.1 Types (`pkg/repo/types.go`)
Core domain entities:
- **Project**: Top-level workspace for assets and layout artifacts
- **Asset**: Raw uploaded image stored on disk
- **ImageSequence**: Named ordering of assets within a project
- **ImageSequenceItem**: Single position inside an image sequence
- **ImageLayoutTemplate**: Reusable layout settings for assets
- **LaidOutImage**: Rendered placement metadata for an asset and template pair
- **LayoutSequence**: Orders laid-out images for downstream page/zine composition
- **LayoutSequenceItem**: Single position within a layout sequence
- **PageTemplate**: Defines how a laid-out image should be placed on a physical print page
- **LaidOutPage**: Print-ready page (one laid-out image placed on a physical page)
- **Zine**: Ordered collection of laid-out pages
- **ZinePage**: Connects a laid-out page to its position within a zine

#### 3.2 Repository Interfaces
All repositories follow a consistent pattern:
- `Create()`, `Update()`, `Get()`, `List()`, `Delete()`
- Sequence repositories also have: `AddItem()`, `ListItems()`, `ReplaceItems()`, `DeleteItem()`
- Specialized methods: `ListByProject()`, `ListGlobal()`, `SetPages()`, `GetPages()`

#### 3.3 SQLite Implementation (`pkg/repo/sqlite/`)
- **migrations.go**: Database schema definition
- **sqlite.go**: Repository factory and utilities
- **Individual repos**: One file per entity (projects.go, assets.go, image_sequences.go, etc.)

### 4. Rendering Components

#### 4.1 Image Layout Engine (`pkg/imagelayout/`)
- **types.go**: ViewportSettings, ViewportResult, Trace, ImageMeta
- **defaults.go**: DefaultSettings() for missing fields
- **engine/engine.go**: Core placement logic
  - `InputsFromSettings()`: Normalizes units, DPI, orientation, crop/fit constraints
  - `ComputeViewport()`: Returns source/target rectangles and diagnostic trace
- **Modes**: page, crop, fit
- **Features**: Contain/cover, crop ratios, fit modes, focus positioning

#### 4.2 Page Layout Renderer (`pkg/pagelayout/`)
- **settings.go**: Page-level sizing helpers (content rectangles, margins, spreads, border metadata)
- **renderer/renderer.go**: Renders laid-out pages
  - Accepts optional `imagelayout.ViewportResult` to crop images before scaling
  - Produces multiple variants: full, thumbnail, combined, left, right spreads
  - Supports borders (plain, dotted, dashed, corner)
  - Positioning modes: absolute, fill, snap

#### 4.3 Zine Layout Parser (`pkg/zinelayout/`)
- **parser/parser.go**: YAML DSL parser for imposition presets
- **parser/units.go**: Unit expression parser (px, in, mm, cm, pt)
- **layout.go**: Layout computation
- **image.go**: Image placement logic
- **margin.go**: Margin handling
- **border.go**: Border rendering
- **color.go**: Color parsing
- **rotation.go**: Rotation support (0°, 180°)

### 5. HTTP Server (`pkg/serve/`)

#### 5.1 Server Structure (`pkg/serve/server.go`)
- **Settings**: Root (frontend dir), DataRoot, Addr
- **Initialization**: Sets up database, repositories, services, directories
- **Routes**: Configures all HTTP endpoints

#### 5.2 API Endpoints

**Health Check**:
- `GET /api/health` - Server health status

**Projects**:
- `GET /api/projects` - List all projects
- `POST /api/projects` - Create project
- `GET /api/projects/{id}` - Get project
- `PATCH /api/projects/{id}` - Update project
- `DELETE /api/projects/{id}` - Delete project
- `GET /api/projects/{id}/assets` - List project assets
- `POST /api/projects/{id}/images` - Upload images
- `GET /api/projects/{id}/image-sequences` - List image sequences
- `GET /api/projects/{id}/image-layout-templates` - List layout templates
- `GET /api/projects/{id}/laid-out-images` - List laid-out images
- `GET /api/projects/{id}/layout-sequences` - List layout sequences
- `GET /api/projects/{id}/page-templates` - List page templates
- `GET /api/projects/{id}/laid-out-pages` - List laid-out pages
- `GET /api/projects/{id}/zines` - List zines
- `POST /api/projects/{id}/zines` - Create zine

**Assets**:
- `GET /api/assets/{id}` - Get asset
- `DELETE /api/assets/{id}` - Delete asset

**Image Sequences**:
- `GET /api/image-sequences/{id}` - Get sequence with items
- `PATCH /api/image-sequences/{id}` - Update sequence
- `DELETE /api/image-sequences/{id}` - Delete sequence
- `POST /api/image-sequences/{id}/items` - Add item
- `DELETE /api/image-sequences/{id}/items/{position}` - Delete item
- `POST /api/image-sequences/{id}/reorder` - Reorder items

**Image Layout Templates**:
- `GET /api/image-layout-templates` - List templates
- `POST /api/image-layout-templates` - Create template
- `GET /api/image-layout-templates/{id}` - Get template
- `PATCH /api/image-layout-templates/{id}` - Update template
- `DELETE /api/image-layout-templates/{id}` - Delete template

**Laid-Out Images**:
- `GET /api/laid-out-images/{id}` - Get laid-out image
- `PATCH /api/laid-out-images/{id}` - Update laid-out image
- `DELETE /api/laid-out-images/{id}` - Delete laid-out image
- `GET /api/laid-out-images/{id}/preview` - Get preview

**Layout Sequences**:
- `GET /api/layout-sequences/{id}` - Get sequence with items
- `PATCH /api/layout-sequences/{id}` - Update sequence
- `DELETE /api/layout-sequences/{id}` - Delete sequence
- `POST /api/layout-sequences/{id}/items` - Add item
- `DELETE /api/layout-sequences/{id}/items/{position}` - Delete item
- `POST /api/layout-sequences/{id}/reorder` - Reorder items

**Page Templates**:
- `GET /api/page-templates` - List templates
- `POST /api/page-templates` - Create template
- `GET /api/page-templates/{id}` - Get template
- `PATCH /api/page-templates/{id}` - Update template
- `DELETE /api/page-templates/{id}` - Delete template

**Laid-Out Pages**:
- `GET /api/laid-out-pages/{id}` - Get laid-out page
- `PATCH /api/laid-out-pages/{id}` - Update laid-out page
- `DELETE /api/laid-out-pages/{id}` - Delete laid-out page
- `POST /api/laid-out-pages/{id}/render` - Render page

**Zines**:
- `GET /api/zines/{id}` - Get zine with pages
- `PATCH /api/zines/{id}` - Update zine
- `DELETE /api/zines/{id}` - Delete zine
- `GET /api/zines/{id}/pages` - Get zine pages
- `POST /api/zines/{id}/pages` - Set zine pages
- `GET /api/zines/{id}/export` - Export zine to PDF

**File Serving**:
- `GET /projects/{id}/...` - Serve project files (images)

### 6. Frontend (`web/`)

#### 6.1 Technology Stack
- **Framework**: React + TypeScript
- **State Management**: Redux Toolkit + RTK Query
- **Styling**: Tailwind CSS
- **Build Tool**: Vite
- **Package Manager**: pnpm

#### 6.2 Structure
- **src/api.ts**: RTK Query API definitions (1060+ lines)
- **src/store.ts**: Redux store configuration
- **src/routes/App.tsx**: Main routing component
- **src/views/**: Page components
  - `Projects.tsx` - Project list
  - `ProjectDetail.tsx` - Project detail with tabs
  - `LaidOutImageViewer.tsx` - Image viewer
  - `LayoutSequenceEditor.tsx` - Sequence editor
  - `LayoutTemplateManager.tsx` - Template manager
  - `Health.tsx` - Health check
  - `Home.tsx` - Home page
  - **tabs/**: Tab components for project detail
    - `AssetsTab.tsx`
    - `ImageLayoutsTab.tsx`
    - `PageLayoutsTab.tsx`
    - `SequencesTab.tsx`
    - `ZineTab.tsx`
- **src/components/**: Reusable components
  - `AnchorGrid.tsx`
  - `ImgCell.tsx`
  - `ProjectAssetsPanel.tsx`
  - `SliderInput.tsx`
  - **ui/**: UI primitives (Button, Card, Input, Tabs)

### 7. Export & PDF (`pkg/export/`)
- **pdf.go**: `SheetsToPDF()` - Converts sheet images to PDF (one sheet per PDF page)

### 8. Validation & Scripts (`scripts/`)
- **imagelayout_validation/main.go**: Generates synthetic test images, validates geometry, emits HTML report
- **pagelayout_validation/main.go**: Builds representative zine layout specs, validates dimensions, assembles HTML dashboard
- **run_cli_playbook.py**: Python script for CLI testing

## Entry Points

### Main Entry Point
- **cmd/zine-layout/main.go**: 
  - Initializes Cobra root command
  - Registers sub-commands: render, serve, api, imagelayout, pagelayout, workflow
  - Sets up help system with embedded docs
  - Configures logging via Viper

### CLI Entry Points
1. **render**: Standalone YAML imposition renderer
2. **serve**: HTTP server launcher
3. **api**: HTTP client commands (requires server)
4. **workflow**: Direct DB/service commands (no server)
5. **imagelayout**: Image layout computation
6. **pagelayout**: Page layout computation

### HTTP Entry Point
- **pkg/serve/server.go**: `ListenAndServe()` - Boots HTTP server

### Frontend Entry Point
- **web/src/main.tsx**: React app initialization with Redux Provider

## Key APIs

### Service APIs

**LayoutService**:
- `CreateLaidOutImage(projectID, assetID, templateID string, overrides *imagelayout.ViewportSettings) (*repo.LaidOutImage, error)`

**PagesService**:
- `CreatePage(projectID, pageTemplateID, laidOutImageID string) (*repo.LaidOutPage, error)`
- `RenderPage(pageID string) error`
- `UpdatePageImage(pageID, laidOutImageID string) error`

**ZinesService**:
- `CreateZine(projectID, name, description string, laidOutPageIDs []string) (*repo.Zine, []*repo.ZinePage, error)`
- `GetZineWithPages(zineID string) (*repo.Zine, []*repo.ZinePage, error)`
- `SetPages(zineID string, laidOutPageIDs []string) error`

**ImpositionService**:
- `ImposeZine(zineID string, presetID string) ([]SheetResult, error)`

### Repository APIs

All repositories implement standard CRUD operations plus specialized methods:
- `Create()`, `Update()`, `Get()`, `List()`, `Delete()`
- `ListByProject(projectID string)`
- Sequence repos: `AddItem()`, `ListItems()`, `ReplaceItems()`, `DeleteItem()`
- Zine repo: `SetPages()`, `GetPages()`

### Rendering APIs

**Image Layout Engine** (`pkg/imagelayout/engine/`):
- `InputsFromSettings(settings imagelayout.ViewportSettings, meta imagelayout.ImageMeta) (Inputs, error)`
- `ComputeViewport(inputs Inputs) (imagelayout.ViewportResult, *imagelayout.Trace, error)`

**Page Renderer** (`pkg/pagelayout/renderer/`):
- `RenderPage(ctx RenderContext) (*PageRenderResult, error)`

**Zine Layout Parser** (`pkg/zinelayout/parser/`):
- `Parse(input string) (Value, error)` - Unit expression parser

## Files and Symbols

### Core Package Files

**cmd/zine-layout/**:
- `main.go` - Root command initialization
- `cmds/api/` - 45 files (44 Go, 1 markdown)
- `cmds/workflow/` - 63 Go files
- `cmds/imagelayout/` - 2 Go files
- `cmds/pagelayout/` - 6 files (3 Go, 2 YAML, 1 JSON)
- `cmds/render/` - 1 Go file
- `cmds/serve/` - 1 Go file

**pkg/repo/**:
- `types.go` - Domain entities and repository interfaces
- `sqlite/migrations.go` - Database schema
- `sqlite/sqlite.go` - Repository factory
- `sqlite/*.go` - Individual repository implementations (11 files)

**pkg/services/**:
- `layout.go` - LayoutService
- `pages.go` - PagesService
- `zines.go` - ZinesService
- `imposition.go` - ImpositionService

**pkg/serve/**:
- `server.go` - HTTP server setup
- `*_routes.go` - Route handlers (11 files)
- `types.go` - HTTP response types

**pkg/imagelayout/**:
- `types.go` - ViewportSettings, ViewportResult, Trace
- `defaults.go` - Default settings
- `engine/engine.go` - Core algorithm
- `engine/engine_test.go` - Tests

**pkg/pagelayout/**:
- `settings.go` - Page layout settings
- `renderer/renderer.go` - Page renderer
- `renderer/renderer_test.go` - Tests

**pkg/zinelayout/**:
- `parser/parser.go` - YAML parser
- `parser/units.go` - Unit parser
- `layout.go` - Layout computation
- `image.go` - Image placement
- `margin.go` - Margin handling
- `border.go` - Border rendering
- `color.go` - Color parsing
- `rotation.go` - Rotation support

**pkg/export/**:
- `pdf.go` - PDF export

**web/src/**:
- `api.ts` - RTK Query API (1060+ lines)
- `store.ts` - Redux store
- `main.tsx` - Entry point
- `routes/App.tsx` - Routing
- `views/` - 12 TSX files
- `components/` - 9 files (8 TSX, 1 TS)

### Key Exported Symbols

**Repository Types**:
- `Project`, `Asset`, `ImageSequence`, `ImageSequenceItem`
- `ImageLayoutTemplate`, `LaidOutImage`
- `LayoutSequence`, `LayoutSequenceItem`
- `PageTemplate`, `LaidOutPage`
- `Zine`, `ZinePage`

**Repository Interfaces**:
- `ProjectRepository`, `AssetRepository`, `ImageSequenceRepository`
- `ImageLayoutTemplateRepository`, `LaidOutImageRepository`
- `LayoutSequenceRepository`, `PageTemplateRepository`
- `LaidOutPageRepository`, `ZineRepository`
- `Repositories` (aggregate struct)

**Service Types**:
- `LayoutService`, `PagesService`, `ZinesService`, `ImpositionService`
- `LayoutComputation`, `SheetResult`

**Rendering Types**:
- `imagelayout.ViewportSettings`, `imagelayout.ViewportResult`, `imagelayout.Trace`
- `pagelayout.PageLayoutSettings`, `pagelayout.RenderContext`, `pagelayout.PageRenderResult`
- `zinelayout.Layout`, `zinelayout.Image`, `zinelayout.Margin`, `zinelayout.Border`

## Documentation

### Embedded Documentation (`pkg/doc/`)
- `topics/dsl.md` - DSL overview
- `topics/render-command.md` - Render command guide

### Living Documentation (`ttmp/`)
- **2025-10-11/**: Recent project handover and onboarding docs
  - `23-project-handover-and-onboarding.md` - Main onboarding guide
  - `00-START-HERE.md` - Starting point
  - Various implementation guides and changelogs
- **2025-10-10/**: Architecture and design docs
  - `index.md` - Codebase index & orientation
  - `09-system-specification-after-phase1-and-phase2.md` - System spec
- **2025-09-24/**: Store design and architecture
- **2025-09-23/**: Algorithm analysis
- **2025-09-14/**: Web UI design

### Key Documentation Files
- `README.md` - User-facing documentation
- `index.md` - Developer navigation index
- `AGENT.md` - Agent guidelines for Go projects

## Dependencies

### Core Dependencies (go.mod)
- **glazed**: CLI framework (`github.com/go-go-golems/glazed`)
- **cobra**: CLI commands (`github.com/spf13/cobra`)
- **viper**: Configuration (`github.com/spf13/viper`)
- **zerolog**: Logging (`github.com/rs/zerolog`)
- **sqlite**: Database (`modernc.org/sqlite`)
- **image**: Image processing (`golang.org/x/image`)
- **gofpdf**: PDF generation (`github.com/phpdave11/gofpdf`)
- **yaml**: YAML parsing (`gopkg.in/yaml.v3`)
- **emrichen**: YAML templating (`github.com/go-go-golems/go-emrichen`)
- **sprig**: Template functions (`github.com/Masterminds/sprig`)

### Frontend Dependencies (web/package.json)
- **react**: UI framework
- **redux-toolkit**: State management
- **rtk-query**: API client
- **vite**: Build tool
- **tailwindcss**: Styling
- **typescript**: Type safety

## Data Model Relationships

```
Project
├── Assets (1:N)
├── ImageSequences (1:N)
│   └── ImageSequenceItems (1:N) → Assets
├── ImageLayoutTemplates (1:N)
├── LaidOutImages (1:N)
│   ├── Asset (N:1)
│   └── ImageLayoutTemplate (N:1)
├── LayoutSequences (1:N)
│   └── LayoutSequenceItems (1:N) → LaidOutImages
├── PageTemplates (1:N)
├── LaidOutPages (1:N)
│   ├── PageTemplate (N:1)
│   └── LaidOutImage (N:1)
└── Zines (1:N)
    └── ZinePages (1:N) → LaidOutPages
```

## Design Decisions

### Architecture Decisions
1. **Layered Architecture**: Clear separation between CLI, services, repositories, and storage
2. **Repository Pattern**: Abstract interfaces allow for different storage backends
3. **Service Layer**: Business logic separated from data access
4. **SQLite**: Chosen for simplicity and portability
5. **REST API**: Standard HTTP endpoints for frontend integration
6. **RTK Query**: Chosen for efficient API caching and state management

### Technical Decisions
1. **WAL Mode**: SQLite uses Write-Ahead Logging for better concurrency
2. **JSON Storage**: Complex settings stored as JSON in database
3. **File-based Assets**: Images stored on filesystem, metadata in database
4. **YAML Presets**: Imposition presets stored as YAML files
5. **Multiple Render Variants**: Pages rendered in multiple sizes/formats for different use cases
6. **Unit Expression Parser**: Custom parser for flexible unit handling

## Open Questions

1. **Scalability**: How well does SQLite scale with large numbers of projects/assets?
2. **Concurrency**: Are there any race conditions in the current implementation?
3. **Error Handling**: Is error handling consistent across all layers?
4. **Testing**: What is the test coverage for critical paths?
5. **Performance**: Are there any performance bottlenecks in rendering or database queries?
6. **Migration Strategy**: How are database migrations handled in production?

## References

- [Project Handover Doc](ttmp/2025-10-11/23-project-handover-and-onboarding.md)
- [Codebase Index](ttmp/2025-10-10/index.md)
- [System Specification](ttmp/2025-10-10/09-system-specification-after-phase1-and-phase2.md)
- [Developer Navigation](index.md)
- [README](README.md)
