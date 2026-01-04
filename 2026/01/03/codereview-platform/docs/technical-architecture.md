# Technical Architecture Documentation

**Code Review Knowledge Platform**

*A comprehensive guide to the codebase structure and implementation*

---

## Table of Contents

1. [Architecture Overview](#architecture-overview)
2. [Technology Stack](#technology-stack)
3. [Project Structure](#project-structure)
4. [Backend Architecture](#backend-architecture)
5. [Frontend Architecture](#frontend-architecture)
6. [Data Layer](#data-layer)
7. [Git Integration with isomorphic-git](#git-integration-with-isomorphic-git)
8. [API Design with tRPC](#api-design-with-trpc)
9. [State Management](#state-management)
10. [Authentication Flow](#authentication-flow)
11. [Demo Repository Initialization](#demo-repository-initialization)
12. [CLI Tools Architecture](#cli-tools-architecture)
13. [Deployment Considerations](#deployment-considerations)
14. [Extending the Platform](#extending-the-platform)

---

## Architecture Overview

The Code Review Knowledge Platform is a full-stack web application that combines code browsing, educational content, and interactive assessments. The architecture follows a modern React + Node.js pattern with several distinctive features that set it apart from typical web applications.

The most notable architectural decision is the use of git notes for content storage rather than a traditional database. This approach keeps educational content version-controlled alongside the code it describes, ensuring synchronization as codebases evolve. The platform reads and writes git notes using isomorphic-git, a pure JavaScript git implementation that eliminates the dependency on the git CLI.

The application is structured as a monorepo containing both the Express backend and React frontend. Communication between frontend and backend uses tRPC, which provides end-to-end type safety without the overhead of maintaining separate API schemas. The frontend uses React 19 with Tailwind CSS 4 and shadcn/ui components for a polished, accessible user interface.

### High-Level Architecture Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                        React Frontend                           │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐        │
│  │   File   │  │  Review  │  │   Quiz   │  │  Guide   │        │
│  │ Browser  │  │  Detail  │  │   Take   │  │Walkthrough│       │
│  └────┬─────┘  └────┬─────┘  └────┬─────┘  └────┬─────┘        │
│       │             │             │             │               │
│       └─────────────┴──────┬──────┴─────────────┘               │
│                            │                                    │
│                     ┌──────┴──────┐                             │
│                     │ tRPC Client │                             │
│                     └──────┬──────┘                             │
└────────────────────────────┼────────────────────────────────────┘
                             │ HTTP/WebSocket
┌────────────────────────────┼────────────────────────────────────┐
│                     ┌──────┴──────┐                             │
│                     │ tRPC Server │                             │
│                     └──────┬──────┘                             │
│                            │                                    │
│  ┌─────────────────────────┼─────────────────────────────┐     │
│  │                    Routers                             │     │
│  │  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌────────┐ │     │
│  │  │repository│  │ reviews  │  │ quizzes  │  │ guides │ │     │
│  │  └────┬─────┘  └────┬─────┘  └────┬─────┘  └───┬────┘ │     │
│  └───────┼─────────────┼─────────────┼───────────┼───────┘     │
│          │             │             │           │              │
│          └─────────────┴──────┬──────┴───────────┘              │
│                               │                                 │
│                     ┌─────────┴─────────┐                       │
│                     │   Storage Layer   │                       │
│                     │  (git-notes.ts)   │                       │
│                     └─────────┬─────────┘                       │
│                               │                                 │
│                     ┌─────────┴─────────┐                       │
│                     │  isomorphic-git   │                       │
│                     └─────────┬─────────┘                       │
│                        Express Backend                          │
└────────────────────────────────┼────────────────────────────────┘
                                 │
                     ┌───────────┴───────────┐
                     │    Git Repository     │
                     │  ┌─────────────────┐  │
                     │  │ refs/notes/     │  │
                     │  │  ├─ reviews     │  │
                     │  │  ├─ quizzes     │  │
                     │  │  ├─ guides      │  │
                     │  │  └─ submissions │  │
                     │  └─────────────────┘  │
                     └───────────────────────┘
```

---

## Technology Stack

The platform uses a carefully selected set of technologies that work well together and provide excellent developer experience.

### Backend Technologies

| Technology | Version | Purpose |
|------------|---------|---------|
| Node.js | 22.x | JavaScript runtime |
| Express | 4.x | HTTP server framework |
| tRPC | 11.x | Type-safe API layer |
| isomorphic-git | 1.x | Pure JavaScript git implementation |
| yaml | 2.x | YAML parsing and serialization |
| Zod | 4.x | Schema validation |
| Drizzle ORM | 0.44.x | Database ORM (for user data) |

### Frontend Technologies

| Technology | Version | Purpose |
|------------|---------|---------|
| React | 19.x | UI framework |
| Tailwind CSS | 4.x | Utility-first CSS |
| shadcn/ui | Latest | Component library |
| wouter | 3.x | Lightweight routing |
| TanStack Query | 5.x | Server state management |
| Lucide React | 0.453.x | Icon library |

### Development Tools

| Tool | Purpose |
|------|---------|
| TypeScript | Static typing |
| Vite | Frontend build tool |
| esbuild | Backend bundling |
| Vitest | Testing framework |
| tsx | TypeScript execution |

---

## Project Structure

The project follows a monorepo structure with clear separation between client and server code.

```
code-review-platform/
├── client/                    # React frontend
│   ├── public/               # Static assets
│   ├── src/
│   │   ├── _core/           # Core utilities and hooks
│   │   ├── components/      # Reusable UI components
│   │   │   └── ui/         # shadcn/ui components
│   │   ├── contexts/        # React contexts
│   │   ├── hooks/           # Custom hooks
│   │   ├── lib/             # Utility libraries
│   │   │   └── trpc.ts     # tRPC client setup
│   │   ├── pages/           # Page components
│   │   │   ├── Home.tsx
│   │   │   ├── FileBrowser.tsx
│   │   │   ├── FileView.tsx
│   │   │   ├── Reviews.tsx
│   │   │   ├── ReviewDetail.tsx
│   │   │   ├── Quizzes.tsx
│   │   │   ├── QuizTake.tsx
│   │   │   ├── Guides.tsx
│   │   │   └── GuideWalkthrough.tsx
│   │   ├── App.tsx          # Root component with routing
│   │   ├── main.tsx         # Entry point
│   │   └── index.css        # Global styles
│   └── index.html           # HTML template
├── server/                    # Express backend
│   ├── _core/               # Framework internals
│   │   ├── context.ts      # tRPC context
│   │   ├── trpc.ts         # tRPC setup
│   │   ├── env.ts          # Environment variables
│   │   └── index.ts        # Server entry point
│   ├── lib/                 # Business logic
│   │   ├── git-notes.ts    # Git notes storage layer
│   │   ├── yaml-parser.ts  # YAML DSL parser
│   │   ├── demo-data.ts    # Demo repository content
│   │   └── demo-init.ts    # Demo initialization
│   ├── routers/             # tRPC routers
│   │   ├── repository.ts   # File browsing API
│   │   ├── reviews.ts      # Code reviews API
│   │   ├── quizzes.ts      # Quizzes API
│   │   └── guides.ts       # Guided tours API
│   ├── db.ts                # Database helpers
│   └── routers.ts           # Router aggregation
├── drizzle/                   # Database schema
│   └── schema.ts
├── shared/                    # Shared types and constants
│   └── const.ts
├── cli/                       # CLI testing tools
│   ├── git-notes.ts
│   ├── yaml-parser.ts
│   └── api-test.ts
├── docs/                      # Documentation
│   ├── yaml-dsl-reference.md
│   ├── quiz-system-walkthrough.md
│   └── technical-architecture.md
├── package.json
├── tsconfig.json
├── vite.config.ts
└── todo.md
```

### Key Directories Explained

The **client** directory contains the React frontend application. The `pages` subdirectory holds route-level components, while `components` contains reusable UI elements. The `_core` directory provides authentication hooks and other platform-specific utilities.

The **server** directory contains the Express backend. The `_core` subdirectory is framework code that should rarely be modified. The `lib` directory contains business logic including the git notes storage layer and YAML parser. The `routers` directory defines tRPC procedures organized by feature.

The **cli** directory contains standalone TypeScript scripts for testing and debugging. These tools can be run directly with `npx tsx` and provide command-line access to the platform's functionality.

---

## Backend Architecture

The backend is built on Express with tRPC providing the API layer. This section explains how requests flow through the system.

### Request Lifecycle

When a request arrives at the server, it passes through several layers:

1. **Express Middleware**: Handles CORS, cookie parsing, and static file serving
2. **tRPC Adapter**: Converts HTTP requests to tRPC procedure calls
3. **Context Creation**: Builds the request context including user authentication
4. **Procedure Execution**: Runs the appropriate router procedure
5. **Response Serialization**: Converts the result to JSON using superjson

### Server Entry Point

The server entry point (`server/_core/index.ts`) sets up Express and mounts the tRPC adapter:

```typescript
import express from 'express';
import { createExpressMiddleware } from '@trpc/server/adapters/express';
import { appRouter } from '../routers';
import { createContext } from './context';

const app = express();

// Middleware
app.use(express.json());
app.use(cookieParser());

// tRPC endpoint
app.use('/api/trpc', createExpressMiddleware({
  router: appRouter,
  createContext
}));

// Static file serving for frontend
app.use(express.static('dist/client'));

app.listen(3000);
```

### Context Creation

The context provides request-scoped data to all procedures:

```typescript
export async function createContext({ req, res }: CreateContextOptions) {
  // Extract user from session cookie
  const user = await getUserFromSession(req);
  
  return {
    req,
    res,
    user
  };
}

export type TrpcContext = Awaited<ReturnType<typeof createContext>>;
```

### Router Organization

Routers are organized by feature and aggregated in `server/routers.ts`:

```typescript
import { router } from './_core/trpc';
import { repositoryRouter } from './routers/repository';
import { reviewsRouter } from './routers/reviews';
import { quizzesRouter } from './routers/quizzes';
import { guidesRouter } from './routers/guides';

export const appRouter = router({
  repository: repositoryRouter,
  reviews: reviewsRouter,
  quizzes: quizzesRouter,
  guides: guidesRouter,
  // ... other routers
});

export type AppRouter = typeof appRouter;
```

---

## Frontend Architecture

The frontend uses React 19 with a component-based architecture. This section covers the key patterns and conventions.

### Component Hierarchy

The application follows a hierarchical component structure:

```
App
├── ThemeProvider
├── TooltipProvider
├── Toaster
└── Router (wouter)
    ├── Home
    ├── FileBrowser
    │   ├── BranchSelector
    │   ├── FileTree
    │   └── Breadcrumbs
    ├── FileView
    │   ├── CodeViewer
    │   ├── AnnotationList
    │   └── QuizList
    ├── Reviews
    │   └── ReviewCard
    ├── ReviewDetail
    │   ├── DiffViewer
    │   └── AnnotationCard
    ├── Quizzes
    │   └── QuizCard
    ├── QuizTake
    │   └── QuestionCard
    ├── Guides
    │   └── GuideCard
    └── GuideWalkthrough
        ├── StopContent
        └── StopNavigation
```

### Routing with wouter

The platform uses wouter for lightweight client-side routing:

```typescript
import { Route, Switch } from 'wouter';

function Router() {
  return (
    <Switch>
      <Route path="/" component={Home} />
      <Route path="/browse/:path*" component={FileBrowser} />
      <Route path="/file/:path*" component={FileView} />
      <Route path="/reviews" component={Reviews} />
      <Route path="/review/:commit" component={ReviewDetail} />
      <Route path="/quizzes" component={Quizzes} />
      <Route path="/quiz/:commit" component={QuizTake} />
      <Route path="/guides" component={Guides} />
      <Route path="/guide/:commit" component={GuideWalkthrough} />
      <Route component={NotFound} />
    </Switch>
  );
}
```

### Data Fetching with tRPC

Components fetch data using tRPC hooks that provide type-safe queries:

```typescript
function Reviews() {
  const { data: demoInfo } = trpc.repository.getDemoInfo.useQuery();
  const repoPath = demoInfo?.path || '';
  
  const { data: reviews, isLoading } = trpc.reviews.list.useQuery(
    { repoPath },
    { enabled: !!repoPath }
  );
  
  if (isLoading) return <LoadingSkeleton />;
  
  return (
    <div className="container py-8">
      {reviews?.map(review => (
        <ReviewCard key={review.id} review={review} />
      ))}
    </div>
  );
}
```

### UI Components with shadcn/ui

The platform uses shadcn/ui components for consistent styling:

```typescript
import { Button } from '@/components/ui/button';
import { Card, CardHeader, CardContent } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';

function ReviewCard({ review }: { review: Review }) {
  return (
    <Card>
      <CardHeader>
        <div className="flex items-center justify-between">
          <h3 className="text-lg font-semibold">{review.title}</h3>
          <Badge variant="outline">PR #{review.pr_number}</Badge>
        </div>
      </CardHeader>
      <CardContent>
        <p className="text-muted-foreground">{review.description}</p>
        <Button asChild className="mt-4">
          <Link to={`/review/${review.commit}?reviewId=${review.id}`}>
            View Review
          </Link>
        </Button>
      </CardContent>
    </Card>
  );
}
```

---

## Data Layer

The platform uses two distinct data storage mechanisms: git notes for educational content and a MySQL database for user data.

### Git Notes Storage

Educational content (reviews, quizzes, guides) is stored in git notes, which are arbitrary data attached to commits. The platform uses three note namespaces:

| Namespace | Content |
|-----------|---------|
| `refs/notes/reviews` | Code review definitions |
| `refs/notes/quizzes` | Quiz definitions |
| `refs/notes/guides` | Guided tour definitions |
| `refs/notes/submissions` | User quiz submissions |

### Database Storage

User-related data is stored in a MySQL database using Drizzle ORM:

```typescript
// drizzle/schema.ts
export const users = mysqlTable('users', {
  id: int('id').autoincrement().primaryKey(),
  openId: varchar('openId', { length: 64 }).notNull().unique(),
  name: text('name'),
  email: varchar('email', { length: 320 }),
  role: mysqlEnum('role', ['user', 'admin']).default('user'),
  createdAt: timestamp('createdAt').defaultNow(),
  updatedAt: timestamp('updatedAt').defaultNow().onUpdateNow(),
  lastSignedIn: timestamp('lastSignedIn').defaultNow()
});
```

### Data Flow Patterns

The platform follows different patterns for different data types:

**Read-heavy content** (reviews, quizzes, guides) is read from git notes on each request. This ensures content is always fresh and synchronized with the repository.

**User data** is read from the database with caching at the tRPC query level. TanStack Query handles cache invalidation and background refetching.

**Submissions** are written to git notes to maintain a complete audit trail. Each submission creates a new note entry, preserving history.

---

## Git Integration with isomorphic-git

The platform uses isomorphic-git for all git operations. This section explains the key integration patterns.

### Why isomorphic-git?

The platform initially used simple-git, which wraps the git CLI. This approach had a critical flaw: it required git to be installed on the server. When deployed to environments without git (like some serverless platforms), the application failed.

Isomorphic-git solves this by implementing git operations entirely in JavaScript. It can read and write git repositories using only the Node.js filesystem API, making the platform portable to any Node.js environment.

### Core Git Operations

The `server/lib/git-notes.ts` module provides all git operations:

```typescript
import * as git from 'isomorphic-git';
import * as fs from 'fs';

// List branches
export async function getBranches(repoPath: string): Promise<string[]> {
  const branches = await git.listBranches({ fs, dir: repoPath });
  return branches;
}

// Get current branch
export async function getCurrentBranch(repoPath: string): Promise<string> {
  const branch = await git.currentBranch({ fs, dir: repoPath });
  return branch || 'main';
}

// Get file tree
export async function getFileTree(
  repoPath: string, 
  ref: string, 
  path: string
): Promise<TreeEntry[]> {
  const commit = await git.resolveRef({ fs, dir: repoPath, ref });
  const { tree } = await git.readTree({ 
    fs, 
    dir: repoPath, 
    oid: commit,
    filepath: path || undefined
  });
  
  return tree.map(entry => ({
    name: entry.path,
    type: entry.type === 'tree' ? 'directory' : 'file',
    oid: entry.oid
  }));
}

// Read file content
export async function getFileContent(
  repoPath: string,
  ref: string,
  filePath: string
): Promise<string> {
  const commit = await git.resolveRef({ fs, dir: repoPath, ref });
  const { blob } = await git.readBlob({
    fs,
    dir: repoPath,
    oid: commit,
    filepath: filePath
  });
  
  return new TextDecoder().decode(blob);
}
```

### Git Notes Operations

Reading and writing git notes requires working with git's object model:

```typescript
// Read a note
async function readNote(
  repoPath: string,
  notesRef: string,
  targetCommit: string
): Promise<string | null> {
  try {
    const notesCommit = await git.resolveRef({ 
      fs, 
      dir: repoPath, 
      ref: notesRef 
    });
    
    const { tree } = await git.readTree({ 
      fs, 
      dir: repoPath, 
      oid: notesCommit 
    });
    
    const noteEntry = tree.find(e => e.path === targetCommit);
    if (!noteEntry) return null;
    
    const { blob } = await git.readBlob({ 
      fs, 
      dir: repoPath, 
      oid: noteEntry.oid 
    });
    
    return new TextDecoder().decode(blob);
  } catch {
    return null;
  }
}

// Write a note
async function writeNote(
  repoPath: string,
  notesRef: string,
  targetCommit: string,
  content: string
): Promise<void> {
  // Create blob
  const blobOid = await git.writeBlob({
    fs,
    dir: repoPath,
    blob: new TextEncoder().encode(content)
  });
  
  // Get or create tree
  let tree: git.TreeEntry[] = [];
  try {
    const notesCommit = await git.resolveRef({ fs, dir: repoPath, ref: notesRef });
    const result = await git.readTree({ fs, dir: repoPath, oid: notesCommit });
    tree = result.tree;
  } catch {
    // Notes ref doesn't exist yet
  }
  
  // Update tree
  tree = tree.filter(e => e.path !== targetCommit);
  tree.push({
    mode: '100644',
    path: targetCommit,
    oid: blobOid,
    type: 'blob'
  });
  
  // Write tree
  const treeOid = await git.writeTree({ fs, dir: repoPath, tree });
  
  // Create commit
  const commitOid = await git.writeCommit({
    fs,
    dir: repoPath,
    commit: {
      tree: treeOid,
      parent: [],
      author: { name: 'System', email: 'system@local', timestamp: Date.now() / 1000, timezoneOffset: 0 },
      committer: { name: 'System', email: 'system@local', timestamp: Date.now() / 1000, timezoneOffset: 0 },
      message: `Update note for ${targetCommit}`
    }
  });
  
  // Update ref
  await git.writeRef({
    fs,
    dir: repoPath,
    ref: notesRef,
    value: commitOid,
    force: true
  });
}
```

---

## API Design with tRPC

The platform uses tRPC for type-safe API communication. This section explains the API design patterns.

### Procedure Types

tRPC supports two procedure types:

**Queries** are for reading data. They are idempotent and can be cached:

```typescript
list: publicProcedure
  .input(z.object({ repoPath: z.string() }))
  .query(async ({ input }) => {
    return await gitNotes.getAllReviews(input.repoPath);
  })
```

**Mutations** are for writing data. They modify state and are not cached:

```typescript
submit: protectedProcedure
  .input(z.object({
    repoPath: z.string(),
    quizId: z.string(),
    answers: z.record(z.string(), z.string())
  }))
  .mutation(async ({ input, ctx }) => {
    // Grade and store submission
    return await gradeAndStoreSubmission(input, ctx.user);
  })
```

### Input Validation with Zod

All procedure inputs are validated using Zod schemas:

```typescript
const fileTreeInput = z.object({
  repoPath: z.string().min(1),
  branch: z.string().optional(),
  path: z.string().optional()
});

fileTree: publicProcedure
  .input(fileTreeInput)
  .query(async ({ input }) => {
    const { repoPath, branch, path } = input;
    // ...
  })
```

### Protected Procedures

Some procedures require authentication:

```typescript
import { protectedProcedure } from '../_core/trpc';

submit: protectedProcedure
  .input(submitInput)
  .mutation(async ({ input, ctx }) => {
    // ctx.user is guaranteed to exist
    const userId = ctx.user.id;
    // ...
  })
```

### Error Handling

tRPC provides typed error handling:

```typescript
import { TRPCError } from '@trpc/server';

get: publicProcedure
  .input(getInput)
  .query(async ({ input }) => {
    const review = await gitNotes.getReview(input.repoPath, input.commit);
    
    if (!review) {
      throw new TRPCError({
        code: 'NOT_FOUND',
        message: 'Review not found'
      });
    }
    
    return review;
  })
```

---

## State Management

The platform uses TanStack Query (via tRPC) for server state management and React's built-in state for UI state.

### Server State with TanStack Query

tRPC integrates with TanStack Query to provide caching, background refetching, and optimistic updates:

```typescript
function QuizTake() {
  // Server state - cached and refetched automatically
  const { data: quiz, isLoading, error } = trpc.quizzes.get.useQuery({
    repoPath,
    commit,
    quizId
  });
  
  // Mutation with optimistic update
  const submitMutation = trpc.quizzes.submit.useMutation({
    onSuccess: () => {
      // Invalidate related queries
      utils.quizzes.progress.invalidate();
    }
  });
}
```

### UI State with React

Local UI state uses React's useState and useReducer:

```typescript
function QuizTake() {
  // UI state - local to this component
  const [answers, setAnswers] = useState<Record<string, string>>({});
  const [submitted, setSubmitted] = useState(false);
  const [currentQuestion, setCurrentQuestion] = useState(0);
  
  const handleAnswerChange = (questionId: string, value: string) => {
    setAnswers(prev => ({ ...prev, [questionId]: value }));
  };
}
```

### Context for Global State

Some state is shared across components using React Context:

```typescript
// Theme context
const ThemeContext = createContext<ThemeContextValue | null>(null);

export function ThemeProvider({ children }: { children: React.ReactNode }) {
  const [theme, setTheme] = useState<'light' | 'dark'>('dark');
  
  return (
    <ThemeContext.Provider value={{ theme, setTheme }}>
      {children}
    </ThemeContext.Provider>
  );
}

export function useTheme() {
  const context = useContext(ThemeContext);
  if (!context) throw new Error('useTheme must be used within ThemeProvider');
  return context;
}
```

---

## Authentication Flow

The platform uses Manus OAuth for authentication. This section explains the authentication flow.

### OAuth Flow

1. User clicks "Sign In" button
2. Frontend redirects to Manus OAuth portal
3. User authenticates with Manus
4. OAuth portal redirects back with authorization code
5. Backend exchanges code for user info
6. Backend creates/updates user in database
7. Backend sets session cookie
8. Frontend receives authenticated state

### Session Management

Sessions are managed using HTTP-only cookies:

```typescript
// Set session cookie
res.cookie(COOKIE_NAME, sessionToken, {
  httpOnly: true,
  secure: true,
  sameSite: 'none',
  maxAge: 7 * 24 * 60 * 60 * 1000 // 7 days
});

// Read session in context
async function getUserFromSession(req: Request) {
  const token = req.cookies[COOKIE_NAME];
  if (!token) return null;
  
  const payload = await verifyToken(token);
  if (!payload) return null;
  
  return await getUserByOpenId(payload.openId);
}
```

### Frontend Authentication Hook

The `useAuth` hook provides authentication state to components:

```typescript
export function useAuth() {
  const { data: user, isLoading } = trpc.auth.me.useQuery();
  const logoutMutation = trpc.auth.logout.useMutation();
  
  return {
    user,
    loading: isLoading,
    isAuthenticated: !!user,
    logout: () => logoutMutation.mutate()
  };
}
```

---

## Demo Repository Initialization

The platform includes a demo repository that is automatically initialized on first load. This section explains how it works.

### Demo Data Structure

Demo content is defined in `server/lib/demo-data.ts`:

```typescript
export const DEMO_FILES = {
  'README.md': `# Demo Codebase\n\nThis is a sample codebase...`,
  'package.json': `{ "name": "demo-app", ... }`,
  'src/index.js': `const express = require('express');...`,
  // ... more files
};

export const DEMO_REVIEWS = [
  {
    id: 'auth-middleware-review',
    title: 'Add authentication middleware',
    // ... review definition
  },
  // ... more reviews
];

export const DEMO_QUIZZES = [
  {
    id: 'security-fundamentals',
    title: 'Security Fundamentals Quiz',
    // ... quiz definition
  },
  // ... more quizzes
];

export const DEMO_GUIDES = [
  {
    id: 'auth-flow-walkthrough',
    title: 'Authentication Flow Walkthrough',
    // ... guide definition
  },
  // ... more guides
];
```

### Initialization Process

The `server/lib/demo-init.ts` module handles initialization:

```typescript
export async function initializeDemoRepo(): Promise<DemoInfo> {
  const demoPath = '/tmp/code-review-demo/demo-codebase';
  
  // Check if already initialized
  if (await repoExists(demoPath)) {
    return { path: demoPath, initialized: true };
  }
  
  // Create directory
  await fs.mkdir(demoPath, { recursive: true });
  
  // Initialize git repo
  await git.init({ fs, dir: demoPath, defaultBranch: 'main' });
  
  // Write demo files
  for (const [filePath, content] of Object.entries(DEMO_FILES)) {
    const fullPath = path.join(demoPath, filePath);
    await fs.mkdir(path.dirname(fullPath), { recursive: true });
    await fs.writeFile(fullPath, content);
  }
  
  // Create initial commit
  await git.add({ fs, dir: demoPath, filepath: '.' });
  const commitOid = await git.commit({
    fs,
    dir: demoPath,
    message: 'Initial commit',
    author: { name: 'Demo', email: 'demo@example.com' }
  });
  
  // Write git notes
  await writeNotesArray(demoPath, 'refs/notes/reviews', commitOid, DEMO_REVIEWS);
  await writeNotesArray(demoPath, 'refs/notes/quizzes', commitOid, DEMO_QUIZZES);
  await writeNotesArray(demoPath, 'refs/notes/guides', commitOid, DEMO_GUIDES);
  
  return { path: demoPath, initialized: true };
}
```

### API Endpoint

The demo info is exposed through a tRPC procedure:

```typescript
getDemoInfo: publicProcedure.query(async () => {
  const info = await initializeDemoRepo();
  return info;
})
```

---

## CLI Tools Architecture

The platform includes CLI tools for testing and debugging. This section explains their architecture.

### Tool Organization

Each CLI tool is a standalone TypeScript file in the `cli` directory:

```
cli/
├── git-notes.ts    # Git notes operations
├── yaml-parser.ts  # YAML parsing and validation
└── api-test.ts     # API endpoint testing
```

### Command Pattern

CLI tools use a simple command pattern:

```typescript
// cli/git-notes.ts
const commands: Record<string, (args: string[]) => Promise<void>> = {
  'init': initRepo,
  'list-reviews': listReviews,
  'write-review': writeReview,
  'list-quizzes': listQuizzes,
  // ... more commands
};

async function main() {
  const [command, ...args] = process.argv.slice(2);
  
  if (!command || !commands[command]) {
    console.log('Usage: npx tsx cli/git-notes.ts <command> [args]');
    console.log('Commands:', Object.keys(commands).join(', '));
    process.exit(1);
  }
  
  await commands[command](args);
}

main().catch(console.error);
```

### Example Commands

```bash
# Initialize a test repository
npx tsx cli/git-notes.ts init /path/to/repo

# List all reviews
npx tsx cli/git-notes.ts list-reviews /path/to/repo

# Write a review from YAML file
npx tsx cli/git-notes.ts write-review /path/to/repo HEAD /path/to/review.yaml

# Validate a quiz YAML file
npx tsx cli/yaml-parser.ts validate-quiz /path/to/quiz.yaml

# Test API endpoints
npx tsx cli/api-test.ts list-reviews /path/to/repo
```

---

## Deployment Considerations

This section covers important considerations for deploying the platform.

### Environment Variables

The platform requires several environment variables:

| Variable | Purpose |
|----------|---------|
| `DATABASE_URL` | MySQL connection string |
| `JWT_SECRET` | Session token signing key |
| `VITE_APP_ID` | Manus OAuth application ID |
| `OAUTH_SERVER_URL` | Manus OAuth server URL |

### Build Process

The build process creates optimized bundles for production:

```bash
# Build frontend and backend
pnpm build

# Output structure
dist/
├── client/          # Vite-built frontend
│   ├── index.html
│   └── assets/
└── index.js         # esbuild-bundled backend
```

### Persistent Storage

The demo repository is stored in `/tmp/code-review-demo`. For production deployments, consider:

1. Using a persistent volume for the demo repository
2. Initializing the demo on container startup
3. Or connecting to external git repositories instead

### Scaling Considerations

The platform is stateless except for:

1. **Session cookies**: Handled by the database
2. **Demo repository**: Stored on the filesystem

For horizontal scaling, ensure all instances share the same database and have access to the demo repository (or initialize independently).

---

## Extending the Platform

This section provides guidance for extending the platform with new features.

### Adding a New Content Type

To add a new content type (e.g., "challenges"):

1. **Define the schema** in `server/lib/yaml-parser.ts`
2. **Add storage functions** in `server/lib/git-notes.ts`
3. **Create a router** in `server/routers/challenges.ts`
4. **Register the router** in `server/routers.ts`
5. **Create frontend pages** in `client/src/pages/`
6. **Add routes** in `client/src/App.tsx`
7. **Add demo data** in `server/lib/demo-data.ts`

### Adding a New Question Type

To add a new quiz question type:

1. **Update the schema** in `server/lib/yaml-parser.ts`
2. **Update grading logic** in `server/routers/quizzes.ts`
3. **Create input component** in `client/src/pages/QuizTake.tsx`
4. **Add demo questions** in `server/lib/demo-data.ts`

### Connecting External Repositories

To support external repositories:

1. **Add clone functionality** using isomorphic-git's `clone` command
2. **Create a repository registry** in the database
3. **Add authentication** for private repositories
4. **Implement periodic sync** to pull updates

---

## Summary

The Code Review Knowledge Platform demonstrates a modern approach to building educational tools on top of git. The architecture leverages git notes for content storage, ensuring educational material stays synchronized with the code it describes. The use of isomorphic-git enables deployment to any Node.js environment without external dependencies.

The technology choices—React, tRPC, Tailwind CSS, shadcn/ui—provide an excellent developer experience with strong typing and consistent styling. The CLI tools enable testing and automation without requiring the full web stack.

This architecture can serve as a foundation for building similar educational platforms, code review tools, or any application that benefits from tight integration with git repositories.

---

*This document is part of the Code Review Knowledge Platform documentation.*
