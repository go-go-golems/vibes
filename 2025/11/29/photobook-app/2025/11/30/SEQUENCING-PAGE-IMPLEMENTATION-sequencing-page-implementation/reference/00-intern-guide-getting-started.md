---
Title: 'Intern Guide: Getting Started with Sequencing Page Implementation'
Ticket: SEQUENCING-PAGE-IMPLEMENTATION
Status: active
Topics:
    - guide
    - onboarding
    - sequencing
    - frontend
    - backend
DocType: reference
Intent: long-term
Owners: []
RelatedFiles: []
Summary: Comprehensive guide for an intern to understand the full context, find relevant files, and implement the sequencing page based on design specs
LastUpdated: 2025-12-01T00:00:00-05:00
---

# Intern Guide: Getting Started with Sequencing Page Implementation

## Welcome!

This guide will help you understand the full context of the sequencing page implementation project, find all relevant files, and get started with implementation.

## What Are We Building?

We're building a new **sequencing page** that allows photographers to:
- Create image sequences (ordered lists of images)
- Reorder images via drag-and-drop
- Add gaps between images
- See visual feedback during interactions
- Handle errors gracefully

The new implementation will be created **alongside** the existing frontend (not modifying it), so we can compare and eventually migrate.

## Project Context

### What is Zine Layout?

Zine Layout is a web application for creating photo books/zines. It helps photographers:
1. **Sequence images** (order them for the book)
2. **Apply layout templates** (crop/scale images)
3. **Create page layouts** (place images on pages)
4. **Generate zines** (arrange pages for printing)

### Where Does Sequencing Fit?

Sequencing is the **first step** in the workflow:
1. **Sequencing** → Order images (what we're building)
2. **Image Layout** → Crop/scale images
3. **Page Layout** → Place images on pages
4. **Zine Generation** → Arrange pages for printing

## Design Specifications

### Primary Spec Document

**📄 `sequencing-ux-walkthrough.md`**
- **Location:** `vibes/2025/11/29/photobook-app/2025/11/30/ZINE-APP-DESIGN-zine-app-design-sequencing-and-layout-application/reference/sequencing-ux-walkthrough.md`
- **What it contains:**
  - Complete UI/UX walkthrough with ASCII diagrams
  - All screen states and interactions
  - API call specifications
  - Error handling patterns
  - Technical implementation notes (RTK Query patterns)

**This is your primary reference!** Read it thoroughly before starting.

### Design Debate Document

**📄 `debate-round-16-sequencing-ux-api.md`**
- **Location:** `vibes/2025/11/29/photobook-app/2025/11/30/ZINE-APP-DESIGN-zine-app-design-sequencing-and-layout-application/reference/debate-round-16-sequencing-ux-api.md`
- **What it contains:**
  - Design decisions and rationale
  - RTK Toolkit recommendations
  - API patterns discussion
  - Trade-offs and considerations

**Read this to understand WHY we made certain design decisions.**

## Codebase Structure

### Backend (Go)

**Location:** `zine-layout/pkg/`

#### Key Files for Sequencing:

1. **API Routes:**
   - `zine-layout/pkg/serve/image_sequences_routes.go`
     - All HTTP endpoints for sequences
     - `GET /api/projects/{id}/image-sequences` - List sequences
     - `POST /api/projects/{id}/image-sequences` - Create sequence
     - `GET /api/image-sequences/{id}` - Get sequence with items
     - `PUT /api/image-sequences/{id}/items` - Reorder items (batch)
     - `POST /api/image-sequences/{id}/items` - Add item
     - `DELETE /api/image-sequences/{id}/items/{position}` - Delete item

2. **Data Models:**
   - `zine-layout/pkg/repo/types.go`
     - `ImageSequence` struct (sequence metadata)
     - `ImageSequenceItem` struct (individual items in sequence)

3. **Repository Interface:**
   - `zine-layout/pkg/repo/repositories.go`
     - `ImageSequenceRepository` interface
     - Methods: `Create`, `Get`, `Update`, `Delete`, `ListByProject`
     - Methods: `AddItem`, `DeleteItem`, `ListItems`, `ReorderItems`

#### Backend API Endpoints Summary:

```
GET    /api/projects/{projectId}/image-sequences
POST   /api/projects/{projectId}/image-sequences
GET    /api/image-sequences/{sequenceId}
PATCH  /api/image-sequences/{sequenceId}
DELETE /api/image-sequences/{sequenceId}
GET    /api/image-sequences/{sequenceId}/items
POST   /api/image-sequences/{sequenceId}/items
PUT    /api/image-sequences/{sequenceId}/items  (batch reorder)
DELETE /api/image-sequences/{sequenceId}/items/{position}
```

### Frontend (React + TypeScript)

**Location:** `zine-layout/web/`

#### Existing Frontend (Don't Modify):

1. **Current Implementation:**
   - `zine-layout/web/src/views/tabs/SequencesTab.tsx`
     - **Don't modify this!** Use it as reference only
     - Shows how sequencing currently works
     - Has basic drag-and-drop (may not match spec)

2. **API Layer:**
   - `zine-layout/web/src/api.ts`
     - RTK Query API definitions
     - All hooks for sequences:
       - `useGetImageSequencesQuery`
       - `useCreateImageSequenceMutation`
       - `useGetImageSequenceDetailQuery`
       - `useAddImageSequenceItemMutation`
       - `useDeleteImageSequenceItemMutation`
       - `useReorderImageSequenceItemsMutation`

3. **State Management:**
   - `zine-layout/web/src/store.ts`
     - Redux store configuration
     - Toast notifications (`uiSlice`)

4. **UI Components:**
   - `zine-layout/web/src/components/ui/`
     - `Button.tsx`, `Card.tsx`, `Input.tsx`, `Tabs.tsx`
     - Reusable UI components

5. **Routing:**
   - `zine-layout/web/src/routes/App.tsx`
     - React Router setup
     - Add new route here for v2 sequencing page

#### New Frontend Structure (What You'll Create):

```
zine-layout/web/src/views/v2/
├── SequencingPage.tsx              # Main page component
└── components/
    ├── SequenceList.tsx            # List of sequences
    ├── SequenceEditor.tsx          # Sequence editing (drag-and-drop)
    ├── SequenceItem.tsx            # Individual sequence item
    └── AssetPicker.tsx            # Asset selection modal (optional)
```

## Key Technologies

### Frontend Stack:

- **React 18** - UI framework
- **TypeScript** - Type safety
- **Redux Toolkit (RTK Query)** - State management and API calls
- **React Router** - Routing
- **Tailwind CSS** - Styling
- **Vite** - Build tool

### Backend Stack:

- **Go** - Backend language
- **SQLite** - Database (via `pkg/repo`)
- **HTTP** - REST API

## Implementation Approach

### Phase 1: Understand Existing Code

1. **Read the specs:**
   - `sequencing-ux-walkthrough.md` (complete walkthrough)
   - `debate-round-16-sequencing-ux-api.md` (design decisions)

2. **Explore existing code:**
   - `zine-layout/web/src/views/tabs/SequencesTab.tsx` (current implementation)
   - `zine-layout/web/src/api.ts` (API hooks)
   - `zine-layout/pkg/serve/image_sequences_routes.go` (backend endpoints)

3. **Test existing API:**
   - Use curl or Postman to test endpoints
   - Understand request/response formats

### Phase 2: Set Up New Frontend

1. **Create directory structure:**
   ```
   zine-layout/web/src/views/v2/
   ```

2. **Create basic components:**
   - Start with `SequencingPage.tsx` (main component)
   - Add `SequenceList.tsx` (list of sequences)
   - Add `SequenceEditor.tsx` (sequence editing)

3. **Set up routing:**
   - Add route in `App.tsx` (optional: `/v2/projects/:id/sequencing`)

### Phase 3: Implement Core Features

1. **Sequence List:**
   - Display sequences
   - Create new sequence
   - Select sequence

2. **Sequence Editor:**
   - Display sequence items
   - Show thumbnails
   - Show gaps

3. **Drag-and-Drop:**
   - Choose library (react-beautiful-dnd, @dnd-kit, or native HTML5)
   - Implement reordering
   - Add visual feedback

4. **Optimistic Updates:**
   - Use RTK Query `onQueryStarted`
   - Update cache immediately
   - Rollback on error (`patchResult.undo()`)

5. **Debounced Reordering:**
   - Debounce rapid reorders (300-500ms)
   - Batch API calls
   - Show loading indicator

### Phase 4: Polish and Error Handling

1. **Visual Feedback:**
   - Loading states
   - Error states
   - Empty states
   - Success notifications

2. **Error Handling:**
   - Network errors
   - Validation errors
   - Server errors
   - Rollback optimistic updates

3. **Testing:**
   - Test all interactions
   - Test error scenarios
   - Test on different screen sizes

## Key Implementation Patterns

### RTK Query Optimistic Updates

**Pattern from spec:**

```typescript
reorderImageSequenceItems: builder.mutation({
  query: ({ sequenceId, items }) => ({
    url: `/image-sequences/${sequenceId}/items`,
    method: 'PUT',
    body: { items },
  }),
  async onQueryStarted({ sequenceId, items }, { dispatch, queryFulfilled }) {
    // Optimistic update
    const patchResult = dispatch(
      api.util.updateQueryData('getImageSequenceDetail', { sequenceId }, (draft) => {
        draft.items = items.map((item, idx) => ({
          ...item,
          position: idx,
        }));
      })
    );
    try {
      await queryFulfilled;
    } catch (error) {
      // Rollback on error
      patchResult.undo();
      // Show error toast
      dispatch(uiSlice.actions.addToast({
        id: Date.now().toString(),
        text: 'Failed to reorder items. Please try again.',
        type: 'error',
      }));
    }
  },
  invalidatesTags: (_result, _error, { sequenceId }) => [
    { type: 'ImageSequenceItems', id: sequenceId },
  ],
}),
```

**Key points:**
- Use `onQueryStarted` for optimistic updates
- Use `patchResult.undo()` to rollback on error
- Use `invalidatesTags` for cache invalidation
- Show toast notifications for errors

### Debounced Reordering

**Pattern from spec:**

```typescript
const debouncedReorder = useMemo(
  () => debounce((items: ImageSequenceItem[]) => {
    reorderItems({ sequenceId, items }).unwrap();
  }, 300),
  [sequenceId, reorderItems]
);

// On drag end:
debouncedReorder(newItems);
```

**Key points:**
- Debounce rapid reorders (300-500ms)
- Batch multiple reorders into single API call
- Show loading indicator during debounce

## File Locations Quick Reference

### Specs and Design Docs:

```
vibes/2025/11/29/photobook-app/2025/11/30/
├── ZINE-APP-DESIGN-zine-app-design-sequencing-and-layout-application/
│   └── reference/
│       ├── sequencing-ux-walkthrough.md          ← PRIMARY SPEC
│       └── debate-round-16-sequencing-ux-api.md ← DESIGN DECISIONS
└── SEQUENCING-PAGE-IMPLEMENTATION-sequencing-page-implementation/
    ├── tasks.md                                  ← TASKS LIST
    └── reference/
        └── 00-intern-guide-getting-started.md   ← THIS FILE
```

### Backend Code:

```
zine-layout/pkg/
├── serve/
│   └── image_sequences_routes.go                ← API ENDPOINTS
└── repo/
    ├── types.go                                  ← DATA MODELS
    └── repositories.go                           ← REPOSITORY INTERFACE
```

### Frontend Code (Existing):

```
zine-layout/web/src/
├── views/
│   └── tabs/
│       └── SequencesTab.tsx                      ← CURRENT IMPLEMENTATION (don't modify)
├── api.ts                                        ← RTK QUERY HOOKS
├── store.ts                                      ← REDUX STORE
└── components/
    └── ui/                                       ← REUSABLE UI COMPONENTS
```

### Frontend Code (New - What You'll Create):

```
zine-layout/web/src/views/v2/
├── SequencingPage.tsx                            ← MAIN PAGE
└── components/
    ├── SequenceList.tsx                          ← SEQUENCE LIST
    ├── SequenceEditor.tsx                         ← SEQUENCE EDITOR
    └── SequenceItem.tsx                           ← SEQUENCE ITEM
```

## Getting Started Checklist

- [ ] Read `sequencing-ux-walkthrough.md` completely
- [ ] Read `debate-round-16-sequencing-ux-api.md` for context
- [ ] Review `zine-layout/web/src/views/tabs/SequencesTab.tsx` (existing implementation)
- [ ] Review `zine-layout/web/src/api.ts` (RTK Query hooks)
- [ ] Review `zine-layout/pkg/serve/image_sequences_routes.go` (backend endpoints)
- [ ] Test backend API with curl/Postman
- [ ] Set up development environment
- [ ] Create `zine-layout/web/src/views/v2/` directory
- [ ] Start implementing `SequencingPage.tsx`
- [ ] Follow tasks in `tasks.md`

## Common Questions

### Q: Should I modify the existing `SequencesTab.tsx`?

**A: No!** Create a new implementation in `v2/` directory. We want to compare both implementations.

### Q: Which drag-and-drop library should I use?

**A: Your choice!** Options:
- `react-beautiful-dnd` (popular, but may have React 18 issues)
- `@dnd-kit/core` (modern, React 18 compatible)
- Native HTML5 drag-and-drop (no dependencies)

Check the spec for requirements, then choose what works best.

### Q: How do I handle errors?

**A: Follow the RTK Query pattern:**
1. Use `onQueryStarted` for optimistic updates
2. Use `patchResult.undo()` to rollback on error
3. Show toast notifications (use `uiSlice.actions.addToast`)

See `sequencing-ux-walkthrough.md` - "Error Handling" section.

### Q: How do I implement debounced reordering?

**A:**
1. Use `lodash.debounce` or custom debounce utility
2. Debounce rapid reorders (300-500ms)
3. Batch multiple reorders into single API call
4. Show loading indicator during debounce

See `sequencing-ux-walkthrough.md` - "Rapid Reordering with Debouncing" section.

### Q: Where do I find the API endpoint specifications?

**A:** Check `sequencing-ux-walkthrough.md` - each section has "API Calls" with:
- Endpoint URL
- HTTP method
- Request parameters
- Response structure

### Q: How do I test the backend API?

**A:** Use curl or Postman:

```bash
# List sequences
curl http://localhost:8090/api/projects/{projectId}/image-sequences

# Create sequence
curl -X POST http://localhost:8090/api/projects/{projectId}/image-sequences \
  -H "Content-Type: application/json" \
  -d '{"name": "My Sequence", "description": "Test"}'

# Get sequence with items
curl http://localhost:8090/api/image-sequences/{sequenceId}

# Reorder items (batch)
curl -X PUT http://localhost:8090/api/image-sequences/{sequenceId}/items \
  -H "Content-Type: application/json" \
  -d '{"items": [{"asset_id": "id1", "is_gap": false}, ...]}'
```

## Next Steps

1. **Read the specs** (sequencing-ux-walkthrough.md)
2. **Explore existing code** (SequencesTab.tsx, api.ts)
3. **Set up new frontend structure** (v2/ directory)
4. **Start implementing** (follow tasks.md)
5. **Ask questions** if you're stuck!

## Resources

- **Primary Spec:** `sequencing-ux-walkthrough.md`
- **Design Decisions:** `debate-round-16-sequencing-ux-api.md`
- **Tasks:** `tasks.md`
- **RTK Query Docs:** https://redux-toolkit.js.org/rtk-query/overview
- **React Router Docs:** https://reactrouter.com/
- **Tailwind CSS Docs:** https://tailwindcss.com/

Good luck! 🚀

