---
Title: 'Sequencing UX Walkthrough'
Ticket: ZINE-APP-DESIGN
Status: draft
Topics:
    - ux-design
    - sequencing
    - walkthrough
DocType: reference
Intent: long-term
Owners: []
RelatedFiles:
    - debate-round-16-sequencing-ux-api.md
Summary: UX walkthrough for image sequencing workflow with ASCII diagrams showing UI states, drag-and-drop interactions, visual feedback, and error handling
LastUpdated: 2025-12-01T00:00:00-05:00
---

# Sequencing UX Walkthrough

**Based on:** Debate Round 16 consensus on sequencing UX+API patterns

**Key Principles:**
- Optimistic updates for instant visual feedback
- Visual feedback (drag preview, drop zones, loading states)
- Batch operations (debounce rapid reordering)
- Error handling (rollback, toast notifications)
- Simple drag-and-drop sequencing

---

## UI Layout

### Initial State: Sequence View

```
┌─────────────────────────────────────────────────────────────────────────┐
│ Image Sequences                                    [+ New Sequence]      │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                           │
│ Sequences:                                                                │
│ ┌──────────────────────┐  ┌──────────────────────┐                      │
│ │ 📸 Street Photography │  │ 🎨 Personal Project  │                      │
│ │ 24 images            │  │ 16 images            │                      │
│ └──────────────────────┘  └──────────────────────┘                      │
│                                                                           │
│ Selected Sequence: "Street Photography"                                   │
│                                                                           │
│ ┌─────────────────────────────────────────────────────────────────────┐ │
│ │ Sequence Items (Drag to reorder)                                     │ │
│ ├─────────────────────────────────────────────────────────────────────┤ │
│ │                                                                       │ │
│ │ [IMG-001] [IMG-002] [IMG-003] [GAP] [IMG-004] [IMG-005]             │ │
│ │    ↓         ↓         ↓       ↓       ↓         ↓                   │ │
│ │  Position 0 Position 1 Position 2 Pos 3 Position 4 Position 5        │ │
│ │                                                                       │ │
│ │ [+ Add Image] [+ Add Gap]                                            │ │
│ │                                                                       │ │
│ └─────────────────────────────────────────────────────────────────────┘ │
│                                                                           │
│ Preview:                                                                  │
│ ┌─────────────────────────────────────────────────────────────────────┐ │
│ │ [Current Image Preview - IMG-001]                                   │ │
│ │ [◀ Previous] [▶ Next] [▶▶ Play]                                    │ │
│ └─────────────────────────────────────────────────────────────────────┘ │
│                                                                           │
└─────────────────────────────────────────────────────────────────────────┘
```

**API Calls:**
- `GET /projects/{projectId}/image-sequences` — List all sequences for project
  - **Params:** `projectId` (path parameter)
  - **Response:** `{ sequences: ImageSequence[] }`
- `GET /image-sequences/{sequenceId}` — Get sequence detail with items
  - **Params:** `sequenceId` (path parameter)
  - **Response:** `{ sequence: ImageSequence, items: ImageSequenceItem[] }`
- `GET /projects/{projectId}/assets` — List all assets for project
  - **Params:** `projectId` (path parameter)
  - **Response:** `{ assets: Asset[] }`

---

## Drag-and-Drop Workflow

### Step 1: User Starts Dragging

**User Action:** Click and hold on IMG-003, start dragging

**UI State:**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Sequence Items (Drag to reorder)                                         │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ [IMG-001] [IMG-002] [   ] [GAP] [IMG-004] [IMG-005]                     │ │
│    ↓         ↓       ↓      ↓       ↓         ↓                         │ │
│                                                                           │ │
│                    ┌─────────┐                                          │ │
│                    │ IMG-003 │  ← Dragged item (semi-transparent)      │ │
│                    └─────────┘                                          │ │
│                                                                           │ │
│ Drop Zones:                                                               │ │
│ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐            │ │
│ │  DROP   │ │  DROP   │ │  DROP   │ │  DROP   │ │  DROP   │            │ │
│ └─────────┘ └─────────┘ └─────────┘ └─────────┘ └─────────┘            │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**Technical Details:**
- Drag starts: `onDragStart` event fires
- Original position highlighted (subtle border/background)
- Dragged item becomes semi-transparent and follows cursor
- Drop zones appear (highlighted areas between items)

---

### Step 2: User Drags Over Drop Zone

**User Action:** Drag IMG-003 over drop zone between IMG-001 and IMG-002

**UI State:**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Sequence Items (Drag to reorder)                                         │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ [IMG-001] ┃━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┃ [IMG-002] │
│    ↓      ┃  DROP ZONE (highlighted)                                  ┃    ↓   │
│           ┃━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┃         │ │
│                                                                           │ │
│ [GAP] [IMG-004] [IMG-005]                                                │ │
│                                                                           │ │
│                    ┌─────────┐                                          │ │
│                    │ IMG-003 │  ← Dragged item                          │ │
│                    └─────────┘                                          │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**API Calls:**
- None (client-side drag state only)

**Technical Details:**
- `onDragOver` event fires
- Drop zone highlights (border/background change)
- Cursor changes to "move" indicator
- Visual preview shows where item will be inserted

---

### Step 3: User Drops Item (Optimistic Update)

**User Action:** Release mouse button, drop IMG-003 between IMG-001 and IMG-002

**UI State (Immediate - Optimistic Update):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Sequence Items (Drag to reorder)                                         │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ [IMG-001] [IMG-003] [IMG-002] [GAP] [IMG-004] [IMG-005]                 │ │
│    ↓         ↓         ↓       ↓       ↓         ↓                      │ │
│  Position 0 Position 1 Position 2 Pos 3 Position 4 Position 5            │ │
│                                                                           │ │
│ ✓ Reordered (syncing...)  ← Toast notification (non-intrusive)          │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**API Calls:**
- `PUT /image-sequences/{sequenceId}/items` — Reorder all items in sequence
  - **Params:** 
    - `sequenceId` (path parameter)
    - `items` (body parameter): `Array<{ asset_id?: string, is_gap: boolean }>`
  - **Response:** `{ items: ImageSequenceItem[] }`
  - **Note:** Debounced if rapid reordering (300ms debounce)

**Technical Details:**
- `onDrop` event fires
- **Optimistic update:** Cache updated immediately via RTK Query `onQueryStarted`
- UI reflects new order instantly (no waiting for server)
- Toast notification appears: "Reordered (syncing...)"
- API call initiated: `PUT /image-sequences/{id}/items` (debounced if rapid reordering)

**RTK Query Pattern:**
```typescript
async onQueryStarted({ sequenceId, items }, { dispatch, queryFulfilled }) {
  // Optimistic update - update cache immediately
  const patchResult = dispatch(
    api.util.updateQueryData('getImageSequenceDetail', { sequenceId }, (draft) => {
      draft.items = items.map((item, idx) => ({
        sequence_id: sequenceId,
        position: idx,
        asset_id: item.assetId,
        is_gap: item.isGap ?? !item.assetId,
      }));
    })
  );
  // ... API call continues in background
}
```

---

### Step 4: Server Response Success

**UI State (After Server Response):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Sequence Items (Drag to reorder)                                         │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ [IMG-001] [IMG-003] [IMG-002] [GAP] [IMG-004] [IMG-005]                 │ │
│    ↓         ↓         ↓       ↓       ↓         ↓                      │ │
│  Position 0 Position 1 Position 2 Pos 3 Position 4 Position 5            │ │
│                                                                           │ │
│ ✓ Reordered successfully  ← Toast notification (auto-dismisses)         │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**API Calls:**
- `PUT /image-sequences/{sequenceId}/items` — Response received
  - **Status:** `200 OK`
  - **Response:** `{ items: ImageSequenceItem[] }`
  - **Note:** Server response automatically replaces optimistic update in RTK Query cache

**Technical Details:**
- Server responds: `200 OK` with updated items array
- RTK Query automatically replaces optimistic update with server response
- Toast notification updates: "Reordered successfully" (auto-dismisses after 2s)
- Cache now matches server state exactly

---

### Step 5: Server Response Error (Rollback)

**UI State (On Error - Rollback):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Sequence Items (Drag to reorder)                                         │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ [IMG-001] [IMG-002] [IMG-003] [GAP] [IMG-004] [IMG-005]                 │ │
│    ↓         ↓         ↓       ↓       ↓         ↓                      │ │
│  Position 0 Position 1 Position 2 Pos 3 Position 4 Position 5            │ │
│                                                                           │ │
│ ✗ Failed to reorder. Please try again.  ← Error toast (red, stays)      │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**API Calls:**
- `PUT /image-sequences/{sequenceId}/items` — Error response
  - **Status:** `500 Internal Server Error` or network error
  - **Error Response:** `{ error: string }`
  - **Note:** RTK Query automatically rolls back optimistic update via `patchResult.undo()`

**Technical Details:**
- Server responds: `500 Internal Server Error` or network error
- **Rollback:** RTK Query `patchResult.undo()` reverts optimistic update
- UI returns to previous state (original order)
- Error toast appears: "Failed to reorder. Please try again." (red, stays until dismissed)
- User can retry drag-and-drop operation

**RTK Query Error Handling:**
```typescript
try {
  await queryFulfilled;
} catch (error) {
  // Automatic rollback on error
  patchResult.undo();
  // Show error notification
  dispatch(uiSlice.actions.addToast({
    id: Date.now().toString(),
    text: 'Failed to reorder items. Please try again.',
    type: 'error',
  }));
}
```

---

## Adding Items

### Adding a Single Image

**User Action:** Click "[+ Add Image]" button

**UI State:**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Sequence Items                                                           │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ [IMG-001] [IMG-002] [IMG-003] [GAP] [IMG-004] [IMG-005]                 │ │
│                                                                           │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │
│ │ Add Image to Sequence                                                │ │
│ ├─────────────────────────────────────────────────────────────────────┤ │
│ │                                                                       │ │
│ │ Available Images:                                                    │ │
│ │ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐                    │ │
│ │ │ IMG-006 │ │ IMG-007 │ │ IMG-008 │ │ IMG-009 │                    │ │
│ │ └─────────┘ └─────────┘ └─────────┘ └─────────┘                    │ │
│ │                                                                       │ │
│ │ [Cancel] [Add Selected]                                               │ │
│ └─────────────────────────────────────────────────────────────────────┘ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**API Calls:**
- `GET /projects/{projectId}/assets` — List available assets (already called on initial load, cached)
  - **Params:** `projectId` (path parameter)
  - **Response:** `{ assets: Asset[] }`
  - **Note:** Used to populate image selection modal, may be filtered to show only unassigned images

**After Selection (Optimistic Update):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Sequence Items                                                           │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ [IMG-001] [IMG-002] [IMG-003] [GAP] [IMG-004] [IMG-005] [IMG-006]      │ │
│                                                                           │ │
│ ✓ Image added (syncing...)  ← Toast notification                        │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**API Calls:**
- `POST /image-sequences/{sequenceId}/items` — Add image to sequence
  - **Params:**
    - `sequenceId` (path parameter)
    - `asset_id` (body parameter): `string` (required)
    - `is_gap` (body parameter): `boolean` (default: `false`)
  - **Response:** `{ items: ImageSequenceItem[] }`
  - **Note:** Optimistic update appears immediately, API call happens in background

**Technical Details:**
- User selects image from modal
- **Optimistic update:** Image appears in sequence immediately
- API call: `POST /image-sequences/{id}/items` with `asset_id`
- On success: Toast updates to "Image added successfully"
- On error: Rollback, image removed, error toast shown

---

### Adding a Gap

**User Action:** Click "[+ Add Gap]" button

**UI State (Immediate - Optimistic Update):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Sequence Items                                                           │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ [IMG-001] [IMG-002] [IMG-003] [GAP] [GAP] [IMG-004] [IMG-005]          │ │
│                                                                           │ │
│ ✓ Gap added (syncing...)  ← Toast notification                         │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**API Calls:**
- `POST /image-sequences/{sequenceId}/items` — Add gap to sequence
  - **Params:**
    - `sequenceId` (path parameter)
    - `is_gap` (body parameter): `boolean` (required, set to `true`)
    - `asset_id` (body parameter): `null` or omitted
  - **Response:** `{ items: ImageSequenceItem[] }`
  - **Note:** Optimistic update appears immediately, API call happens in background

**Technical Details:**
- Gap appears immediately (optimistic update)
- API call: `POST /image-sequences/{id}/items` with `is_gap: true`
- On success: Toast updates to "Gap added successfully"
- On error: Rollback, gap removed, error toast shown

---

## Deleting Items

### Deleting an Image

**User Action:** Click delete button (🗑️) on IMG-003

**UI State (Confirmation):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Sequence Items                                                           │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ [IMG-001] [IMG-002] [🗑️ IMG-003] [GAP] [IMG-004] [IMG-005]             │ │
│                                                                           │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │
│ │ Delete Image?                                                       │ │
│ │                                                                     │ │
│ │ Remove IMG-003 from sequence?                                       │ │
│ │                                                                     │ │
│ │ [Cancel] [Delete]                                                   │ │
│ └─────────────────────────────────────────────────────────────────────┘ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**After Confirmation (Optimistic Update):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Sequence Items                                                           │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ [IMG-001] [IMG-002] [GAP] [IMG-004] [IMG-005]                           │ │
│                                                                           │ │
│ ✓ Image removed (syncing...)  ← Toast notification                     │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**API Calls:**
- `DELETE /image-sequences/{sequenceId}/items/{position}` — Delete item at position
  - **Params:**
    - `sequenceId` (path parameter)
    - `position` (path parameter): `number` (0-based index)
  - **Response:** `204 No Content` or `{ success: boolean }`
  - **Note:** Optimistic update removes item immediately, API call happens in background

**Technical Details:**
- Confirmation dialog appears (non-blocking, can be dismissed)
- On confirm: **Optimistic update** removes image immediately
- API call: `DELETE /image-sequences/{id}/items/{position}`
- On success: Toast updates to "Image removed successfully"
- On error: Rollback, image restored, error toast shown

---

## Rapid Reordering (Debouncing)

### Multiple Drags in Quick Succession

**User Action:** Drag IMG-003 to position 1, then immediately drag IMG-004 to position 0

**UI State (First Drag - Optimistic):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Sequence Items                                                           │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ [IMG-001] [IMG-003] [IMG-002] [GAP] [IMG-004] [IMG-005]                 │ │
│                                                                           │ │
│ ✓ Reordered (syncing...)  ← Toast notification                          │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**UI State (Second Drag - Before Debounce):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Sequence Items                                                           │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ [IMG-004] [IMG-001] [IMG-003] [IMG-002] [GAP] [IMG-005]                 │ │
│                                                                           │ │
│ ✓ Reordered (syncing...)  ← Toast notification (updated)                │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**API Calls:**
- `PUT /image-sequences/{sequenceId}/items` — Reorder items (debounced)
  - **Params:**
    - `sequenceId` (path parameter)
    - `items` (body parameter): `Array<{ asset_id?: string, is_gap: boolean }>`
  - **Response:** `{ items: ImageSequenceItem[] }`
  - **Note:** Debounced 300ms - if second drag happens within 300ms, debounce timer resets, single API call sent with final order

**Technical Details:**
- First drag: Optimistic update, API call queued
- Second drag (within 300ms): Optimistic update, **debounce timer resets**
- After 300ms of no activity: **Single API call** with final order
- Reduces API calls from 2 to 1
- Toast notification updates to reflect latest state

**Debouncing Pattern:**
```typescript
let debounceTimer: NodeJS.Timeout | null = null;

const handleReorder = (sourceIndex: number, targetIndex: number) => {
  // Optimistic update immediately
  updateCacheOptimistically(newOrder);
  
  // Debounce API call
  if (debounceTimer) clearTimeout(debounceTimer);
  debounceTimer = setTimeout(() => {
    callReorderAPI(newOrder);
    debounceTimer = null;
  }, 300); // 300ms debounce
};
```

---

## Visual Feedback States

### Loading State (During Sync)

**UI State:**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Sequence Items                                                           │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ [IMG-001] [IMG-003] [IMG-002] [GAP] [IMG-004] [IMG-005]                 │ │
│    ↓         ↓         ↓       ↓       ↓         ↓                      │ │
│                                                                           │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │
│ │ ⏳ Syncing changes...  ← Subtle loading indicator (non-intrusive)   │ │
│ └─────────────────────────────────────────────────────────────────────┘ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**API Calls:**
- Same as previous operation (API call in progress)
  - **Note:** Loading state shown while waiting for server response

**Technical Details:**
- Subtle loading indicator appears during API call
- Does not block UI (users can continue working)
- Disappears when sync completes
- Optimistic updates make this feel instant

---

### Success State

**UI State:**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Sequence Items                                                           │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ [IMG-001] [IMG-003] [IMG-002] [GAP] [IMG-004] [IMG-005]                 │ │
│    ↓         ↓         ↓       ↓       ↓         ↓                      │ │
│                                                                           │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │
│ │ ✓ Changes saved  ← Success indicator (green, auto-dismisses)        │ │
│ └─────────────────────────────────────────────────────────────────────┘ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**API Calls:**
- Same as previous operation (API call completed successfully)
  - **Status:** `200 OK`
  - **Response:** `{ items: ImageSequenceItem[] }`
  - **Note:** Success state shown after server confirms changes

**Technical Details:**
- Success toast appears (green, auto-dismisses after 2s)
- Visual confirmation that changes are saved
- Non-intrusive, doesn't interrupt workflow

---

### Error State

**UI State:**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Sequence Items                                                           │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ [IMG-001] [IMG-002] [IMG-003] [GAP] [IMG-004] [IMG-005]                 │ │
│    ↓         ↓         ↓       ↓       ↓         ↓                      │ │
│                                                                           │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │
│ │ ✗ Failed to save changes. Please try again.  ← Error toast (red)    │ │
│ │ [Retry]                                                              │ │
│ └─────────────────────────────────────────────────────────────────────┘ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**API Calls:**
- Same as previous operation (API call failed)
  - **Status:** `500 Internal Server Error` or network error
  - **Error Response:** `{ error: string }`
  - **Note:** Error state shown after server error, rollback already occurred

**Technical Details:**
- Error toast appears (red, stays until dismissed)
- Rollback has already occurred (UI shows previous state)
- Retry button available (re-applies last operation)
- User can manually retry or continue working

---

## Summary: Key UX Patterns

### 1. Optimistic Updates
- **What:** UI updates immediately, syncs in background
- **Why:** Feels instant, supports rapid experimentation
- **How:** RTK Query `onQueryStarted` updates cache before API call

### 2. Visual Feedback
- **What:** Drag preview, drop zones, loading states, success/error toasts
- **Why:** Users see what's happening, builds confidence
- **How:** Visual indicators at each interaction point

### 3. Error Handling
- **What:** Automatic rollback, error toasts, retry options
- **Why:** Reliable state, clear error communication
- **How:** RTK Query `patchResult.undo()` reverts optimistic updates

### 4. Batch Operations
- **What:** Debounce rapid reordering, batch API calls
- **Why:** Efficient, reduces server load, faster sync
- **How:** 300ms debounce timer, single API call for multiple operations

### 5. Simple Drag-and-Drop
- **What:** Intuitive drag-and-drop interface
- **Why:** Familiar pattern, easy to learn
- **How:** HTML5 drag API, visual feedback, optimistic updates

---

## Technical Implementation Notes

### RTK Query Mutation Pattern
```typescript
reorderImageSequenceItems: builder.mutation({
  query: ({ sequenceId, items }) => ({
    url: `/image-sequences/${encodeURIComponent(sequenceId)}/items`,
    method: 'PUT',
    body: { items },
  }),
  async onQueryStarted({ sequenceId, items }, { dispatch, queryFulfilled }) {
    // Optimistic update
    const patchResult = dispatch(
      api.util.updateQueryData('getImageSequenceDetail', { sequenceId }, (draft) => {
        draft.items = items.map((item, idx) => ({
          sequence_id: sequenceId,
          position: idx,
          asset_id: item.assetId,
          is_gap: item.isGap ?? !item.assetId,
        }));
      })
    );
    try {
      await queryFulfilled;
      // Server response automatically replaces optimistic update
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

### Debouncing Pattern
```typescript
let debounceTimer: NodeJS.Timeout | null = null;

const handleReorder = (sourceIndex: number, targetIndex: number) => {
  // Update UI optimistically immediately
  const newOrder = computeNewOrder(sourceIndex, targetIndex);
  updateCacheOptimistically(newOrder);
  
  // Debounce API call
  if (debounceTimer) clearTimeout(debounceTimer);
  debounceTimer = setTimeout(() => {
    reorderItems({ sequenceId, items: newOrder });
    debounceTimer = null;
  }, 300); // 300ms debounce
};
```

---

**End of Sequencing UX Walkthrough**

