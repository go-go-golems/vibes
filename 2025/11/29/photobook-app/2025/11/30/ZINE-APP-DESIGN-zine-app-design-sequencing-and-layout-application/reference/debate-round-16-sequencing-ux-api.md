---
Title: 'Debate Round 16: What is the UX and API pattern for sequencing?'
Ticket: ZINE-APP-DESIGN
Status: draft
Topics:
    - debate
    - sequencing
    - ux-design
    - api-design
    - drag-and-drop
DocType: reference
Intent: long-term
Owners: []
RelatedFiles: []
Summary: Sixteenth debate round exploring UX and API patterns for sequencing - drag-and-drop reordering, adding/removing items, batch operations, optimistic updates
LastUpdated: 2025-11-30T23:55:00-05:00
---

# Debate Round 16: What is the UX and API pattern for sequencing?

**Question:** What is the UX and API pattern for sequencing? How should drag-and-drop reordering work (optimistic updates, batch API calls)? Which API endpoints are called when (reorder items, add item, delete item)? How should state sync (RTK Query cache invalidation, optimistic updates)? What UI feedback is needed (loading states, success/error messages)?

**Primary Candidates:**
- **Maya Chen** (Experimental Photographer) — Argues for simple drag-and-drop sequencing
- **Alex Rivera** (Documentary Photographer) — Argues for efficient batch operations
- **Jordan Kim** (Designer/Photographer) — Argues for clear visual feedback
- **Taylor Kim** (UX Designer) — Argues for clear UI patterns and feedback
- **Sam Chen** (Frontend Developer) — Argues for RTK Query mutations and cache invalidation
- **Jordan Park** (Backend Developer) — Argues for efficient API design

**Secondary Participants:**
- `web/src/store/` (State Management) — Can interject with state management patterns
- `pkg/serve/` (The API Layer) — Can interject with API design patterns
- `pkg/repo/` (The Database Layer) — Can interject with data model patterns
- **RTK Toolkit** (The State Management Framework) — Can interject with RTK Query patterns and best practices

**Why this question matters:** Sequencing is the core workflow—needs to feel fast and reliable. We want simple UX—drag images around, see them move instantly, with clear feedback. But we also need efficient APIs and state management.

---

## Pre-Debate Research

### Current Frontend Implementation

**Research conducted by:** Sam Chen (Frontend Developer)

**Current sequencing UI:**
- Drag-and-drop reordering (HTML5 drag API)
- Add items (click button, select asset)
- Delete items (click delete button)
- Gap insertion (click "Add Gap" button)
- No optimistic updates (waits for server response)
- No loading states during reordering

**Code reference:**
```165:187:zine-layout/web/src/views/tabs/SequencesTab.tsx
const handleReorder = async (sourceIndex: number, targetIndex: number | null) => {
  if (!selectedSequenceId) return;
  const itemsCopy = [...sortedItems];
  if (sourceIndex < 0 || sourceIndex >= itemsCopy.length) return;
  if (targetIndex !== null) {
    if (targetIndex < 0 || targetIndex > itemsCopy.length) return;
    if (targetIndex === sourceIndex || targetIndex === sourceIndex + 1) return;
  }
  const [moved] = itemsCopy.splice(sourceIndex, 1);
  if (!moved) return;
  let destination = targetIndex === null ? itemsCopy.length : Math.max(0, targetIndex);
  if (destination > itemsCopy.length) destination = itemsCopy.length;
  if (sourceIndex < destination) {
    destination = destination - 1;
  }
  itemsCopy.splice(destination, 0, moved);
  await reorderItems({
    sequenceId: selectedSequenceId,
    items: mapItemsToPayload(itemsCopy),
  }).unwrap();
  setSlideIndex(destination);
  setIsPlaying(false);
};
```

**Drag-and-drop handlers:**
```189:201:zine-layout/web/src/views/tabs/SequencesTab.tsx
const handleDragOver = (event: React.DragEvent) => {
  event.preventDefault();
  event.dataTransfer.dropEffect = 'move';
};

const handleDrop = async (event: React.DragEvent, targetIndex: number | null) => {
  event.preventDefault();
  event.stopPropagation();
  if (dragSourceIndex !== null) {
    await handleReorder(dragSourceIndex, targetIndex);
  }
  setDragSourceIndex(null);
};
```

**Add/delete operations:**
```153:163:zine-layout/web/src/views/tabs/SequencesTab.tsx
const handleDeleteItem = async (position: number) => {
  if (!selectedSequenceId) return;
  await deleteSequenceItem({ sequenceId: selectedSequenceId, position }).unwrap();
  setIsPlaying(false);
};

const handleAppendGap = async () => {
  if (!selectedSequenceId) return;
  await addSequenceItem({ sequenceId: selectedSequenceId }).unwrap();
  setIsPlaying(false);
};
```

**Conclusion:** Current implementation uses HTML5 drag API, waits for server response (no optimistic updates), no loading states. Reordering feels slow—waits for API call before UI updates.

### Current API Implementation

**Research conducted by:** Jordan Park (Backend Developer)

**Current API endpoints:**
- `GET /image-sequences/{id}` — Get sequence with items
- `POST /image-sequences/{id}/items` — Add item (asset or gap)
- `PUT /image-sequences/{id}/items` — Reorder all items (batch update)
- `DELETE /image-sequences/{id}/items/{position}` — Delete item at position

**Code reference:**
```174:235:zine-layout/pkg/serve/image_sequences_routes.go
func (s *Server) handleSequenceItems(w http.ResponseWriter, r *http.Request, sequenceID string) {
  // ... validation ...
  switch r.Method {
  case http.MethodGet:
    items, err := s.repos.ImageSequences.ListItems(sequenceID)
    // ... return items ...
  case http.MethodPost:
    var req struct {
      AssetID *string `json:"asset_id"`
      IsGap   bool    `json:"is_gap"`
    }
    // ... validate and add item ...
    item := &repo.ImageSequenceItem{
      SequenceID: sequenceID,
      AssetID:    req.AssetID,
      IsGap:      req.IsGap,
    }
    if err := s.repos.ImageSequences.AddItem(item); err != nil {
      // ... error handling ...
    }
    items, err := s.repos.ImageSequences.ListItems(sequenceID)
    // ... return updated items ...
  case http.MethodPut:
    var req struct {
      Items []struct {
        AssetID *string `json:"asset_id"`
        IsGap   bool    `json:"is_gap"`
      } `json:"items"`
    }
    // ... validate and reorder items ...
    if err := s.repos.ImageSequences.ReorderItems(sequenceID, items); err != nil {
      // ... error handling ...
    }
    items, err := s.repos.ImageSequences.ListItems(sequenceID)
    // ... return updated items ...
  }
}
```

**RTK Query mutations:**
```367:385:zine-layout/web/src/api.ts
reorderImageSequenceItems: builder.mutation<
  ImageSequenceItem[],
  { sequenceId: string; items: { assetId?: string; isGap?: boolean }[] }
>({
  query: ({ sequenceId, items }) => ({
    url: `/image-sequences/${encodeURIComponent(sequenceId)}/items`,
    method: 'PUT',
    body: {
      items: items.map((item) => ({
        asset_id: item.assetId,
        is_gap: item.isGap ?? !item.assetId,
      })),
    },
  }),
  transformResponse: (response: { items: ImageSequenceItem[] }) => response.items ?? [],
  invalidatesTags: (_result, _error, { sequenceId }) => [
    { type: 'ImageSequenceItems', id: sequenceId },
  ],
}),
```

**Conclusion:** Current API uses batch reordering (`PUT /items` with full item list), individual add (`POST /items`), individual delete (`DELETE /items/{position}`). Batch reordering is efficient, but frontend waits for server response—no optimistic updates.

### UX Patterns for Drag-and-Drop

**Research conducted by:** Taylor Kim (UX Designer)

**Common drag-and-drop patterns:**
1. **Optimistic updates:** Update UI immediately, sync with server in background
2. **Visual feedback:** Show drag preview, drop zones, loading states
3. **Error handling:** Rollback on error, show toast notification
4. **Batch operations:** Debounce rapid reordering, batch API calls

**Performance targets:**
- Drag start: <16ms (60fps)
- Drop feedback: <100ms (feels instant)
- API sync: <500ms (acceptable delay)

**Conclusion:** Optimistic updates are essential for fast UI. Visual feedback (drag preview, drop zones) improves UX. Error handling (rollback, toast) ensures reliability.

---

## Opening Statements (Round 1)

### Maya Chen — The Experimental Photographer

*[Shows sequencing workflow]*

Look, I just want to drag images around and see them move instantly. When I drag an image to a new position, it should move immediately—no waiting, no loading spinners. If there's an error, show it clearly, but don't interrupt my flow.

**What I need:** Simple drag-and-drop sequencing. Drag images to reorder, see them move instantly. Add gaps with one click. Delete items easily. No complex UI—just drag and drop.

**Evidence:** I've used Lightroom, InDesign, even physical prints on a wall. The best tools let me drag things around and see them move immediately. Waiting for the server feels slow and breaks my creative flow.

**Why drag-and-drop:**
- Intuitive (everyone understands drag-and-drop)
- Fast (feels instant)
- Creative (supports experimentation)
- Simple (no complex UI)

**Why instant feedback:**
- Feels fast (no waiting)
- Supports experimentation (try many sequences quickly)
- Creative flow (don't interrupt with loading)

**What doesn't work:** Waiting for server response. Too slow, breaks creative flow. Complex UI with too many options—keep it simple.

**Simple UX principle:** Drag images around, see them move instantly. Add gaps, delete items easily. No waiting, no complex UI—just simple drag-and-drop.

---

### Alex Rivera — The Documentary Photographer

*[Shows batch operations workflow]*

I have 200 images from a week-long assignment. I need to sequence them quickly—select 16 images, put them in order, add gaps for pacing. Batch operations are essential—I don't want to wait for each individual operation.

**What I need:** Fast sequencing with batch operations. Select multiple images, add them at once. Reorder quickly with drag-and-drop. Add gaps efficiently. Export options for different print shops.

**Evidence:** I work with professional tools daily—Lightroom, Bridge, even custom tools. The best tools support batch operations—select multiple images, add them at once, reorder quickly.

**Why batch operations:**
- Fast (add multiple images at once)
- Efficient (fewer API calls)
- Professional workflow (supports large image sets)
- Time-saving (don't wait for each operation)

**Why efficient APIs:**
- Fast (batch operations reduce API calls)
- Reliable (consistent state)
- Professional (supports large image sets)

**What doesn't work:** Individual operations for each image. Too slow, too many API calls. No batch operations—inefficient workflow.

**Simple UX principle:** Fast sequencing with batch operations. Select multiple images, add them at once. Reorder quickly with drag-and-drop. Efficient APIs support fast workflow.

---

### Jordan Kim — The Designer/Photographer Hybrid

*[Shows visual feedback patterns]*

I want to experiment freely, but I also need clear visual feedback. When I drag an image, show me where it's going. When I drop it, show me it worked. If there's an error, show it clearly—but don't interrupt my workflow.

**What I need:** Clear visual feedback during sequencing. Drag preview, drop zones, success/error feedback. Visual flow indicators (show sequence order). Template preview (optional, but helpful).

**Evidence:** I've designed similar tools—visual feedback is essential. Users need to see what's happening, where things are going, whether operations succeeded.

**Why visual feedback:**
- Clear (users see what's happening)
- Confident (users know operations worked)
- Professional (supports experimentation)
- Accessible (visual cues help all users)

**Why clear error feedback:**
- Reliable (users know when errors occur)
- Non-intrusive (don't interrupt workflow)
- Actionable (users can fix errors)

**What doesn't work:** No visual feedback. Users don't know what's happening, whether operations worked. Alerts are disruptive—use toast notifications instead.

**Simple UX principle:** Clear visual feedback during sequencing. Drag preview, drop zones, success/error feedback. Visual flow indicators. Toast notifications for errors—non-intrusive, actionable.

---

### Taylor Kim — The UX Designer

*[Shows UX patterns for drag-and-drop]*

Users need immediate feedback. When they drag an image to reorder, they should see it move instantly. Optimistic updates are essential—update UI immediately, sync with server in background.

**What I need:** Optimistic updates for fast UI, clear visual feedback, error handling. Update UI immediately (optimistic), sync with server (background), show errors gracefully (toast notifications).

**Evidence:** UX research shows optimistic updates improve perceived performance. Users notice <100ms delays. Visual feedback (drag preview, drop zones) improves UX.

**Why optimistic updates:**
- Fast UI (feels instant)
- Better UX (no waiting for server)
- Users see immediate feedback
- More confident in the app

**Why visual feedback:**
- Drag preview (shows where item is going)
- Drop zones (shows valid drop targets)
- Loading states (shows sync in progress)
- Success/error feedback (shows operation result)

**Why error handling:**
- Rollback on error (reliable state)
- Toast notifications (non-intrusive)
- Retry options (user control)

**What doesn't work:** Waiting for server response. Too slow, bad UX. No visual feedback—users don't know what's happening.

**Simple UX principle:** Optimistic updates for fast UI, clear visual feedback, error handling. Update UI immediately, sync with server in background, show errors gracefully.

---

### Sam Chen — The Frontend Developer

*[Shows RTK Query patterns]*

I need to implement optimistic updates efficiently. RTK Query supports optimistic updates via `onQueryStarted`. Can update cache immediately, rollback on error. But requires careful implementation.

**What I can implement:**
- Optimistic updates (`onQueryStarted`, update cache immediately)
- Error rollback (revert optimistic update on error)
- Toast notifications (use existing toast system)
- Batch operations (debounce rapid reordering)

**The question:** What optimistic update pattern should we use for sequencing?

**My perspective:** Optimistic updates for reordering (update cache immediately), batch API calls (debounce rapid reordering), error rollback (revert on error), toast notifications (show errors gracefully).

**Why optimistic updates:**
- Fast UI (feels instant)
- RTK Query supports it (`onQueryStarted`)
- Better UX (no waiting for server)
- Rollback on error (reliable state)

**Why batch operations:**
- Efficient (fewer API calls)
- Debounce rapid reordering (batch multiple drags)
- Professional workflow (supports large image sets)

**Why error handling:**
- Rollback on error (revert optimistic update)
- Toast notifications (non-intrusive)
- Retry options (user control)

**What doesn't work:** Waiting for server response. Too slow, bad UX. No optimistic updates—UI feels slow.

**Simple UX principle:** Optimistic updates for fast UI, batch operations for efficiency, error rollback for reliability. RTK Query handles this well—fast UI with reliable state.

---

### Jordan Park — The Backend Developer

*[Shows API design patterns]*

The backend should be fast and efficient. Batch reordering is already implemented (`PUT /items` with full item list). But we need to ensure efficient API design—fast responses, consistent state.

**What I can provide:**
- Batch reordering (`PUT /items` with full item list)
- Individual add (`POST /items`)
- Individual delete (`DELETE /items/{position}`)
- Fast responses (optimized queries, proper indexing)

**The question:** What API pattern should we use for sequencing?

**My perspective:** Batch reordering for efficiency (single API call for full reorder), individual operations for add/delete (simpler API), fast responses (optimized queries), consistent state (transaction safety).

**Why batch reordering:**
- Efficient (single API call for full reorder)
- Consistent state (atomic operation)
- Fast (optimized queries)
- Professional workflow (supports large image sets)

**Why individual operations:**
- Simple API (add/delete are straightforward)
- Clear semantics (each operation is explicit)
- Fast (optimized queries)

**Why fast responses:**
- Optimized queries (proper indexing)
- Efficient data model (normalized, consistent)
- Transaction safety (consistent state)

**What doesn't work:** Too many API calls. Inefficient, slow. No batch operations—inefficient workflow.

**Simple UX principle:** Batch reordering for efficiency, individual operations for add/delete, fast responses for good UX. Efficient APIs support fast workflow.

---

## Rebuttals (Round 2)

### Maya Chen — Rebuttal

*[Responds to Alex, Jordan Kim, Taylor, Sam, Jordan Park]*

All of you make good points, but let me clarify the user reality. I just want to drag images around and see them move instantly. Batch operations are fine, but don't complicate the UI. Visual feedback is good, but don't interrupt my flow.

**To Alex:** Batch operations are good, but don't complicate the UI. I want simple drag-and-drop—select images, drag them around, see them move instantly.

**To Jordan Kim:** Visual feedback is good, but don't interrupt my flow. Drag preview is fine, but don't show loading spinners during drag—that breaks my creative flow.

**To Taylor:** Optimistic updates sound good, but make sure they're fast. I don't want to see loading states during drag—just move the image instantly.

**To Sam:** Optimistic updates are good, but make sure they're reliable. If there's an error, rollback and show it clearly—but don't interrupt my flow.

**To Jordan Park:** Batch reordering is good, but make sure it's fast. I don't want to wait for the server—update UI immediately, sync in background.

**What I'm proposing:** Simple drag-and-drop sequencing. Drag images to reorder, see them move instantly (optimistic updates). Add gaps, delete items easily. Visual feedback (drag preview, drop zones), but don't interrupt flow. Error handling (rollback, toast), but don't interrupt workflow.

**Simple UX principle:** Simple drag-and-drop sequencing. Drag images around, see them move instantly. Visual feedback, but don't interrupt flow. Error handling, but don't interrupt workflow.

---

### Alex Rivera — Rebuttal

*[Responds to Maya, Jordan Kim, Taylor, Sam, Jordan Park]*

All of you make good points, but let me clarify the professional workflow reality. I need fast sequencing with batch operations. Optimistic updates are good, but batch operations are essential for large image sets.

**To Maya:** Simple drag-and-drop is good, but batch operations are essential. I have 200 images—I need to select multiple images, add them at once, reorder quickly.

**To Jordan Kim:** Visual feedback is good, but batch operations are more important. I need to see which images are selected, add them at once, reorder quickly.

**To Taylor:** Optimistic updates are good, but batch operations are essential. I need to add multiple images at once, reorder quickly, see results immediately.

**To Sam:** Optimistic updates are good, but batch operations are essential. Debounce rapid reordering, batch API calls, but also support batch add operations.

**To Jordan Park:** Batch reordering is good, but also support batch add operations. I need to add multiple images at once, not one at a time.

**What I'm proposing:** Fast sequencing with batch operations. Select multiple images, add them at once (batch add). Reorder quickly with drag-and-drop (optimistic updates, batch reordering). Visual feedback (selection indicators, drag preview), but prioritize batch operations.

**Simple UX principle:** Fast sequencing with batch operations. Select multiple images, add them at once. Reorder quickly with drag-and-drop. Optimistic updates for fast UI, batch operations for efficiency.

---

### Jordan Kim — Rebuttal

*[Responds to Maya, Alex, Taylor, Sam, Jordan Park]*

All of you make good points, but let me clarify the design reality. Visual feedback is essential—users need to see what's happening, where things are going, whether operations worked.

**To Maya:** Simple drag-and-drop is good, but visual feedback is essential. Drag preview, drop zones, success/error feedback—these don't interrupt flow, they enhance it.

**To Alex:** Batch operations are good, but visual feedback is essential. Show selection indicators, drag preview, success/error feedback—these help users understand what's happening.

**To Taylor:** Optimistic updates are good, but visual feedback is essential. Drag preview, drop zones, loading states, success/error feedback—these improve UX.

**To Sam:** Optimistic updates are good, but visual feedback is essential. Show drag preview, drop zones, loading states, success/error feedback—these help users understand what's happening.

**To Jordan Park:** Batch reordering is good, but visual feedback is essential. Show loading states during sync, success/error feedback—these help users understand what's happening.

**What I'm proposing:** Clear visual feedback during sequencing. Drag preview (shows where item is going), drop zones (shows valid drop targets), loading states (shows sync in progress), success/error feedback (shows operation result). Optimistic updates for fast UI, visual feedback for clarity.

**Simple UX principle:** Clear visual feedback during sequencing. Drag preview, drop zones, loading states, success/error feedback. Optimistic updates for fast UI, visual feedback for clarity.

---

### Taylor Kim — Rebuttal

*[Responds to Maya, Alex, Jordan Kim, Sam, Jordan Park]*

All of you make good points, but let me clarify the UX reality. Optimistic updates are essential for fast UI, but visual feedback is also important. Users need to see what's happening, where things are going, whether operations worked.

**To Maya:** Simple drag-and-drop is good, but optimistic updates are essential. Update UI immediately, sync with server in background—this feels fast without interrupting flow.

**To Alex:** Batch operations are good, but optimistic updates are essential. Update UI immediately, sync with server in background—this feels fast even with batch operations.

**To Jordan Kim:** Visual feedback is good, but optimistic updates are essential. Update UI immediately, sync with server in background—this feels fast with visual feedback.

**To Sam:** Optimistic updates are good, but visual feedback is also important. Show drag preview, drop zones, loading states, success/error feedback—these improve UX.

**To Jordan Park:** Batch reordering is good, but optimistic updates are essential. Update UI immediately, sync with server in background—this feels fast even with batch operations.

**What I'm proposing:** Optimistic updates for fast UI, visual feedback for clarity, error handling for reliability. Update UI immediately (optimistic), sync with server (background), show visual feedback (drag preview, drop zones, loading states), handle errors gracefully (rollback, toast notifications).

**Simple UX principle:** Optimistic updates for fast UI, visual feedback for clarity, error handling for reliability. Fast UI with clear feedback and reliable state.

---

### Sam Chen — Rebuttal

*[Responds to Maya, Alex, Jordan Kim, Taylor, Jordan Park]*

All of you make good points, but let me clarify the technical reality. RTK Query supports optimistic updates via `onQueryStarted`. Can update cache immediately, rollback on error. But requires careful implementation.

**To Maya:** Simple drag-and-drop is good, but optimistic updates are essential. RTK Query `onQueryStarted` handles this well—update cache immediately, rollback on error.

**To Alex:** Batch operations are good, but optimistic updates are essential. Debounce rapid reordering, batch API calls, but also update cache immediately for fast UI.

**To Jordan Kim:** Visual feedback is good, but optimistic updates are essential. RTK Query `onQueryStarted` handles this well—update cache immediately, show visual feedback, rollback on error.

**To Taylor:** Optimistic updates are good, but visual feedback is also important. RTK Query `onQueryStarted` handles this well—update cache immediately, show visual feedback, rollback on error.

**To Jordan Park:** Batch reordering is good, but optimistic updates are essential. RTK Query `onQueryStarted` handles this well—update cache immediately, sync with server, rollback on error.

**What I'm proposing:** Optimistic updates via RTK Query `onQueryStarted`, batch operations (debounce rapid reordering), visual feedback (drag preview, drop zones, loading states), error handling (rollback, toast notifications). Fast UI with reliable state and clear feedback.

**Simple UX principle:** Optimistic updates via RTK Query `onQueryStarted`, batch operations for efficiency, visual feedback for clarity, error handling for reliability. Fast UI with reliable state and clear feedback.

---

### Jordan Park — Rebuttal

*[Responds to Maya, Alex, Jordan Kim, Taylor, Sam]*

All of you make good points, but let me clarify the backend reality. Batch reordering is already implemented (`PUT /items` with full item list). Fast responses, consistent state. But we need to ensure efficient API design.

**To Maya:** Simple drag-and-drop is good, but efficient APIs are essential. Batch reordering is fast, consistent state—this supports fast UI.

**To Alex:** Batch operations are good, and batch reordering is already implemented. Fast responses, consistent state—this supports fast workflow.

**To Jordan Kim:** Visual feedback is good, but efficient APIs are essential. Fast responses, consistent state—this supports visual feedback.

**To Taylor:** Optimistic updates are good, but efficient APIs are essential. Fast responses, consistent state—this supports optimistic updates.

**To Sam:** Optimistic updates are good, but efficient APIs are essential. Batch reordering is fast, consistent state—this supports optimistic updates.

**What I'm proposing:** Batch reordering for efficiency (single API call for full reorder), individual operations for add/delete (simple API), fast responses (optimized queries), consistent state (transaction safety). Efficient APIs support fast UI and reliable state.

**Simple UX principle:** Batch reordering for efficiency, individual operations for add/delete, fast responses for good UX. Efficient APIs support fast UI and reliable state.

---

## Wildcard Interruptions

### `web/src/store/` — Point of Order!

*[Interjects on state management]*

All of you are thinking about different layers, but let me clarify the state management reality. Optimistic updates require careful state management—update cache immediately, rollback on error.

**What RTK Query provides:**
- Optimistic updates (`onQueryStarted`, update cache immediately)
- Error rollback (revert optimistic update on error)
- Cache invalidation (refetch on error)
- Error handling (catch errors, display feedback)

**The question:** What state consistency pattern should we use for sequencing?

**My perspective:** Optimistic updates with rollback on error. Update cache immediately (optimistic), rollback on error (reliable state). Use RTK Query `onQueryStarted` for optimistic updates, toast notifications for errors.

**Why optimistic updates:**
- Fast UI (feels instant)
- RTK Query supports it (`onQueryStarted`)
- Better UX (no waiting for server)
- Rollback on error (reliable state)

**Why rollback on error:**
- Reliable state (don't show incorrect data)
- User sees error (toast notification)
- Can retry (user control)

**What doesn't work:** Waiting for server response. Too slow, bad UX. No error rollback—unreliable state.

**Simple UX principle:** Optimistic updates with rollback on error. Update cache immediately, rollback on error. RTK Query handles this well—fast UI with reliable state.

---

### `pkg/serve/` — Point of Order!

*[Interjects on API design]*

All of you are thinking about different layers, but let me clarify the API design reality. Batch reordering is already implemented (`PUT /items` with full item list). Fast responses, consistent state.

**What the API provides:**
- Batch reordering (`PUT /items` with full item list)
- Individual add (`POST /items`)
- Individual delete (`DELETE /items/{position}`)
- Fast responses (optimized queries)

**The question:** What API pattern should we use for sequencing?

**My perspective:** Batch reordering for efficiency (single API call for full reorder), individual operations for add/delete (simple API), fast responses (optimized queries), consistent state (transaction safety).

**Why batch reordering:**
- Efficient (single API call for full reorder)
- Consistent state (atomic operation)
- Fast (optimized queries)
- Professional workflow (supports large image sets)

**Why individual operations:**
- Simple API (add/delete are straightforward)
- Clear semantics (each operation is explicit)
- Fast (optimized queries)

**What doesn't work:** Too many API calls. Inefficient, slow. No batch operations—inefficient workflow.

**Simple UX principle:** Batch reordering for efficiency, individual operations for add/delete, fast responses for good UX. Efficient APIs support fast UI and reliable state.

---

### RTK Toolkit — Point of Order!

*[Interjects on RTK Query patterns for sequencing]*

All of you are discussing optimistic updates and state management, but let me clarify what RTK Query actually provides for sequencing operations and how to use it correctly.

**What RTK Query provides for sequencing:**
- **`onQueryStarted`:** Lifecycle hook for optimistic updates, runs before query executes
- **`patchResult.undo()`:** Automatic rollback mechanism for optimistic updates
- **`api.util.updateQueryData()`:** Manual cache updates for fine-grained control
- **Tag-based invalidation:** `providesTags` and `invalidatesTags` for automatic refetch
- **Selective invalidation:** Invalidate by ID, by LIST, or by custom tags

**The question:** How should we use RTK Query patterns for sequencing operations?

**My perspective:** Use optimistic updates with `onQueryStarted` and `patchResult.undo()` for fast UI and reliable state. Use tag-based invalidation for consistency. Combine both patterns—optimistic update + tag invalidation = fast UI + reliable state.

**Why optimistic updates for sequencing:**
- Fast UI (feels instant, drag-and-drop feels responsive)
- RTK Query provides `onQueryStarted` hook
- Automatic rollback with `patchResult.undo()` on error
- Better UX (no waiting for server, supports rapid reordering)

**Why tag-based invalidation:**
- Automatic refetch (consistency after mutations)
- Simple implementation (just tags)
- Works well with optimistic updates
- Selective invalidation (by ID, by LIST)

**Best practices for sequencing:**
- Use optimistic updates for reordering (fast UI, feels instant)
- Use tag invalidation for consistency (after mutations)
- Combine both: Optimistic update + tag invalidation
- Return full objects from mutations (enables optimistic updates)
- Use `transformResponse` for data normalization
- Debounce rapid reordering (batch multiple drags into single API call)

**Anti-patterns to avoid:**
- Forgetting `patchResult.undo()` on error (stale cache, incorrect state)
- Over-invalidating tags (unnecessary refetches, slow UI)
- Not using optimistic updates when appropriate (slow UI, bad UX)
- Returning minimal responses (requires refetch, can't do optimistic updates)
- Not debouncing rapid reordering (too many API calls, inefficient)

**Implementation pattern for reordering:**
```typescript
reorderImageSequenceItems: builder.mutation({
  query: ({ sequenceId, items }) => ({
    url: `/image-sequences/${encodeURIComponent(sequenceId)}/items`,
    method: 'PUT',
    body: { items },
  }),
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
    try {
      await queryFulfilled;
      // Server response automatically replaces optimistic update
    } catch (error) {
      // Automatic rollback on error
      patchResult.undo();
      // Show error notification (toast)
    }
  },
  invalidatesTags: (_result, _error, { sequenceId }) => [
    { type: 'ImageSequenceItems', id: sequenceId },
  ],
}),
```

**What doesn't work:** Waiting for server response. Too slow, bad UX. Not using `patchResult.undo()`—stale cache on error. Over-invalidating tags—unnecessary refetches. Not debouncing rapid reordering—too many API calls.

**Simple UX principle:** Use RTK Query patterns correctly for sequencing. Optimistic updates with `onQueryStarted` and `patchResult.undo()` for fast UI and reliable state. Tag-based invalidation for consistency. Debounce rapid reordering. Combine all patterns—fast UI with reliable state and efficient API usage.

---

## Moderator Summary

### Key Arguments

1. **Maya Chen:** Simple drag-and-drop sequencing. Drag images around, see them move instantly. Visual feedback, but don't interrupt flow. Error handling, but don't interrupt workflow.

2. **Alex Rivera:** Fast sequencing with batch operations. Select multiple images, add them at once. Reorder quickly with drag-and-drop. Optimistic updates for fast UI, batch operations for efficiency.

3. **Jordan Kim:** Clear visual feedback during sequencing. Drag preview, drop zones, loading states, success/error feedback. Optimistic updates for fast UI, visual feedback for clarity.

4. **Taylor Kim:** Optimistic updates for fast UI, visual feedback for clarity, error handling for reliability. Update UI immediately, sync with server in background, show visual feedback, handle errors gracefully.

5. **Sam Chen:** Optimistic updates via RTK Query `onQueryStarted`, batch operations (debounce rapid reordering), visual feedback, error handling (rollback, toast notifications). Fast UI with reliable state and clear feedback.

6. **Jordan Park:** Batch reordering for efficiency, individual operations for add/delete, fast responses for good UX. Efficient APIs support fast UI and reliable state.

7. **RTK Toolkit:** Use RTK Query patterns correctly for sequencing. Optimistic updates with `onQueryStarted` and `patchResult.undo()` for fast UI and reliable state. Tag-based invalidation for consistency. Debounce rapid reordering. Combine all patterns—fast UI with reliable state and efficient API usage.

### Tensions

1. **Optimistic updates:** Update UI immediately vs. wait for server response
2. **Batch operations:** Batch add operations vs. individual operations
3. **Visual feedback:** Drag preview, drop zones, loading states vs. don't interrupt flow
4. **Error handling:** Rollback vs. retry vs. manual recovery

### Interesting Ideas

1. **Optimistic updates:** Update cache immediately, rollback on error
2. **Batch operations:** Debounce rapid reordering, batch API calls
3. **Visual feedback:** Drag preview, drop zones, loading states, success/error feedback
4. **Error handling:** Rollback on error, toast notifications, retry options

### Trade-offs

1. **Wait for server response:**
   - ✅ Guaranteed consistency (no incorrect data)
   - ✅ Simple implementation (no optimistic updates)
   - ❌ Slow UI (waits for server)
   - ❌ Bad UX (users notice delays)

2. **Optimistic updates without rollback:**
   - ✅ Fast UI (feels instant)
   - ✅ Better UX (no waiting for server)
   - ❌ May show incorrect data if error occurs
   - ❌ Unreliable state (no error recovery)

3. **Optimistic updates with rollback:**
   - ✅ Fast UI (optimistic updates)
   - ✅ Reliable state (rollback on error)
   - ✅ Better UX (no waiting for server)
   - ❌ More complex implementation (need to coordinate rollback)

4. **Batch operations:**
   - ✅ Efficient (fewer API calls)
   - ✅ Professional workflow (supports large image sets)
   - ❌ More complex implementation (need to batch operations)
   - ❌ May delay individual operations

5. **Visual feedback:**
   - ✅ Clear (users see what's happening)
   - ✅ Confident (users know operations worked)
   - ❌ May interrupt flow if overdone
   - ❌ Need to implement (drag preview, drop zones, loading states)

### Open Questions

1. **Optimistic updates:** Which operations should be optimistic? All? Some?
2. **Batch operations:** Should we support batch add operations? Or just batch reordering?
3. **Visual feedback:** How much visual feedback is too much? When does it interrupt flow?
4. **Error handling:** Rollback vs. retry vs. manual recovery? Which pattern works best?

### Next Steps

1. **Prototype:** Build optimistic updates for reordering
2. **Prototype:** Build visual feedback (drag preview, drop zones, loading states)
3. **Prototype:** Build error handling (rollback, toast notifications)
4. **Benchmark:** Measure UI responsiveness (optimistic vs. server response)
5. **Test:** See which pattern photographers prefer

### Consensus

- ✅ Fast UI is essential (optimistic updates feel instant)
- ✅ Reliable state is essential (rollback on error ensures consistency)
- ✅ Clear visual feedback is essential (drag preview, drop zones, loading states)
- ✅ Batch operations are valuable (efficient, professional workflow)
- ❓ Should we use optimistic updates for all operations or some?
- ❓ Should we support batch add operations or just batch reordering?

### Data Needed

- Performance benchmarks (optimistic updates vs. server response)
- User testing of visual feedback patterns (drag preview, drop zones, loading states)
- Analysis of batch operation patterns (batch add vs. individual add)
- Research on RTK Query optimistic update patterns for sequencing

### Simple UX Principle Applied

**Key insight:** We want simple, streamlined UX. Fast UI is essential, but reliable state and clear visual feedback are also important. Optimistic updates with rollback, visual feedback, and batch operations balance all three.

**Recommendation:** Optimistic updates for reordering (update cache immediately, rollback on error), visual feedback (drag preview, drop zones, loading states), batch operations (debounce rapid reordering, batch API calls), error handling (rollback, toast notifications). Use RTK Query `onQueryStarted` for optimistic updates, existing toast system for errors, batch reordering API for efficiency.

**Rationale:**
- Optimistic updates provide fast UI (feels instant, better UX)
- Rollback on error ensures reliable state (don't show incorrect data, consistency)
- Visual feedback provides clarity (drag preview, drop zones, loading states, success/error feedback)
- Batch operations provide efficiency (fewer API calls, professional workflow)
- RTK Query supports optimistic updates (`onQueryStarted`, update cache immediately)
- Existing toast system can be used (replace `alert()` calls with toast notifications)
- Batch reordering API is already implemented (efficient, consistent state)
- Simple UX (photographers see instant feedback, reliable state, clear visual feedback)

**Workflow:**
1. User drags image to reorder: Update cache immediately (optimistic update via `onQueryStarted`)
2. Debounce rapid reordering: Batch multiple drags into single API call
3. Call mutation API: `PUT /image-sequences/{id}/items` with batch items
4. On success: Replace optimistic update with server response (already done by RTK Query)
5. On error: Rollback optimistic update (revert cache to previous state), show toast notification
6. User sees: Instant feedback (optimistic update), visual feedback (drag preview, drop zones), reliable state (rollback on error), clear error (toast notification)

**Error handling pattern (RTK Query best practices):**
```typescript
reorderImageSequenceItems: builder.mutation({
  query: ({ sequenceId, items }) => ({
    url: `/image-sequences/${encodeURIComponent(sequenceId)}/items`,
    method: 'PUT',
    body: { items },
  }),
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
    try {
      await queryFulfilled;
      // Server response automatically replaces optimistic update
      // RTK Query handles this automatically
    } catch (error) {
      // Automatic rollback on error - RTK Query provides patchResult.undo()
      patchResult.undo();
      // Show toast notification
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

**Key RTK Query features used:**
- `onQueryStarted`: Lifecycle hook for optimistic updates
- `api.util.updateQueryData()`: Manual cache updates
- `patchResult.undo()`: Automatic rollback on error
- `queryFulfilled`: Promise that resolves/rejects based on API response
- `invalidatesTags`: Automatic refetch after mutation

**Visual feedback:**
- Drag preview: Show dragged item following cursor
- Drop zones: Highlight valid drop targets
- Loading states: Show sync indicator during API call (optional, don't interrupt drag)
- Success/error feedback: Toast notifications for errors, visual confirmation for success

**Batch operations:**
- Debounce rapid reordering: Batch multiple drags into single API call (e.g., 300ms debounce)
- Batch add operations: Support adding multiple images at once (future enhancement)
- Individual operations: Add/delete remain individual (simple API, clear semantics)

---

**End of Debate Round 16**

