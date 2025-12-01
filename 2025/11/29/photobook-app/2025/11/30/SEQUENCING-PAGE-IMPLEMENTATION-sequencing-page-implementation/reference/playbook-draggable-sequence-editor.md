---
Title: Draggable Sequence Editor - Implementation Playbook
Ticket: SEQUENCING-PAGE-IMPLEMENTATION
DocType: playbook
Status: living
Owners: []
Summary: Practical, reusable steps to build a performant, optimistic, debounced drag-and-drop sequence editor with image/gap support and clear visual feedback.
---

## Goals
- Instant-feel drag-and-drop sequencing with optimistic updates and rollback
- Debounced batching of reorder requests to reduce API load
- Clear visual feedback (drag preview, drop indicators, target highlights)
- Support for gaps (blank items) and images
- Error handling (toasts, rollback) without interrupting user flow

## Prerequisites
- Frontend: React 18, TypeScript, RTK Query, Tailwind v4 (or equivalent utility CSS)
- Backend: Batch reorder endpoint (PUT), add item endpoint (POST), delete item endpoint (DELETE)
- Data shape: `ImageSequenceItem { sequence_id, position, asset_id?: string, is_gap: boolean }`

## Architecture Overview
- `SequenceEditor` (container): loads sequence + assets, orchestrates DnD, optimistic reorder, add/delete, UI controls
- `SequenceItem` (cell): renders image or gap, delete affordance, drag affordance
- `AssetPicker` (modal): select assets to add; returns IDs
- `Slideshow` (optional): preview sequence (single-page) and book-spread mode (images + gaps)
- `api` (RTK Query): mutations for reorder/add/delete with optimistic updates and rollback

## RTK Query Setup (Optimistic Updates)
Key pattern for a great UX:
1) Update the cache immediately when user completes a drag
2) Send API request in background (debounced)
3) Roll back on error (and show toast)

Skeleton:
```ts
reorderImageSequenceItems: builder.mutation<
  ImageSequenceItem[],
  { sequenceId: string; items: { assetId?: string; isGap: boolean }[] }
>({
  query: ({ sequenceId, items }) => ({
    url: `/image-sequences/${encodeURIComponent(sequenceId)}/items`,
    method: 'PUT',
    body: { items: items.map(i => ({ asset_id: i.assetId, is_gap: i.isGap })) },
  }),
  transformResponse: (r: { items: ImageSequenceItem[] }) => r.items ?? [],
  async onQueryStarted({ sequenceId, items }, { dispatch, queryFulfilled }) {
    const patch = dispatch(
      api.util.updateQueryData('getImageSequenceDetail', { sequenceId }, (draft) => {
        draft.items = items.map((i, idx) => ({
          sequence_id: sequenceId,
          position: idx,
          asset_id: i.assetId,
          is_gap: i.isGap,
        }));
      }),
    );
    try {
      await queryFulfilled;
    } catch {
      patch.undo();
      dispatch(uiSlice.actions.addToast({
        id: `${Date.now()}`,
        text: 'Failed to reorder items. Please try again.',
        type: 'error',
      }));
    }
  },
  invalidatesTags: (_r, _e, { sequenceId }) => [{ type: 'ImageSequenceItems', id: sequenceId }],
});
```

## Drag-and-Drop Logic (Native HTML5)
- Mark each item container as `draggable`
- Track `dragSourceIndex` on `onDragStart`
- Update potential `dragTargetIndex` on `onDragEnter`
- On `onDrop`, compute the new array order and trigger debounce

Forward/backward insertion rule (off-by-one avoidance):
- Remove the source item first
- Insert at `targetIndex` as-is (no `-1` adjustment) because indices already shifted

Example:
```ts
const handleDrop = (targetIndex: number | null) => {
  if (dragSourceIndex == null) return;
  if (targetIndex == null || targetIndex === dragSourceIndex) { resetDrag(); return; }
  const copy = [...sortedItems];
  const [moved] = copy.splice(dragSourceIndex, 1);
  copy.splice(targetIndex, 0, moved);
  const reordered = copy.map((it, i) => ({ ...it, position: i }));
  debouncedReorder(reordered); // calls mutation after debounce
  resetDrag();
};
```

## Debouncing API Calls
Users frequently perform rapid drags; debounce to batch reorders:
```ts
function debounce<T extends (...args: any[]) => void>(fn: T, wait: number) {
  let t: ReturnType<typeof setTimeout> | null = null;
  return (...args: Parameters<T>) => {
    if (t) clearTimeout(t);
    t = setTimeout(() => { t = null; fn(...args); }, wait);
  };
}
// 300–500ms recommended
```

## Visual Feedback
- Dragged item: lower opacity + slight scale to indicate moving
- Drop target: ring highlight
- Insertion indicator: thin line on left/right of target based on drag direction
- Make overlay helper elements pointer-events: none so they don’t block DnD
- Tailwind v4 tip: use `bg-black/10` over `bg-opacity-*` (removed in v4)

Hints:
- Use `group`/`group-hover` for hover affordances (delete on hover)
- For big grids, consider virtualization if items > ~200

## Gaps Support
- Represent gaps with `is_gap: true`
- Rendering: show labeled blank tile; deletion should work same as images
- Reordering: treat uniformly in computations (same list, keep `is_gap`)

## Error Handling
- Non-blocking error toasts (no modal alerts)
- Rollback optimistic cache on mutation error
- Optionally add a Retry button to repeat last action

## Slideshow / Book-Spread Preview (Optional)
- Single mode: filter to images only
- Spread mode: pair consecutive items; blank pages for gaps (or missing right page)
- Maintain an anchor sequence position so switching modes keeps user context
- Fullscreen via browser API (`requestFullscreen`/`exitFullscreen`)
- Avoid “temporal dead zone” by defining callbacks before effects that use them

## Common Pitfalls
- Off-by-one when dragging forward (don’t subtract 1 after removing source)
- Hook ordering errors (“can’t access lexical declaration before initialization”):
  - Define `useCallback` handlers before `useEffect` that capture them
- Tailwind v4 opacity utilities changed (use `/alpha` syntax)
- Over-invalidating RTK Query tags causing unnecessary refetches

## Testing Checklist
- Drag forward/backward across various positions, including ends
- Rapid sequential drags (ensure single PUT call after debounce)
- Add multiple images; add/delete gap; delete image
- Network failure during reorder/add/delete triggers rollback + toast
- Slideshow keyboard navigation and fullscreen toggle
- Responsive grid (e.g., `grid-cols-2 md:grid-cols-3 ...`)

## Minimal Component Contract
- `SequenceEditor`: props { projectId, sequenceId }
- Data hooks: `useGetImageSequenceDetailQuery`, assets list
- Mutations: `reorderImageSequenceItems`, `addImageSequenceItem`, `deleteImageSequenceItem`
- UI: Buttons for “Add Image”, “Add Gap”, indicators for “Saving order…”

## Reuse Guidance
- Keep reorder logic isolated so other editors (e.g., page layout) can import it
- Export a small drag utilities module (compute insert index, debounce)
- Keep RTK optimistic update pattern consistent across features

## References
- Sequencing UX Walkthrough (visual states and flows)
- Debate Round 16 (optimistic updates, batching, feedback patterns)


