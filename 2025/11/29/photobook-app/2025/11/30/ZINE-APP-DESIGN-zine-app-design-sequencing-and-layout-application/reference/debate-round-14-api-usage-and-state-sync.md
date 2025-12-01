---
Title: 'Debate Round 14: Which APIs should be called when, and how should state sync?'
Ticket: ZINE-APP-DESIGN
Status: draft
Topics:
    - debate
    - api-design
    - state-management
    - rtk-query
DocType: reference
Intent: long-term
Owners: []
RelatedFiles: []
Summary: Fourteenth debate round exploring API usage patterns and state synchronization - when to fetch, cache invalidation, optimistic updates
LastUpdated: 2025-11-30T23:50:00-05:00
---

# Debate Round 14: Which APIs should be called when, and how should state sync?

**Question:** Which APIs should be called when, and how should state sync? When to fetch sequences vs. assets vs. layouts? How to handle cache invalidation (tags, refetch)? Optimistic updates vs. wait for server response?

**Primary Candidates:**
- Sam Chen (Frontend Developer) — Argues for RTK Query patterns and cache invalidation
- Jordan Park (Backend Developer) — Argues for API design and response structure
- `web/src/store/` (State Management) — Argues for efficient state updates

**Secondary Participants:**
- RTK Toolkit (The State Management Framework) — Can interject with RTK Query patterns and best practices

**Why this question matters:** Affects UI responsiveness and data consistency between frontend and backend. We want simple UX—fast, reliable state updates that feel instant.

---

## Pre-Debate Research

### Current Codebase Implementation

**Research conducted by:** Sam Chen (Frontend Developer)

**Current RTK Query patterns:**
- Tag-based cache invalidation (`providesTags`, `invalidatesTags`)
- Multiple queries active simultaneously (assets, sequences, sequence detail)
- Mutations invalidate tags, causing automatic refetches
- No optimistic updates currently

**Code reference:**
```363:385:zine-layout/web/src/api.ts
addImageSequenceItem: builder.mutation({
  // ... mutation ...
  invalidatesTags: (_result, _error, { sequenceId }) => [
    { type: 'ImageSequenceItems', id: sequenceId },
  ],
}),
reorderImageSequenceItems: builder.mutation({
  // ... mutation ...
  invalidatesTags: (_result, _error, { sequenceId }) => [
    { type: 'ImageSequenceItems', id: sequenceId },
  ],
}),
```

**Current workflow:**
- Component loads: `useGetAssetsQuery`, `useGetImageSequencesQuery` (parallel)
- User selects sequence: `useGetImageSequenceDetailQuery` (conditional)
- User reorders: `useReorderImageSequenceItemsMutation` → invalidates tags → refetches detail query
- No optimistic updates (UI waits for server response)

**Conclusion:** Current implementation uses tag-based cache invalidation. Mutations invalidate tags, causing automatic refetches. No optimistic updates—UI waits for server response.

### RTK Query Cache Patterns

**Research conducted by:** `web/src/store/` (State Management)

**RTK Query cache strategies:**
1. **Tag-based invalidation:** Mutations invalidate tags, queries refetch automatically
2. **Optimistic updates:** Update cache immediately, rollback on error
3. **Manual refetch:** Call `refetch()` explicitly when needed

**Trade-offs:**
- Tag-based: Automatic, but may cause unnecessary refetches
- Optimistic: Fast UI, but complex error handling
- Manual: Full control, but requires explicit calls

**Conclusion:** Different strategies work for different use cases. Tag-based invalidation is good default, optimistic updates for fast UI.

### RTK Query Features and Patterns

**Research conducted by:** RTK Toolkit (The State Management Framework)

**RTK Query features relevant to this debate:**
1. **`onQueryStarted`:** Lifecycle hook for optimistic updates, runs before query executes
2. **`patchResult.undo()`:** Automatic rollback mechanism for optimistic updates
3. **`api.util.updateQueryData()`:** Manual cache updates for fine-grained control
4. **Tag-based invalidation:** `providesTags` and `invalidatesTags` for automatic refetch
5. **Selective invalidation:** Invalidate by ID, by LIST, or by custom tags
6. **`transformResponse`:** Normalize data before caching
7. **Conditional queries:** `skip` option for conditional fetching

**Optimistic update pattern:**
```typescript
async onQueryStarted(arg, { dispatch, queryFulfilled }) {
  const patchResult = dispatch(
    api.util.updateQueryData('getQuery', arg, (draft) => {
      // Update draft optimistically
    })
  );
  try {
    await queryFulfilled;
  } catch (error) {
    patchResult.undo(); // Automatic rollback
  }
}
```

**Tag invalidation patterns:**
- **By ID:** `{ type: 'Resource', id: resourceId }` — Invalidates specific resource
- **By LIST:** `{ type: 'Resource', id: 'LIST' }` — Invalidates list queries
- **Multiple tags:** Can invalidate multiple tags in one mutation

**Best practices:**
- Use optimistic updates for fast UI (reordering, toggles)
- Use tag invalidation for consistency (after mutations)
- Combine both: Optimistic update + tag invalidation = fast UI + reliable state
- Return full objects from mutations (enables optimistic updates)
- Use `transformResponse` for data normalization

**Anti-patterns to avoid:**
- Forgetting `patchResult.undo()` on error (stale cache)
- Over-invalidating tags (unnecessary refetches)
- Not using optimistic updates when appropriate (slow UI)
- Returning minimal responses (requires refetch, can't do optimistic updates)

**Conclusion:** RTK Query provides powerful patterns for optimistic updates and cache invalidation. Use `onQueryStarted` for optimistic updates, `patchResult.undo()` for rollback, tag-based invalidation for consistency. Combine both patterns for fast UI with reliable state.

---

## Opening Statements (Round 1)

### Sam Chen — The Frontend Developer

*[Shows RTK Query patterns and cache invalidation]*

I need to clarify the technical reality. Current implementation uses RTK Query with tag-based cache invalidation. Mutations invalidate tags, queries refetch automatically. This works, but may cause unnecessary refetches.

**What I can implement:**
- Tag-based invalidation (current, automatic refetch)
- Optimistic updates (update cache immediately, rollback on error)
- Manual refetch (explicit `refetch()` calls)

**The question:** What API usage pattern should we use?

**My perspective:** Optimistic updates for fast UI, tag-based invalidation for consistency. Update cache immediately (optimistic), invalidate tags for refetch (consistency). Best of both worlds—fast UI with reliable state.

**Why optimistic updates:**
- Fast UI (feels instant)
- Better UX (no waiting for server)
- RTK Query supports it (`onQueryStarted`)
- Rollback on error (reliable state)

**Why tag-based invalidation:**
- Automatic refetch (consistency)
- Simple implementation (just tags)
- Works well with optimistic updates

**What doesn't work:** Waiting for server response. Too slow, bad UX. Users notice delays.

**Simple UX principle:** Optimistic updates for fast UI, tag-based invalidation for consistency. Update cache immediately, invalidate tags for refetch. Fast UI with reliable state.

---

### Jordan Park — The Backend Developer

*[Shows API design and response structure]*

Wait, Sam. I need to clarify the backend reality. Current API returns full objects (sequences, items, assets). Mutations return updated data. This enables optimistic updates, but requires careful API design.

**What I can implement:**
- Return full objects (current, enables optimistic updates)
- Return minimal responses (just IDs, requires refetch)
- Batch operations (multiple items in one request)

**The question:** What API response structure should we use?

**My perspective:** Return full objects for mutations. Enables optimistic updates, reduces refetches. Batch operations for efficiency (reorder multiple items in one request).

**Why return full objects:**
- Enables optimistic updates (frontend can update cache immediately)
- Reduces refetches (have data, don't need to refetch)
- Better API design (mutation returns updated state)

**Why batch operations:**
- Efficient (one request instead of many)
- Atomic (all or nothing)
- Better for reordering (update entire sequence at once)

**What doesn't work:** Returning minimal responses. Requires refetch, slower UI. Can't do optimistic updates easily.

**Simple UX principle:** Return full objects for mutations. Enables optimistic updates, reduces refetches. Batch operations for efficiency.

---

### `web/src/store/` — The State Management

*[Shows RTK Query cache strategies]*

Both of you are thinking about different layers, but let me clarify the state management reality. RTK Query handles caching well, but we need to decide what to cache and when to invalidate.

**What RTK Query provides:**
- Automatic caching (queries cached by key)
- Tag-based invalidation (mutations invalidate tags)
- Optimistic updates (`onQueryStarted`, update cache immediately)
- Manual refetch (`refetch()` when needed)

**The question:** What caching strategy should we use?

**My perspective:** Optimistic updates with tag-based invalidation. Update cache immediately (optimistic), invalidate tags for refetch (consistency). RTK Query handles this well.

**Why optimistic updates:**
- Fast UI (feels instant)
- Better UX (no waiting for server)
- RTK Query supports it (`onQueryStarted`)
- Rollback on error (reliable state)

**Why tag-based invalidation:**
- Automatic refetch (consistency)
- Simple implementation (just tags)
- Works well with optimistic updates

**What doesn't work:** Waiting for server response. Too slow, bad UX. Pure tag-based invalidation causes unnecessary refetches.

**Simple UX principle:** Optimistic updates with tag-based invalidation. Update cache immediately, invalidate tags for refetch. RTK Query handles this well—fast UI with reliable state.

---

## Rebuttals (Round 2)

### Sam Chen — Rebuttal

*[Responds to Jordan and `web/src/store/`]*

Both of you make good points, but let me clarify the frontend reality. Optimistic updates require careful implementation—update cache immediately, rollback on error.

**To Jordan:** Returning full objects is good, enables optimistic updates. Batch operations are good for efficiency. But we need to handle errors carefully—rollback optimistic updates on error.

**To `web/src/store/`:** Optimistic updates with tag-based invalidation sounds good. Update cache immediately, invalidate tags for refetch. RTK Query handles this well.

**What I'm proposing:** Optimistic updates for fast UI, tag-based invalidation for consistency. Update cache immediately (optimistic), invalidate tags for refetch (consistency). Handle errors carefully—rollback on error.

**Simple UX principle:** Optimistic updates for fast UI, tag-based invalidation for consistency. Update cache immediately, invalidate tags for refetch. Handle errors carefully—rollback on error.

---

### Jordan Park — Rebuttal

*[Responds to Sam and `web/src/store/`]*

Both of you are thinking about frontend, but let me clarify the backend reality. API design affects frontend patterns. Return full objects, enable batch operations.

**To Sam:** Optimistic updates are good, but require full objects from API. Batch operations reduce requests, better for reordering.

**To `web/src/store/`:** Optimistic updates with tag-based invalidation sounds good, but requires full objects from API. Batch operations enable efficient optimistic updates.

**What I'm proposing:** Return full objects for mutations, enable batch operations. Enables optimistic updates, reduces refetches. Better API design—mutation returns updated state.

**Simple UX principle:** Return full objects for mutations, enable batch operations. Enables optimistic updates, reduces refetches. Better API design.

---

### `web/src/store/` — Rebuttal

*[Responds to Sam and Jordan]*

Both of you are thinking about different layers, but let me clarify the state management reality. RTK Query handles caching well, but we need to coordinate between frontend and backend.

**To Sam:** Optimistic updates are good, but require careful error handling. Tag-based invalidation ensures consistency. RTK Query handles this well.

**To Jordan:** Returning full objects is good, enables optimistic updates. Batch operations are good for efficiency. But we need to coordinate—frontend updates cache, backend returns updated state.

**What I'm proposing:** Optimistic updates with tag-based invalidation. Update cache immediately (optimistic), invalidate tags for refetch (consistency). Coordinate between frontend and backend—frontend updates cache, backend returns updated state.

**Simple UX principle:** Optimistic updates with tag-based invalidation. Update cache immediately, invalidate tags for refetch. Coordinate between frontend and backend.

---

### RTK Toolkit — Point of Order!

*[Interjects on RTK Query patterns and best practices]*

All of you are discussing RTK Query patterns, but let me clarify what RTK Query actually provides and how to use it correctly.

**What RTK Query provides:**
- **`onQueryStarted`:** Lifecycle hook for optimistic updates, runs before query executes
- **`patchResult.undo()`:** Automatic rollback mechanism for optimistic updates
- **Tag-based invalidation:** `providesTags` and `invalidatesTags` for automatic refetch
- **Selective invalidation:** Invalidate by ID, by LIST, or by custom tags
- **`api.util.updateQueryData()`:** Manual cache updates for fine-grained control

**The question:** How should we use RTK Query patterns for sequencing?

**My perspective:** Use optimistic updates with `onQueryStarted` and `patchResult.undo()` for fast UI and reliable state. Use tag-based invalidation for consistency. Combine both patterns—optimistic update + tag invalidation = fast UI + reliable state.

**Why optimistic updates:**
- Fast UI (feels instant)
- RTK Query provides `onQueryStarted` hook
- Automatic rollback with `patchResult.undo()`
- Better UX (no waiting for server)

**Why tag-based invalidation:**
- Automatic refetch (consistency)
- Simple implementation (just tags)
- Works well with optimistic updates
- Selective invalidation (by ID, by LIST)

**Best practices:**
- Use optimistic updates for fast UI (reordering, toggles)
- Use tag invalidation for consistency (after mutations)
- Combine both: Optimistic update + tag invalidation
- Return full objects from mutations (enables optimistic updates)
- Use `transformResponse` for data normalization

**Anti-patterns to avoid:**
- Forgetting `patchResult.undo()` on error (stale cache)
- Over-invalidating tags (unnecessary refetches)
- Not using optimistic updates when appropriate (slow UI)
- Returning minimal responses (requires refetch, can't do optimistic updates)

**What doesn't work:** Waiting for server response. Too slow, bad UX. Not using `patchResult.undo()`—stale cache on error. Over-invalidating tags—unnecessary refetches.

**Simple UX principle:** Use RTK Query patterns correctly. Optimistic updates with `onQueryStarted` and `patchResult.undo()` for fast UI and reliable state. Tag-based invalidation for consistency. Combine both patterns—fast UI with reliable state.

---

## Moderator Summary

### Key Arguments

1. **Sam Chen:** Optimistic updates for fast UI, tag-based invalidation for consistency. Update cache immediately, invalidate tags for refetch. Handle errors carefully—rollback on error.

2. **Jordan Park:** Return full objects for mutations, enable batch operations. Enables optimistic updates, reduces refetches. Better API design—mutation returns updated state.

3. **`web/src/store/`:** Optimistic updates with tag-based invalidation. Update cache immediately, invalidate tags for refetch. RTK Query handles this well—fast UI with reliable state.

4. **RTK Toolkit:** Use RTK Query patterns correctly. Optimistic updates with `onQueryStarted` and `patchResult.undo()` for fast UI and reliable state. Tag-based invalidation for consistency. Combine both patterns—fast UI with reliable state.

### Tensions

1. **API usage:** When to fetch (on mount, on demand, conditional)?
2. **Cache invalidation:** Tag-based vs. manual refetch vs. optimistic updates?
3. **API design:** Return full objects vs. minimal responses vs. batch operations?

### Interesting Ideas

1. **Optimistic updates:** Update cache immediately, rollback on error
2. **Tag-based invalidation:** Automatic refetch when tags invalidated
3. **Batch operations:** Multiple items in one request (efficient, atomic)
4. **Full object responses:** Mutations return updated state (enables optimistic updates)

### Trade-offs

1. **Tag-based invalidation only:**
   - ✅ Automatic refetch (consistency)
   - ✅ Simple implementation (just tags)
   - ❌ May cause unnecessary refetches
   - ❌ Slower UI (waits for server response)

2. **Optimistic updates only:**
   - ✅ Fast UI (feels instant)
   - ✅ Better UX (no waiting for server)
   - ❌ Complex error handling (rollback on error)
   - ❌ May show stale data if error occurs

3. **Optimistic updates + tag-based invalidation:**
   - ✅ Fast UI (optimistic updates)
   - ✅ Reliable state (tag-based invalidation)
   - ✅ Best of both worlds
   - ❌ More complex implementation (need to coordinate)

4. **Batch operations:**
   - ✅ Efficient (one request instead of many)
   - ✅ Atomic (all or nothing)
   - ✅ Better for reordering (update entire sequence at once)
   - ❌ More complex API design

### Open Questions

1. **API usage:** When to fetch sequences vs. assets vs. layouts?
2. **Cache invalidation:** Tag-based vs. manual refetch vs. optimistic updates?
3. **Optimistic updates:** Which operations should be optimistic? All? Some?
4. **Batch operations:** Which operations should be batched? Reordering? Template assignment?

### Next Steps

1. **User research:** Test optimistic updates vs. waiting for server response
2. **Prototype:** Build optimistic updates for reordering
3. **Prototype:** Build batch operations for reordering
4. **Benchmark:** Measure UI responsiveness (optimistic vs. server response)
5. **Test:** See which pattern photographers prefer

### Consensus

- ✅ Fast UI is essential (optimistic updates feel instant)
- ✅ Reliable state is essential (tag-based invalidation ensures consistency)
- ✅ Current implementation uses tag-based invalidation (works, but may be slow)
- ❓ Should we add optimistic updates? Which operations?

### Data Needed

- Performance benchmarks (optimistic updates vs. server response)
- User testing of UI responsiveness
- Analysis of API usage patterns (when to fetch, when to invalidate)
- Research on RTK Query optimistic update patterns

### Simple UX Principle Applied

**Key insight:** We want simple, streamlined UX. Fast UI is essential, but reliable state is also important. Optimistic updates with tag-based invalidation balances both.

**Recommendation:** Optimistic updates with tag-based invalidation. Update cache immediately (optimistic), invalidate tags for refetch (consistency). Return full objects from mutations (enables optimistic updates), enable batch operations (efficient). Handle errors carefully—rollback optimistic updates on error.

**Rationale:**
- Optimistic updates provide fast UI (feels instant, better UX)
- Tag-based invalidation ensures reliable state (automatic refetch, consistency)
- Full object responses enable optimistic updates (have data, don't need to refetch)
- Batch operations are efficient (one request instead of many, atomic)
- Error handling is important (rollback optimistic updates on error)
- Simple UX (photographers see instant feedback, reliable state)

**Workflow:**
1. User reorders items: Update cache immediately (optimistic update via `onQueryStarted`)
2. Call mutation API: `PUT /image-sequences/{id}/items` with batch items
3. Server returns: Full updated items array
4. Update cache: Replace optimistic update with server response (automatic by RTK Query)
5. Invalidate tags: `{ type: 'ImageSequenceItems', id: sequenceId }` (automatic refetch)
6. On error: Rollback optimistic update (`patchResult.undo()`), show error message

**RTK Query implementation pattern:**
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
      // Automatic rollback on error
      patchResult.undo();
      // Show error notification
    }
  },
  invalidatesTags: (_result, _error, { sequenceId }) => [
    { type: 'ImageSequenceItems', id: sequenceId },
  ],
}),
```

---

**End of Debate Round 14**

