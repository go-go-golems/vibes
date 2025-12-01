---
Title: 'Debate Round 15: How should the frontend handle optimistic updates and error recovery?'
Ticket: ZINE-APP-DESIGN
Status: draft
Topics:
    - debate
    - optimistic-updates
    - error-handling
    - user-feedback
DocType: reference
Intent: long-term
Owners: []
RelatedFiles: []
Summary: Fifteenth debate round exploring optimistic updates and error recovery - immediate feedback vs wait for server, error display patterns
LastUpdated: 2025-11-30T23:55:00-05:00
---

# Debate Round 15: How should the frontend handle optimistic updates and error recovery?

**Question:** How should the frontend handle optimistic updates and error recovery? Optimistic updates (update UI immediately, rollback on error)? Wait for server response (slower, but guaranteed consistency)? How to handle errors (toast notifications, inline errors, rollback)?

**Primary Candidates:**
- Taylor Kim (UX Designer) — Argues for immediate feedback and graceful error handling
- Sam Chen (Frontend Developer) — Argues for RTK Query optimistic updates
- `web/src/store/` (State Management) — Argues for state consistency

**Why this question matters:** Affects perceived performance and user confidence in the app. We want simple UX—fast, reliable updates that feel instant, with clear error feedback.

---

## Pre-Debate Research

### Current Codebase Implementation

**Research conducted by:** Sam Chen (Frontend Developer)

**Current approach:**
- No optimistic updates (mutations wait for server response)
- Error handling via `try/catch` with `alert()` calls
- Toast system exists (`uiSlice` with toasts) but not used for errors
- Mutations use `.unwrap()` which throws errors

**Code reference:**
```126:138:zine-layout/web/src/views/tabs/SequencesTab.tsx
const handleCreateSequence = async (event: React.FormEvent) => {
  event.preventDefault();
  if (!projectId || !newSequenceName.trim()) return;
  const sequence = await createSequence({
    projectId,
    name: newSequenceName.trim(),
    description: newSequenceDescription.trim() || undefined,
  }).unwrap();
  setNewSequenceName('');
  setNewSequenceDescription('');
  setIsCreatingSequence(false);
  setSelectedSequenceId(sequence.id);
};
```

**Error handling example:**
```81:96:zine-layout/web/src/views/LaidOutImageViewer.tsx
try {
  const created = await createImage({
    projectId,
    assetId: createAsset,
    templateId: createTemplate,
    overrides,
  }).unwrap();
  // ... success handling ...
} catch (err) {
  alert((err as Error).message);
}
```

**Toast system:**
```4:15:zine-layout/web/src/store.ts
const uiSlice = createSlice({
  name: 'ui',
  initialState: { toasts: [] as { id: string; text: string; type?: 'info' | 'error' }[] },
  reducers: {
    addToast: (s, a) => {
      s.toasts.push(a.payload);
    },
    removeToast: (s, a) => {
      s.toasts = s.toasts.filter((t) => t.id !== a.payload);
    },
  },
});
```

**Conclusion:** Current implementation waits for server response, uses `alert()` for errors. Toast system exists but not used. No optimistic updates—UI feels slow.

### RTK Query Optimistic Updates

**Research conducted by:** `web/src/store/` (State Management)

**RTK Query optimistic update patterns:**
1. **`onQueryStarted`:** Update cache immediately, rollback on error
2. **`onCacheEntryAdded`:** Handle streaming updates
3. **Manual cache updates:** Use `dispatch(api.util.updateQueryData())`

**Error recovery patterns:**
1. **Rollback:** Revert optimistic update on error
2. **Retry:** Automatically retry failed mutations
3. **Error display:** Toast notifications, inline errors, error boundaries

**Conclusion:** RTK Query supports optimistic updates via `onQueryStarted`. Can update cache immediately, rollback on error. Error recovery requires careful implementation.

---

## Opening Statements (Round 1)

### Taylor Kim — The UX Designer

*[Shows immediate feedback patterns]*

Look, users need immediate feedback. When they drag an image to reorder, they should see it move instantly. If there's an error, show it clearly—don't interrupt with alerts.

**What I need:** Optimistic updates for fast UI, clear error feedback. Update UI immediately (feels instant), show errors gracefully (toast notifications, inline errors). Don't use alerts—they're disruptive.

**Evidence:** UX research shows optimistic updates improve perceived performance. Users notice <100ms delays. Alerts are disruptive—toast notifications are better.

**Why optimistic updates:**
- Fast UI (feels instant)
- Better UX (no waiting for server)
- Users see immediate feedback
- More confident in the app

**Why clear error feedback:**
- Toast notifications (non-intrusive)
- Inline errors (contextual)
- Rollback on error (reliable state)
- Retry options (user control)

**What doesn't work:** Waiting for server response. Too slow, bad UX. Alerts are disruptive—use toast notifications instead.

**Simple UX principle:** Optimistic updates for fast UI, clear error feedback. Update UI immediately, show errors gracefully. Don't use alerts—use toast notifications.

---

### Sam Chen — The Frontend Developer

*[Shows RTK Query optimistic update patterns]*

I need to clarify the technical reality. RTK Query supports optimistic updates via `onQueryStarted`. Can update cache immediately, rollback on error. But requires careful implementation.

**What I can implement:**
- Optimistic updates (`onQueryStarted`, update cache immediately)
- Error rollback (revert optimistic update on error)
- Toast notifications (use existing toast system)
- Retry logic (automatic or manual)

**The question:** What optimistic update pattern should we use?

**My perspective:** Optimistic updates for fast UI, rollback on error for reliability. Use RTK Query `onQueryStarted` to update cache immediately, rollback on error. Toast notifications for errors.

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

**Simple UX principle:** Optimistic updates for fast UI, rollback on error for reliability. Use RTK Query `onQueryStarted`, toast notifications for errors.

---

### `web/src/store/` — The State Management

*[Shows state consistency patterns]*

Both of you are thinking about different layers, but let me clarify the state management reality. Optimistic updates require careful state management—update cache immediately, rollback on error.

**What RTK Query provides:**
- Optimistic updates (`onQueryStarted`, update cache immediately)
- Error rollback (revert optimistic update on error)
- Cache invalidation (refetch on error)
- Error handling (catch errors, display feedback)

**The question:** What state consistency pattern should we use?

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

## Rebuttals (Round 2)

### Taylor Kim — Rebuttal

*[Responds to Sam and `web/src/store/`]*

Both of you make good points, but let me clarify the UX reality. Optimistic updates are essential for fast UI, but error handling is also important. Toast notifications are better than alerts.

**To Sam:** Optimistic updates are good, but we need clear error feedback. Toast notifications are better than alerts—non-intrusive, contextual.

**To `web/src/store/`:** Optimistic updates with rollback sound good, but we need clear error feedback. Toast notifications show errors gracefully, don't interrupt workflow.

**What I'm proposing:** Optimistic updates for fast UI, rollback on error for reliability, toast notifications for errors. Best of both worlds—fast UI with clear error feedback.

**Simple UX principle:** Optimistic updates for fast UI, rollback on error for reliability, toast notifications for errors. Fast UI with clear error feedback.

---

### Sam Chen — Rebuttal

*[Responds to Taylor and `web/src/store/`]*

Both of you make good points, but let me clarify the technical reality. RTK Query supports optimistic updates via `onQueryStarted`. Can update cache immediately, rollback on error.

**To Taylor:** Toast notifications are good, but we need to implement them. Current codebase uses `alert()`—need to replace with toast notifications.

**To `web/src/store/`:** Optimistic updates with rollback sound good, but we need to implement them. RTK Query `onQueryStarted` handles this well.

**What I'm proposing:** Optimistic updates via RTK Query `onQueryStarted`, rollback on error, toast notifications for errors. Use existing toast system, implement optimistic updates.

**Simple UX principle:** Optimistic updates via RTK Query `onQueryStarted`, rollback on error, toast notifications for errors. Fast UI with reliable state and clear error feedback.

---

### `web/src/store/` — Rebuttal

*[Responds to Taylor and Sam]*

Both of you are thinking about different layers, but let me clarify the state management reality. Optimistic updates require careful state management—update cache immediately, rollback on error.

**To Taylor:** Toast notifications are good, but we need to coordinate with state management. Use existing toast system, dispatch toast actions on error.

**To Sam:** Optimistic updates via RTK Query `onQueryStarted` are good, but we need to coordinate with error handling. Rollback on error, dispatch toast actions.

**What I'm proposing:** Optimistic updates via RTK Query `onQueryStarted`, rollback on error, toast notifications via existing toast system. Coordinate between RTK Query and toast system.

**Simple UX principle:** Optimistic updates via RTK Query `onQueryStarted`, rollback on error, toast notifications via existing toast system. Fast UI with reliable state and clear error feedback.

---

## Moderator Summary

### Key Arguments

1. **Taylor Kim:** Optimistic updates for fast UI, rollback on error for reliability, toast notifications for errors. Fast UI with clear error feedback.

2. **Sam Chen:** Optimistic updates via RTK Query `onQueryStarted`, rollback on error, toast notifications for errors. Use existing toast system, implement optimistic updates.

3. **`web/src/store/`:** Optimistic updates via RTK Query `onQueryStarted`, rollback on error, toast notifications via existing toast system. Coordinate between RTK Query and toast system.

### Tensions

1. **Optimistic updates:** Update UI immediately vs. wait for server response
2. **Error handling:** Toast notifications vs. inline errors vs. alerts
3. **Error recovery:** Rollback vs. retry vs. manual recovery

### Interesting Ideas

1. **Optimistic updates:** Update cache immediately, rollback on error
2. **Toast notifications:** Non-intrusive error feedback
3. **Rollback on error:** Revert optimistic update, show error
4. **Retry logic:** Automatic or manual retry options

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

4. **Toast notifications:**
   - ✅ Non-intrusive (don't interrupt workflow)
   - ✅ Contextual (show errors where they occur)
   - ✅ Better UX (no alerts)
   - ❌ Need to implement (replace `alert()` calls)

### Open Questions

1. **Optimistic updates:** Which operations should be optimistic? All? Some?
2. **Error handling:** Toast notifications vs. inline errors vs. alerts?
3. **Error recovery:** Rollback vs. retry vs. manual recovery?
4. **Retry logic:** Automatic retry vs. manual retry vs. no retry?

### Next Steps

1. **User research:** Test optimistic updates vs. waiting for server response
2. **Prototype:** Build optimistic updates for reordering
3. **Prototype:** Build toast notification system
4. **Benchmark:** Measure UI responsiveness (optimistic vs. server response)
5. **Test:** See which pattern photographers prefer

### Consensus

- ✅ Fast UI is essential (optimistic updates feel instant)
- ✅ Reliable state is essential (rollback on error ensures consistency)
- ✅ Clear error feedback is essential (toast notifications are better than alerts)
- ❓ Should we use optimistic updates for all operations or some?

### Data Needed

- Performance benchmarks (optimistic updates vs. server response)
- User testing of error handling patterns (toast vs. inline vs. alerts)
- Analysis of error recovery patterns (rollback vs. retry)
- Research on RTK Query optimistic update patterns

### Simple UX Principle Applied

**Key insight:** We want simple, streamlined UX. Fast UI is essential, but reliable state and clear error feedback are also important. Optimistic updates with rollback and toast notifications balance all three.

**Recommendation:** Optimistic updates for fast UI, rollback on error for reliability, toast notifications for errors. Use RTK Query `onQueryStarted` for optimistic updates, rollback on error, use existing toast system for error feedback. Fast UI with reliable state and clear error feedback.

**Rationale:**
- Optimistic updates provide fast UI (feels instant, better UX)
- Rollback on error ensures reliable state (don't show incorrect data, consistency)
- Toast notifications provide clear error feedback (non-intrusive, contextual, better than alerts)
- RTK Query supports optimistic updates (`onQueryStarted`, update cache immediately)
- Existing toast system can be used (replace `alert()` calls with toast notifications)
- Simple UX (photographers see instant feedback, reliable state, clear errors)

**Workflow:**
1. User reorders items: Update cache immediately (optimistic update via `onQueryStarted`)
2. Call mutation API: `PUT /image-sequences/{id}/items` with batch items
3. On success: Replace optimistic update with server response (already done by RTK Query)
4. On error: Rollback optimistic update (revert cache to previous state), show toast notification
5. User sees: Instant feedback (optimistic update), reliable state (rollback on error), clear error (toast notification)

**Error handling pattern:**
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
    } catch (error) {
      // Rollback on error
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

---

**End of Debate Round 15**

