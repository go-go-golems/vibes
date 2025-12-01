# Tasks: Sequencing Page Implementation

## Backend Tasks

### Task 1: Review and Verify Existing API Endpoints
- [x] Review `zine-layout/pkg/serve/image_sequences_routes.go`
- [x] Verify all endpoints match the spec in `sequencing-ux-walkthrough.md`
- [x] Test endpoints with curl/Postman
- [x] Document any missing endpoints or discrepancies

**Files to review:**
- `zine-layout/pkg/serve/image_sequences_routes.go`
- `zine-layout/pkg/repo/types.go` (ImageSequence, ImageSequenceItem types)
- `zine-layout/pkg/repo/repositories.go` (ImageSequenceRepository interface)

**Spec reference:**
- `vibes/2025/11/29/photobook-app/2025/11/30/ZINE-APP-DESIGN-zine-app-design-sequencing-and-layout-application/reference/sequencing-ux-walkthrough.md`

### Task 2: Add Batch Reorder Endpoint (if missing)
- [x] Check if batch reorder endpoint exists (`PUT /api/image-sequences/{id}/items`)
- [x] If missing, implement batch reorder endpoint
- [x] Support debounced reordering (accepts full item list)
- [x] Return updated items list
- [x] Add tests

**Files:**
- `zine-layout/pkg/serve/image_sequences_routes.go`
- `zine-layout/pkg/repo/repositories.go`

**Spec reference:**
- `sequencing-ux-walkthrough.md` - "Rapid Reordering with Debouncing" section

### Task 3: Verify Error Handling
- [x] Ensure all endpoints return proper error codes
- [x] Verify validation errors (400 Bad Request)
- [x] Verify not found errors (404 Not Found)
- [x] Verify server errors (500 Internal Server Error)
- [x] Test error scenarios

**Files:**
- `zine-layout/pkg/serve/image_sequences_routes.go`
- `zine-layout/pkg/serve/types.go` (error response format)

## Frontend Tasks

### Task 4: Set Up New Frontend Structure
- [x] Create new directory: `zine-layout/web/src/views/v2/`
- [x] Create `zine-layout/web/src/views/v2/SequencingPage.tsx`
- [x] Create `zine-layout/web/src/views/v2/components/` directory
- [x] Set up routing for new page (optional: `/v2/projects/:id/sequencing`)
- [x] Ensure new frontend doesn't interfere with existing frontend

**Files to create:**
- `zine-layout/web/src/views/v2/SequencingPage.tsx`
- `zine-layout/web/src/views/v2/components/SequenceList.tsx`
- `zine-layout/web/src/views/v2/components/SequenceEditor.tsx`
- `zine-layout/web/src/views/v2/components/SequenceItem.tsx`

**Reference:**
- Existing frontend: `zine-layout/web/src/views/tabs/SequencesTab.tsx` (don't modify, use as reference)

### Task 5: Implement RTK Query API Hooks
- [x] Review existing API hooks in `zine-layout/web/src/api.ts`
- [x] Verify all sequencing endpoints are defined
- [x] Add optimistic updates using `onQueryStarted` for:
  - `createImageSequence`
  - `addImageSequenceItem`
  - `deleteImageSequenceItem`
  - `reorderImageSequenceItems`
- [x] Add error handling with `patchResult.undo()`
- [x] Add toast notifications for errors

**Files:**
- `zine-layout/web/src/api.ts` (existing hooks)
- `zine-layout/web/src/store.ts` (toast notifications)

**Spec reference:**
- `sequencing-ux-walkthrough.md` - "Technical Implementation Notes" section
- `debate-round-16-sequencing-ux-api.md` - RTK Toolkit recommendations

### Task 6: Implement Sequence List Component
- [x] Create `SequenceList.tsx` component
- [x] Display list of sequences
- [x] Show sequence name, description, item count
- [x] Add "Create Sequence" button
- [x] Handle sequence selection
- [x] Style with Tailwind CSS

**Files:**
- `zine-layout/web/src/views/v2/components/SequenceList.tsx`

**Spec reference:**
- `sequencing-ux-walkthrough.md` - "UI Layout" section

### Task 7: Implement Sequence Editor Component
- [x] Create `SequenceEditor.tsx` component
- [x] Display sequence items in grid/list view
- [x] Show thumbnails for images
- [x] Show gap indicators
- [x] Handle drag-and-drop reordering
- [x] Add visual feedback (drag preview, drop zones)
- [x] Style with Tailwind CSS

**Files:**
- `zine-layout/web/src/views/v2/components/SequenceEditor.tsx`

**Spec reference:**
- `sequencing-ux-walkthrough.md` - "Drag-and-Drop Workflow" section

### Task 8: Implement Drag-and-Drop
- [x] Choose drag-and-drop library (react-beautiful-dnd, @dnd-kit, or native HTML5)
- [x] Implement drag start handler
- [x] Implement drag over handler (show drop zones)
- [x] Implement drop handler (reorder items optimistically)
- [x] Add visual feedback (drag preview, drop zones)
- [x] Handle drag end (call API, rollback on error)

**Files:**
- `zine-layout/web/src/views/v2/components/SequenceEditor.tsx`

**Spec reference:**
- `sequencing-ux-walkthrough.md` - "Drag-and-Drop Workflow" section

### Task 9: Implement Debounced Reordering
- [x] Add debounce utility (use lodash.debounce or custom)
- [x] Debounce rapid reordering (300-500ms delay)
- [x] Batch multiple reorders into single API call
- [x] Show loading indicator during debounce
- [x] Handle errors (rollback optimistic update)

**Files:**
- `zine-layout/web/src/views/v2/components/SequenceEditor.tsx`
- `zine-layout/web/src/views/v2/hooks/useDebouncedReorder.ts` (optional)

**Spec reference:**
- `sequencing-ux-walkthrough.md` - "Rapid Reordering with Debouncing" section

### Task 10: Implement Add/Delete Items
- [x] Add "Add Image" button
- [x] Show asset picker modal
- [x] Add selected images to sequence (optimistic update)
- [x] Add "Add Gap" button
- [x] Add delete button for each item
- [ ] Handle errors (rollback optimistic update)

**Files:**
- `zine-layout/web/src/views/v2/components/SequenceEditor.tsx`
- `zine-layout/web/src/views/v2/components/AssetPicker.tsx` (optional)

**Spec reference:**
- `sequencing-ux-walkthrough.md` - "Adding Items" and "Deleting Items" sections

### Task 11: Implement Visual Feedback States
- [ ] Add loading state (spinner/skeleton)
- [ ] Add error state (error message, retry button)
- [ ] Add empty state (no sequences, no items)
- [x] Add drag state (drag preview, drop zones)
- [ ] Add success state (toast notification)

**Files:**
- `zine-layout/web/src/views/v2/components/SequenceEditor.tsx`
- `zine-layout/web/src/components/ui/` (reuse existing UI components)

**Spec reference:**
- `sequencing-ux-walkthrough.md` - "Visual Feedback States" section

### Task 12: Implement Error Handling
- [ ] Add error boundaries
- [ ] Handle API errors (network, validation, server)
- [ ] Show error toast notifications
- [ ] Rollback optimistic updates on error
- [ ] Add retry functionality

**Files:**
- `zine-layout/web/src/views/v2/components/SequenceEditor.tsx`
- `zine-layout/web/src/store.ts` (toast notifications)

**Spec reference:**
- `sequencing-ux-walkthrough.md` - "Error Handling" section

### Task 13: Testing
- [ ] Test drag-and-drop reordering
- [ ] Test debounced reordering (rapid drags)
- [ ] Test add/delete items
- [ ] Test error scenarios (network errors, validation errors)
- [ ] Test optimistic updates and rollback
- [ ] Test on different screen sizes (responsive)

**Files:**
- All new components

## Integration Tasks

### Task 14: Integrate with Existing App
- [x] Add route for new sequencing page
- [x] Add navigation link (optional)
- [x] Ensure new page doesn't break existing functionality
- [ ] Test integration

**Files:**
- `zine-layout/web/src/routes/App.tsx`
- `zine-layout/web/src/views/ProjectDetail.tsx` (if adding tab)

### Task 15: Documentation
- [ ] Document new components
- [ ] Document API usage patterns
- [ ] Document drag-and-drop implementation
- [ ] Document error handling
- [ ] Update README if needed

**Files:**
- Component files (JSDoc comments)
- `zine-layout/web/README.md` (if exists)

## Notes

- **Don't modify existing frontend:** Create new implementation in `v2/` directory
- **Follow existing patterns:** Use RTK Query, Tailwind CSS, TypeScript
- **Reference specs:** Always check `sequencing-ux-walkthrough.md` for UI/UX details
- **Test thoroughly:** Test all interactions, error scenarios, and edge cases
