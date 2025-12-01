# Changelog

## 2025-11-30

- Initial workspace created


## 2025-11-30

Created comprehensive intern guide (00-intern-guide-getting-started.md) with full context, file locations, implementation approach, and common questions. Created detailed tasks list (tasks.md) with backend and frontend tasks broken down into actionable items.


## 2025-11-30

Created v2 sequencing page structure with SequenceList, SequenceEditor, and SequenceItem components. Added optimistic updates to API hooks (reorderImageSequenceItems, addImageSequenceItem, deleteImageSequenceItem). Added route for /v2/projects/:projectId/sequencing.

### Related Files

- zine-layout/web/src/api.ts — Added optimistic updates to mutations
- zine-layout/web/src/routes/App.tsx — Added route for v2 sequencing page
- zine-layout/web/src/store.ts — Exported uiSlice actions for toast notifications
- zine-layout/web/src/views/v2/SequencingPage.tsx — Main sequencing page component
- zine-layout/web/src/views/v2/components/SequenceEditor.tsx — Sequence editor component
- zine-layout/web/src/views/v2/components/SequenceItem.tsx — Individual sequence item component
- zine-layout/web/src/views/v2/components/SequenceList.tsx — Sequence list sidebar component


## 2025-11-30

Implemented drag-and-drop reordering with debounced API calls (300ms delay). Added add gap and delete item functionality. Added visual feedback for drag operations and loading states.

### Related Files

- zine-layout/web/src/views/v2/components/SequenceEditor.tsx — Added drag-and-drop handlers and debounced reordering
- zine-layout/web/src/views/v2/components/SequenceItem.tsx — Added delete button and drag visual feedback


## 2025-11-30

Tested all backend API endpoints - all endpoints working correctly. Verified GET, POST, PUT, PATCH, DELETE operations for image sequences. Confirmed batch reorder endpoint accepts full item list and returns updated items. Error handling verified (404, 400 responses).

### Related Files

- zine-layout/pkg/serve/image_sequences_routes.go — All endpoints tested and verified


## 2025-11-30

Implemented Add Image functionality: Created AssetPicker modal component for selecting multiple assets. Added 'Add Image' button to SequenceEditor. Integrated with addImageSequenceItem mutation with optimistic updates. Users can now select multiple images from project assets and add them to sequences.

### Related Files

- zine-layout/web/src/views/v2/components/AssetPicker.tsx — Modal component for selecting assets to add to sequence
- zine-layout/web/src/views/v2/components/SequenceEditor.tsx — Added Add Image button and integrated AssetPicker


## 2025-11-30

Integrated v2 sequencing UI into main app as default. Created SequencesTabWrapper component that allows switching between new (v2) and legacy UI. New UI is the default, with a toggle button to switch to legacy. Updated ProjectDetail to use wrapper component.

### Related Files

- zine-layout/web/src/views/ProjectDetail.tsx — Updated to use SequencesTabWrapper
- zine-layout/web/src/views/tabs/SequencesTabWrapper.tsx — Wrapper component with UI version toggle
- zine-layout/web/src/views/v2/SequencingPage.tsx — Updated to accept projectId as prop for embedding


## 2025-11-30

Improved drag-and-drop UX on v2 sequencing page: added placeholder drop indicators and drop-target highlighting so the insertion position is obvious. Fixed asset previews by loading via asset.rel_path/url with cache busting.

### Related Files

- zine-layout/web/src/views/v2/components/SequenceEditor.tsx — Added drop-target placeholder and improved drag handlers
- zine-layout/web/src/views/v2/components/SequenceItem.tsx — Fixed asset preview URL logic


## 2025-11-30

Added visual drag-and-drop feedback: drop zone indicators (blue pulsing line), dragging item becomes semi-transparent and scales down, drop target shows ring highlight, cursor changes to grabbing. Users now see clear visual feedback of where items will be dropped during drag operations.

### Related Files

- zine-layout/web/src/views/v2/components/SequenceEditor.tsx — Added drag target tracking and visual indicators
- zine-layout/web/src/views/v2/components/SequenceItem.tsx — Added isDragging prop and cursor feedback


## 2025-11-30

Added delete button for gaps. Gaps now show position number and delete button on hover, matching the behavior of image items.

### Related Files

- zine-layout/web/src/views/v2/components/SequenceItem.tsx — Added delete button and position indicator for gaps


## 2025-11-30

Added preview slideshow feature with fullscreen capability. Users can preview their sequence as a slideshow, navigate with arrow keys or buttons, enter fullscreen mode, and see position indicators. Slideshow filters out gaps and shows only images in sequence order.

### Related Files

- zine-layout/web/src/views/v2/components/SequenceEditor.tsx — Added Preview Slideshow button
- zine-layout/web/src/views/v2/components/SequenceSlideshow.tsx — New slideshow component with fullscreen support


## 2025-11-30

Added spread view mode to slideshow that shows gaps as blank slides. Users can toggle between normal view (images only) and spread view (images + gaps). Gaps are displayed as dashed border boxes with pause icon. This allows previewing the complete sequence including pacing breaks.

### Related Files

- zine-layout/web/src/views/v2/components/SequenceSlideshow.tsx — Added showGaps toggle and gap slide rendering


## 2025-11-30

Added book-spread preview mode to the sequencing slideshow. Users can toggle between single-page and book-spread views, see gaps as blank pages, and navigate/enter fullscreen from either mode.

### Related Files

- zine-layout/web/src/views/v2/components/SequenceEditor.tsx — Buttons + double-click open slideshow with desired mode
- zine-layout/web/src/views/v2/components/SequenceSlideshow.tsx — Supports single vs book-spread preview modes


## 2025-11-30

Improve book-spread preview: remove rounded corners and frames; make spreads fill available height; keep object-contain scaling for consistency with single mode.

### Related Files

- zine-layout/web/src/views/v2/components/SequenceSlideshow.tsx — Hook order fix and spread layout update


## 2025-11-30

Created comprehensive frontend developer guide covering React patterns, RTK Query optimistic updates, Tailwind v4, drag-and-drop, slideshow modes, common tasks, and troubleshooting. Includes specific file paths, function names, and code examples for quick navigation.

### Related Files

- zine-layout/pkg/docs/topics/frontend-developer-guide.md — Complete guide for React/web developers working on this project

