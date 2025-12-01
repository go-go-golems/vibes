---
Title: 'Image Layout UX Walkthrough'
Ticket: ZINE-APP-DESIGN
Status: draft
Topics:
    - ux-design
    - image-layout
    - template-assignment
    - walkthrough
DocType: reference
Intent: long-term
Owners: []
RelatedFiles:
    - debate-round-17-image-layout-ux-api.md
Summary: UX walkthrough for image layout template assignment workflow with ASCII diagrams showing template selection, preview, optimistic frontend cropping, progressive replacement, batch operations, and error handling
LastUpdated: 2025-12-01T00:00:00-05:00
---

# Image Layout UX Walkthrough

**Based on:** Debate Round 17 consensus on image layout UX+API patterns

**Key Principles:**
- Visual template selection (thumbnails/preview cards)
- Preview before applying (compute layout without creating)
- Optimistic frontend cropping (use backend crop zones immediately)
- Progressive replacement (backend-rendered image replaces optimistic crop)
- Batch operations (apply template to multiple images)
- Template reuse (create once, use many times)

---

## UI Layout

### Initial State: Image Layout View

```
┌─────────────────────────────────────────────────────────────────────────┐
│ Image Layouts                                    [+ New Template]      │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                           │
│ Templates:                                                                │
│ ┌──────────────────────┐  ┌──────────────────────┐                      │
│ │ 📐 Square Crop       │  │ 📐 Portrait Fill     │                      │
│ │ 1:1 aspect ratio     │  │ 3:4 aspect ratio     │                      │
│ └──────────────────────┘  └──────────────────────┘                      │
│ ┌──────────────────────┐  ┌──────────────────────┐                      │
│ │ 📐 Landscape Fit      │  │ 📐 Custom Template   │                      │
│ │ 4:3 aspect ratio      │  │ Custom settings      │                      │
│ └──────────────────────┘  └──────────────────────┘                      │
│                                                                           │
│ Available Images:                                                         │
│ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐             │
│ │ IMG-001 │ │ IMG-002 │ │ IMG-003 │ │ IMG-004 │ │ IMG-005 │             │
│ └─────────┘ └─────────┘ └─────────┘ └─────────┘ └─────────┘             │
│                                                                           │
│ Laid-Out Images:                                                          │
│ ┌─────────────────────────────────────────────────────────────────────┐ │
│ │ [No laid-out images yet]                                            │ │
│ │                                                                     │ │
│ │ Select an image and template to create a laid-out image.            │ │
│ └─────────────────────────────────────────────────────────────────────┘ │
│                                                                           │
└─────────────────────────────────────────────────────────────────────────┘
```

**API Calls:**
- `GET /image-layout-templates` — List global templates
  - **Params:** None
  - **Response:** `{ templates: ImageLayoutTemplate[] }`
- `GET /projects/{projectId}/image-layout-templates` — List project templates
  - **Params:** `projectId` (path parameter)
  - **Response:** `{ templates: ImageLayoutTemplate[] }`
- `GET /projects/{projectId}/assets` — List all assets for project
  - **Params:** `projectId` (path parameter)
  - **Response:** `{ assets: Asset[] }`
- `GET /projects/{projectId}/laid-out-images` — List laid-out images for project
  - **Params:** `projectId` (path parameter)
  - **Response:** `{ laid_out_images: LaidOutImage[] }`

---

## Template Selection Workflow

### Step 1: User Selects Template

**User Action:** Click on "Square Crop" template

**UI State:**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Templates:                                                                │
│ ┌──────────────────────┐  ┌──────────────────────┐                      │
│ │ 📐 Square Crop  ✓    │  │ 📐 Portrait Fill     │                      │
│ │ 1:1 aspect ratio     │  │ 3:4 aspect ratio     │                      │
│ └──────────────────────┘  └──────────────────────┘                      │
│                                                                           │
│ Selected Template: Square Crop                                            │
│ ┌─────────────────────────────────────────────────────────────────────┐ │
│ │ Template Preview:                                                   │ │
│ │ ┌─────────────┐                                                     │ │
│ │ │             │                                                     │ │
│ │ │  1:1 Crop   │  ← Visual preview of template                      │ │
│ │ │             │                                                     │ │
│ │ └─────────────┘                                                     │ │
│ │                                                                     │ │
│ │ Settings:                                                           │ │
│ │ • Aspect Ratio: 1:1                                                 │ │
│ │ • Crop Mode: Fill                                                  │ │
│ │ • Margins: 0.5in all sides                                         │ │
│ └─────────────────────────────────────────────────────────────────────┘ │
│                                                                           │
└─────────────────────────────────────────────────────────────────────────┘
```

**API Calls:**
- `GET /image-layout-templates/{templateId}` — Get template details (optional, if not already cached)
  - **Params:** `templateId` (path parameter)
  - **Response:** `{ template: ImageLayoutTemplate }`
  - **Note:** May be cached from initial template list query

**Technical Details:**
- Template selected: Visual highlight (checkmark, border)
- Template preview shown: Visual representation of template
- Settings displayed: Template configuration visible

---

### Step 2: User Selects Image

**User Action:** Click on IMG-001

**UI State:**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Available Images:                                                        │
│ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐           │
│ │ IMG-001 │ │ IMG-002 │ │ IMG-003 │ │ IMG-004 │ │ IMG-005 │           │
│ │   ✓     │ │         │ │         │ │         │ │         │           │
│ └─────────┘ └─────────┘ └─────────┘ └─────────┘ └─────────┘           │
│                                                                           │
│ Preview:                                                                  │
│ ┌─────────────────────────────────────────────────────────────────────┐ │
│ │ Template: Square Crop                                               │ │
│ │ Image: IMG-001                                                      │ │
│ ├─────────────────────────────────────────────────────────────────────┤ │
│ │                                                                     │ │
│ │ ┌─────────────┐                                                     │ │
│ │ │             │                                                     │ │
│ │ │  [Preview]  │  ← Preview of how template will look on image     │ │
│ │ │             │                                                     │ │
│ │ └─────────────┘                                                     │ │
│ │                                                                     │ │
│ │ [Cancel] [Apply Template]                                           │ │
│ └─────────────────────────────────────────────────────────────────────┘ │
│                                                                           │
└─────────────────────────────────────────────────────────────────────────┘
```

**API Calls:**
- `GET /laid-out-images/preview` — Preview layout without creating (optional)
  - **Params:**
    - `asset_id` (query parameter): `string` (required)
    - `template_id` (query parameter): `string` (required)
    - `overrides` (query parameter): `object` (optional, JSON stringified)
  - **Response:** `{ preview: { crop_zones: CropZones, preview_image_url: string } }`
  - **Note:** Can be computed client-side instead of API call for faster preview

**Technical Details:**
- Image selected: Visual highlight
- Preview computed: `GET /laid-out-images/preview?asset_id={id}&template_id={id}` (optional, can be computed client-side)
- Preview shows: How template will look on selected image
- User can see result before applying

---

## Applying Template (Optimistic Frontend Cropping)

### Step 3: User Applies Template

**User Action:** Click "[Apply Template]" button

**UI State (Immediate - Optimistic Update):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Laid-Out Images:                                                         │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │ │
│ │ ┌─────────────┐                                                     │ │ │
│ │ │             │                                                     │ │ │
│ │ │  [Cropped]  │  ← Optimistic frontend crop (instant)             │ │ │
│ │ │   Image     │                                                     │ │ │
│ │ │             │                                                     │ │ │
│ │ └─────────────┘                                                     │ │ │
│ │ IMG-001 • Square Crop                                                │ │ │
│ │ ⏳ Syncing...  ← Loading indicator                                  │ │ │
│ └─────────────────────────────────────────────────────────────────────┘ │ │
│                                                                           │ │
│ ✓ Template applied (syncing...)  ← Toast notification                   │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**API Calls:**
- `POST /projects/{projectId}/laid-out-images` — Create laid-out image
  - **Params:**
    - `projectId` (path parameter)
    - `asset_id` (body parameter): `string` (required)
    - `template_id` (body parameter): `string` (required)
    - `overrides` (body parameter): `object` (optional)
  - **Response:** `{ laid_out_image: LaidOutImage }`
    - **Response includes:** `result` field with crop zones (layout computation)
  - **Note:** Optimistic update appears immediately, API call happens in background

**Technical Details:**
- **Optimistic update:** Laid-out image appears immediately in list
- **Frontend cropping:** Uses backend crop zones to crop image client-side (instant visual feedback)
- **API call initiated:** `POST /projects/{id}/laid-out-images` with `asset_id`, `template_id`
- **Toast notification:** "Template applied (syncing...)" appears

**RTK Query Pattern:**
```typescript
async onQueryStarted({ projectId, assetId, templateId, overrides }, { dispatch, queryFulfilled, getState }) {
  // Step 1: Get asset for optimistic cropping
  const state = getState() as RootState;
  const asset = state.api.queries[`getAssets({"projectId":"${projectId}"})`]?.data?.find(
    (a: Asset) => a.id === assetId
  );

  // Step 2: Optimistic update - add laid-out image with placeholder
  const tempId = `temp-${Date.now()}`;
  const patchResult = dispatch(
    api.util.updateQueryData('getLaidOutImages', { projectId }, (draft) => {
      draft.push({
        id: tempId,
        project_id: projectId,
        asset_id: assetId,
        template_id: templateId,
        overrides: overrides ?? {},
        result: null, // Will be replaced by server response
        optimisticCrop: true,
        optimisticImageUrl: null, // Will be set by frontend cropping
      });
    })
  );

  try {
    const result = await queryFulfilled;
    // Step 3: Server response includes layout computation (crop zones)
    const laidOutImage = result.data.laid_out_image;
    const cropZones = laidOutImage.result; // Crop zones from backend computation
    
    // Step 4: Do optimistic frontend cropping using backend crop zones
    if (asset && cropZones) {
      const croppedImageUrl = await cropImageClientSide(asset.url, cropZones);
      // Update cache with optimistic cropped image
      dispatch(
        api.util.updateQueryData('getLaidOutImages', { projectId }, (draft) => {
          const item = draft.find((item) => item.id === tempId);
          if (item) {
            item.id = laidOutImage.id;
            item.result = laidOutImage.result;
            item.optimisticImageUrl = croppedImageUrl;
          }
        })
      );
    }
    // ... progressive replacement continues ...
  } catch (error) {
    patchResult.undo();
  }
}
```

---

### Step 4: Server Response (Crop Zones Received)

**UI State (After Server Response - Crop Zones Available):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Laid-Out Images:                                                         │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │ │
│ │ ┌─────────────┐                                                     │ │ │
│ │ │             │                                                     │ │ │
│ │ │  [Cropped]  │  ← Frontend crop using backend crop zones         │ │ │
│ │ │   Image     │                                                     │ │ │
│ │ │             │                                                     │ │ │
│ │ └─────────────┘                                                     │ │ │
│ │ IMG-001 • Square Crop                                                │ │ │
│ │ ⏳ Rendering backend image...  ← Loading indicator                  │ │ │
│ └─────────────────────────────────────────────────────────────────────┘ │ │
│                                                                           │ │
│ ✓ Template applied (rendering...)  ← Toast notification                 │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**API Calls:**
- `POST /projects/{projectId}/laid-out-images` — Response received
  - **Status:** `201 Created`
  - **Response:** `{ laid_out_image: LaidOutImage }`
    - **Response includes:** `result` field containing:
      - `settings`: ViewportSettings (template settings)
      - `result`: ViewportResult (crop zones, scale, position)
      - `trace`: Trace (optional, for debugging)
  - **Note:** Crop zones are in `result.result` field, used immediately for frontend cropping

**Technical Details:**
- Server responds: `201 Created` with `LaidOutImage` including `result` field (crop zones)
- **Backend crop zones received:** Layout computation includes crop regions, scale, position
- **Frontend crops immediately:** Uses crop zones to crop image client-side (no algorithm duplication)
- **Optimistic crop displayed:** User sees cropped image immediately
- Backend starts rendering full image asynchronously (doesn't block UI)

---

### Step 5: Progressive Replacement (Backend Image Ready)

**UI State (Backend Image Replaces Optimistic Crop):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Laid-Out Images:                                                         │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │ │
│ │ ┌─────────────┐                                                     │ │ │
│ │ │             │                                                     │ │ │
│ │ │ [Backend]   │  ← Backend-rendered image (high quality)            │ │ │
│ │ │  Rendered   │                                                     │ │ │
│ │ │   Image     │                                                     │ │ │
│ │ │             │                                                     │ │ │
│ │ └─────────────┘                                                     │ │ │
│ │ IMG-001 • Square Crop                                                │ │ │
│ │ ✓ Complete                                                           │ │ │
│ └─────────────────────────────────────────────────────────────────────┘ │ │
│                                                                           │ │
│ ✓ Template applied successfully  ← Toast notification (success)        │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**API Calls:**
- `GET /laid-out-images/{laidOutImageId}/preview` — Get backend-rendered image
  - **Params:** `laidOutImageId` (path parameter)
  - **Response:** `{ image_url: string, status: 'ready' | 'processing' }`
  - **Note:** Polled after 1 second delay, or use websocket for real-time updates
  - **Alternative:** Websocket event `laid-out-image-rendered` with `{ id: string, image_url: string }`

**Technical Details:**
- Backend image ready: Polling or websocket receives rendered image URL
- **Progressive replacement:** Optimistic crop replaced with backend-rendered image seamlessly
- **Seamless transition:** User may not notice the replacement (same visual result, higher quality)
- Toast updates: "Template applied successfully" (green, auto-dismisses)

**Progressive Replacement Pattern:**
```typescript
// After server response with crop zones
setTimeout(async () => {
  try {
    const previewResponse = await fetch(`/api/laid-out-images/${laidOutImage.id}/preview`);
    const previewData = await previewResponse.json();
    // Replace optimistic crop with backend-rendered image
    dispatch(
      api.util.updateQueryData('getLaidOutImages', { projectId }, (draft) => {
        const item = draft.find((item) => item.id === laidOutImage.id);
        if (item) {
          item.backendImageUrl = previewData.image_url;
          item.optimisticImageUrl = null; // Remove optimistic crop
          item.optimisticCrop = false;
        }
      })
    );
  } catch (error) {
    // Keep optimistic crop if backend image fails
    console.warn('Failed to fetch backend-rendered image, keeping optimistic crop');
  }
}, 1000); // Poll after 1 second, or use websocket for real-time updates
```

---

## Batch Operations

### Applying Template to Multiple Images

**User Action:** Select multiple images (IMG-001, IMG-002, IMG-003), then click "[Apply Template to Selected]"

**UI State (Immediate - Optimistic Updates):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Available Images:                                                        │
│ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐           │
│ │ IMG-001 │ │ IMG-002 │ │ IMG-003 │ │ IMG-004 │ │ IMG-005 │           │
│ │   ✓     │ │   ✓     │ │   ✓     │ │         │ │         │           │
│ └─────────┘ └─────────┘ └─────────┘ └─────────┘ └─────────┘           │
│                                                                           │
│ Laid-Out Images:                                                         │
│ ┌─────────────────────────────────────────────────────────────────────┐ │
│ │ ┌─────────┐ ┌─────────┐ ┌─────────┐                                │ │
│ │ │[Crop-1] │ │[Crop-2] │ │[Crop-3] │  ← Optimistic crops (instant) │ │
│ │ └─────────┘ └─────────┘ └─────────┘                                │ │
│ │ IMG-001    IMG-002    IMG-003                                      │ │
│ │ ⏳ Syncing...                                                       │ │
│ └─────────────────────────────────────────────────────────────────────┘ │
│                                                                           │
│ ✓ Applying template to 3 images (syncing...)  ← Toast notification      │
│                                                                           │
└─────────────────────────────────────────────────────────────────────────┘
```

**API Calls:**
- `POST /projects/{projectId}/laid-out-images/batch` — Create multiple laid-out images
  - **Params:**
    - `projectId` (path parameter)
    - `images` (body parameter): `Array<{ asset_id: string, template_id: string, overrides?: object }>`
  - **Response:** `{ laid_out_images: LaidOutImage[] }`
    - **Response includes:** Each `LaidOutImage` has `result` field with crop zones
  - **Note:** Optimistic updates appear immediately for all images, single API call for batch

**After Server Response (Crop Zones Received):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Laid-Out Images:                                                         │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │ │
│ │ ┌─────────┐ ┌─────────┐ ┌─────────┐                                │ │ │
│ │ │[Crop-1] │ │[Crop-2] │ │[Crop-3] │  ← Frontend crops using zones  │ │ │
│ │ └─────────┘ └─────────┘ └─────────┘                                │ │ │
│ │ IMG-001    IMG-002    IMG-003                                      │ │ │
│ │ ⏳ Rendering backend images...                                      │ │ │
│ └─────────────────────────────────────────────────────────────────────┘ │ │
│                                                                           │ │
│ ✓ Template applied to 3 images (rendering...)  ← Toast notification     │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**After Backend Images Ready:**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Laid-Out Images:                                                         │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │ │
│ │ ┌─────────┐ ┌─────────┐ ┌─────────┐                                │ │ │
│ │ │[Backend]│ │[Backend]│ │[Backend]│  ← Backend-rendered images    │ │ │
│ │ └─────────┘ └─────────┘ └─────────┘                                │ │ │
│ │ IMG-001    IMG-002    IMG-003                                      │ │ │
│ │ ✓ Complete                                                           │ │ │
│ └─────────────────────────────────────────────────────────────────────┘ │ │
│                                                                           │ │
│ ✓ Template applied to 3 images successfully  ← Toast notification        │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**API Calls:**
- `GET /laid-out-images/{laidOutImageId}/preview` — Get backend-rendered images (multiple calls, one per image)
  - **Params:** `laidOutImageId` (path parameter) - called for each image in batch
  - **Response:** `{ image_url: string, status: 'ready' }`
  - **Note:** Polled for each image after 1 second delay, or use websocket for real-time updates
  - **Alternative:** Websocket events `laid-out-image-rendered` with `{ id: string, image_url: string }` for each image

**Technical Details:**
- **Batch API call:** `POST /projects/{id}/laid-out-images/batch` with array of `{ asset_id, template_id, overrides }`
- **Optimistic updates:** All images appear immediately with optimistic crops
- **Server response:** Returns array of `LaidOutImage` objects with crop zones
- **Frontend crops:** Uses crop zones to crop all images client-side
- **Progressive replacement:** Backend images replace optimistic crops as they become available
- **Efficient:** Single API call for multiple images, atomic operation

---

## Error Handling

### Error During Template Application

**UI State (On Error - Rollback):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Laid-Out Images:                                                         │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │ │
│ │ [No laid-out images]  ← Rolled back to previous state               │ │ │
│ └─────────────────────────────────────────────────────────────────────┘ │ │
│                                                                           │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │ │
│ │ ✗ Failed to apply template. Please try again.  ← Error toast (red) │ │ │
│ │ [Retry]                                                              │ │ │
│ └─────────────────────────────────────────────────────────────────────┘ │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**API Calls:**
- `POST /projects/{projectId}/laid-out-images` — Error response
  - **Status:** `500 Internal Server Error` or network error
  - **Error Response:** `{ error: string }`
  - **Note:** RTK Query automatically rolls back optimistic update via `patchResult.undo()`

**Technical Details:**
- Server responds: `500 Internal Server Error` or network error
- **Rollback:** RTK Query `patchResult.undo()` reverts optimistic update
- UI returns to previous state (no laid-out images)
- Error toast appears: "Failed to apply template. Please try again." (red, stays until dismissed)
- Retry button available (re-applies template)

**Error Handling Pattern:**
```typescript
try {
  await queryFulfilled;
  // ... handle success ...
} catch (error) {
  // Automatic rollback on error
  patchResult.undo();
  // Show error notification
  dispatch(uiSlice.actions.addToast({
    id: Date.now().toString(),
    text: 'Failed to apply template. Please try again.',
    type: 'error',
  }));
}
```

---

### Error During Backend Image Rendering

**UI State (Backend Image Fails, Keep Optimistic Crop):**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Laid-Out Images:                                                         │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │ │
│ │ ┌─────────────┐                                                     │ │ │
│ │ │             │                                                     │ │ │
│ │ │  [Cropped]  │  ← Optimistic crop kept (fallback)                │ │ │
│ │ │   Image     │                                                     │ │ │
│ │ │             │                                                     │ │ │
│ │ └─────────────┘                                                     │ │ │
│ │ IMG-001 • Square Crop                                                │ │ │
│ │ ⚠ Using preview crop (backend rendering failed)                    │ │ │
│ └─────────────────────────────────────────────────────────────────────┘ │ │
│                                                                           │ │
│ ⚠ Backend image rendering failed, using preview crop  ← Warning toast  │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**API Calls:**
- `GET /laid-out-images/{laidOutImageId}/preview` — Error or timeout
  - **Status:** `500 Internal Server Error`, `404 Not Found`, or timeout
  - **Error Response:** `{ error: string }`
  - **Note:** Fallback to optimistic crop if backend image fails

**Technical Details:**
- Backend image rendering fails: Preview endpoint returns error or timeout
- **Fallback:** Keep optimistic crop (better than showing nothing)
- Warning toast appears: "Backend image rendering failed, using preview crop" (yellow, auto-dismisses)
- User can still use the laid-out image (optimistic crop is functional)

---

## Template Preview (Before Applying)

### Preview Workflow

**User Action:** Select template and image, preview appears automatically

**UI State:**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Template: Square Crop                                                    │
│ Image: IMG-001                                                           │
├─────────────────────────────────────────────────────────────────────────┤ │
│                                                                           │ │
│ ┌─────────────────────────────────────────────────────────────────────┐ │ │
│ │ Preview (Before Applying):                                          │ │ │
│ │ ┌─────────────┐                                                     │ │ │
│ │ │             │                                                     │ │ │
│ │ │  [Preview]  │  ← Computed layout preview                          │ │ │
│ │ │             │                                                     │ │ │
│ │ └─────────────┘                                                     │ │ │
│ │                                                                     │ │ │
│ │ Crop Zones:                                                          │ │ │
│ │ • Crop Region: (100, 50) to (400, 350)                              │ │ │
│ │ • Scale: 1.2x                                                        │ │ │
│ │ • Position: Center                                                   │ │ │
│ │                                                                     │ │ │
│ │ [Cancel] [Apply Template]                                           │ │ │
│ └─────────────────────────────────────────────────────────────────────┘ │ │
│                                                                           │ │
└─────────────────────────────────────────────────────────────────────────┘ │
```

**API Calls:**
- `GET /laid-out-images/preview` — Preview layout without creating (optional)
  - **Params:**
    - `asset_id` (query parameter): `string` (required)
    - `template_id` (query parameter): `string` (required)
    - `overrides` (query parameter): `object` (optional, JSON stringified)
  - **Response:** `{ preview: { crop_zones: CropZones, preview_image_url: string } }`
  - **Alternative:** Compute client-side using template settings and asset dimensions (no API call)
  - **Note:** Preview does not create `LaidOutImage` record, just computes layout

**Technical Details:**
- Preview computed: Can be done client-side or via `GET /laid-out-images/preview` endpoint
- **No creation:** Preview doesn't create `LaidOutImage` record
- **Fast:** Layout computation is fast (<10ms), can be synchronous
- User sees: How template will look before committing
- Reduces errors: User can adjust before applying

---

## Template Reuse

### Using Existing Templates

**UI State:**
```
┌─────────────────────────────────────────────────────────────────────────┐
│ Templates:                                                                │
│ ┌──────────────────────┐  ┌──────────────────────┐                      │
│ │ 📐 Square Crop        │  │ 📐 Portrait Fill     │                      │
│ │ 1:1 aspect ratio      │  │ 3:4 aspect ratio     │                      │
│ │ Used 5 times          │  │ Used 12 times        │                      │
│ └──────────────────────┘  └──────────────────────┘                      │
│ ┌──────────────────────┐  ┌──────────────────────┐                      │
│ │ 📐 Custom Template    │  │ 📐 Project Template  │                      │
│ │ Custom settings       │  │ Project-specific      │                      │
│ │ Used 3 times          │  │ Used 8 times          │                      │
│ └──────────────────────┘  └──────────────────────┘                      │
│                                                                           │
│ Global Templates:                                                        │
│ ┌──────────────────────┐  ┌──────────────────────┐                      │
│ │ 📐 Standard Square   │  │ 📐 Standard Portrait  │                      │
│ │ Available to all     │  │ Available to all     │                      │
│ └──────────────────────┘  └──────────────────────┘                      │
│                                                                           │
└─────────────────────────────────────────────────────────────────────────┘
```

**API Calls:**
- `GET /image-layout-templates` — List global templates (already called on initial load)
  - **Params:** None
  - **Response:** `{ templates: ImageLayoutTemplate[] }`
- `GET /projects/{projectId}/image-layout-templates` — List project templates (already called on initial load)
  - **Params:** `projectId` (path parameter)
  - **Response:** `{ templates: ImageLayoutTemplate[] }`
- **Note:** Templates are cached, no additional API calls needed for reuse

**Technical Details:**
- **Template reuse:** Templates can be used across multiple images and projects
- **Usage tracking:** Shows how many times template has been used
- **Global templates:** Available to all projects
- **Project templates:** Specific to current project
- **Efficient:** Create once, use many times

---

## Summary: Key UX Patterns

### 1. Visual Template Selection
- **What:** Show templates as thumbnails/preview cards, not just dropdowns
- **Why:** Clear, confident selection, supports experimentation
- **How:** Visual template cards with preview and settings

### 2. Preview Before Applying
- **What:** Show how template will look on image before applying
- **Why:** Reduces errors, builds confidence, supports experimentation
- **How:** Compute layout without creating `LaidOutImage` record

### 3. Optimistic Frontend Cropping
- **What:** Use backend crop zones to crop image client-side immediately
- **Why:** Instant visual feedback, no algorithm duplication
- **How:** Backend computes crop zones synchronously, frontend crops using zones

### 4. Progressive Replacement
- **What:** Backend-rendered image replaces optimistic crop when ready
- **Why:** Accurate rendering, seamless transition, best of both worlds
- **How:** Poll preview endpoint or use websocket, replace image URL

### 5. Batch Operations
- **What:** Apply template to multiple images at once
- **Why:** Efficient, professional workflow, fewer API calls
- **How:** Batch API endpoint, optimistic updates for all images

### 6. Template Reuse
- **What:** Create templates once, use many times
- **Why:** Efficient, consistent, professional workflow
- **How:** Template management, global and project-specific templates

---

## Technical Implementation Notes

### RTK Query Mutation Pattern (Optimistic Frontend Cropping)
```typescript
createLaidOutImage: builder.mutation({
  query: ({ projectId, assetId, templateId, overrides }) => ({
    url: `/projects/${encodeURIComponent(projectId)}/laid-out-images`,
    method: 'POST',
    body: { asset_id: assetId, template_id: templateId, overrides },
  }),
  async onQueryStarted({ projectId, assetId, templateId, overrides }, { dispatch, queryFulfilled, getState }) {
    // Step 1: Get asset for optimistic cropping
    const state = getState() as RootState;
    const asset = state.api.queries[`getAssets({"projectId":"${projectId}"})`]?.data?.find(
      (a: Asset) => a.id === assetId
    );

    // Step 2: Optimistic update - add laid-out image with placeholder
    const tempId = `temp-${Date.now()}`;
    const patchResult = dispatch(
      api.util.updateQueryData('getLaidOutImages', { projectId }, (draft) => {
        draft.push({
          id: tempId,
          project_id: projectId,
          asset_id: assetId,
          template_id: templateId,
          overrides: overrides ?? {},
          result: null,
          optimisticCrop: true,
          optimisticImageUrl: null,
        });
      })
    );

    try {
      const result = await queryFulfilled;
      // Step 3: Server response includes layout computation (crop zones)
      const laidOutImage = result.data.laid_out_image;
      const cropZones = laidOutImage.result; // Crop zones from backend computation
      
      // Step 4: Do optimistic frontend cropping using backend crop zones
      if (asset && cropZones) {
        const croppedImageUrl = await cropImageClientSide(asset.url, cropZones);
        // Update cache with optimistic cropped image
        dispatch(
          api.util.updateQueryData('getLaidOutImages', { projectId }, (draft) => {
            const item = draft.find((item) => item.id === tempId);
            if (item) {
              item.id = laidOutImage.id;
              item.result = laidOutImage.result;
              item.optimisticImageUrl = croppedImageUrl;
            }
          })
        );
      }

      // Step 5: Start fetching backend-rendered image (async)
      setTimeout(async () => {
        try {
          const previewResponse = await fetch(`/api/laid-out-images/${laidOutImage.id}/preview`);
          const previewData = await previewResponse.json();
          // Replace optimistic crop with backend-rendered image
          dispatch(
            api.util.updateQueryData('getLaidOutImages', { projectId }, (draft) => {
              const item = draft.find((item) => item.id === laidOutImage.id);
              if (item) {
                item.backendImageUrl = previewData.image_url;
                item.optimisticImageUrl = null;
                item.optimisticCrop = false;
              }
            })
          );
        } catch (error) {
          // Keep optimistic crop if backend image fails
          console.warn('Failed to fetch backend-rendered image, keeping optimistic crop');
        }
      }, 1000);
    } catch (error) {
      // Rollback on error
      patchResult.undo();
      dispatch(uiSlice.actions.addToast({
        id: Date.now().toString(),
        text: 'Failed to apply template. Please try again.',
        type: 'error',
      }));
    }
  },
  invalidatesTags: (_result, _error, { projectId }) => [
    { type: 'LaidOutImage', id: `LIST-${projectId}` },
  ],
}),
```

### Frontend Cropping Function
```typescript
async function cropImageClientSide(imageUrl: string, cropZones: CropZones): Promise<string> {
  // Load image
  const img = new Image();
  img.src = imageUrl;
  await img.decode();
  
  // Create canvas
  const canvas = document.createElement('canvas');
  const ctx = canvas.getContext('2d');
  
  // Set canvas size to crop region
  canvas.width = cropZones.width;
  canvas.height = cropZones.height;
  
  // Draw cropped image
  ctx.drawImage(
    img,
    cropZones.x, cropZones.y, cropZones.width, cropZones.height, // Source
    0, 0, cropZones.width, cropZones.height // Destination
  );
  
  // Return data URL
  return canvas.toDataURL('image/jpeg', 0.9);
}
```

### Key Workflow Steps
1. **Backend computes crop zones:** Synchronous, fast (<10ms), returns in mutation response
2. **Frontend crops immediately:** Uses crop zones to crop image client-side (no algorithm duplication)
3. **Backend renders image:** Asynchronously generates high-quality rendered image
4. **Frontend replaces:** When backend image ready, replace optimistic crop seamlessly

---

**End of Image Layout UX Walkthrough**

