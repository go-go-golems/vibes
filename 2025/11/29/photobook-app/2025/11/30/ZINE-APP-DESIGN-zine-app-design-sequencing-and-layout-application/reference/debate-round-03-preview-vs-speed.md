---
Title: 'Debate Round 03: How important is real-time preview vs. speed?'
Ticket: ZINE-APP-DESIGN
Status: draft
Topics:
    - debate
    - performance
    - preview
    - user-experience
DocType: reference
Intent: long-term
Owners: []
RelatedFiles: []
Summary: Third debate round exploring performance trade-offs between real-time preview quality and sequencing speed
LastUpdated: 2025-11-30T16:30:00-05:00
---

# Debate Round 03: How important is real-time preview vs. speed?

**Question:** How important is real-time preview vs. speed? Should sequencing feel instant (even if preview is lower quality)? Or is high-quality preview worth a slight delay?

**Primary Candidates:**
- Maya Chen (Experimental Photographer) — Argues for instant feel over perfect preview
- Sam Taylor (Software Developer) — Argues for technical trade-offs
- `web/src/` (The Frontend) — Argues for UI performance

**Why this question matters:** Performance trade-offs affect user experience. If sequencing feels slow, photographers lose creative flow. If preview quality is too low, photographers can't see what they're creating.

---

## Pre-Debate Research

### Current Codebase Performance Analysis

**Research conducted by:** Sam Taylor (Software Developer)

**Current rendering infrastructure:**

1. **Thumbnail generation:**
   - Default thumbnail max size: 512px: ```48:49:zine-layout/pkg/pagelayout/renderer/renderer.go```
   - Thumbnails generated for preview: ```109:111:zine-layout/pkg/pagelayout/renderer/renderer.go```
   - Thumbnails cached on filesystem: ```221:248:zine-layout/pkg/services/pages.go```

2. **Rendering pipeline:**
   - Full page rendering creates multiple variants (thumbnail, full, combined, left, right)
   - Rendering happens server-side (Go)
   - Images decoded, cropped, scaled, composited

3. **Current UI preview:**
   - `SequencesTab.tsx` shows slide show view
   - Uses asset URLs directly (no layout preview during sequencing)
   - No real-time preview of how images will look on pages

**Performance considerations:**
- Image decoding (JPEG/PNG) takes time
- Cropping and scaling operations
- Canvas rendering
- Network transfer for preview images

**Conclusion:** Current codebase generates thumbnails but doesn't provide real-time preview during sequencing. Sequencing happens with raw images; preview happens after applying templates.

### User Experience Research

**Research conducted by:** Maya Chen, `web/src/`

**Maya's perspective:**
- "I just want to drag images around and see how they feel together"
- Values instant feel over perfect preview
- Wants to experiment quickly without waiting

**Frontend perspective:**
- UI performance matters for responsiveness
- Slow previews break creative flow
- Users expect instant feedback

**Research findings:**
- Users notice delays >100ms
- Creative flow breaks with delays >500ms
- Preview quality can be lower during exploration, higher for final review

---

## Opening Statements (Round 1)

### Maya Chen — The Experimental Photographer

*[Demonstrates dragging images quickly]*

Look, when I'm sequencing, I'm moving fast. I drag an image here, drag another there, try five different arrangements in a minute. If I have to wait even half a second for each preview to update, I lose my flow. The creative moment is gone.

**What I need:** Instant feedback. Show me thumbnails—low quality is fine. I don't need to see perfect crops or exact margins. I just need to see: does this image work here? Does the color flow? Does the composition feel right?

**Evidence:** My persona says I want to "quickly try different image sequences to see what 'feels right'" and I want "instant preview of how pages will look." But notice: I said "instant" first. Speed matters more than quality during exploration.

**Performance requirements:**
- Preview updates in <100ms
- Low-quality thumbnails are fine (256px, 128px even)
- Can upgrade to high quality when I'm done exploring

**What doesn't work:** Waiting for high-quality previews. If I have to wait 500ms for each preview, I'll lose my flow. I'll stop experimenting. I'll settle for a sequence that's "good enough" instead of finding the one that feels perfect.

**Trade-off:** I'd rather have instant, low-quality previews during sequencing, then see high-quality previews when I'm done. Don't make me wait during the creative process.

---

### Sam Taylor — The Software Developer

*[Shows performance benchmarks]*

I understand Maya's need for speed, but we need to think about technical trade-offs. Here's what I found:

**Current rendering performance:**
- Image decoding: ~50-100ms per image
- Cropping/scaling: ~20-50ms per image
- Canvas rendering: ~30-80ms per image
- Total: ~100-230ms per image for thumbnail

**If we want instant preview (<100ms):**
- We need to use existing thumbnails (no rendering)
- Or use very low quality (128px thumbnails)
- Or pre-render thumbnails for all images upfront

**If we want high-quality preview:**
- Full rendering takes ~200-500ms per image
- Multiple images = multiple renders
- Network transfer adds latency

**The technical reality:** We can't have both instant *and* high-quality previews. We have to choose:
1. **Fast, low-quality:** Use existing thumbnails, no rendering
2. **Medium, cached:** Render thumbnails once, cache them
3. **Slow, high-quality:** Render full-quality previews on demand

**My proposal:** Hybrid approach. Use low-quality thumbnails for sequencing (fast), render high-quality previews in background (cached), show high-quality when user pauses or requests it.

**Evidence:** The codebase already has thumbnail generation (512px default). We can use smaller thumbnails (256px, 128px) for sequencing, larger thumbnails (512px) for review.

---

### `web/src/` — The Frontend

*[Shows UI performance metrics]*

I'm the user's window into the system. If I'm slow, users feel it immediately. Here's what I know:

**UI performance requirements:**
- Frame rate: 60fps = 16.67ms per frame
- User input response: <100ms feels instant
- Visual feedback: <200ms feels responsive
- >500ms feels slow, breaks flow

**Current UI performance:**
- `SequencesTab.tsx` shows slide show with 2.5s interval: ```110:112:zine-layout/web/src/views/tabs/SequencesTab.tsx```
- Drag-and-drop reordering works smoothly
- But no real-time preview of page layouts

**What I need:**
- Fast image loading (use thumbnails, not full images)
- Lazy loading (load images as needed, not all at once)
- Progressive enhancement (low quality first, upgrade to high quality)
- Caching (don't re-render if nothing changed)

**My proposal:** 
- Use 256px thumbnails for sequencing (fast loading)
- Show full-quality previews on hover or click
- Cache rendered previews
- Use Web Workers for image processing if needed

**Evidence:** Modern web apps use progressive loading. Show low-quality first, upgrade to high-quality. Users understand this pattern. Don't make them wait for perfect previews during exploration.

---

## Rebuttals (Round 2)

### Maya Chen — Rebuttal

*[Responds to Sam and Frontend]*

Sam, I hear you on technical trade-offs, but I think you're overthinking this. I don't need perfect previews during sequencing. I just need to see images quickly.

**To Sam:** Your hybrid approach sounds good—low quality for sequencing, high quality for review. But make sure the low quality is *really* fast. 100ms is my limit. If it's slower, I'll lose flow.

**To Frontend:** Progressive loading is fine, but don't make me wait. Show me something immediately—even if it's blurry. I can work with blurry thumbnails. I can't work with loading spinners.

**What I'm proposing:** 
- 128px thumbnails for sequencing (instant)
- 512px thumbnails for review (cached)
- Full quality for export only

**Evidence:** When I sequence physically (prints on wall), I don't see perfect prints. I see small prints, sometimes blurry. But I can still see relationships, color flow, composition. Digital should be the same—fast, low-quality previews are fine.

---

### Sam Taylor — Rebuttal

*[Responds to Maya and Frontend]*

Maya, I understand your need for speed, but let me clarify the technical reality:

**128px thumbnails:**
- Still need to decode image (~50ms)
- Still need to scale (~10ms)
- Still need network transfer (~20-50ms)
- Total: ~80-110ms (within your 100ms limit)

**But here's the catch:** If we pre-render thumbnails for all images upfront, we can serve them instantly (<10ms). But that means:
- Longer initial load time (render all thumbnails)
- More storage (cache all thumbnails)
- More server resources

**To Frontend:** Progressive loading is good, but we need to decide: pre-render all thumbnails (slow initial load, fast sequencing) or render on demand (fast initial load, slower sequencing)?

**My proposal:** 
- Pre-render 128px thumbnails for all images on upload (background job)
- Serve thumbnails instantly during sequencing (<10ms)
- Render 512px thumbnails on demand for review
- Cache rendered previews

**Evidence:** The codebase already has thumbnail generation. We can extend it to pre-render thumbnails on upload. This gives instant previews during sequencing.

---

### `web/src/` — Rebuttal

*[Responds to Maya and Sam]*

Both of you are right, but let me add the frontend perspective:

**Pre-rendering thumbnails:**
- ✅ Instant previews during sequencing
- ✅ Smooth user experience
- ❌ Longer initial load time
- ❌ More storage needed

**On-demand rendering:**
- ✅ Fast initial load
- ✅ Less storage
- ❌ Slower previews during sequencing
- ❌ Breaks creative flow

**My proposal:** Hybrid approach with smart caching:
- Pre-render 128px thumbnails for first 20 images (fast initial load)
- Render remaining thumbnails in background (progressive)
- Cache all rendered thumbnails (don't re-render)
- Use Web Workers for image processing (don't block UI)

**To Maya:** I can show you 128px thumbnails instantly if they're pre-rendered. But we need to balance initial load time with sequencing speed.

**To Sam:** Pre-rendering all thumbnails upfront might be too slow for initial load. Let's do progressive pre-rendering—first 20 images immediately, rest in background.

**Evidence:** Modern web apps use progressive loading and background processing. We can have both fast initial load and instant previews during sequencing.

---

## Moderator Summary

### Key Arguments

1. **Maya Chen:** Speed is critical—<100ms preview updates. Low-quality thumbnails (128px) are fine during sequencing. High-quality previews can wait until review.

2. **Sam Taylor:** Technical trade-offs exist. Pre-rendering thumbnails gives instant previews but slower initial load. On-demand rendering gives fast initial load but slower previews. Proposes hybrid: pre-render 128px thumbnails, cache them, render 512px on demand.

3. **`web/src/`:** UI performance matters. Proposes progressive pre-rendering—first 20 images immediately, rest in background. Use Web Workers for processing. Cache everything.

### Tensions

1. **Initial load vs. sequencing speed:** Pre-rendering all thumbnails gives instant previews but slower initial load. On-demand rendering gives fast initial load but slower previews.

2. **Preview quality vs. speed:** Low-quality thumbnails (128px) are fast but may not show enough detail. High-quality thumbnails (512px) show detail but are slower.

3. **Storage vs. performance:** Caching all thumbnails uses storage but gives instant previews. On-demand rendering uses less storage but is slower.

### Interesting Ideas

1. **Progressive pre-rendering:** Pre-render first 20 images immediately, rest in background. Balances initial load time with sequencing speed.

2. **Quality tiers:** 128px for sequencing (fast), 512px for review (cached), full quality for export only.

3. **Smart caching:** Cache all rendered thumbnails, don't re-render if nothing changed.

4. **Web Workers:** Use background threads for image processing, don't block UI.

### Trade-offs

1. **Pre-render all thumbnails:**
   - ✅ Instant previews during sequencing
   - ✅ Smooth user experience
   - ❌ Slower initial load time
   - ❌ More storage needed

2. **On-demand rendering:**
   - ✅ Fast initial load
   - ✅ Less storage
   - ❌ Slower previews during sequencing
   - ❌ Breaks creative flow

3. **Progressive pre-rendering:**
   - ✅ Fast initial load (first 20 images)
   - ✅ Instant previews for initial images
   - ✅ Background processing for rest
   - ❌ Later images may be slower initially
   - ❌ More complex implementation

4. **Quality tiers:**
   - ✅ Fast sequencing (128px)
   - ✅ Good review (512px)
   - ✅ Perfect export (full quality)
   - ❌ Multiple thumbnail sizes to manage
   - ❌ More storage needed

### Open Questions

1. **Initial load strategy:** Pre-render all thumbnails upfront, or progressive pre-rendering?

2. **Thumbnail sizes:** What sizes should we support? 128px for sequencing? 512px for review? Others?

3. **Caching strategy:** Cache all thumbnails? Cache only recently used? How long to keep cache?

4. **Background processing:** Use Web Workers? Background jobs? How to prioritize?

5. **User control:** Should users be able to choose preview quality? Or should it be automatic?

### Next Steps

1. **Benchmark:** Measure current rendering performance
2. **Prototype:** Build progressive pre-rendering system
3. **Test:** Compare pre-render all vs. progressive vs. on-demand
4. **Optimize:** Find the right balance between initial load and sequencing speed

### Consensus

- ✅ Speed is critical during sequencing (<100ms)
- ✅ Low-quality thumbnails (128px) are fine during sequencing
- ✅ High-quality previews can wait until review
- ✅ Caching is important (don't re-render if nothing changed)
- ❓ Pre-render all vs. progressive vs. on-demand?

### Data Needed

- Performance benchmarks (rendering time, network latency)
- User testing (what preview quality is acceptable?)
- Storage analysis (how much space do thumbnails use?)
- Load time analysis (how long does pre-rendering take?)

---

**End of Debate Round 03**

