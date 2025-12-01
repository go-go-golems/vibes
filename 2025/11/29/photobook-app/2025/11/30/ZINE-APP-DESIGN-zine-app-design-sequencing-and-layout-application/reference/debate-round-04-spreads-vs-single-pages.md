---
Title: 'Debate Round 04: Do photographers want to see spreads (two pages) or single pages when sequencing?'
Ticket: ZINE-APP-DESIGN
Status: draft
Topics:
    - debate
    - sequencing
    - spreads
    - user-experience
DocType: reference
Intent: long-term
Owners: []
RelatedFiles: []
Summary: Fourth debate round exploring whether sequencing should show spreads or single pages - keeping UX simple and streamlined
LastUpdated: 2025-11-30T17:00:00-05:00
---

# Debate Round 04: Do photographers want to see spreads (two pages) or single pages when sequencing?

**Question:** Do photographers want to see spreads (two pages) or single pages when sequencing? Spread view shows how pages work together. Single page view is simpler.

**Primary Candidates:**
- Alex Rivera (Documentary Photographer) — Argues for spread view (narrative flow)
- Maya Chen (Experimental Photographer) — Argues for simplicity (single page)
- `pkg/pagelayout/` (The Page Renderer) — Argues for technical implications

**Why this question matters:** Affects the primary interface design. We want a simple, streamlined UX—don't try to support both equally if one is clearly better.

---

## Pre-Debate Research

### Current Codebase Spread Support

**Research conducted by:** Sam Taylor (Software Developer)

**Spread infrastructure exists:**

1. **Page layout settings:**
   - `IsSpread` flag in page settings
   - `GutterWidthIn` and `GutterOverlapIn` for binding
   - Spread rendering generates left/right variants: ```113:138:zine-layout/pkg/pagelayout/renderer/renderer.go```

2. **Spread rendering:**
   - Splits wide canvas into left and right pages
   - Handles gutter overlap for binding
   - Generates multiple variants (left, right, combined, thumbnail)

3. **Current UI:**
   - `SequencesTab.tsx` shows single-page slide show
   - No spread preview during sequencing
   - Spreads are rendered after applying page templates

**Conclusion:** Spread support exists in rendering pipeline but not in sequencing interface. Sequencing happens with raw images; spreads are created when applying page templates.

### Zine Design Research

**Research conducted by:** Jordan Kim (Designer/Photographer)

**How zines work:**
- Zines are printed on sheets, folded, bound
- Pages are arranged in spreads (two pages facing each other)
- Spreads are the fundamental unit of zine design
- Single pages don't exist in isolation—they're always part of a spread

**Design implications:**
- Photographers think in spreads, not single pages
- Spreads show how images work together
- Narrative flow happens across spreads
- Visual flow happens across spreads

**Conclusion:** Spreads are fundamental to zine design. Sequencing should show spreads, not single pages.

---

## Opening Statements (Round 1)

### Alex Rivera — The Documentary Photographer

*[Shows a zine spread with two pages facing each other]*

Look at this. These two pages work together. The left page sets up the scene; the right page shows the action. You can't see this relationship in single-page view. You can't understand narrative flow without seeing spreads.

**What I need:** Spread view as the primary interface. Show me two pages together—that's how zines work. Let me see how images flow across spreads. Let me sequence thinking about how pages work together, not in isolation.

**Evidence:** My persona says I want to "see how spreads work together (two-page layouts)" and I need "visual feedback on narrative flow." Spreads are essential for storytelling—single pages don't tell stories.

**Why spreads matter:**
- Narrative flow happens across spreads
- Visual relationships between pages
- How images work together
- How zines are actually printed

**What doesn't work:** Single-page view. It's artificial—pages don't exist in isolation. It doesn't show how images work together. It doesn't support narrative flow.

**Simple UX principle:** One primary view. Spreads are fundamental to zines, so show spreads. Don't complicate with single-page view—photographers don't think that way.

---

### Maya Chen — The Experimental Photographer

*[Shows a simple single-page view]*

Wait, Alex. I get that spreads matter for final output, but during sequencing? I just want to see images in order. I don't want to think about pages or spreads yet. I want to focus on the sequence—which image comes next?

**What I need:** Single-page view for sequencing. Show me images one at a time, in order. Let me focus on the sequence, not the layout. I can see spreads later, when I'm applying templates.

**Evidence:** My persona says I want to "drag images around and see how they feel together" and I want to "see how images flow together visually and narratively." But I'm thinking about image-to-image flow, not page-to-page flow.

**Why single pages work:**
- Simpler mental model
- Focus on sequence, not layout
- Less cognitive load
- Faster iteration

**What doesn't work:** Spread view during sequencing. It's too complex—I have to think about pages and layouts before I've even found the right sequence. It adds unnecessary complexity.

**Simple UX principle:** Keep sequencing simple. Show images in order. Don't add complexity (spreads) until it's needed (layout). Single-page view is simpler, faster, more focused.

---

### `pkg/pagelayout/` — The Page Renderer

*[Shows technical architecture]*

I need to clarify something. Spreads are a rendering concern, not a sequencing concern. Here's how it works:

**Current architecture:**
1. Sequence raw images (`ImageSequence`)
2. Apply layout template → `LaidOutImage`
3. Apply page template → `LaidOutPage` (with `IsSpread` flag)
4. Render page → generates left/right variants if spread

**The question:** Should sequencing show spreads or single pages?

**My perspective:** Sequencing happens with raw images, before page templates are applied. Spreads are created when applying page templates. So sequencing shouldn't show spreads—it should show images.

**But here's the thing:** If photographers want to see spreads during sequencing, we'd need to:
- Apply a default page template during sequencing
- Render spreads on the fly
- Show spread previews

**Technical implications:**
- More rendering during sequencing (slower)
- Need default page template (what if user changes it later?)
- More complex UI (spread rendering, gutter handling)

**Simple UX principle:** Keep sequencing and layout separate. Sequence images first, apply layouts later. Don't mix concerns—it adds complexity.

---

## Rebuttals (Round 2)

### Alex Rivera — Rebuttal

*[Responds to Maya and Page Renderer]*

Maya, I understand you want simplicity, but I think you're missing the point. Spreads aren't complexity—they're reality. Zines are printed as spreads. Photographers think in spreads. Hiding spreads during sequencing doesn't make sequencing simpler—it makes it disconnected from reality.

**To Maya:** You say single-page view is simpler, but is it? If I sequence thinking about single pages, then see spreads later and realize they don't work together, I have to re-sequence. That's not simpler—that's more work.

**To Page Renderer:** I understand spreads are a rendering concern, but sequencing is also a design concern. If I can't see how pages work together during sequencing, I'm designing blind. Show me spreads during sequencing—even if it's a default template. I can refine later.

**What I'm proposing:** Spread view as primary interface. Use a default page template for preview (can be changed later). Show spreads during sequencing. Keep it simple—one view, spreads. Don't complicate with single-page option.

**Evidence:** In zine design, spreads are fundamental. Photographers sequence thinking about spreads. The app should match how photographers think, not force them into an artificial single-page model.

---

### Maya Chen — Rebuttal

*[Responds to Alex and Page Renderer]*

Alex, I hear you on spreads, but I think you're overcomplicating sequencing. Sequencing is about finding the right order of images. Layout is about how those images appear on pages. They're different concerns.

**To Alex:** You say spreads are reality, but sequencing happens before layout. I sequence images, then apply layouts. Don't mix sequencing and layout—it adds complexity when I'm trying to be creative.

**To Page Renderer:** You're right—keep sequencing and layout separate. Sequence images first, apply layouts later. Don't show spreads during sequencing—show images in order.

**What I'm proposing:** Single-page view for sequencing. Show images one at a time, in order. Keep sequencing simple and focused. Show spreads later, when applying page templates.

**Simple UX principle:** One thing at a time. Sequence images first (simple, focused). Apply layouts later (spreads, templates). Don't mix concerns—it adds complexity.

**Evidence:** My workflow is: sequence images → see how they flow → apply templates → see spreads. Don't force me to think about spreads during sequencing—it breaks my creative flow.

---

### `pkg/pagelayout/` — Rebuttal

*[Responds to Alex and Maya]*

Both of you make good points, but let me clarify the technical reality:

**Current architecture:**
- Sequencing: `ImageSequence` (raw images, no layout)
- Layout: `LaidOutImage` (images with layout template)
- Pages: `LaidOutPage` (images on pages, with spread flag)
- Rendering: generates left/right variants

**If we show spreads during sequencing:**
- Need to apply default page template
- Need to render spreads on the fly
- Need to handle gutter, overlap
- More complex, slower

**If we show single pages during sequencing:**
- Just show images in order
- No rendering needed
- Simple, fast
- Apply layouts later

**My proposal:** Single-page view for sequencing (simple, fast). Show spreads when applying page templates (when it matters). Keep sequencing and layout separate.

**To Alex:** I understand spreads matter, but they're a layout concern, not a sequencing concern. Show spreads when applying layouts, not during sequencing.

**To Maya:** You're right—keep sequencing simple. Show images in order. Don't add layout complexity until it's needed.

**Simple UX principle:** Match the architecture. Sequence images (simple). Apply layouts (spreads). Don't mix concerns.

---

## Moderator Summary

### Key Arguments

1. **Alex Rivera:** Spreads are fundamental to zines. Photographers think in spreads. Show spreads during sequencing—it's how zines work. Single-page view is artificial.

2. **Maya Chen:** Keep sequencing simple. Show images one at a time, in order. Don't add layout complexity (spreads) until it's needed. Single-page view is simpler, faster, more focused.

3. **`pkg/pagelayout/`:** Spreads are a rendering/layout concern, not a sequencing concern. Keep sequencing and layout separate. Show images during sequencing, spreads when applying layouts.

### Tensions

1. **Reality vs. simplicity:** Spreads are how zines work, but showing spreads during sequencing adds complexity.

2. **Sequencing vs. layout:** Should sequencing show spreads (layout concern) or just images (sequencing concern)?

3. **Primary view:** Should the app default to spreads or single pages?

### Interesting Ideas

1. **Default template preview:** Show spreads during sequencing using a default page template (can be changed later). Balances reality with simplicity.

2. **Progressive disclosure:** Show single pages during sequencing (simple), show spreads when applying layouts (when it matters).

3. **Separate concerns:** Keep sequencing and layout separate. Sequence images first, apply layouts later.

### Trade-offs

1. **Spread view during sequencing:**
   - ✅ Shows how zines actually work
   - ✅ Supports narrative flow
   - ✅ Matches photographer thinking
   - ❌ More complex (needs default template)
   - ❌ Slower (needs rendering)
   - ❌ Mixes sequencing and layout concerns

2. **Single-page view during sequencing:**
   - ✅ Simple, focused
   - ✅ Fast (no rendering)
   - ✅ Clear separation of concerns
   - ❌ Doesn't show how pages work together
   - ❌ May need re-sequencing after seeing spreads
   - ❌ Doesn't match zine reality

3. **Progressive disclosure:**
   - ✅ Simple during sequencing (single pages)
   - ✅ Shows spreads when needed (layout)
   - ✅ Clear separation of concerns
   - ❌ May need re-sequencing after seeing spreads
   - ❌ Two different views to learn

### Open Questions

1. **Primary view:** Should sequencing default to spreads or single pages?

2. **Default template:** If showing spreads, what default page template should we use?

3. **User control:** Should users be able to toggle between spreads and single pages?

4. **Workflow:** Can we show spreads during sequencing without mixing concerns?

5. **Simple UX:** What's the simplest approach that still works?

### Next Steps

1. **User research:** Interview photographers about sequencing workflow
2. **Prototype:** Build spread view for sequencing
3. **Prototype:** Build single-page view for sequencing
4. **Test:** See which view photographers prefer

### Consensus

- ✅ Spreads are fundamental to zines
- ✅ Sequencing should be simple and focused
- ✅ Keep sequencing and layout separate (don't mix concerns)
- ❓ Should sequencing show spreads or single pages?

### Data Needed

- User interviews about sequencing workflow
- Analysis of how photographers think about sequencing
- Prototype testing of spread vs. single-page view
- Research on zine design workflow

### Simple UX Principle Applied

**Key insight:** We want simple, streamlined UX. Don't try to support both spreads and single pages equally—choose one primary view.

**Recommendation:** Single-page view for sequencing (simple, focused). Show spreads when applying page templates (when it matters). Keep sequencing and layout separate—don't mix concerns.

**Rationale:** 
- Sequencing is about finding the right order of images
- Layout is about how images appear on pages
- Keep them separate—simpler mental model
- Show spreads when applying layouts, not during sequencing

---

**End of Debate Round 04**

