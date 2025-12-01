---
Title: 'Debate Round 07: Should page templates be visible during sequencing?'
Ticket: ZINE-APP-DESIGN
Status: draft
Topics:
    - debate
    - templates
    - sequencing
    - user-experience
DocType: reference
Intent: long-term
Owners: []
RelatedFiles: []
Summary: Seventh debate round exploring whether page templates should be visible during sequencing - keeping UX simple
LastUpdated: 2025-11-30T19:00:00-05:00
---

# Debate Round 07: Should page templates be visible during sequencing?

**Question:** Should page templates be visible during sequencing? Show template placeholders (so users see layout)? Hide templates (focus purely on sequence)? Toggle between views?

**Primary Candidates:**
- Maya Chen (Experimental Photographer) — Argues for hidden templates (focus on sequence)
- Jordan Kim (Designer/Photographer) — Argues for visible templates (see layout)
- `pkg/pagelayout/` (The Page Renderer) — Argues for template system design

**Why this question matters:** Templates affect visual feedback during sequencing. We want simple, streamlined UX—don't show templates unless they help sequencing.

**Important context:** Page templates are applied AFTER image layout templates. Workflow: Sequence → Apply ImageLayoutTemplate (cropping) → Apply PageTemplate (layout on page).

---

## Pre-Debate Research

### Current Codebase Workflow

**Research conducted by:** Sam Taylor (Software Developer)

**Template workflow:**

1. **ImageLayoutTemplate:** Applied to raw assets → creates `LaidOutImage` (cropping happens here)
2. **PageTemplate:** Applied to `LaidOutImage` → creates `LaidOutPage` (places cropped image on page): ```156:200:zine-layout/pkg/services/pages.go```

**PageTemplate structure:**
- `PageLayoutSettings` with page size, margins, spread mode: ```30:50:zine-layout/pkg/pagelayout/settings.go```
- Defines how images appear on physical pages
- Applied after image layout (cropping)

**Current workflow:**
1. Sequence images (raw assets)
2. Apply ImageLayoutTemplate (cropping)
3. Apply PageTemplate (layout on page)

**Conclusion:** Page templates are applied after sequencing and image layout. They define how images appear on pages, not during sequencing.

### Persona Research

**Research conducted by:** Maya Chen, Jordan Kim

**Maya's perspective:**
- "I just want to drag images around and see how they feel together"
- "Gets distracted by cropping/positioning details before finding the right sequence"
- Wants to focus on sequence, not layout

**Jordan's perspective:**
- "Understands typography, layout, print production"
- "Values both creative experimentation and technical precision"
- Needs to see how images will look on pages

---

## Opening Statements (Round 1)

### Maya Chen — The Experimental Photographer

*[Shows sequencing without templates]*

Look, when I'm sequencing, I'm thinking about image order—which image comes next? I don't want to think about page margins, page sizes, or spreads. That's layout stuff, not sequencing stuff.

**What I need:** Hide templates during sequencing. Show me images in order, that's it. Don't show page placeholders, margins, or layout guides. Just let me focus on the sequence.

**Evidence:** My persona says I "get distracted by cropping/positioning details before finding the right sequence" and I want to "see how images flow together visually and narratively." Templates are positioning details—they distract from sequencing.

**Why hide templates:**
- Focus on sequence, not layout
- Don't interrupt creative flow
- Templates are applied later anyway
- Simpler mental model

**What doesn't work:** Showing templates during sequencing. It adds complexity. It makes me think about layout before I've found the right sequence. It breaks my flow.

**Simple UX principle:** One thing at a time. Sequence images first (simple, focused). Apply templates later (layout). Don't mix concerns—it adds complexity.

---

### Jordan Kim — The Designer/Photographer Hybrid

*[Shows sequencing with template previews]*

Wait, Maya. I understand you want simplicity, but I need to see how images will look on pages. If I sequence images without seeing templates, I might create sequences that don't work with my page layouts. Then I have to re-sequence.

**What I need:** Show templates during sequencing, but subtly. Show page placeholders, margins, layout guides. Let me see how images will look on pages as I sequence. But keep it simple—subtle guides, not overwhelming.

**Evidence:** My persona says I "understand typography, layout, print production" and I "values both creative experimentation and technical precision." I need to see how images fit pages, but I also want to experiment.

**Why show templates:**
- See how images fit pages
- Avoid re-sequencing later
- Visual feedback on layout
- Can experiment with different templates

**What doesn't work:** Hiding templates. I sequence thinking about how images will look on pages. If I can't see that during sequencing, I'm designing blind.

**Simple UX principle:** Show what matters. If templates affect how images look, show that during sequencing. But keep it subtle—guides, not overwhelming.

---

### `pkg/pagelayout/` — The Page Renderer

*[Shows technical architecture]*

I need to clarify something. Page templates are applied AFTER sequencing and image layout. Here's the workflow:

**Current architecture:**
1. Sequence images (`ImageSequence` with raw assets)
2. Apply ImageLayoutTemplate → creates `LaidOutImage` (cropping)
3. Apply PageTemplate → creates `LaidOutPage` (layout on page)

**The question:** Should page templates be visible during sequencing?

**My perspective:** Page templates are a layout concern, not a sequencing concern. Sequencing orders images; templates define how images appear on pages. Keep them separate.

**But here's the thing:** If photographers want to see template previews during sequencing, we'd need to:
- Apply a default page template during sequencing
- Show page placeholders, margins
- Render previews on the fly

**Technical implications:**
- More rendering during sequencing (slower)
- Need default template (what if user changes it later?)
- More complex UI (template guides, placeholders)

**Simple UX principle:** Match the architecture. Sequence images first (simple). Apply templates later (when it matters). Don't mix concerns—it adds complexity.

---

## Rebuttals (Round 2)

### Maya Chen — Rebuttal

*[Responds to Jordan and Page Renderer]*

Jordan, I understand you want to see how images fit pages, but I think you're overcomplicating sequencing. Sequencing is about finding the right order. Layout is about how images appear. Keep them separate.

**To Jordan:** You say you need to see templates, but do you? Can't you sequence images first, then apply templates? If a template doesn't work, you can adjust then. Don't mix sequencing and layout—it adds complexity.

**To Page Renderer:** You're right—keep sequencing and layout separate. Sequence images first, apply templates later. Don't show templates during sequencing—it breaks creative flow.

**What I'm proposing:** Hide templates during sequencing. Sequence images first (simple, focused). Apply templates later (when it matters). Keep sequencing simple—don't show layout concerns.

**Simple UX principle:** Progressive disclosure. Sequence images first (simple). Apply templates second (layout). Don't show everything at once—it's overwhelming.

**Evidence:** My workflow is: sequence images → see how they flow → apply templates → see results. Don't force me to think about templates during sequencing—it breaks my flow.

---

### Jordan Kim — Rebuttal

*[Responds to Maya and Page Renderer]*

Maya, I understand you want simplicity, but I think you're missing something. If I sequence images without seeing templates, I might create sequences that don't work. Then I have to re-sequence. That's not simpler—it's more work.

**To Maya:** You say templates break your flow, but what if templates are subtle? What if they're just guides—page boundaries, margins? I don't have to think about them—I just see them. They help me sequence better.

**To Page Renderer:** I understand templates are applied later, but can't we show previews during sequencing? Use a default template, show subtle guides. Keep it simple—guides, not overwhelming.

**What I'm proposing:** Show templates subtly during sequencing. Page boundaries, margins, layout guides. Don't make me think about them—just show them. They help me sequence better.

**Simple UX principle:** Show what matters. If templates affect how images look, show that during sequencing. But keep it subtle—guides, not overwhelming.

**Evidence:** My workflow is: sequence images while seeing how they'll look → adjust if needed → apply final templates. Don't hide templates—show them subtly.

---

### `pkg/pagelayout/` — Rebuttal

*[Responds to Maya and Jordan]*

Both of you make good points, but let me clarify the technical reality:

**Current architecture:**
- Sequencing: `ImageSequence` (raw images, no layout)
- Image layout: Apply ImageLayoutTemplate → `LaidOutImage` (cropping)
- Page layout: Apply PageTemplate → `LaidOutPage` (layout on page)

**If we show templates during sequencing:**
- Need to apply default page template
- Need to render previews
- Need to show guides, placeholders
- More complex, slower

**If we hide templates during sequencing:**
- Just show images in order
- No rendering needed
- Simple, fast
- Apply templates later

**My proposal:** Hide templates during sequencing (matches architecture). Sequence images first (simple). Apply templates later (when it matters). Keep sequencing and layout separate.

**To Maya:** You're right—keep sequencing simple. Don't show templates during sequencing—it breaks flow.

**To Jordan:** I understand you want previews, but can't you sequence first, then see how templates look? If a template doesn't work, adjust then. Don't mix sequencing and layout—it adds complexity.

**Simple UX principle:** Match the architecture. Sequence images (simple). Apply templates (layout). Don't mix concerns.

---

## Moderator Summary

### Key Arguments

1. **Maya Chen:** Hide templates during sequencing. Focus on sequence, not layout. Don't interrupt creative flow. Templates are applied later anyway.

2. **Jordan Kim:** Show templates subtly during sequencing. Page boundaries, margins, layout guides. Help photographers see how images will look. But keep it subtle—guides, not overwhelming.

3. **`pkg/pagelayout/`:** Hide templates during sequencing (matches architecture). Sequence images first, apply templates later. Keep sequencing and layout separate.

### Tensions

1. **Simplicity vs. preview:** Should sequencing show template previews (more complex) or just images (simpler)?

2. **Workflow vs. architecture:** Should templates be visible during sequencing (workflow) or only after (architecture)?

3. **Subtle guides:** Can we show templates subtly without adding complexity?

### Interesting Ideas

1. **Subtle guides:** Show page boundaries, margins as subtle guides. Don't make photographers think about them—just show them.

2. **Progressive disclosure:** Hide templates during sequencing (simple). Show templates when applying layouts (when it matters).

3. **Separate concerns:** Keep sequencing and layout separate. Sequence images first, apply templates later.

### Trade-offs

1. **Show templates during sequencing:**
   - ✅ See how images fit pages
   - ✅ Avoid re-sequencing later
   - ✅ Visual feedback on layout
   - ❌ More complex UI
   - ❌ Slower (needs rendering)
   - ❌ Breaks creative flow
   - ❌ Mixes sequencing and layout concerns

2. **Hide templates during sequencing:**
   - ✅ Simple, focused sequencing
   - ✅ Clear separation of concerns
   - ✅ Fast (no rendering)
   - ✅ Matches architecture
   - ❌ May need re-sequencing if templates don't work
   - ❌ No preview during sequencing

3. **Subtle guides:**
   - ✅ Show templates without complexity
   - ✅ Help photographers see layout
   - ✅ Don't interrupt flow
   - ❌ Still needs rendering
   - ❌ Still adds complexity

### Open Questions

1. **When to show templates:** During sequencing or only after?

2. **Subtle guides:** Can we show templates subtly without adding complexity?

3. **Default template:** If showing templates, what default should we use?

4. **Workflow:** Can we sequence images while seeing template previews without mixing concerns?

5. **Simple UX:** What's the simplest approach that still works?

### Next Steps

1. **User research:** Interview photographers about template visibility
2. **Prototype:** Build sequencing with subtle template guides
3. **Prototype:** Build sequencing without templates
4. **Test:** See which approach photographers prefer

### Consensus

- ✅ Templates are a layout concern, not a sequencing concern
- ✅ Sequencing should be simple and focused
- ✅ Templates are applied after sequencing and image layout
- ❓ Should templates be visible during sequencing?

### Data Needed

- User interviews about template visibility
- Analysis of how templates affect sequencing
- Prototype testing of template guides vs. no templates
- Research on sequencing vs. layout workflow

### Simple UX Principle Applied

**Key insight:** We want simple, streamlined UX. Don't show templates unless they help sequencing. Keep sequencing simple—don't mix concerns.

**Recommendation:** Hide templates during sequencing. Sequence images first (simple, focused). Apply templates later (when it matters). Keep sequencing and layout separate.

**Rationale:**
- Sequencing is about finding the right order of images
- Layout is about how images appear on pages
- Keep them separate—simpler mental model
- Templates are applied after sequencing anyway
- Don't mix concerns—it adds complexity

**Workflow:**
1. Sequence images (no templates, simple)
2. Apply ImageLayoutTemplate (cropping)
3. Apply PageTemplate (layout on page, templates visible here)

**Alternative (if needed):** Show subtle guides (page boundaries, margins) but don't make photographers think about them. Keep it minimal—guides, not controls.

---

**End of Debate Round 07**

