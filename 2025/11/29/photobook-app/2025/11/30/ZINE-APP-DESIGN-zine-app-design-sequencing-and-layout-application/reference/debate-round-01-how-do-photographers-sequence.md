---
Title: 'Debate Round 01: How do photographers currently sequence images?'
Ticket: ZINE-APP-DESIGN
Status: draft
Topics:
    - debate
    - sequencing
    - user-research
DocType: reference
Intent: long-term
Owners: []
RelatedFiles: []
Summary: First debate round exploring how photographers currently sequence images and what this means for app design
LastUpdated: 2025-11-30T15:30:00-05:00
---

# Debate Round 01: How do photographers currently sequence images?

**Question:** How do photographers currently sequence images? Do they use physical prints on a wall? Digital tools (Lightroom, Bridge)? Mental/notebook planning?

**Primary Candidates:**
- Maya Chen (Experimental Photographer)
- Alex Rivera (Documentary Photographer)
- The New Hire (Fresh Eyes)

**Why this question matters:** Understanding current workflow helps design the sequencing interface. If photographers use physical prints on a wall, we should design a digital equivalent. If they use digital tools, we should understand what works and what doesn't.

---

## Pre-Debate Research

### Current Codebase Analysis

**Research conducted by:** Sam Taylor (Software Developer)

**Codebase findings:**

1. **Existing sequence infrastructure:**
   - `ImageSequence` type exists: ```28:36:zine-layout/pkg/repo/types.go```
   - `ImageSequenceItem` with `Position` field: ```38:44:zine-layout/pkg/repo/types.go```
   - Database schema supports sequences with position ordering: ```63:85:zine-layout/pkg/repo/sqlite/migrations.go```
   - API endpoints for creating, reordering sequences: `zine-layout/cmd/zine-layout/cmds/api/image_sequences/`

2. **Current UI implementation:**
   - `SequencesTab.tsx` exists with drag-and-drop: ```36:150:zine-layout/web/src/views/tabs/SequencesTab.tsx```
   - Slide show view for previewing sequences
   - Reorder functionality via `useReorderImageSequenceItemsMutation`
   - Supports gaps in sequences (`IsGap` field)

3. **Workflow commands:**
   - CLI commands for managing sequences: `zine-layout/cmd/zine-layout/cmds/workflow/image_sequences/`
   - `reorder.go` command for reordering items

**Conclusion:** The codebase already has sequence infrastructure, but the UI may not match photographer workflows.

### Persona Research

**Research conducted by:** Maya Chen, Alex Rivera

**Maya's current workflow:**
- Uses physical prints on wall for sequencing (mentioned in persona: "Physical prints on wall for sequencing (old school)")
- Lightroom for basic editing
- Instagram for quick sharing
- Wants to "drag images around and see how they feel together"

**Alex's current workflow:**
- Photo Mechanic for culling (hundreds of images)
- Lightroom for editing
- InDesign for final layout (but finds it slow)
- Needs to "find the 16 that tell the story, put them in the right order"

**Tools mentioned:**
- Lightroom (both personas)
- Photo Mechanic (Alex)
- InDesign (Alex, but slow)
- Physical prints (Maya)

---

## Opening Statements (Round 1)

### Maya Chen — The Experimental Photographer

*[Stands up, gestures toward a wall covered in printed photos]*

Look, I know this sounds old-fashioned, but I sequence images by printing them and putting them on my wall. There's something about seeing them physically, being able to move them around with my hands, stepping back to see the whole flow. It's tactile. It's immediate. I can try five different arrangements in ten minutes.

When I use digital tools like Lightroom, I'm stuck in a grid view or a filmstrip. I can't see the whole sequence at once. I can't feel how images flow together. The tools force me to think linearly—one image after another—instead of seeing relationships.

**What I need:** A digital version of my wall. Let me see all my images at once, drag them around, see how they feel together. Don't make me think about pages or layouts yet—just let me arrange images and see the sequence.

**Evidence:** My persona says I use "Physical prints on wall for sequencing (old school)" and I want to "drag images around and see how they feel together." The current `SequencesTab.tsx` has drag-and-drop, but it's still a linear list. I need a canvas view where I can see multiple images at once.

**What works in current codebase:** The drag-and-drop infrastructure exists. The `ImageSequenceItem` with `Position` field supports ordering. But the UI is still too linear.

**What doesn't work:** The slide show view is nice for preview, but it's not how I sequence. I need to see multiple images simultaneously, like a wall.

---

### Alex Rivera — The Documentary Photographer

*[Opens laptop, shows Photo Mechanic and Lightroom]*

I work with hundreds of images from assignments. I can't print them all—that's impractical. I use Photo Mechanic for culling because it's fast. Then Lightroom for editing. Then... well, then I struggle.

The problem is that sequencing happens in my head or in a notebook. I sketch out narrative structures: chronological, thematic, emotional arc. But there's no tool that helps me visualize that flow digitally.

**What I need:** A tool that lets me quickly arrange images, see how they tell a story together, test different narrative structures. I need to see spreads—two pages together—because that's how zines work. I need batch operations: apply a template to all pages, not configure each one individually.

**Evidence:** My persona says I "need to tell coherent narratives through image selection and sequencing" and I want to "test different narrative structures (chronological vs. thematic)." I use Photo Mechanic for culling, which is fast, but there's no equivalent for sequencing.

**What works in current codebase:** The sequence infrastructure exists. The `ImageSequence` type supports named sequences (I could create "Chronological" and "Thematic" versions). The API supports reordering.

**What doesn't work:** The UI doesn't help me see narrative flow. I need spread preview. I need to compare sequences side-by-side. The current UI is too focused on individual images, not the story they tell together.

---

### The New Hire — Fresh Eyes

*[Looks confused, raises hand]*

Wait, I'm new here, so maybe I'm missing something. But I looked at the codebase, and I see:

1. There's a `SequencesTab` with drag-and-drop
2. There's a slide show view
3. There's reordering functionality

But when I look at the personas, Maya says she uses "physical prints on wall" and Alex says he sequences "in my head or in a notebook." Neither of them mentions using a digital sequencing tool.

**My question:** Are we building a tool that photographers don't currently have? Or are we trying to replicate existing workflows? Because if photographers are using physical prints or mental planning, maybe we should start there instead of assuming they want a digital tool.

**Evidence:** 
- Maya: "Physical prints on wall for sequencing (old school)"
- Alex: "Sequencing happens in my head or in a notebook"
- Neither persona mentions using a digital sequencing tool

**What I'm confused about:** The codebase has sequence infrastructure, but do photographers actually want to sequence digitally? Or do they want something else—like a way to visualize sequences they've already planned?

**Alternative hypothesis:** Maybe photographers don't sequence digitally because existing tools don't support it well. Maybe they would use a digital tool if it felt right—like Maya's wall, or Alex's narrative structures.

---

## Rebuttals (Round 2)

### Maya Chen — Rebuttal

*[Responds to The New Hire]*

You're right to question assumptions, but I think you're missing the point. I use physical prints because digital tools don't give me what I need. If there was a digital tool that felt like arranging photos on a wall—where I could see multiple images at once, drag them around, step back to see the flow—I would use it.

The current `SequencesTab` is a start, but it's still too linear. I need a canvas view, not a list. Think Figma or Miro, but for photo sequences. Let me see thumbnails arranged spatially, drag them around, see relationships.

**To Alex:** I hear you on narrative structures, but I think we need different views. You want spread preview and narrative flow. I want spatial arrangement and visual flow. Maybe the app needs both: a canvas view for experimentation (me) and a spread view for narrative (you).

**Evidence:** The codebase has drag-and-drop, but it's still a vertical list. I need horizontal, spatial arrangement. The `Position` field is just an integer—it doesn't capture spatial relationships.

---

### Alex Rivera — Rebuttal

*[Nods at Maya, then addresses The New Hire]*

The New Hire makes a good point: photographers don't currently sequence digitally. But that's the opportunity. Photo Mechanic is fast for culling, but there's no equivalent for sequencing. InDesign is slow and overkill.

**To Maya:** I agree we need different views. But I think the primary view should be spreads—two pages together—because that's how zines work. You can't see narrative flow in single pages. The canvas view you want could be a secondary view for experimentation.

**To The New Hire:** You're right that photographers sequence mentally or physically. But that's because digital tools don't support it. If we build a tool that feels right—fast, visual, narrative-focused—photographers will use it. The question is: what does "feels right" mean?

**Evidence:** The current codebase has `LayoutSequence` which orders laid-out images for pages. But sequencing happens before layout. We need to sequence raw images first, then apply layout templates. The workflow should be: sequence images → apply templates → render pages → impose for print.

**What I'm proposing:** A spread-first view where photographers see two pages together, drag images between positions, see how the narrative flows. Then they can apply templates to the sequence.

---

### The New Hire — Rebuttal

*[Takes notes, then responds]*

Okay, I think I understand now. Both Maya and Alex want digital sequencing, but they want different things:

- **Maya:** Canvas view, spatial arrangement, visual flow
- **Alex:** Spread view, narrative flow, story structure

**My question:** Can we support both? Or do we need to pick one primary view?

**Also:** I looked at the codebase more, and I see there's a `LayoutSequence` that orders laid-out images. But Alex is talking about sequencing raw images before layout. Are these the same thing, or different?

**Evidence:** 
- `ImageSequence` orders raw `Asset` objects: ```28:36:zine-layout/pkg/repo/types.go```
- `LayoutSequence` orders `LaidOutImage` objects: ```69:77:zine-layout/pkg/repo/types.go```

So there are two types of sequences: raw images and laid-out images. Which one should photographers interact with?

**My hypothesis:** Photographers should sequence raw images first (Maya's canvas, Alex's narrative), then apply layout templates to create laid-out images, then arrange those into pages. The current codebase has both, but maybe the UI doesn't make this workflow clear.

---

### Sam Taylor — Point of Order!

*[Interrupts from the technical perspective]*

I need to clarify something. The codebase has:

1. **`ImageSequence`** — Orders raw `Asset` objects (what photographers upload)
2. **`LayoutSequence`** — Orders `LaidOutImage` objects (images after applying layout templates)

The workflow is:
- Upload assets → Create `ImageSequence` → Apply template → Create `LaidOutImage` → Create `LayoutSequence` → Render pages

**To The New Hire:** You're right—there are two types of sequences. But I think photographers should interact with `ImageSequence` first (raw images), then the system applies templates to create `LaidOutImage` objects, then arranges those into pages.

**To Maya and Alex:** The current `SequencesTab` works with `ImageSequence` (raw images), which is correct. But the UI might not feel right. Maya wants a canvas view. Alex wants spread preview. Both are valid—we might need multiple views of the same `ImageSequence`.

**Evidence:** The `SequencesTab.tsx` uses `useGetImageSequenceDetailQuery` which fetches `ImageSequence` items. The slide show view shows raw images. But there's no canvas view or spread view yet.

---

## Moderator Summary

### Key Arguments

1. **Maya Chen:** Wants a canvas view (like physical prints on wall) where she can see multiple images at once, drag them around spatially, see visual flow. Current UI is too linear.

2. **Alex Rivera:** Wants a spread view (two pages together) to see narrative flow. Needs to test different narrative structures. Current UI doesn't support spread preview or narrative visualization.

3. **The New Hire:** Questions whether photographers want digital sequencing at all, or if they prefer physical/mental workflows. Suggests we might be building something photographers don't currently use.

4. **Sam Taylor:** Clarifies technical architecture—`ImageSequence` (raw images) vs `LayoutSequence` (laid-out images). Confirms current UI works with `ImageSequence`, which is correct, but needs better views.

### Tensions

1. **Canvas view vs. Spread view:** Maya wants spatial arrangement; Alex wants spread preview. Can we support both?

2. **Digital vs. Physical:** Photographers currently use physical prints or mental planning. Do they want digital sequencing, or are we solving the wrong problem?

3. **Sequence type:** `ImageSequence` (raw) vs `LayoutSequence` (laid-out). Which should photographers interact with?

### Interesting Ideas

1. **Multiple views of the same sequence:** Canvas view for experimentation (Maya), spread view for narrative (Alex), list view for management.

2. **Workflow clarity:** Sequence raw images → Apply templates → Render pages. The UI should make this workflow obvious.

3. **Hybrid approach:** Start with digital equivalent of physical workflow (canvas view), then add narrative tools (spread view).

### Trade-offs

1. **Canvas view:**
   - ✅ Supports visual experimentation
   - ✅ Feels like physical prints on wall
   - ❌ Doesn't show how pages will look
   - ❌ May not support narrative structures

2. **Spread view:**
   - ✅ Shows how pages work together
   - ✅ Supports narrative flow
   - ❌ May be too focused on final output
   - ❌ Doesn't support spatial experimentation

3. **List view (current):**
   - ✅ Simple, linear
   - ✅ Easy to reorder
   - ❌ Doesn't show relationships
   - ❌ Doesn't support experimentation

### Open Questions

1. **Primary view:** Should the app default to canvas view, spread view, or list view?

2. **Sequence type:** Should photographers sequence raw images (`ImageSequence`) or laid-out images (`LayoutSequence`)?

3. **Workflow:** Should sequencing happen before or after applying layout templates?

4. **Multiple sequences:** Can photographers create multiple sequences (e.g., "Chronological" vs "Thematic") and compare them?

5. **Physical workflow:** Should we replicate physical prints on wall, or create something new that works better digitally?

### Next Steps

1. **Research:** Interview photographers about current sequencing workflows
2. **Prototype:** Build canvas view and spread view prototypes
3. **Test:** See which view photographers prefer for sequencing
4. **Decide:** Choose primary view (or support multiple views)

### Consensus

- ✅ Sequencing should happen with raw images (`ImageSequence`), not laid-out images
- ✅ Current drag-and-drop infrastructure is good, but UI needs improvement
- ✅ Photographers want to see relationships between images, not just linear order
- ❓ Whether to prioritize canvas view, spread view, or both

### Data Needed

- User interviews with photographers about sequencing workflows
- Prototype testing of canvas view vs spread view
- Analysis of how photographers currently sequence (physical vs digital vs mental)

---

**End of Debate Round 01**

