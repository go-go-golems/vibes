---
Title: 'Debate Round 14: What image formats should be supported?'
Ticket: ZINE-APP-DESIGN
Status: draft
Topics:
    - debate
    - image-formats
    - file-upload
    - raw-processing
DocType: reference
Intent: long-term
Owners: []
RelatedFiles: []
Summary: Fourteenth debate round exploring image format support - JPEG/PNG only vs RAW support vs automatic processing
LastUpdated: 2025-11-30T23:30:00-05:00
---

# Debate Round 14: What image formats should be supported?

**Question:** What image formats should be supported? RAW files (need processing)? JPEG/PNG (ready to use)? Both (with automatic processing)?

**Primary Candidates:**
- Maya Chen (Experimental Photographer) — Argues for JPEG/PNG (ready to use)
- Alex Rivera (Documentary Photographer) — Argues for both (RAW + processed)
- Jordan Park (Backend Developer) — Argues for technical feasibility

**Why this question matters:** Affects import workflow and processing pipeline. We want simple UX—photographers shouldn't think about formats. But RAW support adds complexity.

---

## Pre-Debate Research

### Current Codebase Implementation

**Research conducted by:** Jordan Park (Backend Developer)

**Current support:**
- **PNG only** — `SavePNGImage` function validates `.png` extension
- File validation: `if !strings.HasSuffix(lower, ".png")` returns error
- Max file size: 64MB (`r.ParseMultipartForm(64 << 20)`)
- No RAW support currently

**Code reference:**
```43:54:zine-layout/pkg/projects/projects.go
func SavePNGImage(projectsRoot, projectID string, fh *multipart.FileHeader) (*SavedImage, error) {
	// ... validation ...
	lower := strings.ToLower(fh.Filename)
	if !strings.HasSuffix(lower, ".png") {
		return nil, fmt.Errorf("only .png uploads are supported (got %s)", fh.Filename)
	}
	// ... save PNG ...
}
```

**System spec:**
- "PNG only, max 64MB" (from system specification)
- Go's `image/draw` package supports PNG/JPEG natively
- No RAW processing libraries currently

**Conclusion:** Current implementation only supports PNG. Adding JPEG is straightforward (Go supports it). RAW support requires external libraries (libraw, dcraw) and adds complexity.

### Image Format Requirements

**Research conducted by:** Maya Chen, Alex Rivera

**Maya's workflow:**
- Film photography, scans images herself
- Exports scans as JPEG or PNG
- Ready to use, no processing needed

**Alex's workflow:**
- Digital cameras, shoots RAW
- Processes RAW files in Lightroom
- Exports processed JPEGs for zine
- Sometimes wants to use RAW directly

**Conclusion:** Different photographers have different needs. JPEG/PNG covers most cases. RAW support is nice-to-have for professional workflows.

---

## Opening Statements (Round 1)

### Maya Chen — The Experimental Photographer (Technical-Aware)

*[Shows JPEG/PNG workflow]*

Look, I work with film photography. I scan my images and export them as JPEG or PNG. They're ready to use—no processing needed. I don't want to think about formats—just upload and go.

**What I need:** JPEG and PNG support. That's it. Simple, ready to use. No RAW, no processing—just upload JPEG/PNG and sequence them.

**Evidence:** My persona says I "work primarily with film photography, scans images herself" and I "values experimentation and 'feeling' over technical precision." JPEG/PNG is enough—no RAW needed.

**Why JPEG/PNG only:**
- Simple, ready to use
- No processing needed
- Works for most photographers
- Matches my workflow (scanned images)

**What doesn't work:** RAW support. Too complex, requires processing. I don't need it—I process images before uploading.

**Simple UX principle:** Support JPEG and PNG. Simple, ready to use. No RAW, no processing—just upload and sequence.

---

### Alex Rivera — The Documentary Photographer (Technical-Aware)

*[Shows RAW + processed workflow]*

Wait, Maya. I understand you want JPEG/PNG only, but I work with RAW files. Sometimes I want to use RAW directly, sometimes I process them first. I need flexibility.

**What I need:** Both RAW and JPEG/PNG support. Automatic processing for RAW (convert to JPEG/PNG), but also allow direct RAW upload. Flexibility for different workflows.

**Evidence:** My persona says I "work with digital cameras, shoots hundreds of images per project" and I "needs to tell coherent narratives through image selection and sequencing." RAW support enables professional workflows.

**Why both formats:**
- Professional workflow (RAW + processed)
- Flexibility (use RAW directly or processed)
- Automatic processing (convert RAW to JPEG/PNG)
- Supports different workflows

**What doesn't work:** JPEG/PNG only. Too limiting for professional workflows. I need RAW support for flexibility.

**Simple UX principle:** Support both RAW and JPEG/PNG. Automatic processing for RAW (convert to JPEG/PNG), but also allow direct RAW upload. Flexibility for different workflows.

---

### Jordan Park — The Backend Developer

*[Shows technical implementation]*

I need to clarify what's possible. Current implementation only supports PNG. Adding JPEG is straightforward (Go's `image` package supports it). RAW support requires external libraries and adds complexity.

**What I can implement:**
- JPEG/PNG support (straightforward, Go supports it)
- RAW support (requires libraw/dcraw, adds complexity)
- Automatic RAW processing (convert to JPEG/PNG on upload)

**The question:** What formats should we support?

**My perspective:** Start with JPEG/PNG (simple, covers most cases). Add RAW support later if needed (more complex, requires external libraries).

**Why JPEG/PNG first:**
- Simple implementation (Go supports it)
- Covers most cases (Maya's workflow, most photographers)
- No external dependencies
- Fast upload (no processing needed)

**Why RAW later:**
- More complex (requires libraw/dcraw)
- External dependencies (C libraries)
- Processing overhead (convert RAW to JPEG/PNG)
- Can add later if needed

**What doesn't work:** Starting with RAW support. Too complex, adds dependencies. Start simple, add complexity later.

**Simple UX principle:** Start with JPEG/PNG (simple, covers most cases). Add RAW support later if needed. Progressive enhancement—start simple, add complexity when needed.

---

## Rebuttals (Round 2)

### Maya Chen — Rebuttal

*[Responds to Alex and Jordan]*

Alex, I understand you want RAW support, but I think you're overcomplicating this. JPEG/PNG covers most cases. Most photographers process images before uploading—they don't need RAW support.

**To Alex:** You say you need RAW support, but can't you process RAW files in Lightroom first, then upload JPEGs? That's simpler—process once, upload ready-to-use images.

**To Jordan:** JPEG/PNG first sounds good. Simple, covers most cases. Add RAW later if really needed.

**What I'm proposing:** Support JPEG and PNG. Simple, ready to use. No RAW, no processing—just upload and sequence. Add RAW later if really needed.

**Simple UX principle:** Support JPEG and PNG. Simple, ready to use. Add RAW later if really needed. Progressive enhancement—start simple.

---

### Alex Rivera — Rebuttal

*[Responds to Maya and Jordan]*

Maya, I understand you want JPEG/PNG only, but I think you're missing something. Professional workflows need RAW support. Sometimes I want to use RAW directly, sometimes I process them first.

**To Maya:** You say process RAW files first, but that's extra work. If the app can process RAW automatically, that's better—upload RAW, app converts to JPEG/PNG automatically.

**To Jordan:** RAW support is important for professional workflows. Yes, it's more complex, but worth it. Automatic processing (convert RAW to JPEG/PNG on upload) makes it simple for users.

**What I'm proposing:** Support both RAW and JPEG/PNG. Automatic processing for RAW (convert to JPEG/PNG on upload). Flexibility for different workflows.

**Simple UX principle:** Support both RAW and JPEG/PNG. Automatic processing for RAW. Flexibility for different workflows. Simple for users, complex backend is fine.

---

### Jordan Park — Rebuttal

*[Responds to Maya and Alex]*

Both of you make good points, but let me clarify the technical reality:

**What I can implement:**
- JPEG/PNG support (straightforward, Go supports it)
- RAW support (requires libraw/dcraw, adds complexity)
- Automatic RAW processing (convert to JPEG/PNG on upload)

**If we prioritize JPEG/PNG only:**
- Simple implementation (no external dependencies)
- Covers most cases (Maya's workflow, most photographers)
- Fast upload (no processing needed)
- Can add RAW later if needed

**If we prioritize RAW support:**
- More complex (requires libraw/dcraw)
- External dependencies (C libraries)
- Processing overhead (convert RAW to JPEG/PNG)
- Supports professional workflows (Alex's needs)

**My proposal:** Start with JPEG/PNG (simple, covers most cases). Add RAW support later if needed (more complex, requires external libraries). Progressive enhancement—start simple, add complexity when needed.

**To Maya:** JPEG/PNG first is simple, covers most cases.

**To Alex:** RAW support is important, but can add later. Start simple, add complexity when needed.

**Simple UX principle:** Start with JPEG/PNG (simple, covers most cases). Add RAW support later if needed. Progressive enhancement—start simple, add complexity when needed.

---

## Moderator Summary

### Key Arguments

1. **Maya Chen:** Support JPEG and PNG. Simple, ready to use. No RAW, no processing—just upload and sequence.

2. **Alex Rivera:** Support both RAW and JPEG/PNG. Automatic processing for RAW (convert to JPEG/PNG on upload). Flexibility for different workflows.

3. **Jordan Park:** Start with JPEG/PNG (simple, covers most cases). Add RAW support later if needed (more complex, requires external libraries).

### Tensions

1. **Format support:** JPEG/PNG only vs. RAW + JPEG/PNG
2. **Processing:** No processing vs. automatic RAW processing
3. **Complexity:** Simple implementation vs. professional workflow support

### Interesting Ideas

1. **JPEG/PNG first:** Simple, covers most cases, can add RAW later
2. **Automatic RAW processing:** Convert RAW to JPEG/PNG on upload (simple for users)
3. **Progressive enhancement:** Start simple, add complexity when needed

### Trade-offs

1. **JPEG/PNG only:**
   - ✅ Simple implementation (no external dependencies)
   - ✅ Covers most cases (Maya's workflow, most photographers)
   - ✅ Fast upload (no processing needed)
   - ❌ Doesn't support professional workflows (Alex's needs)
   - ❌ No RAW support

2. **RAW + JPEG/PNG:**
   - ✅ Supports professional workflows (Alex's needs)
   - ✅ Flexibility (use RAW directly or processed)
   - ✅ Automatic processing (convert RAW to JPEG/PNG)
   - ❌ More complex (requires libraw/dcraw)
   - ❌ External dependencies (C libraries)
   - ❌ Processing overhead (convert RAW to JPEG/PNG)

3. **Progressive enhancement:**
   - ✅ Start simple (JPEG/PNG)
   - ✅ Add complexity when needed (RAW support)
   - ✅ Covers most cases initially
   - ✅ Can add RAW later if needed
   - ❌ May need to refactor later

### Open Questions

1. **Format support:** JPEG/PNG only vs. RAW + JPEG/PNG?
2. **Processing:** No processing vs. automatic RAW processing?
3. **RAW libraries:** Which library? libraw? dcraw? Go bindings?
4. **Processing strategy:** Convert on upload vs. lazy conversion?
5. **File size limits:** Different limits for RAW vs. JPEG/PNG?

### Next Steps

1. **User research:** Survey photographers about format needs
2. **Prototype:** Add JPEG support (straightforward)
3. **Prototype:** Add RAW support (libraw/dcraw integration)
4. **Benchmark:** Measure processing time (RAW conversion)
5. **Test:** See which formats photographers prefer

### Consensus

- ✅ JPEG/PNG support is essential (covers most cases)
- ✅ RAW support is nice-to-have (professional workflows)
- ✅ Simple implementation is preferred (start with JPEG/PNG)
- ❓ Should we add RAW support now or later?

### Data Needed

- User survey about format needs
- Analysis of photographer workflows (RAW vs. processed)
- Performance benchmarks (RAW conversion time)
- Library evaluation (libraw vs. dcraw vs. Go bindings)

### Simple UX Principle Applied

**Key insight:** We want simple, streamlined UX. Photographers shouldn't think about formats. Support JPEG/PNG (simple, ready to use), add RAW support later if needed.

**Recommendation:** Start with JPEG/PNG support (simple, covers most cases). Add RAW support later if needed (more complex, requires external libraries). Progressive enhancement—start simple, add complexity when needed.

**Rationale:**
- JPEG/PNG covers most cases (Maya's workflow, most photographers)
- Simple implementation (no external dependencies, Go supports it)
- Fast upload (no processing needed)
- Can add RAW later if needed (progressive enhancement)
- Simple UX (photographers upload JPEG/PNG, no format thinking)

**Workflow:**
1. Support JPEG and PNG uploads (simple, Go supports it)
2. Validate file format (check extension, MIME type)
3. Save images directly (no processing needed)
4. Add RAW support later if needed (automatic conversion to JPEG/PNG)

---

**End of Debate Round 14**

