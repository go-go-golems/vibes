---
Title: 'Debate Round 13: How should the app handle large image sets?'
Ticket: ZINE-APP-DESIGN
Status: draft
Topics:
    - debate
    - performance
    - large-datasets
    - lazy-loading
DocType: reference
Intent: long-term
Owners: []
RelatedFiles: []
Summary: Thirteenth debate round exploring how to handle large image sets - load all upfront vs lazy load vs thumbnail-first
LastUpdated: 2025-11-30T23:00:00-05:00
---

# Debate Round 13: How should the app handle large image sets?

**Question:** How should the app handle large image sets? Load all images upfront (simple, but slow)? Lazy load as needed (complex, but fast)? Thumbnail-first approach?

**Primary Candidates:**
- Alex Rivera (Documentary Photographer) — Argues for performance (hundreds of images)
- Sam Chen (Frontend Developer) — Argues for UI performance and efficient loading
- Jordan Park (Backend Developer) — Argues for API design and database performance
- `web/src/store/` (State Management) — Argues for RTK Query caching strategies

**Why this question matters:** Performance affects user experience with large projects. We want simple UX, but need to handle hundreds of images efficiently. Trade-offs between simplicity and performance.

---

## Pre-Debate Research

### Current Codebase Implementation

**Research conducted by:** Sam Chen (Frontend Developer)

**Current approach:**
- `useGetAssetsQuery({ projectId })` loads ALL assets upfront
- RTK Query caches responses (30s default)
- No pagination or lazy loading
- Assets loaded in `SequencesTab.tsx` line 37: `const assetsQuery = useGetAssetsQuery({ projectId })`
- All assets mapped to `AssetSummary[]` immediately (lines 61-76)

**Code reference:**
```37:76:zine-layout/web/src/views/tabs/SequencesTab.tsx
const assetsQuery = useGetAssetsQuery({ projectId }, { skip: !projectId });
// ... later ...
const assets = useMemo<AssetSummary[]>(() => {
  if (!assetsQuery.data || !projectId) return [];
  return assetsQuery.data.map((asset) => {
    const base =
      asset.url ?? `/projects/${projectId}/images/${encodeURIComponent(asset.filename)}`;
    const bust = asset.uploaded_at ? new Date(asset.uploaded_at).getTime() : Date.now();
    return {
      id: asset.id,
      name: asset.filename,
      width: asset.width,
      height: asset.height,
      src: `${base}?t=${bust}`,
      uploadedPath: base,
    };
  });
}, [assetsQuery.data, projectId]);
```

**Current performance characteristics:**
- System spec mentions ~10,000 assets per project is sufficient
- SQLite indexes: `idx_assets_project` for fast listing
- RTK Query caching reduces redundant API calls
- No pagination or lazy loading currently

**Conclusion:** Current implementation loads all assets upfront. Works for small-medium projects, but may be slow for large projects (hundreds of images).

### Database Performance Analysis

**Research conducted by:** Jordan Park (Backend Developer)

**Database queries:**
- `GET /api/projects/{id}/images` returns all assets for a project
- SQLite query: `SELECT * FROM assets WHERE project_id = ?`
- Indexed on `project_id` (`idx_assets_project`)
- Query is fast (<10ms for 1000 assets)

**API response size:**
- Each asset: ~200 bytes JSON (id, filename, width, height, url, uploaded_at)
- 1000 assets = ~200KB JSON response
- 10000 assets = ~2MB JSON response

**Conclusion:** Database queries are fast, but large JSON responses may be slow over network. API design needs pagination or filtering.

---

## Opening Statements (Round 1)

### Alex Rivera — The Documentary Photographer (Technical-Aware)

*[Shows workflow with 200+ images]*

Look, I work with hundreds of images per project. When I open a project, I need to see my images quickly. If the app loads all 200 images upfront, that's fine—I can wait a few seconds. But if it's slow or laggy, that's a problem.

**What I need:** Fast loading, even with hundreds of images. I don't care how it's implemented—just make it fast. If loading all upfront is fast enough, great. If not, lazy load or thumbnail-first is fine.

**Evidence:** My persona says I "work with digital cameras, shoots hundreds of images per project" and I "need fast image selection and sequencing." Speed matters, but I'm flexible on implementation.

**Why performance matters:**
- Hundreds of images per project
- Need to see images quickly
- Don't want to wait for loading
- Sequencing should feel fast

**What doesn't work:** Slow loading, laggy UI, waiting for images to load. If the app is slow with large image sets, I'll use a different tool.

**Simple UX principle:** Fast loading is essential. Load all upfront if it's fast enough. Otherwise, lazy load or thumbnail-first. Just make it fast.

---

### Sam Chen — The Frontend Developer

*[Shows RTK Query caching and loading strategies]*

I need to clarify the technical reality. Current implementation loads all assets upfront via `useGetAssetsQuery`. This works for small projects, but may be slow for large projects.

**What I can implement:**
- Load all upfront (current, simple)
- Lazy load (pagination, infinite scroll)
- Thumbnail-first (load thumbnails, then full images)
- Virtual scrolling (render only visible items)

**The question:** What loading strategy should we use?

**My perspective:** Thumbnail-first approach. Load thumbnails immediately (fast), then load full images as needed. RTK Query caching handles this well—cache thumbnails, lazy load full images.

**Why thumbnail-first:**
- Fast initial load (thumbnails are small)
- Progressive enhancement (full images when needed)
- RTK Query caching (cache thumbnails, lazy load full)
- Virtual scrolling (render only visible thumbnails)

**What doesn't work:** Loading all full images upfront. Too slow for large projects. Network overhead, memory usage, slow initial load.

**Simple UX principle:** Fast initial load (thumbnails), progressive enhancement (full images). RTK Query caching makes this efficient.

---

### Jordan Park — The Backend Developer

*[Shows API design and database performance]*

Wait, Sam. I need to clarify the backend reality. Current API returns all assets in one response. Database queries are fast (<10ms for 1000 assets), but large JSON responses may be slow over network.

**What I can implement:**
- Return all assets (current, simple)
- Pagination (limit/offset, cursor-based)
- Filtering (by sequence, by template)
- Thumbnail metadata only (exclude full image URLs)

**The question:** What API design should we use?

**My perspective:** Pagination with thumbnail metadata. Return paginated list of assets with thumbnail URLs, exclude full image URLs. Frontend can lazy load full images as needed.

**Why pagination:**
- Smaller API responses (faster network)
- Database queries stay fast (indexed)
- Frontend can lazy load (progressive)
- Scales to large projects (1000+ assets)

**What doesn't work:** Returning all assets in one response. Large JSON responses (2MB+ for 10000 assets) are slow over network. Network overhead, slow initial load.

**Simple UX principle:** Pagination with thumbnail metadata. Fast API responses, frontend lazy loads full images. Scales to large projects.

---

### `web/src/store/` — The State Management

*[Shows RTK Query caching strategies]*

All of you are missing something. RTK Query already handles caching well. The question is: what should we cache?

**What RTK Query provides:**
- Automatic caching (30s default)
- Cache invalidation (tags, refetch)
- Optimistic updates (feel fast)
- Background refetching (keep cache fresh)

**The question:** What loading strategy works best with RTK Query caching?

**My perspective:** Load all asset metadata upfront (small JSON), lazy load full images. RTK Query caches metadata (fast), lazy loads full images (progressive). Best of both worlds.

**Why metadata-first:**
- Small API responses (metadata only, ~200 bytes per asset)
- RTK Query caches metadata (fast subsequent loads)
- Lazy load full images (progressive enhancement)
- Virtual scrolling (render only visible items)

**What doesn't work:** Loading all full images upfront. Too slow, too much memory. RTK Query caching helps, but still slow initial load.

**Simple UX principle:** Load metadata upfront (fast), lazy load full images (progressive). RTK Query caching makes this efficient.

---

## Rebuttals (Round 2)

### Alex Rivera — Rebuttal

*[Responds to Sam, Jordan, and `web/src/store/`]*

All of you are overcomplicating this. I just need fast loading. If loading all assets upfront is fast enough, great. If not, lazy load or thumbnail-first is fine.

**To Sam:** Thumbnail-first sounds good, but make sure it's fast. I don't want to wait for thumbnails to load.

**To Jordan:** Pagination sounds good, but make sure it's simple. I don't want to think about pagination—just show me my images.

**To `web/src/store/`:** Metadata-first sounds good, but make sure full images load quickly when I need them.

**What I'm proposing:** Load all asset metadata upfront (fast), lazy load full images (progressive). Simple for me, efficient for you. Just make it fast.

**Simple UX principle:** Fast loading is essential. Load metadata upfront, lazy load full images. Just make it fast.

---

### Sam Chen — Rebuttal

*[Responds to Alex, Jordan, and `web/src/store/`]*

Alex, I understand you want fast loading, but let me clarify the technical reality. Loading all assets upfront works for small projects, but may be slow for large projects (200+ images).

**To Alex:** Loading all assets upfront may be slow for large projects. Thumbnail-first is faster—load thumbnails immediately, then full images as needed.

**To Jordan:** Pagination is good, but RTK Query can handle it. Load paginated metadata, cache it, lazy load full images.

**To `web/src/store/`:** Metadata-first is good, but we need thumbnails for sequencing. Load thumbnails immediately, then full images as needed.

**What I'm proposing:** Thumbnail-first approach. Load thumbnails immediately (fast), lazy load full images (progressive). RTK Query caching makes this efficient.

**Simple UX principle:** Fast initial load (thumbnails), progressive enhancement (full images). RTK Query caching makes this efficient.

---

### Jordan Park — Rebuttal

*[Responds to Alex, Sam, and `web/src/store/`]*

Both of you make good points, but let me clarify the backend reality. Current API returns all assets in one response. Database queries are fast, but large JSON responses may be slow over network.

**To Alex:** Loading all assets upfront may be slow for large projects. Pagination reduces API response size, faster network transfer.

**To Sam:** Thumbnail-first is good, but we need pagination for API responses. Return paginated metadata with thumbnail URLs, frontend lazy loads full images.

**To `web/src/store/`:** Metadata-first is good, but we need pagination for large projects. Return paginated metadata, RTK Query caches it, frontend lazy loads full images.

**What I'm proposing:** Pagination with thumbnail metadata. Return paginated list of assets with thumbnail URLs, exclude full image URLs. Frontend lazy loads full images as needed.

**Simple UX principle:** Pagination with thumbnail metadata. Fast API responses, frontend lazy loads full images. Scales to large projects.

---

### `web/src/store/` — Rebuttal

*[Responds to Alex, Sam, and Jordan]*

All of you are thinking about different layers, but let me clarify the state management reality. RTK Query handles caching well, but we need to decide what to cache.

**To Alex:** Fast loading is important, but RTK Query caching helps. Load metadata upfront, cache it, lazy load full images.

**To Sam:** Thumbnail-first is good, but RTK Query can cache thumbnails. Load thumbnails immediately, cache them, lazy load full images.

**To Jordan:** Pagination is good, but RTK Query can handle it. Load paginated metadata, cache it, lazy load full images.

**What I'm proposing:** Load paginated metadata with thumbnails upfront (fast), lazy load full images (progressive). RTK Query caches metadata and thumbnails, lazy loads full images. Best of all worlds.

**Simple UX principle:** Load metadata with thumbnails upfront (fast), lazy load full images (progressive). RTK Query caching makes this efficient.

---

## Moderator Summary

### Key Arguments

1. **Alex Rivera:** Fast loading is essential. Load all assets upfront if it's fast enough. Otherwise, lazy load or thumbnail-first. Just make it fast.

2. **Sam Chen:** Thumbnail-first approach. Load thumbnails immediately (fast), lazy load full images (progressive). RTK Query caching makes this efficient.

3. **Jordan Park:** Pagination with thumbnail metadata. Return paginated list of assets with thumbnail URLs, exclude full image URLs. Frontend lazy loads full images as needed.

4. **`web/src/store/`:** Load paginated metadata with thumbnails upfront (fast), lazy load full images (progressive). RTK Query caches metadata and thumbnails, lazy loads full images.

### Tensions

1. **Loading strategy:** Load all upfront vs. lazy load vs. thumbnail-first
2. **API design:** Return all assets vs. pagination vs. filtering
3. **Caching strategy:** What to cache (metadata, thumbnails, full images)

### Interesting Ideas

1. **Thumbnail-first:** Load thumbnails immediately, then full images as needed
2. **Pagination with thumbnails:** Return paginated metadata with thumbnail URLs
3. **Metadata-first:** Load asset metadata upfront, lazy load full images
4. **RTK Query caching:** Cache metadata and thumbnails, lazy load full images

### Trade-offs

1. **Load all upfront:**
   - ✅ Simple implementation
   - ✅ Works for small projects
   - ❌ Slow for large projects (200+ images)
   - ❌ Large JSON responses (2MB+ for 10000 assets)

2. **Lazy load:**
   - ✅ Fast initial load
   - ✅ Scales to large projects
   - ❌ More complex implementation
   - ❌ Need pagination API

3. **Thumbnail-first:**
   - ✅ Fast initial load (thumbnails are small)
   - ✅ Progressive enhancement (full images when needed)
   - ✅ RTK Query caching (cache thumbnails)
   - ❌ Need thumbnail generation
   - ❌ More complex implementation

4. **Pagination with thumbnails:**
   - ✅ Fast API responses
   - ✅ Scales to large projects
   - ✅ RTK Query caching (cache paginated metadata)
   - ❌ More complex implementation
   - ❌ Need pagination API

### Open Questions

1. **Loading strategy:** Load all upfront vs. lazy load vs. thumbnail-first?
2. **API design:** Return all assets vs. pagination vs. filtering?
3. **Thumbnail generation:** When to generate thumbnails? Server-side or client-side?
4. **Virtual scrolling:** Should we use virtual scrolling for large lists?
5. **Performance budget:** What's acceptable load time? <1s? <2s?

### Next Steps

1. **User research:** Test loading performance with large image sets (200+ images)
2. **Prototype:** Build thumbnail-first loading
3. **Prototype:** Build pagination API
4. **Benchmark:** Measure load times (metadata vs. thumbnails vs. full images)
5. **Test:** See which strategy photographers prefer

### Consensus

- ✅ Fast loading is essential for large image sets
- ✅ Current implementation (load all upfront) works for small projects
- ✅ Need better strategy for large projects (200+ images)
- ❓ Should we use thumbnail-first, pagination, or both?

### Data Needed

- Performance benchmarks (load times for different strategies)
- User testing with large image sets (200+ images)
- API response size analysis (metadata vs. thumbnails vs. full images)
- RTK Query caching performance analysis

### Simple UX Principle Applied

**Key insight:** We want simple, streamlined UX. Fast loading is essential, but implementation can be complex. Load metadata/thumbnails upfront (fast), lazy load full images (progressive).

**Recommendation:** Thumbnail-first approach with pagination. Load paginated metadata with thumbnails upfront (fast), lazy load full images as needed (progressive). RTK Query caches metadata and thumbnails, lazy loads full images.

**Rationale:**
- Fast initial load (thumbnails are small, paginated metadata is fast)
- Progressive enhancement (full images when needed)
- RTK Query caching (cache metadata and thumbnails, lazy load full images)
- Scales to large projects (pagination handles large image sets)
- Simple UX (photographers see thumbnails immediately, full images load when needed)

**Workflow:**
1. Load paginated metadata with thumbnails upfront (fast API response, RTK Query caches)
2. Display thumbnails immediately (fast initial load)
3. Lazy load full images as needed (progressive enhancement)
4. RTK Query caching handles subsequent loads (fast cache hits)

---

**End of Debate Round 13**

