---
Title: Engine inputs grouping analysis
Ticket: ENGINE-INPUTS-STRUCT
Status: active
Topics:
    - imagelayout
    - layout
DocType: design
Intent: long-term
Owners: []
RelatedFiles: []
ExternalSources: []
Summary: ""
LastUpdated: 2025-11-30T21:18:14.449922927-05:00
---

# Engine inputs grouping analysis

## 1. Context

The imagelayout engine still uses a single `Inputs` struct in `zine-layout/pkg/imagelayout/engine/engine.go`. Even though the public API is moving to `LayoutRequest` (Frame / Crop / Presentation), this internal struct mixes every normalized value (margins, crop offsets, zoom factors, presentation offsets). That makes it hard to reason about the data flow and prevents us from enforcing invariants such as “margins only apply to page mode”.

## 2. Current state

- `Inputs` currently contains ~25 fields that are a blend of frame, crop, and presentation concepts.
- `InputsFromSettings` and `InputsFromRequest` both populate the same struct, so the engine math still accepts impossible combinations (e.g., `MarginTopPx` can be non-zero while `Mode=viewport`).
- The three helper phases (`buildFrame`, `resolveCrop`, `composeTarget`) conceptually operate on separate domains but still read/write the shared struct.

## 3. Problems

| Issue | Evidence | Impact |
| --- | --- | --- |
| Impossible field combinations sneak through | `Inputs` allows margins + viewport + fit flags simultaneously | Bugs become runtime-only; no compile-time guidance |
| Tests have to reason about dozens of fields | `engine_test.go` fixtures set only a subset of the required knobs | Harder to extend coverage for new features |
| Hard to trace Frame/Crop/Presentation responsibilities | Code comments/trace steps recreate the grouping ad hoc | New contributors must re-derive the conceptual model |

## 4. Proposal

Introduce a typed internal representation that mirrors the top-level API:

```go
type NormalizedInputs struct {
    Frame        FrameInputs
    Crop         CropInputs
    Presentation PresentationInputs
    Source       SourceMeta
}
```

Where each sub-struct contains only the knobs relevant to that phase. Helper functions (`buildFrame`, etc.) would accept/return these structs instead of raw floats. Validation for mode-specific fields happens when we construct `FrameInputs`.

Key decisions:

1. **Tagged frames** – implement a small sum type (`type FrameInputs struct { Kind FrameKind; Data any }`) or explicit structs (RatioFrameInputs, PageFrameInputs, ViewportFrameInputs) referenced via interfaces.
2. **Crop strategies** – encode the strategy enum and per-strategy payload (anchor preset vs. focus point vs. manual pan) so `resolveCrop` no longer needs to inspect unrelated fields.
3. **Presentation** – make offsets/scale/clamp explicit and drop the overloaded `Units` field.

## 5. Plan

1. **Survey consumers**  
   - Confirm only `ComputeViewport` reads `Inputs` (today true).  
   - Note any helper utilities/tests that need adaption.

2. **Define internal structs**  
   - Add `FrameInputs`, `CropInputs`, `PresentationInputs`, `SourceMeta` in `engine/types_internal.go` (new file).  
   - Implement constructors + validation helpers with mode-specific checks.

3. **Update normalization**  
   - Teach `InputsFromRequest` / `InputsFromSettings` to build the new structs.  
   - Add unit tests per mode/strategy to ensure invalid combos fail early.

4. **Refactor engine helpers**  
   - `buildFrame` takes `FrameInputs` instead of the full struct.  
   - `resolveCrop` operates on `CropInputs` plus `SourceMeta`.  
   - `composeTarget` uses `PresentationInputs` + frame output.

5. **Trim legacy struct**  
   - Remove the mega `Inputs`.  
   - Update traces to log grouped data (Frame/Crop/Presentation sections).

6. **Docs & follow-ups**  
   - Document the new internal types in the reference analysis.  
   - Update dev docs describing how to extend the engine.

## 6. Risks / Mitigations

- **Large diff touching core math** → keep helper semantics identical; rely on existing engine tests + new builder tests to catch regressions.
- **Template/CLI overlap** → none, since public API remains `LayoutRequest`.
- **Time cost** → estimate 1–1.5 days including tests + docs; can be tackled after the current ticket or in parallel once API stabilizes.

## 7. Next steps

1. Add tasks to `ENGINE-INPUTS-STRUCT/tasks.md` mirroring the plan above.  
2. Schedule the refactor after `REVAMP-CROP-ALGORITHM` reaches the backend milestone so we don’t chase a moving target.  
3. When ready, execute steps 2–5 in order, running `go test ./zine-layout/...` between phases.
