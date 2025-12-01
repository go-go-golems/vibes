# Tasks

## TODO

- [x] Catalog all `Inputs` consumers and confirm only the engine touches it
- [ ] Define `FrameInputs`, `CropInputs`, `PresentationInputs`, `SourceMeta` structs with validation
- [ ] Update `InputsFromRequest`/`InputsFromSettings` to build the new grouped structs
- [ ] Refactor `buildFrame`, `resolveCrop`, `composeTarget` to consume grouped inputs
- [ ] Remove legacy `Inputs` struct and update traces/tests/documentation accordingly

