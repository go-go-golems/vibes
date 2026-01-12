---
Title: Diary
Ticket: js-plugin-redux-fix
Status: active
Topics:
    - frontend
    - redux
    - plugins
DocType: reference
Intent: long-term
Owners: []
RelatedFiles:
    - Path: 2026/01/12/js-plugin-system/DEBUG_FINDINGS.md
      Note: Prior debugging notes
    - Path: 2026/01/12/js-plugin-system/client/src/App.tsx
      Note: Docs route
    - Path: 2026/01/12/js-plugin-system/client/src/components/PluginEditor.tsx
      Note: Editor completions
    - Path: 2026/01/12/js-plugin-system/client/src/pages/Docs.tsx
      Note: In-app docs page added
    - Path: 2026/01/12/js-plugin-system/client/src/pages/Playground.tsx
      Note: Docs nav
    - Path: 2026/01/12/js-plugin-system/pasted_content(1).txt
      Note: Original architecture sketch
    - Path: 2026/01/12/js-plugin-system/pasted_content_2.txt
      Note: QuickJS worker sketch
    - Path: 2026/01/12/js-plugin-system/plugin-playground.bundle
      Note: Bundle containing plugin-playground history
    - Path: 2026/01/12/js-plugin-system/test-log.md
      Note: Prior test log and claimed working behavior
    - Path: ttmp/2026/01/12/js-plugin-redux-fix--fix-js-plugin-experiment-redux-integration/analysis/01-plugin-system-deep-analysis.md
      Note: Deep analysis doc created and referenced in the diary
ExternalSources: []
Summary: ""
LastUpdated: 2026-01-12T15:55:08.892024352-05:00
WhatFor: ""
WhenToUse: ""
---



# Diary

## Goal
Capture the investigation steps for the plugin experiment, focusing on architecture mapping, history artifacts, and concrete breakpoints.

## Step 1: Ticket setup and artifact triage

I created the ticket workspace and initial docs so there is a clean place to track the investigation. I then validated the plugin-playground Git artifacts and the bundle history to understand what previous states exist and whether they are trustworthy.

The .git directories in both `plugin-playground` and `/tmp/plugin-playground` are incomplete, so direct Git operations fail. The bundle file is valid and can be cloned to a fresh directory, which I used for history inspection.

### What I did
- Created the ticket, analysis doc, and diary doc with docmgr.
- Checked the `plugin-playground` and `/tmp/plugin-playground` .git directories and confirmed they are missing objects.
- Verified `plugin-playground.bundle` and cloned it to `/tmp/plugin-playground-bundle` to inspect commit history.

### Why
- We need a structured workspace to capture the fix work.
- The .git confusion blocks history inspection unless the bundle is validated and cloned.

### What worked
- `docmgr ticket create-ticket` created the workspace and docs.
- `git bundle verify` confirmed the bundle has a complete history.
- Cloning the bundle to a fresh path worked and produced a valid repo.

### What didn't work
- `git clone /home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/2026/01/12/js-plugin-system/plugin-playground.bundle /tmp/plugin-playground`
  - Error: `fatal: destination path '/tmp/plugin-playground' already exists and is not an empty directory.`
- `git --git-dir /home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/2026/01/12/js-plugin-system/plugin-playground/.git --work-tree /home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/2026/01/12/js-plugin-system/plugin-playground status -sb`
  - Error: `fatal: not a git repository: '/home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/2026/01/12/js-plugin-system/plugin-playground/.git'`

### What I learned
- The plugin-playground copies are not valid Git repos due to missing object storage.
- The bundle is the reliable history source and can be used for diffs and comparisons.

### What was tricky to build
- Disambiguating the workspace-level Git worktree from the broken nested .git directories.

### What warrants a second pair of eyes
- Confirm whether the bundle clone is the canonical history and whether we should keep `/tmp/plugin-playground-bundle` as a long-lived reference.

### What should be done in the future
- Decide on a single, durable location for the bundle clone and reference it consistently in docs.

### Code review instructions
- Start in the ticket docs to see the recorded artifacts and references.
- Validate bundle integrity with `git bundle verify /home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/2026/01/12/js-plugin-system/plugin-playground.bundle` if needed.

### Technical details
- Bundle heads: `git bundle list-heads /home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/2026/01/12/js-plugin-system/plugin-playground.bundle`
- Bundle history: `git -C /tmp/plugin-playground-bundle log --oneline --decorate -10`

## Step 2: System deep dive and breakpoints

I read the active runtime files to map the plugin execution path and compare it to the intended design. The implementation currently mixes multiple plugin runtimes and UI/event contracts, which explains why the experiment feels unstable.

I captured these findings in the deep analysis doc, focusing on state shape mismatches, event payload loss, and unused sandbox code paths.

### What I did
- Reviewed `Playground.tsx`, `pluginManager.ts`, `pluginSandboxClient.ts`, `pluginSandbox.worker.ts`, `presetPlugins.ts`, `presets.ts`, `store.ts`, and `WidgetRenderer.tsx`.
- Traced render and event flow from UI DSL to plugin handlers and Redux updates.
- Documented mismatches and likely breakpoints in the analysis doc.

### Why
- The experiment is not working reliably, and the cause appears to be contract drift between UI, plugin runtime, and Redux state shape.

### What worked
- The file-level read clarified that two different runtime paths exist and only one is wired.
- The state shape mismatch between `presetPlugins.ts` and `store.ts` is a clear, reproducible break in the Redux contract.

### What didn't work
- N/A

### What I learned
- The active runtime drops input event payloads and assumes a different handler signature than the sandbox path.
- The plugin registry in Redux is unused, so plugin list and status UI cannot function as written.

### What was tricky to build
- Reconciling three overlapping plugin paths (Playground + pluginManager, PluginWidget + sandbox, worker sandbox) that share similar code but different contracts.

### What warrants a second pair of eyes
- Confirm the desired handler signature (`args` vs `event`) and the intended Redux state shape so we can unify the runtime.

### What should be done in the future
- Choose a single runtime path and remove or wire unused components to avoid drifting contracts.

### Code review instructions
- Start with `client/src/pages/Playground.tsx` for the active runtime path.
- Then review `client/src/lib/presetPlugins.ts` and `client/src/store/store.ts` to see the state shape mismatch.
- Compare to `client/src/lib/pluginSandboxClient.ts` and `client/src/workers/pluginSandbox.worker.ts` for the unused sandbox paths.

### Technical details
- Event payload is dropped by `Playground.tsx` because `WidgetRenderer` passes a second argument that is ignored by the handler.
- `pluginSandboxClient.event` forwards `eventPayload?.args`, which is undefined for button clicks that use raw args.
- `store.ts` writes `calculator` and `greeter` under `state.plugins`, while `presetPlugins.ts` reads them from root.

## Step 3: Upload analysis doc to reMarkable

I prepared a reMarkable upload for the deep analysis document using the ticket-aware mirroring mode so the PDF lands under the ticket subdirectory. I ran a dry-run to verify the remote path and then executed the upload.

The first upload attempt timed out, so I reran the command with a longer timeout and confirmed the PDF landed at the expected location.

### What I did
- Verified the remarkable upload script is available.
- Ran a dry-run upload for the analysis doc using `--ticket-dir` and `--mirror-ticket-structure`.
- Uploaded the PDF to the reMarkable and confirmed the destination path.

### Why
- Provide the deep analysis doc on the device in the same ticket structure for easy review.

### What worked
- Dry-run showed the correct `ai/2026/01/12/.../analysis/` destination.
- The final upload completed successfully and reported the correct path.

### What didn't work
- `python3 /home/manuel/.local/bin/remarkable_upload.py --ticket-dir /home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/ttmp/2026/01/12/js-plugin-redux-fix--fix-js-plugin-experiment-redux-integration --mirror-ticket-structure /home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/ttmp/2026/01/12/js-plugin-redux-fix--fix-js-plugin-experiment-redux-integration/analysis/01-plugin-system-deep-analysis.md`
  - Error: `command timed out after 10006 milliseconds`

### What I learned
- The upload can take longer than the default timeout even for a single PDF.

### What was tricky to build
- N/A

### What warrants a second pair of eyes
- Confirm the device-side folder matches the expected ticket structure under `ai/2026/01/12/`.

### What should be done in the future
- If additional docs are uploaded, keep using `--mirror-ticket-structure` to avoid collisions.

### Code review instructions
- N/A (documentation upload only).

### Technical details
- Dry-run: `python3 /home/manuel/.local/bin/remarkable_upload.py --ticket-dir /home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/ttmp/2026/01/12/js-plugin-redux-fix--fix-js-plugin-experiment-redux-integration --mirror-ticket-structure --dry-run /home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/ttmp/2026/01/12/js-plugin-redux-fix--fix-js-plugin-experiment-redux-integration/analysis/01-plugin-system-deep-analysis.md`
- Upload: `python3 /home/manuel/.local/bin/remarkable_upload.py --ticket-dir /home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/ttmp/2026/01/12/js-plugin-redux-fix--fix-js-plugin-experiment-redux-integration --mirror-ticket-structure /home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/ttmp/2026/01/12/js-plugin-redux-fix--fix-js-plugin-experiment-redux-integration/analysis/01-plugin-system-deep-analysis.md`

## Step 4: Store diff scripts in the ticket

I added the two scripts used to build the sqlite snapshot database and record bundle diffs into the ticket scripts directory. This keeps the analysis tooling close to the ticket context and makes it repeatable.

The scripts are parameterized for snapshot roots, database path, and commit range so we can reuse them for follow-up comparisons.

### What I did
- Added `build_snapshot_sqlite.py` to capture file hashes and code definition matches.
- Added `add_bundle_diff.py` to store `git diff` metadata into the sqlite DB.

### Why
- Preserve the exact tooling used for historical analysis so the investigation is reproducible.

### What worked
- Scripts written with argument parsing so they can be reused with different paths/commits.

### What didn't work
- N/A

### What I learned
- N/A

### What was tricky to build
- Ensuring the diff script handles rename statuses by using the new path from `git diff --name-status`.

### What warrants a second pair of eyes
- Confirm the regex coverage for definitions is sufficient for TypeScript/JS patterns we care about.

### What should be done in the future
- If we add more snapshots (e.g., another bundle), re-run the scripts and store the new DB under `various/` or `archive/`.

### Code review instructions
- Start with `/home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/ttmp/2026/01/12/js-plugin-redux-fix--fix-js-plugin-experiment-redux-integration/scripts/build_snapshot_sqlite.py`.
- Then review `/home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/ttmp/2026/01/12/js-plugin-redux-fix--fix-js-plugin-experiment-redux-integration/scripts/add_bundle_diff.py`.

### Technical details
- Snapshot builder usage: `build_snapshot_sqlite.py --current <path> --bundle <path> --out <db-path>`
- Diff appender usage: `add_bundle_diff.py --repo <git-path> --db <db-path> --from <sha> --to <sha>`

## Step 5: Run snapshot scripts and start the local dev server

I ran the snapshot/diff scripts to generate a sqlite DB under the ticket and recorded bundle diffs for two commit ranges. This captures the simplify pass deltas in a reusable format for follow-up analysis.

I also installed dependencies and started the Vite dev server so the page can be validated locally. The server picked port 5174 (5173 was already in use).

### What I did
- Ran `build_snapshot_sqlite.py` to create `various/js-plugin-diff.sqlite`.
- Appended bundle diffs for `cca2452 -> dc83162` and `dc83162 -> 7736a42`.
- Installed dependencies with `pnpm install`.
- Started the dev server with `pnpm dev --host --port 5173` (auto-selected 5174).

### Why
- The sqlite snapshot makes history comparisons reproducible.
- Local validation requires the dev server running.

### What worked
- Snapshot DB creation and diff ingestion completed without errors.
- Vite dev server started and reported `http://localhost:5174/`.

### What didn't work
- N/A

### What I learned
- Port 5173 was already occupied, so Vite auto-bumped to 5174.

### What was tricky to build
- N/A

### What warrants a second pair of eyes
- Confirm the dev server port and that the UI reflects the simplified runtime (textarea-based loader).

### What should be done in the future
- If we need additional history ranges, rerun `add_bundle_diff.py` with those commits.

### Code review instructions
- Snapshot DB at `/home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/ttmp/2026/01/12/js-plugin-redux-fix--fix-js-plugin-experiment-redux-integration/various/js-plugin-diff.sqlite`.
- Dev server log at `/tmp/js-plugin-system-dev.log`.

### Technical details
- Build snapshot: `python3 /home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/ttmp/2026/01/12/js-plugin-redux-fix--fix-js-plugin-experiment-redux-integration/scripts/build_snapshot_sqlite.py --current /home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/2026/01/12/js-plugin-system --bundle /tmp/plugin-playground-bundle --out /home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/ttmp/2026/01/12/js-plugin-redux-fix--fix-js-plugin-experiment-redux-integration/various/js-plugin-diff.sqlite`
- Append diff: `python3 /home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/ttmp/2026/01/12/js-plugin-redux-fix--fix-js-plugin-experiment-redux-integration/scripts/add_bundle_diff.py --repo /tmp/plugin-playground-bundle --db /home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/ttmp/2026/01/12/js-plugin-redux-fix--fix-js-plugin-experiment-redux-integration/various/js-plugin-diff.sqlite --from dc83162 --to 7736a42`
- Append diff: `python3 /home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/ttmp/2026/01/12/js-plugin-redux-fix--fix-js-plugin-experiment-redux-integration/scripts/add_bundle_diff.py --repo /tmp/plugin-playground-bundle --db /home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/ttmp/2026/01/12/js-plugin-redux-fix--fix-js-plugin-experiment-redux-integration/various/js-plugin-diff.sqlite --from cca2452 --to dc83162`
- Install deps: `pnpm install`
- Start dev server: `pnpm dev --host --port 5173`

## Step 6: Fix preset plugin state access and event payloads

I patched the simplified Playground runtime to forward UI event payloads to plugin handlers, and aligned preset plugin code with the actual Redux state shape. This fixes the calculator display and makes input-based plugins (like the greeter) able to read values.

**Commit (code):** 4fd2c3e4 — "Fix preset plugin state and event payloads"

### What I did
- Passed `eventPayload` from `WidgetRenderer` through `Playground` to `pluginManager.callHandler`.
- Updated `presetPlugins` to read calculator/greeter state from `state.plugins.*`.

### Why
- The calculator UI was reading from `state.calculator` even though the reducer stores it under `state.plugins.calculator`.
- Input handlers never received payloads because `Playground` dropped the event payload.

### What worked
- Event payloads now reach handlers via `pluginManager.callHandler`.
- Preset plugins read from the correct state path.

### What didn't work
- N/A

### What I learned
- The simplified runtime still needs to respect the widget event contract from `WidgetRenderer` to keep input handlers functional.

### What was tricky to build
- N/A

### What warrants a second pair of eyes
- Confirm the new handler payload behavior does not break button-driven handlers that expect numeric args.

### What should be done in the future
- Decide whether to normalize the state shape (root vs `plugins` slice) so plugins don’t need to guess.

### Code review instructions
- Start with `/home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/2026/01/12/js-plugin-system/client/src/pages/Playground.tsx` and verify `onEvent` forwards payloads.
- Then review `/home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/2026/01/12/js-plugin-system/client/src/lib/presetPlugins.ts` for state path updates.

### Technical details
- `handleEvent` now takes `eventPayload` and passes it as the handler arg.
- Calculator and greeter plugins read from `state.plugins.calculator` and `state.plugins.greeter`.

## Step 7: Document options and begin registry wiring

I added a new analysis document outlining the two stabilization options: normalize the Redux state shape, and wire the plugin registry UI/runtime. I then started on option 2 by wiring the current `pluginManager` runtime to dispatch plugin lifecycle actions in `Playground`, so the registry stays in sync.

This establishes a path to use the existing registry UI and status data without immediately restoring the sandbox worker path.

**Commit (code):** e7cac8b6 — "Wire plugin registry into Playground"

### What I did
- Added `analysis/02-options-normalize-store-vs-registry-wiring.md`.
- Updated `Playground.tsx` to dispatch plugin lifecycle actions and render from the registry state.

### Why
- The user asked for a clear comparison of option 1 (state normalization) vs option 2 (registry wiring).
- The registry data was stale because the simplified runtime never dispatched lifecycle actions.

### What worked
- The registry now reflects plugins loaded through the simplified UI.

### What didn't work
- N/A

### What I learned
- Wiring the registry can be done without reintroducing the sandbox, but isolation and allowlisting remain absent.

### What was tricky to build
- Handling custom plugin IDs so the registry entry matches the runtime’s returned ID.

### What warrants a second pair of eyes
- Confirm the registry UI and plugin metadata are consistent across preset and custom plugin loads.

### What should be done in the future
- Decide whether to restore the sandbox path (option 2A) or continue with the in-process runtime (option 2B).

### Code review instructions
- Start with `/home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/2026/01/12/js-plugin-system/client/src/pages/Playground.tsx`.
- Review `/home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/ttmp/2026/01/12/js-plugin-redux-fix--fix-js-plugin-experiment-redux-integration/analysis/02-options-normalize-store-vs-registry-wiring.md`.

### Technical details
- Custom plugins use a temporary ID during load; the registry entry is swapped to the plugin’s declared ID once resolved.

## Step 8: Normalize store shape and restore the sandbox runtime

I removed the duplicate top-level counter reducer so plugins read from a single state shape and updated the remaining selectors to match `state.plugins.*`. This eliminates the root vs slice split that was causing state display confusion.

I then restored the sandboxed runtime in `Playground` (with `PluginSandboxClient` + QuickJS worker), switched the UI back to the registry-driven layout, and aligned event payloads for the sandbox handler signature.

**Commit (code):** c8b76a66 — "Normalize plugin state shape"
**Commit (code):** 747ae8f1 — "Restore sandbox runtime in Playground"

### What I did
- Removed the duplicate top-level `counter` reducer and aligned preset/mini plugin selectors.
- Replaced the in-process `PluginSandboxClient` with the worker RPC version.
- Restored QuickJS worker implementation and the registry-driven Playground layout.
- Adjusted button event payloads to send `{ args }` so sandbox handlers receive `event.args`.

### Why
- A single state shape prevents stale UI reads and repeated path mismatches.
- The sandbox runtime reintroduces registry-backed UX and isolation boundaries.

### What worked
- Registry-backed plugin list is active again in the UI layout.
- Sandbox handlers now receive consistent event payloads for buttons and inputs.

### What didn't work
- N/A

### What I learned
- The sandbox path expects `event.args`; wiring the payload consistently is required for digit/operation handlers.

### What was tricky to build
- Ensuring the sandbox worker, client, and widget renderer agree on payload shape.

### What warrants a second pair of eyes
- Confirm plugin dispatch allowlisting still behaves correctly for custom plugin IDs.

### What should be done in the future
- Decide whether to keep `pluginManager` as a fallback or remove it entirely to avoid split paths.

### Code review instructions
- Start with `/home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/2026/01/12/js-plugin-system/client/src/store/store.ts`.
- Review `/home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/2026/01/12/js-plugin-system/client/src/pages/Playground.tsx` and `/home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/2026/01/12/js-plugin-system/client/src/lib/pluginSandboxClient.ts`.
- Validate `/home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/2026/01/12/js-plugin-system/client/src/workers/pluginSandbox.worker.ts` + `/home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/2026/01/12/js-plugin-system/client/src/components/WidgetRenderer.tsx`.

### Technical details
- `WidgetRenderer` now sends button events as `{ args: onClick.args }` for sandbox handlers.

## Step 9: Keep greeter input focused during updates

The greeter input lost focus because the sandbox widget renderer was swapping to a loading placeholder on every state change, which unmounted the input. I changed the loading logic to keep the rendered tree mounted unless there is no tree yet, so typing no longer drops focus.

**Commit (code):** c756756e — "Keep plugin widgets mounted during re-render"

### What I did
- Adjusted `PluginWidget` to avoid setting `loading` when a tree already exists.
- Only show the loading placeholder when no tree is present.

### Why
- Unmounting the input during each render cycle caused focus loss when typing.

### What worked
- The widget tree remains mounted, preserving input focus.

### What didn't work
- N/A

### What I learned
- The sandbox render path should avoid placeholder swapping during normal state updates.

### What was tricky to build
- Ensuring loading logic still covers the initial render without flicker.

### What warrants a second pair of eyes
- Confirm other widgets that rely on transient loading states still behave as expected.

### What should be done in the future
- Consider a non-blocking “rendering” indicator that does not replace the widget tree.

### Code review instructions
- Start with `/home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/2026/01/12/js-plugin-system/client/src/components/PluginWidget.tsx`.

### Technical details
- Loading state now gates the placeholder only when `tree` is null.

## Step 10: Remove unused in-process helpers

I cleaned out a couple of unused helper files from the earlier in-process (non-sandbox) path to reduce confusion. The active runtime is the sandbox worker path, so keeping old presets and a minimal widget around just adds drift.

**Commit (code):** 41a458ef — "Remove unused in-process plugin helpers"

### What I did
- Removed `presetPlugins.ts` and `MinimalPluginWidget.tsx` from the tracked codebase.

### Why
- These files were no longer referenced after restoring the sandbox path, and their presence implied a working non-sandbox runtime that is not active.

### What worked
- Cleaned the tree without affecting the sandbox runtime.

### What didn't work
- N/A

### What I learned
- N/A

### What was tricky to build
- N/A

### What warrants a second pair of eyes
- Confirm nothing in the UI or docs still references the removed files.

### What should be done in the future
- Decide whether to remove `pluginManager.ts` entirely if we commit to the sandbox path.

### Code review instructions
- Verify removal in `2026/01/12/js-plugin-system/client/src/components/MinimalPluginWidget.tsx` and `2026/01/12/js-plugin-system/client/src/lib/presetPlugins.ts`.

### Technical details
- N/A

## Step 11: Remove in-process runtime and add sandbox unload

I removed the remaining in-process runtime files and added an explicit unload RPC to the sandbox worker. The Playground now calls `unloadPlugin` when a plugin is removed so the worker can dispose of the VM context and free memory.

**Commit (code):** ee776f2a — "Add sandbox unload support"

### What I did
- Removed `pluginManager.ts` and `minimalPlugin.ts` from the workspace.
- Added `unloadPlugin` support to `pluginSandbox.worker.ts` and `pluginSandboxClient.ts`.
- Hooked `Playground` to call `sandbox.unloadPlugin` when registry entries are removed.

### Why
- Keeping an unused in-process runtime risks future drift and confusion.
- The worker needed a cleanup path to avoid accumulating VM contexts.

### What worked
- The sandbox can now dispose contexts on removal.

### What didn't work
- N/A

### What I learned
- The sandbox path benefits from explicit lifecycle hooks, not just load/render/event.

### What was tricky to build
- Detecting removed plugin IDs without changing the existing PluginList UI behavior.

### What warrants a second pair of eyes
- Verify that unload is safe while a widget is rendering and does not race with in-flight RPC calls.

### What should be done in the future
- N/A

### Code review instructions
- Start with `/home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/2026/01/12/js-plugin-system/client/src/pages/Playground.tsx`.
- Review `/home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/2026/01/12/js-plugin-system/client/src/lib/pluginSandboxClient.ts` and `/home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/2026/01/12/js-plugin-system/client/src/workers/pluginSandbox.worker.ts`.

### Technical details
- Unload RPC: `sandbox.unloadPlugin(id)` -> worker `handleUnload` disposes the VM context.

## Step 13: Restore sandbox presets file

After cleanup, `presets.ts` needed to be restored because the sandbox Playground imports `PRESET_PLUGINS` from there. I restored the file so the preset selector can load plugins again.

**Commit (code):** 889d2252 — "Restore sandbox preset plugins"

### What I did
- Restored `presets.ts` from the bundle history.

### Why
- The sandbox UI depends on `PRESET_PLUGINS` and failed to load without this module.

### What worked
- Preset selector resolves again; no missing module errors.

### What didn't work
- N/A

### What I learned
- Preset scripts are part of the sandbox runtime, not the in-process path.

### What was tricky to build
- N/A

### What warrants a second pair of eyes
- Confirm the preset scripts still align with the normalized `state.plugins.*` shape.

### What should be done in the future
- Keep preset scripts alongside the sandbox runtime and avoid deleting them during cleanup.

### Code review instructions
- Review `/home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/2026/01/12/js-plugin-system/client/src/lib/presets.ts`.

### Technical details
- N/A

## Step 14: Add example plugins and scripting reference

I expanded the preset plugin list with additional examples that exercise the existing Redux state shape and UI DSL, then added a detailed scripting reference document. This gives concrete examples to copy from and a stable contract to align future plugins.

**Commit (code):** 3bdd3c04 — "Add more preset plugins"

### What I did
- Added new preset plugins: Counter Dial, Greeter Banner, and State Audit.
- Created `reference/02-plugin-scripting-reference.md` with a full DSL + handler contract guide.

### Why
- More examples make it easier to validate the sandbox and help users author plugins.
- A reference doc reduces ambiguity about the DSL, event payloads, and state shape.

### What worked
- Preset additions remain compatible with the normalized `state.plugins.*` shape.
- The reference doc captures the lifecycle and contract details in one place.

### What didn't work
- N/A

### What I learned
- Reusing existing actions keeps new examples functional without adding reducers.

### What was tricky to build
- N/A

### What warrants a second pair of eyes
- Confirm the new presets match the intended UX and naming conventions.

### What should be done in the future
- Add a new reducer slice if we want example plugins to persist additional state.

### Code review instructions
- Review `/home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/2026/01/12/js-plugin-system/client/src/lib/presets.ts`.
- Review `/home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/ttmp/2026/01/12/js-plugin-redux-fix--fix-js-plugin-experiment-redux-integration/reference/02-plugin-scripting-reference.md`.

### Technical details
- New presets use `plugin.counter`, `plugin.greeter`, and `plugin.calculator` actions only.

## Step 12: Add explicit unload control

I added an explicit unload button to the plugin list that disables a plugin before removing it, matching the intent you requested. This should make it clearer when a plugin is being disposed versus simply toggled on/off.

**Commit (code):** f5c9006c — "Add explicit unload control"

### What I did
- Added an unload button to `PluginList` that toggles the plugin off and then removes it.

### Why
- The sandbox now supports explicit unload; the UI should expose that action clearly.

### What worked
- The list now offers distinct toggle (power), unload (stop), and remove (trash) actions.

### What didn't work
- N/A

### What I learned
- N/A

### What was tricky to build
- N/A

### What warrants a second pair of eyes
- Confirm the unload action is not redundant with remove for your workflow expectations.

### What should be done in the future
- Consider consolidating unload/remove once behavior expectations are confirmed.

### Code review instructions
- Start with `/home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/2026/01/12/js-plugin-system/client/src/components/PluginList.tsx`.

### Technical details
- Unload dispatches `pluginToggled` when needed, then `pluginRemoved`.

## Step 15: Add in-app docs page and editor completions

I added a dedicated documentation page inside the app that explains the plugin DSL, handler contract, and state shape with copy/paste-ready examples. I also expanded the editor IntelliSense so plugin authors get snippets and helper completions directly in the editor.

This pairs the in-app guidance with the reference doc updates so the UI and docs stay aligned, and it makes the plugin authoring flow easier to navigate from the Playground.

**Commit (code):** 1b05ec81 — "Add plugin docs page and editor completions"

### What I did
- Added `Docs` page content with QuickJS DSL reference and examples.
- Wired `/docs` routing in `App.tsx` and added a Docs button to the Playground header.
- Expanded Monaco completions for `definePlugin` and the `ui.*` helpers.
- Updated the scripting reference doc to note editor support and related files.
- Checked off tasks 5 and 6 in the ticket.

### Why
- The in-app docs reduce friction for writing new plugins and clarifies the DSL contract.
- Editor completions provide lightweight LSP-style guidance without introducing a full language server.

### What worked
- The docs page layout matches the existing brutalist UI theme and uses the same Tailwind tokens.
- Completions are registered once per editor instance to avoid duplicate suggestions.

### What didn't work
- N/A

### What I learned
- The in-app docs can reuse the same code snippets as the reference doc without adding new dependencies.

### What was tricky to build
- Making the docs page readable on small viewports without losing the monospace-heavy layout.

### What warrants a second pair of eyes
- Validate that the docs examples and action namespace guidance match the runtime allowlist behavior.

### What should be done in the future
- Consider adding more snippet templates (tables, badge rows) if authors request them.

### Code review instructions
- Start with `/home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/2026/01/12/js-plugin-system/client/src/pages/Docs.tsx`.
- Then review `/home/manuel/workspaces/2026-01-12/add-quick-js-redux-experiment/vibes/2026/01/12/js-plugin-system/client/src/components/PluginEditor.tsx` for completions.
- Validate by running `pnpm dev --host` and visiting `http://localhost:5174/docs`.

### Technical details
- Snippets include `definePlugin`, `ui.panel`, `ui.row`, `ui.column`, `ui.text`, `ui.badge`, `ui.button`, `ui.input`, `ui.counter`, and `ui.table`.
