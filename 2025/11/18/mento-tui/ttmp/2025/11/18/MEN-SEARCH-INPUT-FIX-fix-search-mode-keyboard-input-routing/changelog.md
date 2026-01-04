# Changelog

## 2025-11-18

- Initial workspace created


## 2025-11-18

Implemented reverse routing order in app.go: delegate to child screens first, then handle global keys. Fixes ESC and Enter key handling in search mode.

### Related Files

- /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/internal/ui/app.go — Modified Update method to check search mode before updating and return early if in search mode


## 2025-11-18

Extended search mode routing fix to also handle ConfigScreen search mode, preventing global keys from intercepting config search input

### Related Files

- /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/internal/ui/app.go — Added ConfigScreen search mode check to isInSearchMode

