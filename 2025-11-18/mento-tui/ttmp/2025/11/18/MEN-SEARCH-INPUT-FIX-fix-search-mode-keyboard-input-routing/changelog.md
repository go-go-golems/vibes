# Changelog

## 2025-11-18

- Initial workspace created


## 2025-11-18

Implemented reverse routing order in app.go: delegate to child screens first, then handle global keys. Fixes ESC and Enter key handling in search mode.

### Related Files

- /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/internal/ui/app.go — Modified Update method to check search mode before updating and return early if in search mode

