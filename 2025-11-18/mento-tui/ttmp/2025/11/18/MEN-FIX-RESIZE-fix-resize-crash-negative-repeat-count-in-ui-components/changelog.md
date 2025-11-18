# Changelog

## 2025-11-18

- Initial workspace created


## 2025-11-18

Fixed resize crash by replacing all strings.Repeat calls with Lipgloss-based layouts. Added max() helper, minimum width checks, and refactored headers/footers to use JoinHorizontal for safe responsive rendering.

### Related Files

- /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/internal/ui/dashboard.go — Refactored header
- /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/internal/ui/logviewer.go — Refactored header
- /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/internal/ui/utils.go — Added max() helper function


## 2025-11-18

Also fixed strings.Repeat calls in help.go and config.go headers to complete the fix across all UI components.

### Related Files

- /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/internal/ui/config.go — Fixed header spacing
- /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/internal/ui/help.go — Fixed header and key binding spacing

