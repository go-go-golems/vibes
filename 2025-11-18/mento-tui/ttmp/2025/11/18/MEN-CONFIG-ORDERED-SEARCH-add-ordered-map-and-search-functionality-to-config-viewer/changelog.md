# Changelog

## 2025-11-18

- Initial workspace created


## 2025-11-18

Created comprehensive analysis document covering ordered map implementation options and search functionality design


## 2025-11-18

Implemented search functionality for config viewer: press / to search, real-time filtering, ESC to cancel, Enter to apply. Environment variables maintain order from YAML (already ordered).

### Related Files

- /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/internal/ui/config.go — Added search mode with textinput


## 2025-11-18

Transformed env_vars from list format to map format in YAML. Environment variables now parsed into OrderedMap preserving YAML insertion order. Added demo environment variables to both YAML files. Removed edit functionality from config viewer (changed header from '[E] Edit' to just search).

### Related Files

- /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/internal/config/config.go — Updated UnmarshalYAML to parse env_vars as map into OrderedMap using yaml.Node to preserve order
- /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/internal/services/manager.go — Convert OrderedMap to []string format for exec.Cmd.Env
- /home/manuel/code/wesen/corporate-headquarters/vibes/2025-11-18/mento-tui/internal/ui/config.go — Updated to work with OrderedMap

