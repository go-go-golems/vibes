# Repository Analysis: Making Services Configurable via YAML

## Current State Analysis

### Service Configuration Location

Services are currently **hardcoded** in `internal/services/manager.go` in the `NewManager()` function:

```24:55:internal/services/manager.go
func NewManager() *Manager {
	return &Manager{
		Services: []*models.Service{
			{
				Name:       "Identity Server",
				Port:       8083,
				Status:     models.StatusStopped,
				LogBuffer:  models.NewLogBuffer(1000),
				BinaryPath: "./mock-binaries/identity-server",
				EnvVars:    []string{"IDENTITY_SERVICE_PORT=8083"},
			},
			{
				Name:       "Frontend (Vite)",
				Port:       5173,
				Status:     models.StatusStopped,
				LogBuffer:  models.NewLogBuffer(1000),
				BinaryPath: "./mock-binaries/frontend",
				EnvVars:    []string{"VITE_PORT=5173"},
			},
			{
				Name:       "Mento Worker",
				Port:       8082,
				Status:     models.StatusStopped,
				LogBuffer:  models.NewLogBuffer(1000),
				BinaryPath: "./mock-binaries/worker",
				EnvVars:    []string{"MENTO_SERVICE_PORT=8082"},
			},
		},
		GlobalLog:     models.NewLogBuffer(10000),
		SelectedIndex: 0,
	}
}
```

### Service Model Structure

Each service has the following fields (from `internal/models/models.go`):

```48:60:internal/models/models.go
type Service struct {
	Name       string
	Port       int
	Status     ServiceStatus
	PID        int
	CPUPercent float64
	MemoryMB   int
	StartTime  time.Time
	Cmd        *exec.Cmd
	LogBuffer  *LogBuffer
	BinaryPath string
	EnvVars    []string
}
```

**Note**: Currently, `exec.Command()` is called with only the binary path (line 83 in `manager.go`), and no working directory is set. We need to add support for:
- Working directory (PWD) per service
- Command-line arguments/flags per service

### Key Configuration Points

1. **Service Definition**: Name, Port, BinaryPath, EnvVars are hardcoded
2. **Working Directory**: Hardcoded in `cmd/main.go` line 13: `/home/ubuntu/mento-tui`
3. **Log Buffer Sizes**: Hardcoded (1000 per service, 10000 global)
4. **Binary Paths**: Relative paths like `./mock-binaries/identity-server`
5. **Command Arguments**: Not supported - only binary path is executed (line 83 in `manager.go`)
6. **Per-Service Working Directory**: Not supported - uses process working directory

### Current Dependencies

- YAML library (`gopkg.in/yaml.v3`) is already in `go.sum` but not directly imported in `go.mod`
- No existing YAML parsing code in the codebase
- No configuration file loading mechanism

## Proposed YAML Configuration Structure

### Suggested YAML Schema

```yaml
# mento-tui.yaml (or config.yaml)
services:
  - name: "Identity Server"
    port: 8083
    binary_path: "./mock-binaries/identity-server"
    working_directory: "./identity-service"  # optional, per-service PWD
    args:                                    # optional, command-line arguments
      - "--port"
      - "8083"
      - "--debug"
    # Alternative: args as space-separated string
    # args: "--port 8083 --debug"
    env_vars:
      - "IDENTITY_SERVICE_PORT=8083"
    log_buffer_size: 1000  # optional, defaults to 1000
  
  - name: "Frontend (Vite)"
    port: 5173
    binary_path: "./mock-binaries/frontend"
    working_directory: "./frontend"  # optional
    args: ["--host", "0.0.0.0", "--port", "5173"]  # list format
    env_vars:
      - "VITE_PORT=5173"
    log_buffer_size: 1000
  
  - name: "Mento Worker"
    port: 8082
    binary_path: "./mock-binaries/worker"
    working_directory: "."  # optional, defaults to global or current dir
    args: "--config worker.yaml --verbose"  # string format (space-separated)
    env_vars:
      - "MENTO_SERVICE_PORT=8082"
    log_buffer_size: 1000

# Global settings
global:
  log_buffer_size: 10000  # optional, defaults to 10000
  working_directory: "."  # optional, defaults to current directory (fallback for services)
```

### Alternative: More Flexible Schema

```yaml
# Alternative with more options
services:
  - name: "Identity Server"
    port: 8083
    binary_path: "./mock-binaries/identity-server"
    # Can specify absolute or relative paths
    working_directory: "./identity-service"  # optional, per-service PWD
    args:                                    # optional, supports both formats
      - "--port"
      - "8083"
      - "--log-level"
      - "debug"
    # Or as string: args: "--port 8083 --log-level debug"
    env_vars:
      IDENTITY_SERVICE_PORT: "8083"  # map format instead of list
      LOG_LEVEL: "debug"
    log_buffer_size: 1000
    enabled: true  # optional, to disable services without removing them

global:
  log_buffer_size: 10000
  default_working_directory: "."  # fallback if service doesn't specify
  config_file: "mento-tui.yaml"  # self-reference for clarity
```

### Command-Line Arguments Format

The `args` field supports two formats:

1. **List format** (recommended for complex arguments):
   ```yaml
   args:
     - "--port"
     - "8083"
     - "--config"
     - "/path/to/config.yaml"
   ```

2. **String format** (space-separated, simpler for basic cases):
   ```yaml
   args: "--port 8083 --config /path/to/config.yaml"
   ```

**Implementation Note**: When parsing string format, use `strings.Fields()` or similar to split on whitespace. For list format, use directly as `[]string`.

### Current Command Execution (for reference)

Currently in `internal/services/manager.go` line 83:
```go
cmd := exec.Command(svc.BinaryPath)
cmd.Env = append(os.Environ(), svc.EnvVars...)
```

**Needs to become**:
```go
// Build command with arguments
var cmd *exec.Cmd
if len(svc.Args) > 0 {
    cmd = exec.Command(svc.BinaryPath, svc.Args...)
} else {
    cmd = exec.Command(svc.BinaryPath)
}

// Set working directory (fallback: global -> current dir)
if svc.WorkingDirectory != "" {
    cmd.Dir = svc.WorkingDirectory
} else if globalConfig.WorkingDirectory != "" {
    cmd.Dir = globalConfig.WorkingDirectory
}
// If neither is set, cmd.Dir remains empty and uses process working directory

cmd.Env = append(os.Environ(), svc.EnvVars...)
```

## Implementation Plan

### 1. Add YAML Dependency

- Add `gopkg.in/yaml.v3` to `go.mod` (it's already in go.sum as transitive dependency)
- Run `go get gopkg.in/yaml.v3`

### 2. Create Configuration Package

Create `internal/config/config.go` with:
- YAML struct definitions matching the schema
- Load function to read and parse YAML file
- Validation logic
- Default values handling

### 3. Modify Manager Initialization

- Change `NewManager()` to accept a config parameter or load config internally
- Convert YAML config to `[]*models.Service` slice
- Handle missing config file (fallback to defaults or error)
- **Update Service model** to include:
  - `WorkingDirectory string` (optional)
  - `Args []string` (optional, for command-line arguments)

### 4. Update Main Entry Point

- Remove hardcoded working directory from `cmd/main.go`
- Add config file path flag/argument (default: `mento-tui.yaml` or `config.yaml`)
- Pass config to manager initialization

### 5. Configuration File Location Strategy

Options:
- **Option A**: Look for config in current working directory
- **Option B**: Look for config in executable directory
- **Option C**: Accept config path as command-line flag
- **Option D**: Look in multiple locations (current dir, home dir, etc.)

**Recommendation**: Option C (command-line flag) with fallback to `./mento-tui.yaml`

## Code Changes Required

### Files to Create

1. `internal/config/config.go` - Configuration loading and parsing
2. `internal/config/config_test.go` - Unit tests
3. `mento-tui.yaml.example` - Example configuration file

### Files to Modify

1. `internal/models/models.go`
   - Add `WorkingDirectory string` field to `Service` struct
   - Add `Args []string` field to `Service` struct

2. `internal/services/manager.go`
   - Change `NewManager()` signature to accept config
   - Remove hardcoded services array
   - Load services from config
   - **Update `StartService()` method**:
     - Set `cmd.Dir` from `svc.WorkingDirectory` (if set)
     - Use `exec.Command(svc.BinaryPath, svc.Args...)` instead of `exec.Command(svc.BinaryPath)`
     - Handle both global and per-service working directory fallback

3. `cmd/main.go`
   - Add flag parsing for config file path
   - Remove hardcoded working directory
   - Pass config to manager

4. `go.mod`
   - Add `gopkg.in/yaml.v3` dependency

### Files to Consider Modifying

1. `internal/ui/app.go`
   - May need to handle config loading errors gracefully

2. `README.md`
   - Document configuration file format
   - Update installation/usage instructions

## Error Handling Considerations

1. **Missing Config File**: 
   - Should we error out or use defaults?
   - Recommendation: Error with helpful message

2. **Invalid YAML**:
   - Parse errors should be clear and point to line numbers

3. **Invalid Service Config**:
   - Missing required fields (name, port, binary_path)
   - Invalid port numbers
   - Non-existent binary paths
   - Duplicate service names or ports
   - Invalid working directory paths
   - Invalid args format (should be list or string)

4. **Binary Path Validation**:
   - Check if binary exists before starting service
   - Support both absolute and relative paths
   - Handle path resolution correctly

## Testing Strategy

1. **Unit Tests**:
   - Config loading with valid YAML
   - Config loading with invalid YAML
   - Config loading with missing file
   - Service validation
   - Default value handling

2. **Integration Tests**:
   - End-to-end: Load config, start services
   - Test with different config file locations
   - Test with relative and absolute binary paths
   - Test with per-service working directories
   - Test with command-line arguments (both list and string formats)
   - Test working directory fallback (service -> global -> current dir)

3. **Example Configs**:
   - Create example configs for different scenarios
   - Test with minimal config (only required fields)
   - Test with full config (all optional fields)

## Migration Path

1. **Backward Compatibility**: 
   - If no config file exists, could fall back to current hardcoded services
   - Or require config file (cleaner approach)

2. **Default Config Generation**:
   - Could add a `--generate-config` flag to create a default `mento-tui.yaml`
   - Useful for first-time setup

## Additional Considerations

1. **Environment Variable Expansion**:
   - Should env vars in YAML support `${VAR}` expansion?
   - Or keep it simple and literal?

2. **Multiple Config Files**:
   - Support for includes/imports?
   - Probably overkill for initial implementation

3. **Hot Reload**:
   - Should config changes be reloadable without restart?
   - Probably not needed initially

4. **Config Validation**:
   - Validate port ranges (1-65535)
   - Validate binary paths exist
   - Check for port conflicts between services
   - Validate working directory paths exist (if specified)
   - Validate args format (must be list or string, not both)
   - Handle args parsing: convert string to []string using `strings.Fields()`

## Next Steps

1. ✅ Create ticket (done)
2. ⏳ Design YAML schema (in progress)
3. ⏳ Implement config loading package
4. ⏳ Update manager to use config
5. ⏳ Update main.go to handle config file
6. ⏳ Add tests
7. ⏳ Update documentation
8. ⏳ Create example config file

