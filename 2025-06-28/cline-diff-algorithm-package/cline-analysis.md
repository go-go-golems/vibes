# Cline Diff Algorithm and File Edit Tools Analysis

## Overview

Cline uses a sophisticated SEARCH/REPLACE block format for file editing that provides robust matching capabilities with multiple fallback strategies. This analysis covers the core algorithm, tool interface, and implementation details needed for a Go port.

## SEARCH/REPLACE Block Format

### Basic Structure
```
------- SEARCH
[exact content to find]
=======
[new content to replace with]
+++++++ REPLACE
```

### Flexible Marker Patterns
The algorithm supports flexible marker patterns:
- **SEARCH markers**: `------- SEARCH` (7+ dashes) or `<<<<<<< SEARCH` (7+ less-than symbols)
- **SEPARATOR**: `=======` (3+ equals signs)
- **REPLACE markers**: `+++++++ REPLACE` (7+ plus signs) or `>>>>>>> REPLACE` (7+ greater-than symbols)

### Key Features
1. **Flexible marker lengths**: Supports 3+ characters for separators, 7+ for search/replace markers
2. **Mixed legacy support**: Supports both new (`-`/`+`) and legacy (`<`/`>`) marker styles
3. **Multiple blocks**: Can process multiple SEARCH/REPLACE blocks in sequence
4. **Out-of-order handling**: Supports replacements that appear out of file order
5. **Streaming support**: Can process partial content chunks with final completion

## Matching Strategies

The algorithm employs a three-tier matching strategy:

### 1. Exact Match (Primary)
- Direct string matching using `indexOf()` from the last processed position
- Most efficient and reliable method
- Handles exact character-for-character matches including whitespace

### 2. Line-Trimmed Fallback
- Splits content into lines and compares after trimming whitespace
- Useful for handling indentation differences
- Maintains line structure while ignoring leading/trailing spaces

### 3. Block Anchor Fallback
- For blocks of 3+ lines, uses first and last lines as anchors
- Matches start and end lines while allowing middle content variation
- Useful for code blocks where structure is preserved but content may differ slightly

### 4. Full File Search (Last Resort)
- Searches entire file from beginning if local search fails
- Handles out-of-order replacements
- Marks as pending out-of-order replacement for special handling

## Error Handling

### Validation Rules
1. **Empty SEARCH with non-empty file**: Throws error unless it's a complete file replacement
2. **No match found**: Throws descriptive error with the unmatched SEARCH content
3. **Out-of-order conflicts**: Handles gracefully by deferring to final processing
4. **Malformed markers**: Strict validation of marker format

### Edge Cases Handled
- Missing final REPLACE marker when `isFinal=true`
- Partial content streaming
- Multiple consecutive edits
- Empty replacement (deletion)
- File creation (empty original content)

## Tool Interface

### replace_in_file Tool
```xml
<replace_in_file>
<path>relative/path/to/file</path>
<diff>
------- SEARCH
exact content to find
=======
new content to replace
+++++++ REPLACE
</diff>
</replace_in_file>
```

### Parameters
- **path**: Required. Relative path to file from current working directory
- **diff**: Required. One or more SEARCH/REPLACE blocks

### Tool Rules
1. SEARCH content must match exactly (character-for-character)
2. Only replaces first match occurrence per block
3. Multiple blocks processed in file order
4. Each line must be complete (no truncation)
5. Concise blocks preferred over large ones

### write_to_file Tool
```xml
<write_to_file>
<path>relative/path/to/file</path>
<content>Complete file content</content>
</write_to_file>
```

## Implementation Details

### State Management
- Tracks processing state (Idle, Search, Replace)
- Maintains last processed file position
- Handles pending non-standard lines
- Manages out-of-order replacement queue

### Content Processing
- Line-by-line processing with state transitions
- Accumulates search and replace content
- Immediate output for in-order replacements
- Deferred processing for out-of-order cases

### Final Assembly
- Sorts all replacements by file position
- Rebuilds entire file content
- Appends remaining original content
- Validates completion state

## Performance Characteristics

### Strengths
- Efficient exact matching for common cases
- Graceful fallback for edge cases
- Streaming support for large files
- Memory-efficient processing

### Considerations
- Multiple fallback attempts can be expensive
- Out-of-order processing requires full file rebuild
- Line-by-line processing overhead
- Regex compilation for marker detection

## Test Coverage

### Core Test Cases
- Empty file creation
- Exact match replacement
- Line-trimmed matching
- Block anchor matching
- Multiple ordered replacements
- Out-of-order replacements
- Deletion (empty replace)
- Incremental processing

### Edge Cases
- Flexible marker lengths
- Mixed marker styles
- Missing final markers
- Malformed content
- Complex whitespace scenarios
- Large file handling

## Go Implementation Requirements

### Core Components Needed
1. **Marker Detection**: Regex-based pattern matching
2. **Content Matching**: Three-tier matching strategy implementation
3. **State Machine**: Processing state management
4. **File Operations**: Read/write with proper error handling
5. **Streaming Support**: Chunk-based processing capability

### Key Algorithms to Port
1. `constructNewFileContent()` - Main processing function
2. `lineTrimmedFallbackMatch()` - Whitespace-tolerant matching
3. `blockAnchorFallbackMatch()` - Structural matching
4. Marker detection regex patterns
5. State transition logic

### Error Handling Strategy
- Descriptive error messages matching Cline's format
- Proper validation of marker formats
- Graceful handling of edge cases
- Clear indication of match failures

This analysis provides the foundation for implementing a compatible Go version of Cline's file editing capabilities.

