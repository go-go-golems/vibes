---
Title: Dual Mode Operation - Human and Structured Output
Slug: dual-mode
Short: Understanding dual output modes for both interactive use and automation integration
Topics:
  - dual-mode
  - output-formats
  - automation
  - glazed
  - human-readable
  - structured-data
Commands:
  - all
Flags:
  - with-glaze-output
  - output
  - fields
  - filters
IsTopLevel: true
IsTemplate: false
ShowPerDefault: true
SectionType: GeneralTopic
---

# Dual Mode Operation

## Architecture and Philosophy

The dual mode operation in pr-analyzer-dual represents a fundamental design principle: the same powerful analysis engine should serve both human users and automated systems without compromise. By implementing both `BareCommand` and `GlazeCommand` interfaces, each command seamlessly switches between providing rich, formatted output for direct consumption and structured data for programmatic use.

This approach eliminates the need for separate tools or complex output parsing, making pr-analyzer-dual equally valuable for interactive analysis and automation workflows.

## Mode Selection and Behavior

### Default Mode: Human-Readable Output

When commands run without output format flags, they produce rich, markdown-formatted output designed for direct reading. This mode prioritizes clarity, context, and visual organization to help users quickly understand the analysis results.

**Characteristics of human-readable mode:**
- Narrative structure with headers and sections
- Contextual information and explanations  
- Formatted tables and lists for easy scanning
- Color and styling (when terminal supports it)
- Summary information and insights

**Example:**
```bash
./pr-analyzer-dual get commits --owner kubernetes --repo kubernetes --pr-number 12345
```

Produces output like:
```markdown
# Pull Request #12345 Commits

**Repository:** kubernetes/kubernetes  
**Total Commits:** 4

## 1. Implement pod security standards validation
- **SHA:** `a1b2c3d4e5f6789`
- **Author:** Jane Developer <jane@k8s.io>
- **Date:** 2024-01-15 09:30:00 UTC

## 2. Add unit tests for validation logic
- **SHA:** `f6e5d4c3b2a1987`
- **Author:** John Contributor <john@k8s.io>  
- **Date:** 2024-01-15 11:45:00 UTC
```

### Structured Mode: Machine-Readable Output

Activated with the `--with-glaze-output` flag, structured mode transforms the same analysis results into formats optimized for programmatic consumption. This mode focuses on data accuracy, completeness, and consistency.

**Characteristics of structured mode:**
- Consistent field names across all records
- Complete data without formatting artifacts
- Multiple output format options
- Support for field selection and filtering
- Standardized error handling

**Basic activation:**
```bash
./pr-analyzer-dual get commits --owner kubernetes --repo kubernetes --pr-number 12345 \
    --with-glaze-output
```

## Output Format Options

### JSON Format
Perfect for API integration, web applications, and complex data processing workflows.

```bash
./pr-analyzer-dual analyze functions --owner golang --repo go --pr-number 54321 \
    --with-glaze-output --output json
```

Produces:
```json
[
  {
    "file_path": "src/runtime/proc.go",
    "function_name": "newproc",
    "receiver": "",
    "start_line": 4200,
    "end_line": 4250,
    "is_exported": false,
    "is_changed": true,
    "signature": "func newproc(fn *funcval)"
  }
]
```

### CSV Format
Ideal for spreadsheet analysis, data science workflows, and simple automated processing.

```bash
./pr-analyzer-dual get context --owner docker --repo docker --pr-number 98765 \
    --with-glaze-output --output csv
```

Produces:
```csv
file_path,lines_added,lines_removed,total_functions,changed_functions
cmd/docker/main.go,25,10,3,1
pkg/client/client.go,45,15,8,3
```

### YAML Format
Excellent for configuration-based tools and human-readable structured data.

```bash
./pr-analyzer-dual get commits --owner prometheus --repo prometheus --pr-number 11111 \
    --with-glaze-output --output yaml
```

### Table Format
Provides structured data in a readable tabular format, bridging human and machine readability.

```bash
./pr-analyzer-dual analyze functions --owner hashicorp --repo terraform --pr-number 22222 \
    --only-changed --with-glaze-output --output table
```

## Advanced Output Control

### Field Selection
Choose specific fields to include in the output, reducing data volume and focusing on relevant information.

```bash
# Only function names and change status
./pr-analyzer-dual analyze functions --owner owner --repo repo --pr-number 123 \
    --with-glaze-output --output json \
    --fields function_name,is_changed

# Commit metadata only
./pr-analyzer-dual get commits --owner owner --repo repo --pr-number 456 \
    --with-glaze-output --output csv \
    --fields sha,author_name,commit_date
```

### Data Filtering
Apply filters to reduce the dataset to specific criteria, enabling targeted analysis.

```bash
# Only changed functions
./pr-analyzer-dual analyze functions --owner owner --repo repo --pr-number 789 \
    --with-glaze-output --output json \
    --filter is_changed=true

# Recent commits only
./pr-analyzer-dual get commits --owner owner --repo repo --pr-number 101 \
    --with-glaze-output --output table \
    --filter "commit_date>2024-01-01"
```

## Integration Patterns

### CI/CD Pipeline Integration
Structured output enables seamless integration with continuous integration systems for automated code analysis.

```bash
# Extract changed function count for build metrics
CHANGED_FUNCTIONS=$(./pr-analyzer-dual analyze functions \
    --owner $REPO_OWNER --repo $REPO_NAME --pr-number $PR_NUMBER \
    --only-changed --with-glaze-output --output json | \
    jq 'length')

# Generate change report for notification systems
./pr-analyzer-dual get context \
    --owner $REPO_OWNER --repo $REPO_NAME --pr-number $PR_NUMBER \
    --with-glaze-output --output json > pr-analysis.json
```

### Data Processing Workflows
Structured output integrates naturally with data processing tools and analytics platforms.

```bash
# Process multiple PRs for trend analysis
for pr in 100 101 102; do
    ./pr-analyzer-dual analyze functions \
        --owner company --repo project --pr-number $pr \
        --with-glaze-output --output csv >> function-changes.csv
done

# Generate summary statistics
./pr-analyzer-dual get context \
    --owner owner --repo repo --pr-number 123 \
    --with-glaze-output --output json | \
    jq '.[] | .changed_functions' | \
    awk '{sum+=$1} END {print "Average changed functions:", sum/NR}'
```

### Script Integration
The consistent structured output makes pr-analyzer-dual an excellent building block for larger analysis scripts.

```bash
#!/bin/bash
# Custom analysis script combining multiple commands

# Get PR metadata
PR_DATA=$(./pr-analyzer-dual get commits \
    --owner $1 --repo $2 --pr-number $3 \
    --with-glaze-output --output json)

# Analyze function changes
FUNCTION_DATA=$(./pr-analyzer-dual analyze functions \
    --owner $1 --repo $2 --pr-number $3 \
    --only-changed --with-glaze-output --output json)

# Generate custom report
echo "PR Analysis Report" | python3 generate_report.py \
    --commits "$PR_DATA" --functions "$FUNCTION_DATA"
```

## Performance and Considerations

### Output Mode Performance
- Human-readable mode includes additional formatting processing
- Structured mode focuses on data extraction efficiency
- Large datasets may benefit from field selection to reduce output size
- JSON output is generally fastest for large data volumes

### Memory Usage
- Structured mode can handle larger datasets more efficiently
- Field selection reduces memory footprint significantly
- CSV format is most memory-efficient for large datasets
- JSON format provides best balance of features and performance

### Error Handling in Dual Mode
Both modes handle errors consistently:
- Human-readable mode provides contextual error explanations
- Structured mode includes error fields in the data output
- Network and API errors are handled gracefully in both modes
- Parse errors are reported with helpful diagnostic information

The dual mode architecture ensures that pr-analyzer-dual scales from quick interactive queries to large-scale automated analysis workflows, making it a versatile tool for any GitHub-based development process.
