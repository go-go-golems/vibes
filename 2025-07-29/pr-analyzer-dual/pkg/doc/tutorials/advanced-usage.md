---
Title: Advanced Usage Techniques and Automation
Slug: advanced-usage
Short: Advanced techniques for filtering, automation, and complex analysis workflows
Topics:
  - advanced
  - automation
  - filtering
  - output-formatting
Commands:
  - get
  - analyze
Flags:
  - fields
  - filter
  - with-glaze-output
IsTopLevel: true
IsTemplate: false
ShowPerDefault: true
SectionType: Tutorial
---

# Advanced Usage Tutorial

## Field Selection and Filtering

### Selecting Specific Fields
Control exactly what data is returned:

```bash
# Only show function names and their change status
./pr-analyzer analyze functions --owner owner --repo repo --pr-number 123 \
  --fields function_name,is_changed,file_path

# Minimal commit information
./pr-analyzer get commits --owner owner --repo repo --pr-number 123 \
  --fields sha,message,author_name
```

### Filtering and Sorting
```bash
# Sort commits by date
./pr-analyzer get commits --owner owner --repo repo --pr-number 123 \
  --sort-by commit_date

# Reverse sort order
./pr-analyzer get commits --owner owner --repo repo --pr-number 123 \
  --sort-by commit_date --sort-reverse
```

## Complex Analysis Workflows

### Multi-Step Analysis
Combine multiple commands for comprehensive analysis:

```bash
#!/bin/bash
OWNER=$1
REPO=$2
PR=$3

echo "=== PR Analysis Report ==="

# Get basic context
echo "## Overview"
./pr-analyzer get context --owner $OWNER --repo $REPO --pr-number $PR

# Identify critical changes
echo "## Critical Function Changes"
./pr-analyzer analyze functions --owner $OWNER --repo $REPO --pr-number $PR \
  --only-changed --fields function_name,file_path,is_exported | \
  grep -E "(main|init|New|Start|Stop)"

# Generate machine-readable summary
echo "## Data Export"
./pr-analyzer get context --owner $OWNER --repo $REPO --pr-number $PR \
  --output json > "pr-${PR}-context.json"
  
./pr-analyzer analyze functions --owner $OWNER --repo $REPO --pr-number $PR \
  --only-changed --output csv > "pr-${PR}-functions.csv"
```

### Function-Focused Analysis
Deep dive into function changes:

```bash
# Show function bodies for changed functions
./pr-analyzer analyze functions --owner owner --repo repo --pr-number 123 \
  --only-changed --show-body

# Export function signatures for documentation
./pr-analyzer analyze functions --owner owner --repo repo --pr-number 123 \
  --fields function_name,signature,is_exported --output csv
```

## Integration Patterns

### Webhook Processing
Process PR events automatically:

```python
#!/usr/bin/env python3
import json
import subprocess
import sys
from pathlib import Path

def process_pr_webhook(webhook_data):
    """Process GitHub PR webhook and generate analysis"""
    
    if webhook_data['action'] not in ['opened', 'synchronize']:
        return
    
    pr = webhook_data['pull_request']
    repo = webhook_data['repository']
    
    owner = repo['owner']['login']
    repo_name = repo['name']
    pr_number = pr['number']
    
    # Generate analysis
    analysis = analyze_pr_changes(owner, repo_name, pr_number)
    
    # Save results
    output_dir = Path(f"pr-analysis/{pr_number}")
    output_dir.mkdir(parents=True, exist_ok=True)
    
    with open(output_dir / "analysis.json", "w") as f:
        json.dump(analysis, f, indent=2)

def analyze_pr_changes(owner, repo, pr_number):
    """Run comprehensive PR analysis"""
    
    # Get context
    context_cmd = [
        './pr-analyzer', 'get', 'context',
        '--owner', owner, '--repo', repo, '--pr-number', str(pr_number),
        '--output', 'json'
    ]
    context_result = subprocess.run(context_cmd, capture_output=True, text=True)
    context = json.loads(context_result.stdout) if context_result.returncode == 0 else []
    
    # Get changed functions
    functions_cmd = [
        './pr-analyzer', 'analyze', 'functions',
        '--owner', owner, '--repo', repo, '--pr-number', str(pr_number),
        '--only-changed', '--output', 'json'
    ]
    functions_result = subprocess.run(functions_cmd, capture_output=True, text=True)
    functions = json.loads(functions_result.stdout) if functions_result.returncode == 0 else []
    
    return {
        'pr_number': pr_number,
        'repository': f"{owner}/{repo}",
        'files_changed': len(context),
        'functions_changed': len(functions),
        'context': context,
        'functions': functions,
        'critical_functions': [f for f in functions if is_critical_function(f['function_name'])]
    }

def is_critical_function(name):
    """Check if function name indicates critical functionality"""
    critical_patterns = ['main', 'init', 'Start', 'Stop', 'New', 'Create', 'Delete']
    return any(pattern in name for pattern in critical_patterns)

if __name__ == "__main__":
    webhook_data = json.load(sys.stdin)
    process_pr_webhook(webhook_data)
```

### Quality Gates
Implement automated quality checks:

```bash
#!/bin/bash
# quality-gate.sh - Check PR changes against quality criteria

OWNER=$1
REPO=$2
PR=$3

# Check 1: No more than 50% of functions changed in any file
echo "Checking function change ratio..."
VIOLATION=$(./pr-analyzer get context --owner $OWNER --repo $REPO --pr-number $PR \
  --output json | \
  jq -r '.[] | select(.total_functions > 0 and (.changed_functions / .total_functions) > 0.5) | .file_path')

if [ -n "$VIOLATION" ]; then
    echo "❌ Quality gate failed: Too many functions changed in:"
    echo "$VIOLATION"
    exit 1
fi

# Check 2: No critical functions modified
echo "Checking for critical function changes..."
CRITICAL=$(./pr-analyzer analyze functions --owner $OWNER --repo $REPO --pr-number $PR \
  --only-changed --fields function_name | \
  grep -E "(main|init|Start|Stop)" || true)

if [ -n "$CRITICAL" ]; then
    echo "⚠️  Warning: Critical functions modified:"
    echo "$CRITICAL"
    echo "Manual review required."
fi

# Check 3: Reasonable commit message quality
echo "Checking commit message quality..."
BAD_MESSAGES=$(./pr-analyzer get commits --owner $OWNER --repo $REPO --pr-number $PR \
  --fields message | \
  grep -E "^(fix|update|change|wip)" || true)

if [ -n "$BAD_MESSAGES" ]; then
    echo "ℹ️  Info: Consider improving commit messages:"
    echo "$BAD_MESSAGES"
fi

echo "✅ Quality checks completed"
```

## Performance Optimization

### Efficient Data Processing
For large PRs, optimize your analysis:

```bash
# Process only Go files for function analysis
./pr-analyzer get context --owner owner --repo repo --pr-number 123 \
  --output json | \
  jq -r '.[] | select(.file_path | endswith(".go")) | .file_path' | \
  head -10  # Limit to first 10 Go files

# Focus on high-impact changes
./pr-analyzer get context --owner owner --repo repo --pr-number 123 \
  --output json | \
  jq -r 'sort_by(.lines_added + .lines_removed) | reverse | .[0:5]'
```

### Parallel Processing
Process multiple PRs concurrently:

```bash
#!/bin/bash
# parallel-analysis.sh - Analyze multiple PRs in parallel

OWNER=$1
REPO=$2
shift 2
PRS=("$@")

analyze_pr() {
    local pr=$1
    echo "Analyzing PR #$pr..."
    ./pr-analyzer get context --owner $OWNER --repo $REPO --pr-number $pr \
      --output json > "pr-${pr}-context.json"
    echo "Completed PR #$pr"
}

# Export function for parallel execution
export -f analyze_pr
export OWNER REPO

# Run analyses in parallel
printf '%s\n' "${PRS[@]}" | xargs -n1 -P4 -I{} bash -c 'analyze_pr {}'

echo "All analyses completed"
```

## Advanced Output Customization

### Custom Templates
Create custom output formats:

```bash
# Custom commit format
./pr-analyzer get commits --owner owner --repo repo --pr-number 123 \
  --template "{{.sha}}: {{.message}} ({{.author_name}})"

# Function summary template
./pr-analyzer analyze functions --owner owner --repo repo --pr-number 123 \
  --only-changed \
  --template "{{.file_path}}:{{.function_name}}:{{.start_line}}-{{.end_line}}"
```

### Data Aggregation
Combine multiple analyses:

```bash
# Generate comprehensive report
{
    echo "# PR Analysis Report"
    echo
    echo "## Summary"
    ./pr-analyzer get context --owner $OWNER --repo $REPO --pr-number $PR
    echo
    echo "## Changed Functions"
    ./pr-analyzer analyze functions --owner $OWNER --repo $REPO --pr-number $PR --only-changed
    echo
    echo "## Commit History"
    ./pr-analyzer get commits --owner $OWNER --repo $REPO --pr-number $PR
} > "pr-${PR}-report.md"
```

## Error Handling and Resilience

### Graceful Degradation
Handle partial failures:

```python
def robust_pr_analysis(owner, repo, pr_number):
    """Perform PR analysis with error handling"""
    results = {}
    
    # Try each analysis component independently
    try:
        results['context'] = get_pr_context(owner, repo, pr_number)
    except Exception as e:
        results['context_error'] = str(e)
    
    try:
        results['functions'] = get_changed_functions(owner, repo, pr_number)
    except Exception as e:
        results['functions_error'] = str(e)
    
    try:
        results['commits'] = get_pr_commits(owner, repo, pr_number)
    except Exception as e:
        results['commits_error'] = str(e)
    
    return results
```

### Retry Logic
Handle rate limiting and transient failures:

```bash
retry_command() {
    local max_attempts=3
    local attempt=1
    
    while [ $attempt -le $max_attempts ]; do
        if "$@"; then
            return 0
        fi
        
        echo "Attempt $attempt failed, retrying..."
        sleep $((attempt * 2))
        ((attempt++))
    done
    
    echo "All attempts failed"
    return 1
}

# Usage
retry_command ./pr-analyzer get commits --owner owner --repo repo --pr-number 123
```
