---
Title: Automation Examples
Slug: automation
Short: Examples for integrating PR analysis into CI/CD pipelines and automation workflows
Topics:
  - automation
  - ci-cd
  - github-actions
  - scripting
  - structured-output
Commands:
  - get
  - analyze
Flags:
  - with-glaze-output
  - output
  - format
IsTopLevel: true
IsTemplate: false
ShowPerDefault: true
SectionType: Example
---

# Automation Examples

## CI/CD Pipeline Integration

### GitHub Actions Workflow
```yaml
name: PR Analysis
on:
  pull_request:
    types: [opened, synchronize]

jobs:
  analyze:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Setup Go
        uses: actions/setup-go@v3
        with:
          go-version: '1.21'
      
      - name: Build PR Analyzer
        run: go build -o pr-analyzer .
        
      - name: Analyze PR
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
        run: |
          ./pr-analyzer get context \
            --owner ${{ github.repository_owner }} \
            --repo ${{ github.event.repository.name }} \
            --pr-number ${{ github.event.number }} \
            --output json > pr-analysis.json
            
      - name: Check Critical Functions
        run: |
          ./pr-analyzer analyze functions \
            --owner ${{ github.repository_owner }} \
            --repo ${{ github.event.repository.name }} \
            --pr-number ${{ github.event.number }} \
            --only-changed \
            --fields function_name \
            | grep -E "(main|init|Start|Stop)" || echo "No critical functions changed"
```

### Jenkins Pipeline
```groovy
pipeline {
    agent any
    environment {
        GITHUB_TOKEN = credentials('github-token')
    }
    
    stages {
        stage('Analyze PR') {
            steps {
                script {
                    sh '''
                        ./pr-analyzer get context \
                            --owner ${GITHUB_REPOSITORY_OWNER} \
                            --repo ${GITHUB_REPOSITORY} \
                            --pr-number ${CHANGE_ID} \
                            --output json > pr-analysis.json
                    '''
                    
                    def analysis = readJSON file: 'pr-analysis.json'
                    echo "PR Analysis: ${analysis.size()} files changed"
                }
            }
        }
    }
}
```

## Scripting Examples

### Bash Script for PR Review
```bash
#!/bin/bash
set -e

OWNER=$1
REPO=$2
PR_NUMBER=$3

if [ -z "$PR_NUMBER" ]; then
    echo "Usage: $0 <owner> <repo> <pr-number>"
    exit 1
fi

echo "=== PR Analysis Report ==="
echo "Repository: $OWNER/$REPO"
echo "PR Number: $PR_NUMBER"
echo

# Get overview
echo "=== Context Overview ==="
./pr-analyzer get context --owner $OWNER --repo $REPO --pr-number $PR_NUMBER

echo
echo "=== Changed Functions ==="
./pr-analyzer analyze functions --owner $OWNER --repo $REPO --pr-number $PR_NUMBER --only-changed

echo
echo "=== Commit History ==="
./pr-analyzer get commits --owner $OWNER --repo $REPO --pr-number $PR_NUMBER
```

### Python Integration
```python
import subprocess
import json
import sys

def analyze_pr(owner, repo, pr_number):
    """Analyze a PR and return structured data"""
    cmd = [
        './pr-analyzer', 'get', 'context',
        '--owner', owner,
        '--repo', repo, 
        '--pr-number', str(pr_number),
        '--output', 'json'
    ]
    
    result = subprocess.run(cmd, capture_output=True, text=True)
    if result.returncode != 0:
        raise Exception(f"Command failed: {result.stderr}")
    
    return json.loads(result.stdout)

def check_critical_changes(owner, repo, pr_number):
    """Check if critical functions were changed"""
    cmd = [
        './pr-analyzer', 'analyze', 'functions',
        '--owner', owner,
        '--repo', repo,
        '--pr-number', str(pr_number),
        '--only-changed',
        '--output', 'json'
    ]
    
    result = subprocess.run(cmd, capture_output=True, text=True)
    if result.returncode != 0:
        return []
    
    functions = json.loads(result.stdout)
    critical = ['main', 'init', 'Start', 'Stop', 'New']
    
    return [f for f in functions if any(c in f['function_name'] for c in critical)]

# Usage
if __name__ == "__main__":
    owner, repo, pr_num = sys.argv[1:4]
    
    context = analyze_pr(owner, repo, pr_num)
    critical = check_critical_changes(owner, repo, pr_num)
    
    print(f"Files changed: {len(context)}")
    if critical:
        print(f"Critical functions changed: {len(critical)}")
        for func in critical:
            print(f"  - {func['function_name']} in {func['file_path']}")
```

## Data Processing

### jq Examples for JSON Processing
```bash
# Extract just function names that changed
./pr-analyzer analyze functions --owner owner --repo repo --pr-number 123 --only-changed --output json | \
  jq -r '.[].function_name'

# Get files with most changes
./pr-analyzer get context --owner owner --repo repo --pr-number 123 --output json | \
  jq -r 'sort_by(.lines_added + .lines_removed) | reverse | .[0:5] | .[] | "\(.file_path): +\(.lines_added)/-\(.lines_removed)"'

# Check for exported function changes
./pr-analyzer analyze functions --owner owner --repo repo --pr-number 123 --only-changed --output json | \
  jq -r '.[] | select(.is_exported == true) | .function_name'
```

### CSV Processing with awk
```bash
# Count functions by file
./pr-analyzer analyze functions --owner owner --repo repo --pr-number 123 --output csv | \
  awk -F',' 'NR>1 {count[$4]++} END {for (file in count) print file, count[file]}'

# Find files with high change rates
./pr-analyzer get context --owner owner --repo repo --pr-number 123 --output csv | \
  awk -F',' 'NR>1 && $5>0 {rate=$6/$5*100; if(rate>50) print $1, rate"%"}'
```
