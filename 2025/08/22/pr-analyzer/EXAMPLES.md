# PR Analyzer Examples

This document provides practical examples of using the PR analyzer tool with real-world scenarios.

## Test Repository Examples

The included test repository (`testdata/fake-repo`) contains three merge commits that demonstrate different types of changes:

### Example 1: Frontend-Only Changes (Merge Commit: 9fae05f)

This merge commit represents a typical frontend-focused pull request.

```bash
./pr-analyzer analyze --repo-path testdata/fake-repo --merge-commit 9fae05f5c32139da3b0673e2b735c08b1e0a9445 --use-defaults
```

**Results:**
- **Languages**: CSS (3 files), JavaScript (2 files)
- **Systems**: frontend only
- **Cross-system rate**: 0%
- **Commits**: 2 commits, both touching only frontend files

**Key Insights:**
- Pure frontend development with no backend dependencies
- Good separation of concerns
- Low risk of breaking other systems

### Example 2: Backend-Only Changes (Merge Commit: dae4140)

This merge commit shows backend API improvements with database changes.

```bash
./pr-analyzer analyze --repo-path testdata/fake-repo --merge-commit dae4140739dd2ffba5d9b3b4d8c563bf9ec69fc5 --use-defaults
```

**Results:**
- **Languages**: Go (3 files), SQL (1 file)
- **Systems**: backend, database
- **Cross-system rate**: 0%
- **Commits**: 2 commits, each touching a single system

**Key Insights:**
- Well-structured backend development
- Database migrations handled separately
- Each commit has a single responsibility

### Example 3: Full-Stack Integration (Merge Commit: f76cb9d)

This merge commit demonstrates a complex change touching multiple subsystems.

```bash
./pr-analyzer analyze --repo-path testdata/fake-repo --merge-commit f76cb9d5ee2aa23289371aa0f3cd84d886cba0a7 --use-defaults
```

**Results:**
- **Languages**: Markdown, YAML, JavaScript, Go
- **Systems**: config, docs, frontend, tests
- **Cross-system rate**: 100%
- **Co-occurrence**: All systems touched together in single commit

**Key Insights:**
- High integration complexity
- Single commit affects multiple systems
- Requires careful testing across all affected areas

## Custom Category Examples

### Example 4: Microservices Architecture

For a microservices project, you might want to categorize by service:

```bash
./pr-analyzer analyze --merge-commit abc123 --categories "user-service:services/user/**;order-service:services/order/**;payment-service:services/payment/**;shared:shared/**,common/**"
```

### Example 5: Mobile App Development

For a mobile app with shared code:

```bash
./pr-analyzer analyze --merge-commit def456 --categories "ios:ios/**,*.swift,*.m;android:android/**,*.java,*.kt;shared:shared/**,*.dart;backend:backend/**,*.go"
```

### Example 6: Web Application Layers

For a traditional web application:

```bash
./pr-analyzer analyze --merge-commit ghi789 --categories "presentation:views/**,templates/**,*.html;business:services/**,models/**;data:repositories/**,*.sql;infrastructure:config/**,docker/**"
```

## Output Format Examples

### JSON Output for CI/CD Integration

```bash
./pr-analyzer analyze --merge-commit abc123 --output json > pr-analysis.json
```

Use in CI/CD pipeline:
```bash
# Check if cross-system rate is too high
CROSS_SYSTEM_RATE=$(cat pr-analysis.json | jq '.cross_system_stats.cross_system_rate')
if (( $(echo "$CROSS_SYSTEM_RATE > 50" | bc -l) )); then
  echo "Warning: High cross-system rate ($CROSS_SYSTEM_RATE%)"
fi
```

### YAML Output for Documentation

```bash
./pr-analyzer analyze --merge-commit abc123 --output yaml > pr-analysis.yaml
```

## Filtering Examples

### Example 7: Exclude Documentation and Tests

Focus on production code changes:

```bash
./pr-analyzer analyze --merge-commit abc123 --excludes "*.md,docs/**,test/**,tests/**,*_test.*" --use-defaults
```

### Example 8: Exclude Configuration Files

Focus on application logic:

```bash
./pr-analyzer analyze --merge-commit abc123 --excludes "*.json,*.yaml,*.yml,config/**,*.conf" --use-defaults
```

### Example 9: Include Only Source Code

Analyze only programming language files:

```bash
./pr-analyzer analyze --merge-commit abc123 --excludes "*.md,*.txt,*.json,*.yaml,*.yml,*.xml,*.html,*.css" --use-defaults
```

## Branch Comparison Examples

### Example 10: Feature Branch Analysis

Compare a feature branch against main:

```bash
./pr-analyzer analyze --pr-branch feature/user-authentication --base-branch main --use-defaults
```

### Example 11: Release Branch Analysis

Compare release branch against previous release:

```bash
./pr-analyzer analyze --pr-branch release/v2.0 --base-branch release/v1.9 --use-defaults
```

## Real-World Scenarios

### Scenario 1: Code Review Preparation

Before code review, analyze the PR to understand its scope:

```bash
./pr-analyzer analyze --merge-commit $MERGE_COMMIT --use-defaults --output json | jq '.cross_system_stats.cross_system_rate'
```

If cross-system rate > 30%, consider:
- Breaking into smaller PRs
- Additional testing
- More thorough review

### Scenario 2: Release Risk Assessment

Analyze all PRs in a release:

```bash
for commit in $(git log --merges --format="%H" v1.0..v2.0); do
  echo "Analyzing $commit"
  ./pr-analyzer analyze --merge-commit $commit --use-defaults
done
```

### Scenario 3: Team Productivity Analysis

Track language distribution over time:

```bash
# Analyze last 10 merge commits
git log --merges --format="%H" -10 | while read commit; do
  ./pr-analyzer analyze --merge-commit $commit --output json | jq '.language_stats'
done
```

### Scenario 4: Architecture Compliance

Ensure changes follow architectural boundaries:

```bash
./pr-analyzer analyze --merge-commit abc123 --categories "ui:ui/**;business:business/**;data:data/**" --output json | jq '.cross_system_stats.cross_system_rate'
```

## Performance Considerations

### Large Repositories

For large repositories, consider:

1. **Exclude patterns** to reduce analysis scope
2. **Specific merge commits** rather than branch comparisons
3. **JSON output** for faster processing

### Automation

For automated analysis:

```bash
#!/bin/bash
# Automated PR analysis script
MERGE_COMMIT=$1
THRESHOLD=50

RESULT=$(./pr-analyzer analyze --merge-commit $MERGE_COMMIT --output json)
CROSS_RATE=$(echo $RESULT | jq '.cross_system_stats.cross_system_rate')

if (( $(echo "$CROSS_RATE > $THRESHOLD" | bc -l) )); then
  echo "❌ High cross-system rate: $CROSS_RATE%"
  exit 1
else
  echo "✅ Cross-system rate acceptable: $CROSS_RATE%"
fi
```

## Troubleshooting

### Common Issues

1. **"object not found"**: Ensure you're using the full commit hash
2. **Empty results**: Check if the merge commit has the expected structure
3. **Wrong categorization**: Verify your glob patterns match the file paths

### Debug Commands

```bash
# Check commit structure
git show --stat $MERGE_COMMIT

# Verify merge commit parents
git show --format="%P" $MERGE_COMMIT

# Test glob patterns
echo "frontend/src/App.js" | grep -E "frontend/\*\*"
```

