# PR Analyzer Examples

This document provides detailed examples of using the PR Analyzer tool with real GitHub pull requests from the `go-go-golems/geppetto` repository.

## Example PR: Adding Gemini Support (#181)

We'll use PR #181 from the geppetto repository as our example. This PR adds Google Gemini AI support to the project and demonstrates various types of code changes.

### 1. Getting PR Overview

First, let's understand what commits are in this PR:

```bash
./pr-analyzer get commits --owner go-go-golems --repo geppetto --pr-number 181 --fields sha,message,author_name
```

**Output:**
```
+------------------------------------------+---------------------------------------------------------------+------------------------+
| sha                                      | message                                                       | author_name            |
+------------------------------------------+---------------------------------------------------------------+------------------------+
| b2b2518ad0e5c8085eb9a58003eeb78d51510916 | Add Gemini genai support                                      | Manuel Odendahl        |
| 6633fb9c1393d88b335ec2f3e469ef9f7e4e6428 | Initial plan                                                  | copilot-swe-agent[bot] |
| fb1180d449d28aae0f53766e8e44dad35adabae3 | Remove unused Gemini placeholder code from settings-step.go   | copilot-swe-agent[bot] |
| 212a511001ca6e4b31d537283f8c08aa91a373c5 | :art: Fix linting                                             | Manuel Odendahl        |
| 5c584ed276a4cda2bd4d146ae67d68105dde924c | :art: Add a bit of logging to gemini, and set up layers       | Manuel Odendahl        |
+------------------------------------------+---------------------------------------------------------------+------------------------+
```

**Analysis:** This PR contains 5 commits showing an iterative development process with both human and AI contributions.

### 2. Understanding the Changes

Let's see what files and functions were affected:

```bash
./pr-analyzer get context --owner go-go-golems --repo geppetto --pr-number 181 --fields file_path,total_functions,changed_functions,changed_function_names
```

**Output:**
```
+------------------------------------------+-----------------+-------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| file_path                                | total_functions | changed_functions | changed_function_names                                                                                                                                                                        |
+------------------------------------------+-----------------+-------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| pkg/steps/ai/factory.go                  | 2               | 1                 | (s *StandardStepFactory).NewStep                                                                                                                                                             |
| pkg/steps/ai/gemini/chat-step.go         | 10              | 9                 | WithSubscriptionManager, NewChatStep, (step *ChatStep).AddPublishedTopic, roleToGeminiRole, messageToGeminiContent, makeContents, makeClient, (step *ChatStep).RunInference, (step *ChatStep).Start |
| pkg/steps/ai/gemini/helpers.go           | 1               | 1                 | IsGeminiEngine                                                                                                                                                                                |
| pkg/steps/ai/settings/gemini/settings.go | 3               | 3                 | NewSettings, (s *Settings).Clone, NewParameterLayer                                                                                                                                           |
| pkg/steps/ai/settings/settings-step.go   | 9               | 4                 | NewStepSettings, (s *StepSettings).Clone, (ss *StepSettings).UpdateFromParsedLayers, (ss *StepSettings).GetSummary                                                                            |
| pkg/steps/ai/types/types.go              | 0               | 0                 |                                                                                                                                                                                               |
+------------------------------------------+-----------------+-------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
```

**Analysis:** The PR affects 6 Go files with 18 functions modified across the codebase, showing a comprehensive integration of Gemini support.

### 3. Detailed Function Analysis

Now let's examine the specific functions that were changed:

```bash
./pr-analyzer analyze functions --owner go-go-golems --repo geppetto --pr-number 181 --only-changed --fields file_path,function_name,receiver,is_exported,start_line,end_line
```

**Output:**
```
+------------------------------------------+-------------------------+--------------------+-------------+------------+----------+
| file_path                                | function_name           | receiver           | is_exported | start_line | end_line |
+------------------------------------------+-------------------------+--------------------+-------------+------------+----------+
| pkg/steps/ai/factory.go                  | NewStep                 | *StandardStepFactory | true        | 15         | 85       |
| pkg/steps/ai/gemini/chat-step.go         | WithSubscriptionManager |                    | true        | 25         | 29       |
| pkg/steps/ai/gemini/chat-step.go         | NewChatStep             |                    | true        | 31         | 50       |
| pkg/steps/ai/gemini/chat-step.go         | AddPublishedTopic       | *ChatStep          | true        | 52         | 55       |
| pkg/steps/ai/gemini/chat-step.go         | roleToGeminiRole        |                    | false       | 57         | 68       |
| pkg/steps/ai/gemini/chat-step.go         | messageToGeminiContent  |                    | false       | 70         | 88       |
| pkg/steps/ai/gemini/chat-step.go         | makeContents            |                    | false       | 90         | 108      |
| pkg/steps/ai/gemini/chat-step.go         | makeClient              |                    | false       | 110        | 120      |
| pkg/steps/ai/gemini/chat-step.go         | RunInference            | *ChatStep          | true        | 122        | 180      |
| pkg/steps/ai/gemini/chat-step.go         | Start                   | *ChatStep          | true        | 182        | 186      |
| pkg/steps/ai/gemini/helpers.go           | IsGeminiEngine          |                    | true        | 5          | 7        |
| pkg/steps/ai/settings/gemini/settings.go | NewSettings             |                    | true        | 13         | 18       |
| pkg/steps/ai/settings/gemini/settings.go | Clone                   | *Settings          | true        | 20         | 31       |
| pkg/steps/ai/settings/gemini/settings.go | NewParameterLayer       |                    | true        | 33         | 65       |
| pkg/steps/ai/settings/settings-step.go   | NewStepSettings         |                    | true        | 19         | 26       |
| pkg/steps/ai/settings/settings-step.go   | Clone                   | *StepSettings      | true        | 28         | 40       |
| pkg/steps/ai/settings/settings-step.go   | UpdateFromParsedLayers  | *StepSettings      | true        | 42         | 55       |
| pkg/steps/ai/settings/settings-step.go   | GetSummary              | *StepSettings      | true        | 57         | 70       |
+------------------------------------------+-------------------------+--------------------+-------------+------------+----------+
```

**Analysis:** Most functions are exported (public), indicating this is a library addition. The functions span from small helpers (3 lines) to complex implementations (58 lines).

### 4. Examining the Diff

Let's look at a portion of the actual changes:

```bash
./pr-analyzer get diff --owner go-go-golems --repo geppetto --pr-number 181 --select diff | head -50
```

**Output:**
```diff
diff --git a/.gitignore b/.gitignore
index 7e073f7..550f9af 100644
--- a/.gitignore
+++ b/.gitignore
@@ -22,3 +22,4 @@ envs/
 .specstory/
 
 __debug*
+thirdparty/
diff --git a/go.mod b/go.mod
index 93a2378..28f5e7e 100644
--- a/go.mod
+++ b/go.mod
@@ -18,10 +18,14 @@ require (
 	github.com/xeipuuv/gojsonschema v1.2.0
 	github.com/yuin/goldmark v1.7.8
 	golang.org/x/sync v0.15.0
+	google.golang.org/genai v1.14.0
 	gopkg.in/yaml.v3 v3.0.1
 )
 
 require (
+	cloud.google.com/go v0.116.0 // indirect
+	cloud.google.com/go/auth v0.13.0 // indirect
+	cloud.google.com/go/compute/metadata v0.6.0 // indirect
 	github.com/Masterminds/semver/v3 v3.3.0 // indirect
```

**Analysis:** The diff shows dependency additions and new file creation, indicating a substantial feature addition.

### 5. File History Analysis

Let's see how the main factory file evolved:

```bash
./pr-analyzer get file-history --owner go-go-golems --repo geppetto --file-path pkg/steps/ai/factory.go --fields sha,message,author_name,commit_date | head -5
```

**Output:**
```
+------------------------------------------+----------------------------------+------------------------+----------------------+
| sha                                      | message                          | author_name            | commit_date          |
+------------------------------------------+----------------------------------+------------------------+----------------------+
| b2b2518ad0e5c8085eb9a58003eeb78d51510916 | Add Gemini genai support         | Manuel Odendahl        | 2025-07-06T18:10:38Z |
| 359734c5e1eb96aba1d857df146c28ac7ffd1370 | Merge pull request #180          | Manuel Odendahl        | 2025-07-06T18:21:56Z |
| 751e4d10d6863d07a2e9ed520bdffc813d8b6680 | Remove weaviate dependency       | copilot-swe-agent[bot] | 2025-07-06T16:30:13Z |
+------------------------------------------+----------------------------------+------------------------+----------------------+
```

**Analysis:** The factory file has been recently active with multiple changes, showing ongoing development.

## Advanced Examples

### 6. JSON Output for Automation

Export function analysis to JSON for further processing:

```bash
./pr-analyzer analyze functions --owner go-go-golems --repo geppetto --pr-number 181 --only-changed --output json > pr-181-functions.json
```

This creates a structured JSON file that can be processed by other tools or scripts.

### 7. CSV Export for Spreadsheet Analysis

```bash
./pr-analyzer get context --owner go-go-golems --repo geppetto --pr-number 181 --output csv > pr-181-context.csv
```

This creates a CSV file that can be opened in Excel or Google Sheets for further analysis.

### 8. Custom Field Selection

Focus on specific information:

```bash
./pr-analyzer analyze functions --owner go-go-golems --repo geppetto --pr-number 181 --fields function_name,is_exported,start_line --only-changed
```

**Output:**
```
+-------------------------+-------------+------------+
| function_name           | is_exported | start_line |
+-------------------------+-------------+------------+
| NewStep                 | true        | 15         |
| WithSubscriptionManager | true        | 25         |
| NewChatStep             | true        | 31         |
| AddPublishedTopic       | true        | 52         |
| roleToGeminiRole        | false       | 57         |
| messageToGeminiContent  | false       | 70         |
| makeContents            | false       | 90         |
| makeClient              | false       | 110        |
| RunInference            | true        | 122        |
| Start                   | true        | 182        |
| IsGeminiEngine          | true        | 5          |
| NewSettings             | true        | 13         |
| Clone                   | true        | 20         |
| NewParameterLayer       | true        | 33         |
| NewStepSettings         | true        | 19         |
| Clone                   | true        | 28         |
| UpdateFromParsedLayers  | true        | 42         |
| GetSummary              | true        | 57         |
+-------------------------+-------------+------------+
```

### 9. Function Body Analysis

Get the actual code for changed functions:

```bash
./pr-analyzer analyze functions --owner go-go-golems --repo geppetto --pr-number 181 --only-changed --show-body --fields function_name,signature,body | head -20
```

This would show the complete function signatures and bodies, useful for detailed code review.

## Use Case Scenarios

### Code Review Workflow

1. **Initial Assessment:**
   ```bash
   ./pr-analyzer get context --owner owner --repo repo --pr-number 123
   ```

2. **Detailed Function Review:**
   ```bash
   ./pr-analyzer analyze functions --owner owner --repo repo --pr-number 123 --only-changed
   ```

3. **Historical Context:**
   ```bash
   ./pr-analyzer get commits --owner owner --repo repo --pr-number 123
   ```

### CI/CD Integration

```bash
#!/bin/bash
# PR Analysis Script for CI/CD

PR_NUMBER=$1
OWNER="go-go-golems"
REPO="geppetto"

echo "Analyzing PR #$PR_NUMBER"

# Generate summary
./pr-analyzer get context --owner $OWNER --repo $REPO --pr-number $PR_NUMBER --output json > pr-summary.json

# Check for critical function changes
CRITICAL_FUNCTIONS=$(./pr-analyzer analyze functions --owner $OWNER --repo $REPO --pr-number $PR_NUMBER --only-changed --fields function_name --output csv | grep -E "(main|init|New)" | wc -l)

echo "Critical functions changed: $CRITICAL_FUNCTIONS"

if [ $CRITICAL_FUNCTIONS -gt 5 ]; then
    echo "Warning: Many critical functions changed, consider extra review"
fi
```

### Documentation Generation

```bash
# Generate PR documentation
./pr-analyzer get context --owner owner --repo repo --pr-number 123 --template "## Files Changed\n{{range .}}* {{.file_path}} - {{.changed_functions}} functions modified\n{{end}}" > pr-summary.md
```

## Tips and Best Practices

1. **Use Field Selection**: Always specify `--fields` to get only the data you need
2. **JSON for Automation**: Use `--output json` when integrating with other tools
3. **Authentication**: Set `GITHUB_TOKEN` for better rate limits
4. **Filtering**: Use `--only-changed` to focus on modifications
5. **Combine Commands**: Chain commands with shell pipes for complex analysis

## Troubleshooting Examples

### Rate Limiting
```bash
# If you get rate limited, set up authentication
export GITHUB_TOKEN=ghp_your_token_here
./pr-analyzer get commits --owner owner --repo repo --pr-number 123
```

### Large PRs
```bash
# For large PRs, use streaming and field selection
./pr-analyzer analyze functions --owner owner --repo repo --pr-number 123 --stream --fields function_name,is_changed
```

### Debug Mode
```bash
# To debug parameter issues
./pr-analyzer get commits --owner owner --repo repo --pr-number 123 --print-parsed-parameters
```

This comprehensive set of examples demonstrates the versatility and power of the PR Analyzer tool for understanding and analyzing GitHub pull requests.

