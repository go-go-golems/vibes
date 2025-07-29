# Dual Command Pattern Demonstration

This document demonstrates the dual command pattern implementation in the PR Analyzer tool, showing how the same commands provide both human-readable reports and structured data output.

## Overview

The dual command pattern allows each command to implement both:
- **BareCommand**: Human-readable, markdown-style output (default)
- **GlazeCommand**: Structured data output with full glazed features

## Command Examples

### 1. Get Commits Command

#### Human-Readable Output (Default)
```bash
./pr-analyzer-dual get commits --owner go-go-golems --repo geppetto --pr-number 181
```

**Output:**
```markdown
# Pull Request #181 Commits

**Repository:** go-go-golems/geppetto
**Total Commits:** 5

## 1. Add Gemini genai support

- **SHA:** `b2b2518a`
- **Author:** Manuel Odendahl <wesen@ruinwesen.com>
- **Date:** 2025-07-06 18:10:38

## 2. Initial plan

- **SHA:** `6633fb9c`
- **Author:** copilot-swe-agent[bot] <198982749+Copilot@users.noreply.github.com>
- **Date:** 2025-07-06 18:24:28

## 3. Remove unused Gemini placeholder code from settings-step.go

Co-authored-by: wesen <128441+wesen@users.noreply.github.com>

- **SHA:** `fb1180d4`
- **Author:** copilot-swe-agent[bot] <198982749+Copilot@users.noreply.github.com>
- **Date:** 2025-07-06 18:30:28

## 4. :art: Fix linting

- **SHA:** `212a5110`
- **Author:** Manuel Odendahl <wesen@ruinwesen.com>
- **Date:** 2025-07-07 01:46:46

## 5. :art: Add a bit of logging to gemini, and set up layers

- **SHA:** `5c584ed2`
- **Author:** Manuel Odendahl <wesen@ruinwesen.com>
- **Date:** 2025-07-07 02:27:02
```

#### Structured Data Output
```bash
./pr-analyzer-dual get commits --owner go-go-golems --repo geppetto --pr-number 181 --with-glaze-output --output json --fields sha,message,author_name
```

**Output:**
```json
[
{
  "author_name": "Manuel Odendahl",
  "message": "Add Gemini genai support",
  "sha": "b2b2518ad0e5c8085eb9a58003eeb78d51510916"
},
{
  "author_name": "copilot-swe-agent[bot]",
  "message": "Initial plan",
  "sha": "6633fb9c1393d88b335ec2f3e469ef9f7e4e6428"
},
{
  "author_name": "copilot-swe-agent[bot]",
  "message": "Remove unused Gemini placeholder code from settings-step.go\n\nCo-authored-by: wesen <128441+wesen@users.noreply.github.com>",
  "sha": "fb1180d449d28aae0f53766e8e44dad35adabae3"
},
{
  "author_name": "Manuel Odendahl",
  "message": ":art: Fix linting",
  "sha": "212a511001ca6e4b31d537283f8c08aa91a373c5"
},
{
  "author_name": "Manuel Odendahl",
  "message": ":art: Add a bit of logging to gemini, and set up layers",
  "sha": "5c584ed276a4cda2bd4d146ae67d68105dde924c"
}
]
```

### 2. Get Context Command

#### Human-Readable Output (Default)
```bash
./pr-analyzer-dual get context --owner go-go-golems --repo geppetto --pr-number 181
```

**Output:**
```markdown
# Pull Request #181 Context Analysis

**Repository:** go-go-golems/geppetto
**Files Changed:** 11

## 📁 pkg/steps/ai/factory.go

**Changes:**
- Lines Added: 17
- Lines Removed: 0
- Lines Modified: 0

**Functions:**
- Total Functions: 2
- Changed Functions: 1
- Changed Function Names: NewStep

## 📁 pkg/steps/ai/gemini/chat-step.go

**Changes:**
- Lines Added: 218
- Lines Removed: 0
- Lines Modified: 1

**Functions:**
- Total Functions: 9
- Changed Functions: 9
- Changed Function Names: WithSubscriptionManager, NewChatStep, AddPublishedTopic, roleToGeminiRole, messageToGeminiContent, makeContents, makeClient, RunInference, Start

## 📁 pkg/steps/ai/gemini/helpers.go

**Changes:**
- Lines Added: 6
- Lines Removed: 0
- Lines Modified: 1

**Functions:**
- Total Functions: 1
- Changed Functions: 1
- Changed Function Names: IsGeminiEngine

## 📁 pkg/steps/ai/settings/gemini/settings.go

**Changes:**
- Lines Added: 41
- Lines Removed: 0
- Lines Modified: 1

**Functions:**
- Total Functions: 3
- Changed Functions: 3
- Changed Function Names: NewSettings, Clone, NewParameterLayer

## 📁 pkg/steps/ai/settings/settings-step.go

**Changes:**
- Lines Added: 18
- Lines Removed: 0
- Lines Modified: 0

**Functions:**
- Total Functions: 9
- Changed Functions: 4
- Changed Function Names: NewStepSettings, Clone, UpdateFromParsedLayers, GetSummary
```

#### Structured Data Output
```bash
./pr-analyzer-dual get context --owner go-go-golems --repo geppetto --pr-number 181 --with-glaze-output --output csv --fields file_path,total_functions,changed_functions
```

**Output:**
```csv
file_path,total_functions,changed_functions
pkg/steps/ai/factory.go,2,1
pkg/steps/ai/gemini/chat-step.go,9,9
pkg/steps/ai/gemini/helpers.go,1,1
pkg/steps/ai/settings/gemini/settings.go,3,3
pkg/steps/ai/settings/settings-step.go,9,4
pkg/steps/ai/types/types.go,0,0
```

### 3. Analyze Functions Command

#### Human-Readable Output (Default)
```bash
./pr-analyzer-dual analyze functions --owner go-go-golems --repo geppetto --pr-number 181 --only-changed
```

**Output:**
```markdown
# Function Analysis for PR #181

**Repository:** go-go-golems/geppetto
**Filter:** Only showing changed functions

## 📁 pkg/steps/ai/factory.go

### 🔄 NewStep

- **Type:** Method
- **Lines:** 15-85
- **Exported:** true
- **Status:** Changed in this PR
- **Receiver:** *StandardStepFactory

## 📁 pkg/steps/ai/gemini/chat-step.go

### 🔄 WithSubscriptionManager

- **Type:** Function
- **Lines:** 25-29
- **Exported:** true
- **Status:** Changed in this PR

### 🔄 NewChatStep

- **Type:** Function
- **Lines:** 31-50
- **Exported:** true
- **Status:** Changed in this PR

### 🔄 AddPublishedTopic

- **Type:** Method
- **Lines:** 52-55
- **Exported:** true
- **Status:** Changed in this PR
- **Receiver:** *ChatStep

*File Summary: 9 functions (9 changed)*

---

**Overall Summary:**
- Total Functions: 18
- Changed Functions: 18
- Change Rate: 100.0%
```

#### Structured Data Output
```bash
./pr-analyzer-dual analyze functions --owner go-go-golems --repo geppetto --pr-number 181 --only-changed --with-glaze-output --output table --fields function_name,receiver,is_exported,start_line,end_line
```

**Output:**
```
+-------------------------+--------------------+-------------+------------+----------+
| function_name           | receiver           | is_exported | start_line | end_line |
+-------------------------+--------------------+-------------+------------+----------+
| NewStep                 | *StandardStepFactory | true        | 15         | 85       |
| WithSubscriptionManager |                    | true        | 25         | 29       |
| NewChatStep             |                    | true        | 31         | 50       |
| AddPublishedTopic       | *ChatStep          | true        | 52         | 55       |
| roleToGeminiRole        |                    | false       | 57         | 68       |
| messageToGeminiContent  |                    | false       | 70         | 88       |
| makeContents            |                    | false       | 90         | 108      |
| makeClient              |                    | false       | 110        | 120      |
| RunInference            | *ChatStep          | true        | 122        | 180      |
| Start                   | *ChatStep          | true        | 182        | 186      |
| IsGeminiEngine          |                    | true        | 5          | 7        |
| NewSettings             |                    | true        | 13         | 18       |
| Clone                   | *Settings          | true        | 20         | 31       |
| NewParameterLayer       |                    | true        | 33         | 65       |
| NewStepSettings         |                    | true        | 19         | 26       |
| Clone                   | *StepSettings      | true        | 28         | 40       |
| UpdateFromParsedLayers  | *StepSettings      | true        | 42         | 55       |
| GetSummary              | *StepSettings      | true        | 57         | 70       |
+-------------------------+--------------------+-------------+------------+----------+
```

## Key Benefits

### 1. **User Experience**
- **Default behavior**: Beautiful, readable reports perfect for human consumption
- **Power user features**: Full glazed capabilities when needed
- **No complexity**: Simple commands work out of the box

### 2. **Automation Friendly**
- **Scriptable**: Easy to parse structured output for automation
- **Flexible formats**: JSON, CSV, YAML, and more
- **Field selection**: Get only the data you need

### 3. **Best of Both Worlds**
- **Single command**: No need to maintain separate tools
- **Consistent interface**: Same parameters work in both modes
- **Progressive disclosure**: Start simple, add complexity as needed

## Usage Patterns

### Interactive Use
```bash
# Quick overview
./pr-analyzer-dual get context --owner owner --repo repo --pr-number 123

# Detailed analysis
./pr-analyzer-dual analyze functions --owner owner --repo repo --pr-number 123 --only-changed
```

### Automation/Scripting
```bash
# Export to JSON for processing
./pr-analyzer-dual get commits --owner owner --repo repo --pr-number 123 --with-glaze-output --output json > commits.json

# Get specific fields as CSV
./pr-analyzer-dual analyze functions --owner owner --repo repo --pr-number 123 --with-glaze-output --output csv --fields function_name,is_changed > functions.csv

# Pipe to other tools
./pr-analyzer-dual get context --owner owner --repo repo --pr-number 123 --with-glaze-output --output json | jq '.[] | select(.changed_functions > 0)'
```

## Implementation Details

Each dual command implements both:
- `cmds.BareCommand` interface with `Run()` method for human-readable output
- `cmds.GlazeCommand` interface with `RunIntoGlazeProcessor()` method for structured output

The glazed framework automatically chooses the appropriate interface based on the presence of glazed flags like `--with-glaze-output`.

