---
Title: Obsidian Integration
Slug: obsidian-integration
Short: How to use diary CLI with Obsidian and the Tasks plugin
Topics:
- obsidian
- tasks
- integration
- workflow
SectionType: tutorial
---

# Obsidian Integration

The diary CLI is designed to work seamlessly with Obsidian and the Tasks plugin, creating a powerful workflow for managing both diary entries and actionable tasks.

## Setup

### 1. Initialize in Your Obsidian Vault

Navigate to your existing Obsidian vault and initialize the diary:

```bash
cd /path/to/your/obsidian/vault
diary init
```

This creates a `Logs` directory structure that integrates with your existing vault.

### 2. Install Obsidian Tasks Plugin

1. Open Obsidian
2. Go to Settings → Community Plugins
3. Search for "Tasks" and install it
4. Enable the Tasks plugin

### 3. Configure Tasks Plugin

The diary CLI uses specific tags that work with the Tasks plugin:

- `#toProcess` - Main tag for task queries
- `#todo` - Specific todo identification
- `#til`, `#thought`, `#did`, `#link` - Entry type tags

## Workflow Integration

### Daily Workflow

1. **Morning Planning**: Create todos for the day
   ```bash
   diary todo add "Review PRs" --priority high
   diary todo add "Write documentation" --due today
   ```

2. **Throughout the Day**: Add entries as they happen
   ```bash
   diary add til "Learned about Go channels"
   diary add thought "Need to refactor authentication module"
   diary add link "https://example.com/article" --title "Interesting Read"
   ```

3. **Evening Review**: Record accomplishments
   ```bash
   diary add did "Completed user authentication feature"
   diary add did "Fixed bug in payment processing"
   ```

### Obsidian Tasks Plugin Queries

Use these queries in your Obsidian notes to display diary entries:

#### All Unfinished Tasks
```tasks
not done
(description includes #toProcess)
```

#### Today's Tasks
```tasks
not done
(description includes #toProcess)
created today
```

#### High Priority Tasks
```tasks
not done
(description includes #toProcess)
(description includes Priority: high)
```

#### Recent TIL Entries
```tasks
(description includes #til)
created after last week
sort by created reverse
```

## File Structure

The diary CLI creates files that are fully compatible with Obsidian:

```
YourVault/
├── Logs/
│   ├── README.md
│   ├── 2025-08-04.md
│   ├── 2025-08-05.md
│   └── 2025/
│       └── 08/
└── (your other Obsidian files)
```

### Daily File Format

Each daily file follows this structure:

```markdown
# Log 2025/08/2025-08-04

## To Process

- [ ] **TIL**: Go interfaces are satisfied implicitly #toProcess #til
  - Added: 2025-08-04 09:31

- [ ] Review pull requests 📅 2025-08-05 #todo #toProcess
  - Priority: high
  - ID: abc123...
  - Added: 2025-08-04 10:15

## TIL: Learning Go Channels

Content about channels...

*Added: 2025-08-04 14:30*
```

## Task Format Integration

When using `--format task`, entries are created in Obsidian Tasks format:

### Todo Tasks
```markdown
- [ ] Review pull requests 📅 2025-08-05 #todo #toProcess
  - Priority: high
  - ID: abc123...
  - Added: 2025-08-04 10:15
```

### Entry Tasks
```markdown
- [ ] **TIL**: Go interfaces are satisfied implicitly #toProcess #til
  - Added: 2025-08-04 09:31
```

## Advanced Obsidian Features

### Linking and References

You can reference diary entries from other Obsidian notes:

```markdown
As I learned in [[2025-08-04#TIL Go interfaces]], interfaces in Go...
```

### Templates

Create Obsidian templates that include diary CLI commands:

```markdown
# Daily Note Template

## Tasks for Today
<!-- Use: diary todo add "task description" -->

## What I Learned
<!-- Use: diary add til "learning" -->

## Thoughts and Ideas
<!-- Use: diary add thought "idea" -->
```

### Dataview Integration

If you use the Dataview plugin, you can query diary entries:

```dataview
TABLE file.ctime as "Created", type, content
FROM "Logs"
WHERE contains(file.content, "#til")
SORT file.ctime DESC
```

## Best Practices

### 1. Consistent Tagging
- Always use the `#toProcess` tag for task queries
- Use specific type tags (`#til`, `#todo`, etc.) for filtering
- Add custom tags for personal organization

### 2. File Organization
- Keep daily files in the `Logs` directory
- Use consistent date format (YYYY-MM-DD)
- Consider creating monthly or yearly subdirectories for large volumes

### 3. Task Management
- Review tasks regularly using Obsidian Tasks queries
- Mark completed tasks using Obsidian's checkbox interface
- Use priorities and due dates effectively

### 4. Cross-Referencing
- Link diary entries to project notes
- Reference specific learnings in documentation
- Create index notes for important topics

## Troubleshooting

### Tasks Not Appearing in Queries
- Ensure the `#toProcess` tag is present
- Check that the Tasks plugin is enabled
- Verify the query syntax in your task blocks

### File Conflicts
- The diary CLI appends to existing files safely
- Manual edits in Obsidian are preserved
- Use `diary config` to verify file paths

### Sync Issues
- If using Obsidian Sync, ensure the Logs directory is included
- Consider using git for version control of your vault
- Be careful with concurrent edits from multiple devices

This integration creates a powerful system where you can use the diary CLI for quick entry creation and Obsidian for rich viewing, editing, and task management.

