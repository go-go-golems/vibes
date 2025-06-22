# Enhanced Search Features Documentation

## Overview

The Conversation Manager now includes advanced search capabilities with comprehensive tag filtering and date range search functionality.

## Tag Search Features

### Basic Tag Search
- **Syntax**: `tag:tagname`
- **Example**: `tag:python` - Find conversations tagged with "python"
- **Multiple tags**: `tag:python tag:analysis` - Find conversations with both tags

### Tag Search Mode
- **Activation**: Press `Ctrl+T` in search mode
- **Usage**: Type tag names to see suggestions with usage counts
- **Selection**: Press `Enter` to add selected tag to main search

### Tag Categories
The system automatically categorizes tags:
- 🔴 **Code**: Programming and development (python, react, css, etc.)
- 🟠 **Writing**: Creative writing and content
- 🟡 **Analysis**: Data analysis and research
- 🟢 **Creative**: Creative projects
- 🔵 **Q&A**: Questions and answers
- 🟣 **Other**: Miscellaneous topics

## Date Range Search Features

### Date Range Syntax
- **Range**: `range:RANGE` - Search within a specific date range
- **After**: `after:DATE` - Find conversations after a specific date
- **Before**: `before:DATE` - Find conversations before a specific date
- **On**: `on:DATE` - Find conversations from a specific date

### Predefined Date Ranges
- `today` - Conversations from today
- `yesterday` - Conversations from yesterday
- `this-week` - Conversations from this week
- `last-week` - Conversations from last week
- `this-month` - Conversations from this month
- `last-month` - Conversations from last month
- `this-year` - Conversations from this year
- `last-year` - Conversations from last year
- `last-7-days` - Conversations from the last 7 days
- `last-30-days` - Conversations from the last 30 days
- `last-90-days` - Conversations from the last 90 days

### Relative Date Ranges
- `last-N-days` - Last N days (e.g., `last-14-days`)
- `last-N-weeks` - Last N weeks (e.g., `last-2-weeks`)
- `last-N-months` - Last N months (e.g., `last-3-months`)

### Absolute Date Ranges
- `YYYY-MM-DD` - Specific date (e.g., `2024-06-22`)
- `YYYY-MM-DD to YYYY-MM-DD` - Date range (e.g., `2024-06-01 to 2024-06-22`)

### Date Range Search Mode
- **Activation**: Press `Ctrl+D` in search mode
- **Usage**: Type date expressions to see suggestions
- **Selection**: Press `Enter` to add selected date range to main search

## Combined Search Examples

### Tag + Date Range
```
tag:python range:last-week
```
Find Python-related conversations from last week.

### Multiple Filters
```
tag:react tag:typescript after:2024-06-01
```
Find React and TypeScript conversations after June 1st, 2024.

### Content + Date
```
machine learning range:last-30-days
```
Find conversations containing "machine learning" from the last 30 days.

### Title Search + Tags
```
title:dashboard tag:react range:today
```
Find conversations with "dashboard" in the title, tagged with React, from today.

## Advanced Search Modifiers

### Search Scope Modifiers
- `title:TERM` - Search only in conversation titles
- `content:TERM` - Search only in message content
- `model:TERM` - Filter by AI model used

### Search Operators
- **Exact phrases**: Use quotes for exact matches: `"exact phrase"`
- **Multiple terms**: Space-separated terms are treated as AND
- **Tag combinations**: Multiple tag filters are treated as AND

## Keyboard Shortcuts

### Search Mode
- `/` - Enter search mode
- `Ctrl+T` - Toggle tag search mode
- `Ctrl+D` - Toggle date range search mode
- `Ctrl+M` - Cycle through search modes
- `Tab` - Show/hide suggestions
- `Ctrl+U` - Clear entire search
- `Ctrl+W` - Clear last word
- `↑/↓` - Navigate search history or results
- `Enter` - Select result or suggestion
- `Esc` - Exit search mode

### Navigation
- `j/k` - Navigate up/down in results
- `Space` - Preview selected conversation
- `f` - Toggle filter mode
- `q` - Quit application

## Search Suggestions

The system provides intelligent suggestions based on:
- **Popular tags** with usage counts
- **Search history** with recent queries
- **Date range patterns** with common ranges
- **Search prefixes** (tag:, range:, title:, etc.)

## Performance Features

### Smart Categorization
- Automatic tag categorization based on content analysis
- Visual indicators with emoji icons for quick recognition
- Color-coded categories for better organization

### Relevance Scoring
- Tag matches weighted higher than content matches
- Title matches weighted higher than message content
- Recent conversations prioritized in equal-score situations

### Real-time Search
- Instant results as you type
- Debounced input handling for performance
- Efficient filtering with multiple criteria

## Usage Tips

1. **Start broad, then narrow**: Begin with general terms, then add tags or date ranges
2. **Use tag mode for discovery**: Press `Ctrl+T` to explore available tags
3. **Combine filters**: Mix tags, dates, and content search for precise results
4. **Use date shortcuts**: `range:today` is faster than typing full dates
5. **Leverage suggestions**: Press `Tab` to see relevant suggestions
6. **Check search history**: Use `↑/↓` to revisit previous searches

## Examples by Use Case

### Finding Recent Work
```
tag:code range:this-week
```

### Research Sessions
```
tag:analysis range:last-month
```

### Creative Projects
```
tag:creative tag:writing range:last-90-days
```

### Debugging Sessions
```
tag:debug after:yesterday
```

### Learning Sessions
```
tag:tutorial tag:help range:this-month
```

This enhanced search system makes it easy to find exactly what you're looking for in your conversation history, whether you're searching by topic, timeframe, or specific content.

