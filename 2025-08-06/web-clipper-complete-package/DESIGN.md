# Clipper Extension Design Document

## Overview
A cross-browser extension (Chrome & Firefox) that allows users to clip web content with metadata and save it via a Go backend process using native messaging.

## Architecture

### Browser Extension Components
1. **Popup UI** (`popup.html`, `popup.js`, `popup.css`)
   - Form with fields: title, category, note
   - Shows current URL and selected text
   - Send button to trigger clipping

2. **Content Script** (`content.js`)
   - Detects text selection
   - Extracts page metadata (title, URL)
   - Communicates with popup

3. **Background Script** (`background.js`)
   - Handles native messaging communication
   - Manages extension lifecycle
   - Coordinates between popup and content script

### Data Structure
```json
{
  "timestamp": "2025-01-08T10:30:00Z",
  "url": "https://example.com/article",
  "title": "User-provided title",
  "category": "article|TIL|thought|quote",
  "selectedText": "Selected text from page",
  "note": "User's additional notes",
  "pageTitle": "Original page title",
  "domain": "example.com"
}
```

### Go Backend
- Native messaging host application
- Receives JSON data from extension
- Saves clips as markdown files
- Organizes by date and category

### Native Messaging Protocol
- JSON-based communication
- Standard stdin/stdout messaging
- Message length prefixing (4 bytes)

## File Structure
```
clipper-extension/
├── extension/
│   ├── shared/           # Common files for both browsers
│   │   ├── popup.html
│   │   ├── popup.js
│   │   ├── popup.css
│   │   ├── content.js
│   │   └── background.js
│   ├── chrome/
│   │   └── manifest.json
│   └── firefox/
│       └── manifest.json
├── backend/
│   ├── main.go
│   ├── go.mod
│   └── go.sum
├── native-messaging/
│   ├── chrome-host.json
│   └── firefox-host.json
└── screenshots/
```

## Categories
- **TIL**: Today I Learned
- **article**: Full articles or blog posts
- **thought**: Personal thoughts or insights
- **quote**: Notable quotes or excerpts

## Output Format
Markdown files organized by date:
```
clips/
├── 2025-01-08/
│   ├── article-hacker-news-story.md
│   ├── quote-interesting-comment.md
│   └── TIL-new-technique.md
```

Each markdown file contains:
- Metadata header
- Original content
- User notes
- Source information

