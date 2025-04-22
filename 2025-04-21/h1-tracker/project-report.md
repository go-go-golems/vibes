# H1 Tracker Project Report

## Overview

This project implements a system for tracking H1 headings from web pages using a Firefox extension and a Go HTTP server. The extension automatically captures H1 elements from web pages (including dynamically added content) and sends them to a local server, which stores the data in a JSON file.

## Components

The system consists of two main components:

1. **Go HTTP Server**: A backend server that receives H1 heading data from the browser extension and stores it in a JSON file.
2. **Firefox Extension**: A browser extension that captures H1 headings from web pages and sends them to the server.

## Implementation Details

### Go HTTP Server

The server is implemented in Go and provides the following features:

- Receives H1 heading data via HTTP POST requests
- Supports both single events and batch events
- Outputs events to stdout for monitoring
- Appends events to a single JSON file as requested
- Implements CORS support to allow browser extension communication
- Provides a simple web interface for status monitoring

#### Server Endpoints

- `/h1`: Receives a single H1 event
- `/batch`: Receives multiple H1 events in a batch
- `/`: Provides a simple status page

#### Data Structure

Each H1 event is stored with the following structure:

```json
{
  "url": "https://example.com",
  "text": "Example Heading",
  "timestamp": "2025-04-21T18:49:19.171326903-04:00"
}
```

### Firefox Extension

The extension is implemented using the WebExtensions API and provides the following features:

- Automatically captures H1 headings from web pages
- Uses MutationObserver to detect dynamically added H1 elements
- Batches events to reduce server load
- Provides notifications when events are sent to the server

#### Extension Components

- **manifest.json**: Defines the extension metadata, permissions, and structure
- **background.js**: Handles communication with the server and manages batch processing
- **content.js**: Detects H1 elements on web pages using MutationObserver
- **icons**: Visual assets for the extension

#### Batch Processing

The extension implements an intelligent batching system:
- Events are collected until reaching a batch size (5 events)
- If batch size isn't reached, events are sent after a timeout (10 seconds)
- Batch events are sent to the `/batch` endpoint
- Single events are sent to the `/h1` endpoint

## Testing

### Server Testing

The server was tested using curl commands to verify:
- Single event processing
- Batch event processing
- JSON file storage functionality

Example curl commands:

```bash
# Test single event
curl -X POST -H "Content-Type: application/json" \
  -d '{"url":"http://example.com","text":"Example Website Heading"}' \
  http://localhost:8080/h1

# Test batch events
curl -X POST -H "Content-Type: application/json" \
  -d '{"events":[{"url":"http://example.com/page1","text":"Page 1 Heading"},{"url":"http://example.com/page2","text":"Page 2 Heading"}]}' \
  http://localhost:8080/batch
```

### Extension Testing

Due to the sandbox environment constraints, extension testing was simulated using a Node.js script that:
- Simulated H1 headings from popular news websites
- Tested both single event and batch event functionality
- Verified server responses and data storage

Test results showed successful processing of both individual and batched H1 events, with proper storage in the JSON file.

## Verification

The JSON file was examined after testing to confirm that all events were properly stored with timestamps. The file contained all test events, demonstrating that both the server and the extension simulation were functioning correctly.

## Installation and Usage

### Server Installation

1. Install Go if not already installed
2. Clone the repository
3. Navigate to the project directory
4. Build and run the server:
   ```bash
   go build -o h1-tracker
   ./h1-tracker
   ```

The server will start on port 8080 by default. You can specify a different port using the `-port` flag.

### Extension Installation

1. Open Firefox
2. Navigate to `about:debugging`
3. Click "This Firefox"
4. Click "Load Temporary Add-on"
5. Select the `manifest.json` file from the extension directory

Alternatively, you can install the extension from the packaged zip file.

## Limitations and Future Improvements

- The server currently only stores events locally; a database could be added for better scalability
- The extension could be enhanced to filter or categorize H1 headings
- Authentication could be added to the server for security
- The extension could be published to the Firefox Add-ons store for easier installation
- Support for Chrome and other browsers could be added

## Conclusion

This project successfully implements a system for tracking H1 headings from web pages using a Firefox extension and a Go HTTP server. The system meets all the specified requirements, including automatic detection of H1 elements, support for dynamically added content, batch processing, and notifications.
