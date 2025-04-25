# TTMP Web Interface Documentation

This document provides an overview of the TTMP web interface, which allows users to navigate and interact with TTMP documents through a user-friendly browser interface.

## Overview

The TTMP web interface provides the following key features:

1. File tree visualization for navigating document hierarchies
2. Document listing with metadata display
3. Detailed document view with content and metadata presentation
4. Query functionality for searching documents
5. Document validation tool
6. Statistics dashboard for collection insights

## Accessing the Web Interface

Start the TTMP web server using the following command:

```bash
./ttmp-server --path /path/to/documents --port 8003
```

Then access the web interface by navigating to http://localhost:8003 in your web browser.

## Interface Components

### Document Explorer

The Document Explorer provides two ways to navigate your TTMP documents:

1. **File Tree View**: A hierarchical view of all documents, organized by directory
2. **Document List**: A card-based view showing document metadata

![Document Explorer](https://8003-imarvb-tjwkkd.public.scrapybara.com)

### Document View

When you select a document, the Document View displays:

1. The document title
2. Key metadata displayed in an easy-to-read format
3. The full document content with markdown rendering and syntax highlighting
4. A "View All Metadata" button for accessing the complete metadata set

![Document View](https://8003-imarvb-tjwkkd.public.scrapybara.com/#document)

### Query Interface

The Query interface allows you to search for documents using:

1. Metadata queries using the TTMP query syntax
2. Content keywords for full-text search

Example queries:
- `document_type='tutorial' && status='published'`
- `owner='Scout' || tags contains 'reference'`

![Query Interface](https://8003-imarvb-tjwkkd.public.scrapybara.com/#query)

### Validation Tool

The Validation tool allows you to:

1. Paste TTMP document content for validation
2. See validation results with detailed error messages
3. Test document compliance with the TTMP specification

![Validation Tool](https://8003-imarvb-tjwkkd.public.scrapybara.com/#validate)

### Statistics Dashboard

The Statistics dashboard provides insights about your document collection:

1. Total document count
2. Document type distribution with pie chart visualization
3. Metadata usage statistics

![Statistics Dashboard](https://8003-imarvb-tjwkkd.public.scrapybara.com/#stats)

## Using the Interface

### Navigating Documents

1. Use the file tree on the left to navigate the document hierarchy
2. Click on document cards in the main view to open a document
3. Use the search box to filter documents by title, content, or metadata

### Running Queries

1. Navigate to the Query tab
2. Enter a query expression using the TTMP query syntax
3. Optionally add content keywords for full-text search
4. Click "Run Query" to see matching documents

### Validating Documents

1. Navigate to the Validate tab
2. Paste a TTMP document including both YAML front matter and markdown content
3. Click "Validate" to check if the document complies with the TTMP specification

### Viewing Statistics

1. Navigate to the Stats tab
2. View overall collection statistics
3. Explore document type distribution
4. Analyze metadata key usage patterns

## Implementation Notes

The web interface is built using:

- **Backend**: Go HTTP server with RESTful API endpoints
- **Frontend**: HTML5, CSS3, and JavaScript with Bootstrap 5 for responsive layout
- **Libraries**: Chart.js for data visualization, Marked for markdown rendering, Highlight.js for syntax highlighting

## Customization

You can customize the web interface by modifying the following files:

- `pkg/server/static/index.html`: Main HTML structure
- `pkg/server/static/styles.css`: Styling
- `pkg/server/static/app.js`: Frontend functionality
- `pkg/server/static/js/filetree.js`: File tree component

## Future Enhancements

Planned future enhancements for the web interface include:

1. Document editing capabilities
2. User authentication and permissions
3. Real-time collaboration features
4. Advanced visualization of document relationships
5. Customizable dashboard widgets