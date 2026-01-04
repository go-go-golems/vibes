# TTMP Web Interface Screenshots

This document provides screenshots of the TTMP web interface in action. These screenshots showcase the various features and views of the web interface.

## Web Interface URL

The TTMP web interface is available at: https://8003-imarvb-tjwkkd.public.scrapybara.com

## Documents View

The Documents View is the main screen of the TTMP web interface. It shows a file tree navigation on the left and document cards on the right.

**Key features visible in this view:**
- File tree navigation showing the document hierarchy
- Search box for finding documents
- Document cards displaying key metadata
- Visual indicators for document type and status

![Documents View](https://8003-imarvb-tjwkkd.public.scrapybara.com)

## Document View

The Document View shows a selected document with its metadata and content.

**Key features visible in this view:**
- Document title at the top
- Formatted metadata display with badges for document type, status, etc.
- Creation and update dates
- Tags displayed as badges
- Formatted markdown content with syntax highlighting
- "View All Metadata" button for accessing all metadata fields

![Document View](https://8003-imarvb-tjwkkd.public.scrapybara.com/#document)

## Query View

The Query View allows users to search for documents using TTMP's query syntax.

**Key features visible in this view:**
- Query expression input field
- Content keywords input field
- Query syntax help text
- Results display showing matching documents
- Result summary showing the number of matches

![Query View](https://8003-imarvb-tjwkkd.public.scrapybara.com/#query)

## Validation View

The Validation View allows users to validate TTMP documents.

**Key features visible in this view:**
- Text area for pasting document content
- Validation button
- Validation result display showing success or failure
- Detailed error messages when validation fails

![Validation View](https://8003-imarvb-tjwkkd.public.scrapybara.com/#validate)

## Statistics View

The Statistics View provides insights about the document collection.

**Key features visible in this view:**
- Total document count
- Document type distribution with pie chart
- Metadata usage statistics showing how frequently each metadata field is used
- Percentage of documents using each metadata field

![Statistics View](https://8003-imarvb-tjwkkd.public.scrapybara.com/#stats)

## Responsive Design

The TTMP web interface is designed to be responsive and work well on different screen sizes.

**Key features of the responsive design:**
- Collapsible navigation menu
- Responsive grid layout
- Appropriate font sizes for different devices
- Touch-friendly interface elements

## Interactive Elements

The web interface includes several interactive elements:

- Clickable document cards
- Interactive file tree with expandable folders
- Modal dialogs for viewing all metadata
- Form inputs with validation
- Interactive charts in the statistics view

## Styling and Visual Design

The interface uses a clean, modern design with:

- Consistent color scheme
- Clear typography
- Visual hierarchy emphasizing important information
- Bootstrap 5 components for a polished look
- Icons for improved usability

## Usage Instructions

To use the web interface:

1. Start the TTMP web server using `./ttmp-server --path /path/to/documents --port 8080`
2. Open your browser to http://localhost:8080
3. Navigate the file tree or use the search to find documents
4. Click on document cards to view full documents
5. Use the navigation tabs to access different features (Query, Validate, Stats)

For more detailed instructions, see the [Web Interface Documentation](web_interface.md).