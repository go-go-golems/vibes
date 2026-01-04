// DOM Elements
const documentsView = document.getElementById('documents-view');
const documentView = document.getElementById('document-view');
const queryView = document.getElementById('query-view');
const validateView = document.getElementById('validate-view');
const statsView = document.getElementById('stats-view');

const fileTreeContainer = document.getElementById('file-tree-container');
const documentsList = document.getElementById('documents-list');
const documentTitle = document.getElementById('document-title');
const documentMetadata = document.getElementById('document-metadata');
const documentContent = document.getElementById('document-content');
const documentSearch = document.getElementById('document-search');
const documentSearchBtn = document.getElementById('document-search-btn');
const backToDocumentsBtn = document.getElementById('back-to-documents');

const queryInput = document.getElementById('query-input');
const queryKeywords = document.getElementById('query-keywords');
const runQueryBtn = document.getElementById('run-query-btn');
const queryResults = document.getElementById('query-results');
const querySummary = document.getElementById('query-summary');
const queryResultsList = document.getElementById('query-results-list');

const validateContent = document.getElementById('validate-content');
const validateBtn = document.getElementById('validate-btn');
const validateResult = document.getElementById('validate-result');

const statsOverview = document.getElementById('stats-overview');
const statsMetadataKeys = document.getElementById('stats-metadata-keys');
const statsDocTypes = document.getElementById('stats-doc-types');

// Global state
let allDocuments = [];
let fileTree = null;

// Navigation
document.getElementById('nav-documents').addEventListener('click', showDocumentsView);
document.getElementById('nav-query').addEventListener('click', showQueryView);
document.getElementById('nav-validate').addEventListener('click', showValidateView);
document.getElementById('nav-stats').addEventListener('click', showStatsView);
backToDocumentsBtn.addEventListener('click', showDocumentsView);

// Initialize
document.addEventListener('DOMContentLoaded', function() {
    initializeApp();
});

async function initializeApp() {
    // Initialize file tree component
    fileTree = new FileTreeView('file-tree-container', (path, metadata) => {
        showDocumentView(path);
    });
    
    // Load documents and populate file tree
    await loadDocuments();
}

// View Controllers
function showDocumentsView() {
    setActiveNavItem('nav-documents');
    documentsView.style.display = 'block';
    documentView.style.display = 'none';
    queryView.style.display = 'none';
    validateView.style.display = 'none';
    statsView.style.display = 'none';
    
    // Load documents if they haven't been loaded yet
    if (documentsList.innerHTML === '' && allDocuments.length === 0) {
        loadDocuments();
    }
}

function showDocumentView(documentPath) {
    documentsView.style.display = 'none';
    documentView.style.display = 'block';
    loadDocument(documentPath);
}

function showQueryView() {
    setActiveNavItem('nav-query');
    documentsView.style.display = 'none';
    documentView.style.display = 'none';
    queryView.style.display = 'block';
    validateView.style.display = 'none';
    statsView.style.display = 'none';
}

function showValidateView() {
    setActiveNavItem('nav-validate');
    documentsView.style.display = 'none';
    documentView.style.display = 'none';
    queryView.style.display = 'none';
    validateView.style.display = 'block';
    statsView.style.display = 'none';
}

function showStatsView() {
    setActiveNavItem('nav-stats');
    documentsView.style.display = 'none';
    documentView.style.display = 'none';
    queryView.style.display = 'none';
    validateView.style.display = 'none';
    statsView.style.display = 'block';
    loadStats();
}

function setActiveNavItem(id) {
    document.querySelectorAll('.nav-link').forEach(link => {
        link.classList.remove('active');
    });
    document.getElementById(id).classList.add('active');
}

// API Functions
async function loadDocuments() {
    try {
        documentsList.innerHTML = '<div class="col-12 text-center"><div class="spinner-border" role="status"><span class="visually-hidden">Loading...</span></div></div>';
        
        // Initialize and load the file tree
        if (fileTree) {
            allDocuments = await fileTree.loadFileTree();
        } else {
            const response = await fetch('/api/documents');
            if (!response.ok) {
                throw new Error(`Failed to load documents: ${response.statusText}`);
            }
            
            allDocuments = await response.json();
        }
        
        renderDocumentsList(allDocuments);
        
        // Set up search functionality
        documentSearchBtn.addEventListener('click', () => {
            const searchTerm = documentSearch.value.toLowerCase();
            filterDocuments(allDocuments, searchTerm);
        });
        
        documentSearch.addEventListener('keyup', (e) => {
            if (e.key === 'Enter') {
                const searchTerm = documentSearch.value.toLowerCase();
                filterDocuments(allDocuments, searchTerm);
            }
        });
    } catch (error) {
        documentsList.innerHTML = `<div class="col-12"><div class="alert alert-danger">${error.message}</div></div>`;
        console.error('Error loading documents:', error);
    }
}

async function loadDocument(path) {
    try {
        documentTitle.innerHTML = '<div class="spinner-border spinner-border-sm" role="status"><span class="visually-hidden">Loading...</span></div> Loading...';
        documentMetadata.innerHTML = '';
        documentContent.innerHTML = '';
        
        const response = await fetch(`/api/documents/${encodeURIComponent(path)}`);
        if (!response.ok) {
            throw new Error(`Failed to load document: ${response.statusText}`);
        }
        
        const document = await response.json();
        
        // Set the title (use the title from metadata or the file path)
        documentTitle.textContent = document.Metadata.title || path.split('/').pop();
        
        // Render metadata
        renderDocumentMetadata(document.Metadata);
        
        // Render content with syntax highlighting
        renderDocumentContent(document.Content);
    } catch (error) {
        documentTitle.textContent = 'Error';
        documentMetadata.innerHTML = '';
        documentContent.innerHTML = `<div class="alert alert-danger">${error.message}</div>`;
        console.error('Error loading document:', error);
    }
}

function renderDocumentMetadata(metadata) {
    // Create metadata sections
    const metadataContainer = document.createElement('div');
    
    // Create main metadata section
    const mainMetadata = document.createElement('div');
    mainMetadata.className = 'metadata-main mb-4';
    
    // Create badges for important metadata
    const badgesContainer = document.createElement('div');
    badgesContainer.className = 'metadata-badges mb-3';
    
    // Document type badge
    if (metadata.document_type) {
        const typeBadge = document.createElement('span');
        typeBadge.className = 'badge bg-primary metadata-badge';
        typeBadge.textContent = metadata.document_type;
        badgesContainer.appendChild(typeBadge);
    }
    
    // Status badge
    if (metadata.status) {
        const statusBadge = document.createElement('span');
        statusBadge.className = 'badge bg-info metadata-badge';
        statusBadge.textContent = metadata.status;
        badgesContainer.appendChild(statusBadge);
    }
    
    // Category badge
    if (metadata.category) {
        const categoryBadge = document.createElement('span');
        categoryBadge.className = 'badge bg-secondary metadata-badge';
        categoryBadge.textContent = metadata.category;
        badgesContainer.appendChild(categoryBadge);
    }
    
    // Created/Updated dates
    const datesDiv = document.createElement('div');
    datesDiv.className = 'metadata-dates small text-muted mb-3';
    
    if (metadata.created) {
        const createdDate = new Date(metadata.created);
        const createdSpan = document.createElement('span');
        createdSpan.innerHTML = `<i class="bi bi-calendar-plus"></i> Created: ${createdDate.toLocaleDateString()} `;
        datesDiv.appendChild(createdSpan);
    }
    
    if (metadata.updated) {
        const updatedDate = new Date(metadata.updated);
        const updatedSpan = document.createElement('span');
        updatedSpan.innerHTML = `<i class="bi bi-calendar-check"></i> Updated: ${updatedDate.toLocaleDateString()}`;
        datesDiv.appendChild(updatedSpan);
    }
    
    // Author information
    if (metadata.owner) {
        const authorDiv = document.createElement('div');
        authorDiv.className = 'metadata-author small text-muted mb-3';
        authorDiv.innerHTML = `<i class="bi bi-person"></i> Author: ${escapeHtml(metadata.owner)}`;
        mainMetadata.appendChild(authorDiv);
    }
    
    // Word count
    if (metadata.word_count) {
        const wordCountDiv = document.createElement('div');
        wordCountDiv.className = 'metadata-word-count small text-muted mb-3';
        wordCountDiv.innerHTML = `<i class="bi bi-file-text"></i> Word count: ${metadata.word_count}`;
        mainMetadata.appendChild(wordCountDiv);
    }
    
    // Tags
    if (metadata.tags && metadata.tags.length > 0) {
        const tagsDiv = document.createElement('div');
        tagsDiv.className = 'metadata-tags mb-3';
        
        const tagsLabel = document.createElement('div');
        tagsLabel.className = 'small text-muted mb-1';
        tagsLabel.innerHTML = '<i class="bi bi-tags"></i> Tags:';
        tagsDiv.appendChild(tagsLabel);
        
        const tagsContent = document.createElement('div');
        metadata.tags.forEach(tag => {
            const tagBadge = document.createElement('span');
            tagBadge.className = 'badge bg-secondary metadata-badge';
            tagBadge.textContent = tag;
            tagsContent.appendChild(tagBadge);
        });
        tagsDiv.appendChild(tagsContent);
        mainMetadata.appendChild(tagsDiv);
    }
    
    // Abstract (if exists)
    if (metadata.abstract) {
        const abstractDiv = document.createElement('div');
        abstractDiv.className = 'metadata-abstract mb-3';
        
        const abstractLabel = document.createElement('div');
        abstractLabel.className = 'small text-muted mb-1';
        abstractLabel.innerHTML = '<i class="bi bi-card-text"></i> Abstract:';
        abstractDiv.appendChild(abstractLabel);
        
        const abstractContent = document.createElement('div');
        abstractContent.className = 'card';
        abstractContent.innerHTML = `<div class="card-body">${escapeHtml(metadata.abstract)}</div>`;
        abstractDiv.appendChild(abstractContent);
        mainMetadata.appendChild(abstractDiv);
    }
    
    // Add badges and dates to main metadata
    mainMetadata.prepend(datesDiv);
    mainMetadata.prepend(badgesContainer);
    metadataContainer.appendChild(mainMetadata);
    
    // Add a "View All Metadata" button that shows a modal with all metadata
    const viewAllBtn = document.createElement('button');
    viewAllBtn.className = 'btn btn-sm btn-outline-secondary';
    viewAllBtn.innerHTML = '<i class="bi bi-list-columns"></i> View All Metadata';
    viewAllBtn.setAttribute('data-bs-toggle', 'modal');
    viewAllBtn.setAttribute('data-bs-target', '#metadataModal');
    metadataContainer.appendChild(viewAllBtn);
    
    // Create the metadata modal
    const modalHTML = `
        <div class="modal fade" id="metadataModal" tabindex="-1" aria-labelledby="metadataModalLabel" aria-hidden="true">
            <div class="modal-dialog modal-lg">
                <div class="modal-content">
                    <div class="modal-header">
                        <h5 class="modal-title" id="metadataModalLabel">All Metadata</h5>
                        <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
                    </div>
                    <div class="modal-body">
                        <table class="table metadata-table">
                            <thead>
                                <tr>
                                    <th>Key</th>
                                    <th>Value</th>
                                </tr>
                            </thead>
                            <tbody>
                                ${Object.entries(metadata).map(([key, value]) => `
                                    <tr>
                                        <th scope="row">${escapeHtml(key)}</th>
                                        <td>${renderMetadataValue(value)}</td>
                                    </tr>
                                `).join('')}
                            </tbody>
                        </table>
                    </div>
                </div>
            </div>
        </div>
    `;
    
    const modalContainer = document.createElement('div');
    modalContainer.innerHTML = modalHTML;
    metadataContainer.appendChild(modalContainer);
    
    // Add to the document metadata container
    documentMetadata.appendChild(metadataContainer);
}

function renderDocumentContent(content) {
    // Add syntax highlighting for code blocks
    const renderer = new marked.Renderer();
    
    renderer.code = function(code, language) {
        const validLang = !!(language && hljs.getLanguage(language));
        const highlighted = validLang ? hljs.highlight(code, { language }).value : hljs.highlightAuto(code).value;
        return `<pre><code class="hljs ${language}">${highlighted}</code></pre>`;
    };
    
    marked.setOptions({
        renderer: renderer,
        highlight: function(code, lang) {
            const language = hljs.getLanguage(lang) ? lang : 'plaintext';
            return hljs.highlight(code, { language }).value;
        },
        langPrefix: 'hljs language-',
        gfm: true,
        breaks: false
    });
    
    documentContent.innerHTML = marked.parse(content);
}

async function runQuery() {
    try {
        const query = queryInput.value.trim();
        if (!query) {
            alert('Please enter a query expression');
            return;
        }
        
        const keywords = queryKeywords.value ? queryKeywords.value.split(',').map(k => k.trim()) : [];
        
        runQueryBtn.disabled = true;
        runQueryBtn.innerHTML = '<span class="spinner-border spinner-border-sm" role="status" aria-hidden="true"></span> Running...';
        
        const response = await fetch('/api/query', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({
                query: query,
                keywords: keywords
            })
        });
        
        if (!response.ok) {
            throw new Error(`Query failed: ${response.statusText}`);
        }
        
        const results = await response.json();
        renderQueryResults(results, query, keywords);
    } catch (error) {
        queryResults.style.display = 'block';
        querySummary.className = 'alert alert-danger';
        querySummary.textContent = error.message;
        queryResultsList.innerHTML = '';
        console.error('Error running query:', error);
    } finally {
        runQueryBtn.disabled = false;
        runQueryBtn.textContent = 'Run Query';
    }
}

async function validateDocument() {
    try {
        const content = validateContent.value.trim();
        if (!content) {
            alert('Please enter document content');
            return;
        }
        
        validateBtn.disabled = true;
        validateBtn.innerHTML = '<span class="spinner-border spinner-border-sm" role="status" aria-hidden="true"></span> Validating...';
        
        const response = await fetch('/api/validate', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({
                content: content
            })
        });
        
        if (!response.ok) {
            throw new Error(`Validation request failed: ${response.statusText}`);
        }
        
        const result = await response.json();
        
        validateResult.style.display = 'block';
        if (result.valid) {
            validateResult.className = 'alert alert-success';
            validateResult.textContent = 'Document is valid!';
        } else {
            validateResult.className = 'alert alert-danger';
            validateResult.textContent = `Validation failed: ${result.error}`;
        }
    } catch (error) {
        validateResult.style.display = 'block';
        validateResult.className = 'alert alert-danger';
        validateResult.textContent = error.message;
        console.error('Error validating document:', error);
    } finally {
        validateBtn.disabled = false;
        validateBtn.textContent = 'Validate';
    }
}

async function loadStats() {
    try {
        statsOverview.innerHTML = '<div class="text-center"><div class="spinner-border" role="status"><span class="visually-hidden">Loading...</span></div></div>';
        statsMetadataKeys.innerHTML = '';
        statsDocTypes.innerHTML = '';
        
        const response = await fetch('/api/stats');
        if (!response.ok) {
            throw new Error(`Failed to load stats: ${response.statusText}`);
        }
        
        const stats = await response.json();
        
        // Render overview
        statsOverview.innerHTML = `
            <div class="card bg-light mb-3 stats-card">
                <div class="card-body">
                    <h5 class="card-title">Total Documents</h5>
                    <p class="card-text display-4">${stats.totalDocuments}</p>
                </div>
            </div>
        `;
        
        // Render metadata keys
        const keysArray = Object.entries(stats.metadataKeys).sort((a, b) => b[1] - a[1]);
        const tableHTML = `
            <table class="table table-striped">
                <thead>
                    <tr>
                        <th>Metadata Key</th>
                        <th>Usage Count</th>
                        <th>% of Documents</th>
                    </tr>
                </thead>
                <tbody>
                    ${keysArray.map(([key, count]) => `
                        <tr>
                            <td>${escapeHtml(key)}</td>
                            <td>${count}</td>
                            <td>${Math.round((count / stats.totalDocuments) * 100)}%</td>
                        </tr>
                    `).join('')}
                </tbody>
            </table>
        `;
        statsMetadataKeys.innerHTML = tableHTML;
        
        // Render document types chart
        if (stats.documentTypes && Object.keys(stats.documentTypes).length > 0) {
            const docTypesArray = Object.entries(stats.documentTypes);
            
            // Create a canvas for the chart
            statsDocTypes.innerHTML = '<canvas id="docTypesChart"></canvas>';
            
            // Get canvas context
            const ctx = document.getElementById('docTypesChart').getContext('2d');
            
            // Prepare data for the chart
            const labels = docTypesArray.map(item => item[0]);
            const data = docTypesArray.map(item => item[1]);
            const colors = generateColors(labels.length);
            
            // Create the chart
            new Chart(ctx, {
                type: 'pie',
                data: {
                    labels: labels,
                    datasets: [{
                        data: data,
                        backgroundColor: colors,
                        borderWidth: 1
                    }]
                },
                options: {
                    responsive: true,
                    plugins: {
                        legend: {
                            position: 'right',
                        },
                        title: {
                            display: true,
                            text: 'Documents by Type'
                        }
                    }
                }
            });
        } else {
            statsDocTypes.innerHTML = '<div class="alert alert-info">No document type data available</div>';
        }
    } catch (error) {
        statsOverview.innerHTML = `<div class="alert alert-danger">${error.message}</div>`;
        console.error('Error loading stats:', error);
    }
}

// UI Rendering Functions
function renderDocumentsList(documents) {
    if (documents.length === 0) {
        documentsList.innerHTML = '<div class="col-12"><div class="alert alert-info">No documents found</div></div>';
        return;
    }
    
    documentsList.innerHTML = documents.map(doc => {
        const path = doc.path;
        const metadata = doc.metadata;
        
        // Get title from metadata or use the filename
        const title = metadata.title || path.split('/').pop().replace('.md', '');
        
        // Format date if it exists
        const dateStr = metadata.created || metadata.updated;
        const date = dateStr ? new Date(dateStr).toLocaleDateString() : '';
        
        // Get author
        const author = metadata.owner || metadata.author || '';
        
        // Get tags
        const tags = Array.isArray(metadata.tags) ? metadata.tags : 
                    (typeof metadata.tags === 'string' ? [metadata.tags] : []);
        
        // Get document type
        const docType = metadata.document_type || '';
        
        // Generate badge for document type
        const docTypeBadge = docType ? 
            `<span class="badge bg-primary">${escapeHtml(docType)}</span>` : '';
        
        // Generate badge for status
        const statusBadge = metadata.status ? 
            `<span class="badge bg-info">${escapeHtml(metadata.status)}</span>` : '';
        
        return `
            <div class="col-md-4 mb-4">
                <div class="card h-100 document-card" onclick="showDocumentView('${path}')">
                    <div class="card-header d-flex gap-2">
                        ${docTypeBadge}
                        ${statusBadge}
                    </div>
                    <div class="card-body">
                        <h5 class="card-title">${escapeHtml(title)}</h5>
                        <h6 class="card-subtitle mb-2 text-muted small">${escapeHtml(path)}</h6>
                        ${author ? `<p class="card-text small"><i class="bi bi-person"></i> ${escapeHtml(author)}</p>` : ''}
                        ${date ? `<p class="card-text small"><i class="bi bi-calendar"></i> ${date}</p>` : ''}
                        <div class="mt-2">
                            ${tags.map(tag => `<span class="badge bg-secondary metadata-badge">${escapeHtml(tag)}</span>`).join(' ')}
                        </div>
                    </div>
                </div>
            </div>
        `;
    }).join('');
}

function filterDocuments(documents, searchTerm) {
    if (!searchTerm) {
        renderDocumentsList(documents);
        return;
    }
    
    const filtered = documents.filter(doc => {
        const path = doc.path.toLowerCase();
        const metadata = doc.metadata;
        
        // Check path
        if (path.includes(searchTerm)) {
            return true;
        }
        
        // Check metadata values
        for (const [key, value] of Object.entries(metadata)) {
            if (typeof value === 'string' && value.toLowerCase().includes(searchTerm)) {
                return true;
            }
            if (Array.isArray(value)) {
                for (const item of value) {
                    if (typeof item === 'string' && item.toLowerCase().includes(searchTerm)) {
                        return true;
                    }
                }
            }
        }
        
        return false;
    });
    
    renderDocumentsList(filtered);
}

function renderQueryResults(results, query, keywords) {
    queryResults.style.display = 'block';
    
    if (results.length === 0) {
        querySummary.className = 'alert alert-info';
        querySummary.textContent = 'No documents match your query.';
        queryResultsList.innerHTML = '';
        return;
    }
    
    querySummary.className = 'alert alert-success';
    querySummary.textContent = `Found ${results.length} document(s) matching your query: "${query}"`;
    
    queryResultsList.innerHTML = results.map(doc => {
        const metadata = doc.Metadata;
        const path = metadata._path || '';
        
        // Get title from metadata or use the filename
        const title = metadata.title || path.split('/').pop().replace('.md', '');
        
        // Format date if it exists
        const dateStr = metadata.created || metadata.updated;
        const date = dateStr ? new Date(dateStr).toLocaleDateString() : '';
        
        // Get author
        const author = metadata.owner || metadata.author || '';
        
        // Get tags
        const tags = Array.isArray(metadata.tags) ? metadata.tags : 
                    (typeof metadata.tags === 'string' ? [metadata.tags] : []);
                    
        // Get document type
        const docType = metadata.document_type || '';
        
        // Generate badge for document type
        const docTypeBadge = docType ? 
            `<span class="badge bg-primary">${escapeHtml(docType)}</span>` : '';
        
        // Generate badge for status
        const statusBadge = metadata.status ? 
            `<span class="badge bg-info">${escapeHtml(metadata.status)}</span>` : '';
        
        // Generate a snippet of content
        const snippet = doc.Content.substring(0, 200) + (doc.Content.length > 200 ? '...' : '');
        
        return `
            <div class="col-md-6 mb-4">
                <div class="card h-100 document-card" onclick="showDocumentView('${path}')">
                    <div class="card-header d-flex gap-2">
                        ${docTypeBadge}
                        ${statusBadge}
                    </div>
                    <div class="card-body">
                        <h5 class="card-title">${escapeHtml(title)}</h5>
                        <h6 class="card-subtitle mb-2 text-muted small">${escapeHtml(path)}</h6>
                        ${author ? `<p class="card-text small"><i class="bi bi-person"></i> ${escapeHtml(author)}</p>` : ''}
                        ${date ? `<p class="card-text small"><i class="bi bi-calendar"></i> ${date}</p>` : ''}
                        <div class="mb-2">
                            ${tags.map(tag => `<span class="badge bg-secondary metadata-badge">${escapeHtml(tag)}</span>`).join(' ')}
                        </div>
                        <div class="mt-3 small text-muted">
                            <p>${escapeHtml(snippet)}</p>
                        </div>
                    </div>
                </div>
            </div>
        `;
    }).join('');
}

function renderMetadataValue(value) {
    if (value === null || value === undefined) {
        return '<em>null</em>';
    }
    
    if (Array.isArray(value)) {
        if (value.length === 0) {
            return '<em>empty array</em>';
        }
        return value.map(item => `<span class="badge bg-secondary metadata-badge">${escapeHtml(String(item))}</span>`).join(' ');
    }
    
    if (typeof value === 'object') {
        return `<pre>${escapeHtml(JSON.stringify(value, null, 2))}</pre>`;
    }
    
    return escapeHtml(String(value));
}

// Utility Functions
function escapeHtml(unsafe) {
    return unsafe
        .replace(/&/g, "&amp;")
        .replace(/</g, "&lt;")
        .replace(/>/g, "&gt;")
        .replace(/"/g, "&quot;")
        .replace(/'/g, "&#039;");
}

function generateColors(count) {
    const colors = [];
    const baseColors = [
        '#4285F4', '#34A853', '#FBBC05', '#EA4335', 
        '#673AB7', '#3F51B5', '#2196F3', '#03A9F4', 
        '#00BCD4', '#009688', '#4CAF50', '#8BC34A', 
        '#CDDC39', '#FFC107', '#FF9800', '#FF5722'
    ];
    
    for (let i = 0; i < count; i++) {
        colors.push(baseColors[i % baseColors.length]);
    }
    
    return colors;
}

// Event Listeners
runQueryBtn.addEventListener('click', runQuery);
validateBtn.addEventListener('click', validateDocument);