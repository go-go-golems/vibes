/**
 * Go File Analyzer Web Interface
 * Main JavaScript functionality
 */

// Global variables for sharing with visualization
window.analysisData = null;
window.currentFile = null; 
window.currentFunction = null;

document.addEventListener('DOMContentLoaded', function() {
    // DOM Elements
    const projectPathInput = document.getElementById('project-path');
    const analyzeBtn = document.getElementById('analyze-btn');
    const resultsContainer = document.getElementById('results-container');
    const projectName = document.getElementById('project-name');
    const fileCount = document.getElementById('file-count');
    const functionCount = document.getElementById('function-count');
    const methodCount = document.getElementById('method-count');
    const packageList = document.getElementById('package-list');
    const currentFilePath = document.getElementById('current-file-path');
    const currentPackage = document.getElementById('current-package');
    const functionList = document.getElementById('function-list');
    const codeDisplay = document.getElementById('code-display');
    const loadingOverlay = document.getElementById('loading-overlay');
    
    // Event Listeners
    analyzeBtn.addEventListener('click', analyzeProject);
    
    // Check for URL parameters to automatically analyze a project
    const urlParams = new URLSearchParams(window.location.search);
    const autoPath = urlParams.get('path');
    if (autoPath) {
        projectPathInput.value = autoPath;
        // Use setTimeout to ensure the DOM is fully loaded
        setTimeout(analyzeProject, 100);
    }
    
    /**
     * Analyze a Go project
     */
    function analyzeProject() {
        const path = projectPathInput.value.trim();
        
        if (!path) {
            showError('Please enter a valid path');
            return;
        }
        
        showLoading('Analyzing project...');
        
        fetch('/api/analyze', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({ path })
        })
        .then(response => {
            if (!response.ok) {
                return response.text().then(text => {
                    throw new Error(text);
                });
            }
            return response.json();
        })
        .then(data => {
            // Store analysis data globally
            window.analysisData = data;
            
            displayResults();
            hideLoading();
            
            // Update URL with the path for sharing
            const url = new URL(window.location);
            url.searchParams.set('path', path);
            window.history.pushState({}, '', url);
        })
        .catch(error => {
            hideLoading();
            showError('Error analyzing project: ' + error.message);
        });
    }
    
    /**
     * Display analysis results
     */
    function displayResults() {
        // Show results container
        resultsContainer.style.display = 'flex';
        
        // Update summary
        if (window.analysisData.metadata) {
            // Enhanced format
            projectName.textContent = window.analysisData.metadata.name;
            fileCount.textContent = window.analysisData.metadata.numFiles;
            functionCount.textContent = window.analysisData.metadata.numFunctions - window.analysisData.metadata.numMethods;
            methodCount.textContent = window.analysisData.metadata.numMethods;
        } else {
            // Basic format
            projectName.textContent = window.analysisData.stats ? 'Go Project' : 'Unknown';
            fileCount.textContent = window.analysisData.stats ? window.analysisData.stats.fileCount : window.analysisData.files.length;
            functionCount.textContent = window.analysisData.stats ? (window.analysisData.stats.functionCount - window.analysisData.stats.methodCount) : 'Unknown';
            methodCount.textContent = window.analysisData.stats ? window.analysisData.stats.methodCount : 'Unknown';
        }
        
        // Display packages
        displayPackages();
        
        // Select the first file if available
        if (window.analysisData.files && window.analysisData.files.length > 0) {
            selectFile(window.analysisData.files[0]);
        }
    }
    
    /**
     * Display packages and files
     */
    function displayPackages() {
        packageList.innerHTML = '';
        
        // Group files by package
        const packageMap = {};
        
        window.analysisData.files.forEach(file => {
            if (!packageMap[file.package]) {
                packageMap[file.package] = [];
            }
            packageMap[file.package].push(file);
        });
        
        // Sort packages by name
        const packageNames = Object.keys(packageMap).sort();
        
        // Create package elements
        packageNames.forEach(packageName => {
            const packageDiv = document.createElement('div');
            packageDiv.className = 'package-item';
            
            const packageHeader = document.createElement('div');
            packageHeader.className = 'package-header';
            packageHeader.textContent = packageName;
            packageHeader.addEventListener('click', function() {
                const filesDiv = this.nextElementSibling;
                filesDiv.style.display = filesDiv.style.display === 'none' ? 'block' : 'none';
            });
            
            const filesDiv = document.createElement('div');
            filesDiv.className = 'files-list';
            
            // Sort files by path
            const files = packageMap[packageName].sort((a, b) => a.path.localeCompare(b.path));
            
            // Create file elements
            files.forEach(file => {
                const fileDiv = document.createElement('div');
                fileDiv.className = 'file-item';
                fileDiv.textContent = file.path.split('/').pop(); // Just the filename
                fileDiv.title = file.path;
                fileDiv.addEventListener('click', function() {
                    selectFile(file);
                });
                
                filesDiv.appendChild(fileDiv);
            });
            
            packageDiv.appendChild(packageHeader);
            packageDiv.appendChild(filesDiv);
            packageList.appendChild(packageDiv);
        });
    }
    
    /**
     * Select a file to display
     */
    window.selectFile = function(file) {
        window.currentFile = file;
        
        // Update UI
        currentFilePath.textContent = file.path;
        currentPackage.textContent = file.package;
        
        // Display functions
        displayFunctions(file);
        
        // Load file content
        loadFileContent(file.path);
        
        // Highlight all file elements
        document.querySelectorAll('.file-item').forEach(el => {
            el.classList.remove('active');
            if (el.title === file.path) {
                el.classList.add('active');
            }
        });
    };
    
    /**
     * Display functions for a file
     */
    function displayFunctions(file) {
        functionList.innerHTML = '';
        
        if (!file.functions || file.functions.length === 0) {
            functionList.innerHTML = '<div class="p-3 text-muted">No functions found in this file</div>';
            return;
        }
        
        // Sort functions by line number
        const functions = [...file.functions].sort((a, b) => a.startLine - b.startLine);
        
        functions.forEach(func => {
            const functionDiv = document.createElement('div');
            functionDiv.className = 'function-item';
            functionDiv.classList.toggle('exported', func.isExported);
            functionDiv.classList.toggle('method', func.isMethod);
            
            let displayName = func.name;
            if (func.isMethod) {
                // Try to extract the receiver type from the function ID if available
                if (func.id && func.id.includes('.') && func.id !== `${func.package}.${func.name}`) {
                    const parts = func.id.split('.');
                    const receiverType = parts[parts.length - 2];
                    displayName = `(${receiverType}) ${func.name}`;
                } else {
                    displayName = `(receiver) ${func.name}`;
                }
            }
            
            functionDiv.textContent = displayName;
            functionDiv.title = `Line ${func.startLine}-${func.endLine} ${func.isMethod ? '(method)' : '(function)'}`;
            
            functionDiv.addEventListener('click', function() {
                // Save current function
                window.currentFunction = func;
                
                // Highlight this function in code
                highlightFunction(func);
                
                // Mark this function as selected
                document.querySelectorAll('.function-item').forEach(el => el.classList.remove('selected'));
                this.classList.add('selected');
            });
            
            functionList.appendChild(functionDiv);
        });
    }
    
    /**
     * Load file content from server
     */
    function loadFileContent(path) {
        showLoading('Loading file...');
        
        fetch(`/api/file?path=${encodeURIComponent(path)}`)
            .then(response => {
                if (!response.ok) {
                    return response.text().then(text => {
                        throw new Error(text);
                    });
                }
                return response.text();
            })
            .then(content => {
                // Display code with line numbers
                displayCode(content);
                hideLoading();
                
                // If a function was selected previously, try to highlight it again
                if (window.currentFunction && window.currentFunction.filepath === window.currentFile.path) {
                    highlightFunction(window.currentFunction);
                }
            })
            .catch(error => {
                hideLoading();
                codeDisplay.innerHTML = `<div class="alert alert-danger">Error loading file: ${error.message}</div>`;
            });
    }
    
    /**
     * Display code with syntax highlighting and line numbers
     */
    function displayCode(content) {
        // Highlight the code
        codeDisplay.textContent = content;
        hljs.highlightElement(codeDisplay);
        
        // Add line numbers and make lines targetable
        const lines = codeDisplay.innerHTML.split('\n');
        const numberedLines = lines.map((line, index) => {
            return `<div class="code-line" id="line-${index + 1}" data-line="${index + 1}">
                <span class="line-numbers">${index + 1}</span> ${line || '&nbsp;'}
            </div>`;
        });
        
        codeDisplay.innerHTML = numberedLines.join('');
    }
    
    /**
     * Highlight function in code display
     */
    window.highlightFunction = function(func) {
        // Clear previous highlights
        document.querySelectorAll('.code-line.highlighted').forEach(el => {
            el.classList.remove('highlighted');
        });
        
        // Highlight the function lines
        for (let i = func.startLine; i <= func.endLine; i++) {
            const lineEl = document.getElementById(`line-${i}`);
            if (lineEl) {
                lineEl.classList.add('highlighted');
            }
        }
        
        // Scroll to the function
        const firstLine = document.getElementById(`line-${func.startLine}`);
        if (firstLine) {
            firstLine.scrollIntoView({ behavior: 'smooth', block: 'center' });
        }
    };
    
    /**
     * Show loading overlay
     */
    function showLoading(message = 'Loading...') {
        loadingOverlay.querySelector('.visually-hidden').textContent = message;
        loadingOverlay.style.display = 'flex';
    }
    
    /**
     * Hide loading overlay
     */
    function hideLoading() {
        loadingOverlay.style.display = 'none';
    }
    
    /**
     * Show error message
     */
    function showError(message) {
        // Create a toast notification
        const toastContainer = document.createElement('div');
        toastContainer.className = 'toast-container position-fixed bottom-0 end-0 p-3';
        toastContainer.style.zIndex = '1050';
        
        const toast = document.createElement('div');
        toast.className = 'toast show';
        toast.innerHTML = `
            <div class="toast-header">
                <strong class="me-auto">Error</strong>
                <button type="button" class="btn-close" data-bs-dismiss="toast" aria-label="Close"></button>
            </div>
            <div class="toast-body">${message}</div>
        `;
        
        toastContainer.appendChild(toast);
        document.body.appendChild(toastContainer);
        
        // Remove after 5 seconds
        setTimeout(() => {
            toastContainer.remove();
        }, 5000);
        
        // Close button functionality
        toast.querySelector('.btn-close').addEventListener('click', () => {
            toastContainer.remove();
        });
    }
});