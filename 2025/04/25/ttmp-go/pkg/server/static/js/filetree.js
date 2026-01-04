// File tree visualization component
class FileTreeView {
    constructor(containerId, onFileSelect) {
        this.container = document.getElementById(containerId);
        this.onFileSelect = onFileSelect;
        this.treeData = {};
        this.currentPath = '';
    }

    // Load file tree data from API
    async loadFileTree() {
        try {
            const response = await fetch('/api/documents');
            if (!response.ok) {
                throw new Error(`Failed to load documents: ${response.statusText}`);
            }
            
            const documents = await response.json();
            this.buildTreeData(documents);
            this.render();
            return documents;
        } catch (error) {
            this.container.innerHTML = `<div class="alert alert-danger">${error.message}</div>`;
            console.error('Error loading file tree:', error);
            return [];
        }
    }

    // Build tree data structure from flat list of documents
    buildTreeData(documents) {
        this.treeData = { name: 'root', children: [], type: 'directory' };
        
        documents.forEach(doc => {
            const path = doc.path;
            const parts = path.split('/');
            let currentNode = this.treeData;
            
            // Create folder structure
            for (let i = 0; i < parts.length - 1; i++) {
                const part = parts[i];
                if (part === '') continue;
                
                let found = false;
                for (const child of currentNode.children) {
                    if (child.name === part && child.type === 'directory') {
                        currentNode = child;
                        found = true;
                        break;
                    }
                }
                
                if (!found) {
                    const newNode = { name: part, children: [], type: 'directory' };
                    currentNode.children.push(newNode);
                    currentNode = newNode;
                }
            }
            
            // Add file node
            const fileName = parts[parts.length - 1];
            currentNode.children.push({
                name: fileName,
                path: path,
                type: 'file',
                metadata: doc.metadata
            });
        });
        
        // Sort each level - directories first, then files, alphabetically
        this.sortTreeNodes(this.treeData);
    }
    
    // Sort tree nodes recursively
    sortTreeNodes(node) {
        if (node.children && node.children.length > 0) {
            // Sort children
            node.children.sort((a, b) => {
                // Directories first
                if (a.type !== b.type) {
                    return a.type === 'directory' ? -1 : 1;
                }
                // Alphabetical within same type
                return a.name.localeCompare(b.name);
            });
            
            // Sort recursively
            node.children.forEach(child => {
                if (child.type === 'directory') {
                    this.sortTreeNodes(child);
                }
            });
        }
    }

    // Render the file tree
    render() {
        this.container.innerHTML = '';
        const treeRoot = document.createElement('div');
        treeRoot.className = 'file-tree';
        
        this.renderNode(this.treeData, treeRoot, 0);
        this.container.appendChild(treeRoot);
        
        // Add event listeners for toggle buttons
        this.container.querySelectorAll('.directory-toggle').forEach(toggle => {
            toggle.addEventListener('click', (e) => {
                e.stopPropagation();
                const directoryItem = toggle.closest('.directory-item');
                const collapsed = directoryItem.classList.toggle('collapsed');
                
                // Change icon based on collapsed state
                const icon = toggle.querySelector('i');
                icon.className = collapsed ? 'bi bi-chevron-right' : 'bi bi-chevron-down';
                
                // Toggle visibility of child container
                const childContainer = directoryItem.querySelector('.directory-children');
                if (childContainer) {
                    childContainer.style.display = collapsed ? 'none' : 'block';
                }
            });
        });
    }

    // Render a single node and its children
    renderNode(node, parentElement, level) {
        if (node.type === 'directory') {
            // Skip rendering the root node itself
            if (node.name !== 'root') {
                const directoryItem = document.createElement('div');
                directoryItem.className = 'directory-item';
                
                const directoryHeader = document.createElement('div');
                directoryHeader.className = 'directory-header';
                directoryHeader.style.paddingLeft = `${level * 12}px`;
                
                const toggle = document.createElement('button');
                toggle.className = 'directory-toggle btn btn-sm';
                toggle.innerHTML = '<i class="bi bi-chevron-down"></i>';
                
                const directoryName = document.createElement('span');
                directoryName.className = 'directory-name';
                directoryName.innerHTML = `<i class="bi bi-folder"></i> ${node.name}`;
                
                directoryHeader.appendChild(toggle);
                directoryHeader.appendChild(directoryName);
                directoryItem.appendChild(directoryHeader);
                
                const childContainer = document.createElement('div');
                childContainer.className = 'directory-children';
                directoryItem.appendChild(childContainer);
                
                parentElement.appendChild(directoryItem);
                
                // Render children inside the child container
                if (node.children && node.children.length > 0) {
                    node.children.forEach(child => {
                        this.renderNode(child, childContainer, level + 1);
                    });
                }
            } else {
                // For root, just render its children directly
                if (node.children && node.children.length > 0) {
                    node.children.forEach(child => {
                        this.renderNode(child, parentElement, level);
                    });
                }
            }
        } else if (node.type === 'file') {
            const fileItem = document.createElement('div');
            fileItem.className = 'file-item';
            fileItem.style.paddingLeft = `${(level * 12) + 20}px`; // Extra padding to align with folders
            
            const fileName = document.createElement('span');
            fileName.className = 'file-name';
            fileName.innerHTML = `<i class="bi bi-file-text"></i> ${node.name}`;
            
            fileItem.appendChild(fileName);
            parentElement.appendChild(fileItem);
            
            // Add click handler
            fileItem.addEventListener('click', () => {
                // Highlight current file
                this.container.querySelectorAll('.file-item').forEach(item => {
                    item.classList.remove('selected');
                });
                fileItem.classList.add('selected');
                
                // Call the file select callback
                if (this.onFileSelect && typeof this.onFileSelect === 'function') {
                    this.onFileSelect(node.path, node.metadata);
                }
            });
        }
    }
}