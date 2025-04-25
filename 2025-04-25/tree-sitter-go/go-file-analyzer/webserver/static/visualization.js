/**
 * Go File Analyzer - Visualization Module
 * 
 * This module provides visualization capabilities for the Go File Analyzer,
 * including package dependency graphs, function call trees, and heatmaps.
 */

// Visualization namespace
const Visualization = (function() {
    // Private variables
    let svg = null;
    let width = 800;
    let height = 600;
    let simulation = null;
    let analysisData = null;
    
    // D3.js visualization configuration
    const color = d3.scaleOrdinal(d3.schemeCategory10);
    
    // Public methods
    return {
        /**
         * Initialize the visualization module
         * @param {string} containerId - ID of the container element
         * @param {object} data - Analysis data
         */
        initialize: function(containerId, data) {
            analysisData = data;
            
            // Get container dimensions
            const container = document.getElementById(containerId);
            width = container.clientWidth;
            height = container.clientHeight;
            
            // Create SVG element
            svg = d3.select(`#${containerId}`)
                .append("svg")
                .attr("width", width)
                .attr("height", height)
                .attr("class", "visualization-svg");
                
            // Add zoom behavior
            svg.call(d3.zoom().on("zoom", function(event) {
                svg.select("g").attr("transform", event.transform);
            }));
            
            // Create a group for all visualization elements
            svg.append("g")
                .attr("class", "viz-group");
                
            return this;
        },
        
        /**
         * Create a package dependency graph
         */
        createPackageDependencyGraph: function() {
            if (!analysisData || !svg) return;
            
            // Clear previous visualization
            svg.select(".viz-group").selectAll("*").remove();
            
            // Extract packages
            const packages = analysisData.packages || [];
            
            // Create nodes for packages
            const nodes = packages.map(pkg => ({
                id: pkg.name,
                name: pkg.name,
                type: "package",
                files: pkg.files ? pkg.files.length : 0,
                functions: pkg.functions ? pkg.functions.length : 0
            }));
            
            // Create links between packages (dummy data for now)
            // In a real implementation, we would analyze imports between packages
            const links = [];
            for (let i = 0; i < nodes.length; i++) {
                for (let j = i + 1; j < nodes.length; j++) {
                    if (Math.random() > 0.7) { // Create random links for demonstration
                        links.push({
                            source: nodes[i].id,
                            target: nodes[j].id,
                            value: Math.floor(Math.random() * 5) + 1
                        });
                    }
                }
            }
            
            // Create force simulation
            simulation = d3.forceSimulation(nodes)
                .force("link", d3.forceLink(links).id(d => d.id).distance(100))
                .force("charge", d3.forceManyBody().strength(-300))
                .force("center", d3.forceCenter(width / 2, height / 2))
                .force("x", d3.forceX(width / 2).strength(0.1))
                .force("y", d3.forceY(height / 2).strength(0.1));
                
            // Create links
            const link = svg.select(".viz-group")
                .append("g")
                .attr("class", "links")
                .selectAll("line")
                .data(links)
                .enter()
                .append("line")
                .attr("stroke-width", d => Math.sqrt(d.value))
                .attr("stroke", "#999")
                .attr("stroke-opacity", 0.6);
                
            // Create nodes
            const node = svg.select(".viz-group")
                .append("g")
                .attr("class", "nodes")
                .selectAll("g")
                .data(nodes)
                .enter()
                .append("g")
                .call(d3.drag()
                    .on("start", dragstarted)
                    .on("drag", dragged)
                    .on("end", dragended));
                    
            // Add circles to nodes
            node.append("circle")
                .attr("r", d => 10 + Math.sqrt(d.files + d.functions))
                .attr("fill", d => color(d.name))
                .append("title")
                .text(d => `${d.name}\nFiles: ${d.files}\nFunctions: ${d.functions}`);
                
            // Add labels to nodes
            node.append("text")
                .attr("dy", 4)
                .attr("text-anchor", "middle")
                .text(d => d.name.slice(0, 15))
                .style("font-size", "10px")
                .style("fill", "#fff");
                
            // Update positions on tick
            simulation.on("tick", () => {
                link
                    .attr("x1", d => d.source.x)
                    .attr("y1", d => d.source.y)
                    .attr("x2", d => d.target.x)
                    .attr("y2", d => d.target.y);
                    
                node
                    .attr("transform", d => `translate(${d.x},${d.y})`);
            });
            
            // Drag functions
            function dragstarted(event, d) {
                if (!event.active) simulation.alphaTarget(0.3).restart();
                d.fx = d.x;
                d.fy = d.y;
            }
            
            function dragged(event, d) {
                d.fx = event.x;
                d.fy = event.y;
            }
            
            function dragended(event, d) {
                if (!event.active) simulation.alphaTarget(0);
                d.fx = null;
                d.fy = null;
            }
        },
        
        /**
         * Create a function dependency graph for a specific file
         * @param {string} filePath - Path of the file to visualize
         */
        createFunctionDependencyGraph: function(filePath) {
            if (!analysisData || !svg) return;
            
            // Clear previous visualization
            svg.select(".viz-group").selectAll("*").remove();
            
            // Find the file
            const file = analysisData.files.find(f => f.path === filePath);
            if (!file) return;
            
            // Create nodes for functions
            const nodes = file.functions.map(func => ({
                id: `${file.package}.${func.name}`,
                name: func.name,
                type: "function",
                isMethod: func.isMethod,
                isExported: func.isExported,
                startLine: func.startLine,
                endLine: func.endLine
            }));
            
            // Create links between functions (dummy data for demo)
            // In a real implementation, we would analyze function calls
            const links = [];
            for (let i = 0; i < nodes.length; i++) {
                for (let j = 0; j < nodes.length; j++) {
                    if (i !== j && Math.random() > 0.8) {
                        links.push({
                            source: nodes[i].id,
                            target: nodes[j].id,
                            value: 1
                        });
                    }
                }
            }
            
            // Create force simulation
            simulation = d3.forceSimulation(nodes)
                .force("link", d3.forceLink(links).id(d => d.id).distance(100))
                .force("charge", d3.forceManyBody().strength(-200))
                .force("center", d3.forceCenter(width / 2, height / 2));
                
            // Create links
            const link = svg.select(".viz-group")
                .append("g")
                .attr("class", "links")
                .selectAll("line")
                .data(links)
                .enter()
                .append("line")
                .attr("stroke-width", 1)
                .attr("stroke", "#999")
                .attr("stroke-opacity", 0.6)
                .attr("marker-end", "url(#arrowhead)");
                
            // Add arrowhead marker
            svg.select(".viz-group")
                .append("defs")
                .append("marker")
                .attr("id", "arrowhead")
                .attr("viewBox", "0 -5 10 10")
                .attr("refX", 20)
                .attr("refY", 0)
                .attr("markerWidth", 6)
                .attr("markerHeight", 6)
                .attr("orient", "auto")
                .append("path")
                .attr("d", "M0,-5L10,0L0,5")
                .attr("fill", "#999");
                
            // Create nodes
            const node = svg.select(".viz-group")
                .append("g")
                .attr("class", "nodes")
                .selectAll("g")
                .data(nodes)
                .enter()
                .append("g")
                .call(d3.drag()
                    .on("start", dragstarted)
                    .on("drag", dragged)
                    .on("end", dragended))
                .on("click", function(event, d) {
                    // Highlight the selected function in the code view
                    highlightFunction({
                        name: d.name,
                        startLine: d.startLine,
                        endLine: d.endLine,
                        isMethod: d.isMethod,
                        isExported: d.isExported
                    });
                });
                
            // Add circles to nodes
            node.append("circle")
                .attr("r", d => d.isExported ? 10 : 6)
                .attr("fill", d => d.isMethod ? "#ff9800" : "#2196f3")
                .append("title")
                .text(d => `${d.name} (${d.isMethod ? "Method" : "Function"})\nLines: ${d.startLine}-${d.endLine}\n${d.isExported ? "Exported" : "Not exported"}`);
                
            // Add labels to nodes
            node.append("text")
                .attr("dy", 20)
                .attr("text-anchor", "middle")
                .text(d => d.name)
                .style("font-size", "10px");
                
            // Update positions on tick
            simulation.on("tick", () => {
                link
                    .attr("x1", d => d.source.x)
                    .attr("y1", d => d.source.y)
                    .attr("x2", d => d.target.x)
                    .attr("y2", d => d.target.y);
                    
                node
                    .attr("transform", d => `translate(${d.x},${d.y})`);
            });
            
            // Drag functions
            function dragstarted(event, d) {
                if (!event.active) simulation.alphaTarget(0.3).restart();
                d.fx = d.x;
                d.fy = d.y;
            }
            
            function dragged(event, d) {
                d.fx = event.x;
                d.fy = event.y;
            }
            
            function dragended(event, d) {
                if (!event.active) simulation.alphaTarget(0);
                d.fx = null;
                d.fy = null;
            }
        },
        
        /**
         * Create a heatmap of function complexity
         */
        createComplexityHeatmap: function() {
            if (!analysisData || !svg) return;
            
            // Clear previous visualization
            svg.select(".viz-group").selectAll("*").remove();
            
            // Extract functions from all files
            const allFunctions = analysisData.functions || [];
            
            // Calculate "complexity" based on number of lines
            // This is a simplified metric for demo purposes
            const functions = allFunctions.map(func => ({
                name: func.name,
                package: func.package,
                complexity: func.endLine - func.startLine,
                isMethod: func.isMethod,
                isExported: func.isExported
            }));
            
            // Sort by complexity
            functions.sort((a, b) => b.complexity - a.complexity);
            
            // Take the top 50 functions
            const topFunctions = functions.slice(0, 50);
            
            // Create a color scale for complexity
            const colorScale = d3.scaleSequential(d3.interpolateYlOrRd)
                .domain([0, d3.max(topFunctions, d => d.complexity)]);
                
            // Create a cell size based on available space
            const cellSize = Math.min(width, height) / Math.ceil(Math.sqrt(topFunctions.length));
            const cols = Math.floor(width / cellSize);
            
            // Create cells
            const cells = svg.select(".viz-group")
                .selectAll("g")
                .data(topFunctions)
                .enter()
                .append("g")
                .attr("transform", (d, i) => `translate(${(i % cols) * cellSize}, ${Math.floor(i / cols) * cellSize})`)
                .on("click", function(event, d) {
                    // Find the complete function info and highlight it
                    const func = allFunctions.find(f => f.name === d.name && f.package === d.package);
                    if (func) {
                        // Trigger function selection event
                        const event = new CustomEvent("functionSelected", {
                            detail: {
                                function: func,
                                filepath: func.filepath
                            }
                        });
                        document.dispatchEvent(event);
                    }
                });
                
            // Add rectangles to cells
            cells.append("rect")
                .attr("width", cellSize - 2)
                .attr("height", cellSize - 2)
                .attr("fill", d => colorScale(d.complexity))
                .append("title")
                .text(d => `${d.name} (${d.package})\nComplexity: ${d.complexity} lines\n${d.isMethod ? "Method" : "Function"}\n${d.isExported ? "Exported" : "Not exported"}`);
                
            // Add text to cells
            cells.append("text")
                .attr("x", cellSize / 2)
                .attr("y", cellSize / 2)
                .attr("text-anchor", "middle")
                .attr("dominant-baseline", "middle")
                .style("font-size", "10px")
                .style("fill", d => d.complexity > d3.max(topFunctions, d => d.complexity) / 2 ? "#fff" : "#000")
                .text(d => d.name.length > 8 ? d.name.substring(0, 8) + "..." : d.name);
                
            // Add a legend
            const legendWidth = 200;
            const legendHeight = 20;
            
            const legend = svg.select(".viz-group")
                .append("g")
                .attr("class", "legend")
                .attr("transform", `translate(${width - legendWidth - 20}, ${height - 50})`);
                
            // Create a linear gradient for the legend
            const defs = svg.select(".viz-group").append("defs");
            
            const linearGradient = defs.append("linearGradient")
                .attr("id", "complexity-gradient")
                .attr("x1", "0%")
                .attr("y1", "0%")
                .attr("x2", "100%")
                .attr("y2", "0%");
                
            // Add color stops
            linearGradient.selectAll("stop")
                .data(d3.range(0, 1.01, 0.1))
                .enter()
                .append("stop")
                .attr("offset", d => `${d * 100}%`)
                .attr("stop-color", d => colorScale(d * d3.max(topFunctions, d => d.complexity)));
                
            // Add a rectangle with the gradient
            legend.append("rect")
                .attr("width", legendWidth)
                .attr("height", legendHeight)
                .style("fill", "url(#complexity-gradient)");
                
            // Add legend axis
            const legendScale = d3.scaleLinear()
                .domain([0, d3.max(topFunctions, d => d.complexity)])
                .range([0, legendWidth]);
                
            const legendAxis = d3.axisBottom(legendScale)
                .ticks(5);
                
            legend.append("g")
                .attr("transform", `translate(0, ${legendHeight})`)
                .call(legendAxis);
                
            // Add legend title
            legend.append("text")
                .attr("x", 0)
                .attr("y", -5)
                .style("font-size", "12px")
                .text("Function Complexity (Lines of Code)");
        },
        
        /**
         * Create a package tree visualization
         */
        createPackageTree: function() {
            if (!analysisData || !svg) return;
            
            // Clear previous visualization
            svg.select(".viz-group").selectAll("*").remove();
            
            // Convert packages to tree structure
            const root = {
                name: "root",
                children: []
            };
            
            // Group packages by path segments
            const packages = analysisData.packages || [];
            
            // Function to build tree structure
            function buildTree(packages) {
                const tree = {};
                
                packages.forEach(pkg => {
                    // Split package path by slash
                    const parts = pkg.path.split('/');
                    let currentLevel = tree;
                    
                    parts.forEach((part, i) => {
                        if (!currentLevel[part]) {
                            currentLevel[part] = {
                                name: part,
                                package: i === parts.length - 1 ? pkg : null,
                                children: {}
                            };
                        }
                        
                        currentLevel = currentLevel[part].children;
                    });
                });
                
                // Convert tree to d3 hierarchy format
                return convertToHierarchy(tree);
            }
            
            // Convert from object format to array format for d3
            function convertToHierarchy(node) {
                const result = [];
                
                Object.keys(node).forEach(key => {
                    const child = node[key];
                    const item = {
                        name: child.name,
                        package: child.package
                    };
                    
                    const children = convertToHierarchy(child.children);
                    if (children.length > 0) {
                        item.children = children;
                    }
                    
                    result.push(item);
                });
                
                return result;
            }
            
            // Create tree data
            root.children = buildTree(packages);
            
            // Create tree layout
            const treeLayout = d3.tree()
                .size([height - 50, width - 160]);
                
            // Create root hierarchy
            const hierarchy = d3.hierarchy(root);
            
            // Apply the tree layout to the hierarchy
            const treeData = treeLayout(hierarchy);
            
            // Add links
            svg.select(".viz-group")
                .append("g")
                .attr("class", "links")
                .selectAll("path")
                .data(treeData.links())
                .enter()
                .append("path")
                .attr("d", d => {
                    return `M${d.source.y},${d.source.x}
                            C${(d.source.y + d.target.y) / 2},${d.source.x}
                             ${(d.source.y + d.target.y) / 2},${d.target.x}
                             ${d.target.y},${d.target.x}`;
                })
                .attr("fill", "none")
                .attr("stroke", "#999")
                .attr("stroke-width", 1);
                
            // Add nodes
            const node = svg.select(".viz-group")
                .append("g")
                .attr("class", "nodes")
                .selectAll("g")
                .data(treeData.descendants())
                .enter()
                .append("g")
                .attr("transform", d => `translate(${d.y},${d.x})`);
                
            // Add circles to nodes
            node.append("circle")
                .attr("r", d => d.data.package ? 8 : 4)
                .attr("fill", d => d.data.package ? "#4CAF50" : "#ccc");
                
            // Add labels to nodes
            node.append("text")
                .attr("dy", 3)
                .attr("x", d => d.children ? -15 : 15)
                .attr("text-anchor", d => d.children ? "end" : "start")
                .text(d => d.data.name)
                .style("font-size", "10px");
        }
    };
})();

// Initialize the visualization when the DOM is loaded
document.addEventListener('DOMContentLoaded', function() {
    // Create a new tab for visualizations
    if (!document.getElementById('visualization-tab')) {
        const resultsContainer = document.getElementById('results-container');
        if (resultsContainer) {
            // Create a visualization tab button
            const tabsDiv = document.createElement('div');
            tabsDiv.className = 'mb-3';
            tabsDiv.innerHTML = `
                <ul class="nav nav-tabs" id="analyzerTabs" role="tablist">
                    <li class="nav-item" role="presentation">
                        <button class="nav-link active" id="code-tab" data-bs-toggle="tab" data-bs-target="#code-panel" type="button" role="tab" aria-controls="code-panel" aria-selected="true">Code</button>
                    </li>
                    <li class="nav-item" role="presentation">
                        <button class="nav-link" id="visualization-tab" data-bs-toggle="tab" data-bs-target="#visualization-panel" type="button" role="tab" aria-controls="visualization-panel" aria-selected="false">Visualizations</button>
                    </li>
                </ul>
                <div class="tab-content" id="analyzerTabContent">
                    <div class="tab-pane fade show active" id="code-panel" role="tabpanel" aria-labelledby="code-tab">
                        <!-- Code content will be moved here -->
                    </div>
                    <div class="tab-pane fade" id="visualization-panel" role="tabpanel" aria-labelledby="visualization-tab">
                        <div class="card">
                            <div class="card-body">
                                <div class="mb-3">
                                    <div class="btn-group" role="group" aria-label="Visualization types">
                                        <button type="button" class="btn btn-outline-primary" id="viz-package-deps">Package Dependencies</button>
                                        <button type="button" class="btn btn-outline-primary" id="viz-function-deps">Function Dependencies</button>
                                        <button type="button" class="btn btn-outline-primary" id="viz-complexity">Complexity Heatmap</button>
                                        <button type="button" class="btn btn-outline-primary" id="viz-package-tree">Package Tree</button>
                                    </div>
                                </div>
                                <div id="visualization-container" class="visualization-container"></div>
                            </div>
                        </div>
                    </div>
                </div>
            `;
            
            // Move existing content to code tab
            const contentDiv = document.querySelector('.content .card');
            const codePanel = tabsDiv.querySelector('#code-panel');
            
            if (contentDiv && codePanel) {
                // Insert the tabs before the content
                contentDiv.parentNode.insertBefore(tabsDiv, contentDiv);
                // Move the content inside the code tab panel
                codePanel.appendChild(contentDiv.cloneNode(true));
                contentDiv.remove();
            }
            
            // Add event listeners for visualization buttons
            setTimeout(() => {
                document.getElementById('viz-package-deps')?.addEventListener('click', function() {
                    if (window.analysisData) {
                        Visualization.initialize('visualization-container', window.analysisData).createPackageDependencyGraph();
                    }
                });
                
                document.getElementById('viz-function-deps')?.addEventListener('click', function() {
                    if (window.analysisData && window.currentFile) {
                        Visualization.initialize('visualization-container', window.analysisData).createFunctionDependencyGraph(window.currentFile.path);
                    }
                });
                
                document.getElementById('viz-complexity')?.addEventListener('click', function() {
                    if (window.analysisData) {
                        Visualization.initialize('visualization-container', window.analysisData).createComplexityHeatmap();
                    }
                });
                
                document.getElementById('viz-package-tree')?.addEventListener('click', function() {
                    if (window.analysisData) {
                        Visualization.initialize('visualization-container', window.analysisData).createPackageTree();
                    }
                });
            }, 100);
        }
    }
});

// Listen for function selection events from visualizations
document.addEventListener('functionSelected', function(event) {
    const func = event.detail.function;
    const filepath = event.detail.filepath;
    
    // Find the file in the analysis data
    const file = window.analysisData.files.find(f => f.path === filepath);
    if (file) {
        // Select the file
        selectFile(file);
        
        // Find the function in the file
        const fileFunction = file.functions.find(f => f.name === func.name);
        if (fileFunction) {
            // Highlight the function
            setTimeout(() => {
                highlightFunction(fileFunction);
                
                // Select the function in the function list
                document.querySelectorAll('.function-item').forEach(el => {
                    if (el.textContent.includes(fileFunction.name)) {
                        el.classList.add('selected');
                    }
                });
            }, 500);
        }
    }
});