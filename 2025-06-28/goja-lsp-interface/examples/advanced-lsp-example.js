// Advanced LSP JavaScript Example
// This demonstrates more sophisticated usage patterns

console.log("=== Advanced LSP Usage Example ===");

// Helper function to analyze a Go file
function analyzeGoFile(client, filePath) {
    console.log("\\nAnalyzing file:", filePath);
    
    try {
        // Open the file
        var openFile = client.OpenFile(filePath);
        console.log("File opened successfully");
        
        // Read the file content to find interesting positions
        var content = readFile(filePath);
        var lines = content.split('\\n');
        
        // Find function definitions (simple heuristic)
        var functionPositions = [];
        for (var i = 0; i < lines.length; i++) {
            var line = lines[i];
            if (line.match(/^func\\s+\\w+/) || line.match(/^func\\s+\\([^)]+\\)\\s+\\w+/)) {
                var match = line.match(/func\\s+(?:\\([^)]+\\)\\s+)?(\\w+)/);
                if (match) {
                    functionPositions.push({
                        line: i,
                        name: match[1],
                        text: line.trim()
                    });
                }
            }
        }
        
        console.log("Found", functionPositions.length, "functions:");
        
        // Analyze each function
        for (var i = 0; i < functionPositions.length; i++) {
            var func = functionPositions[i];
            console.log("\\n--- Function:", func.name, "at line", func.line + 1, "---");
            
            // Get hover information for the function
            var hover = client.GetHover(openFile.uri, func.line, 5);
            if (hover && hover.text) {
                console.log("Documentation:", hover.text.substring(0, 100) + "...");
            }
            
            // Find references to this function
            var references = client.GetReferences(openFile.uri, func.line, 5, false);
            console.log("References found:", references.length);
            
            // Show where this function is used
            for (var j = 0; j < Math.min(3, references.length); j++) {
                var ref = references[j];
                console.log("  Used in:", ref.text);
            }
        }
        
        // Find struct definitions
        var structPositions = [];
        for (var i = 0; i < lines.length; i++) {
            var line = lines[i];
            if (line.match(/^type\\s+\\w+\\s+struct/)) {
                var match = line.match(/type\\s+(\\w+)\\s+struct/);
                if (match) {
                    structPositions.push({
                        line: i,
                        name: match[1],
                        text: line.trim()
                    });
                }
            }
        }
        
        console.log("\\nFound", structPositions.length, "structs:");
        
        // Analyze each struct
        for (var i = 0; i < structPositions.length; i++) {
            var struct = structPositions[i];
            console.log("\\n--- Struct:", struct.name, "at line", struct.line + 1, "---");
            
            // Get hover information
            var hover = client.GetHover(openFile.uri, struct.line, 5);
            if (hover && hover.text) {
                console.log("Type info:", hover.text.substring(0, 150) + "...");
            }
            
            // Find all usages of this struct
            var references = client.GetReferences(openFile.uri, struct.line, 5, false);
            console.log("Usages found:", references.length);
        }
        
        // Test code completion at various positions
        console.log("\\n--- Testing Code Completion ---");
        
        // Look for lines with method calls or field access
        for (var i = 0; i < Math.min(lines.length, 50); i++) {
            var line = lines[i];
            var dotIndex = line.indexOf('.');
            if (dotIndex > 0 && dotIndex < line.length - 1) {
                console.log("Testing completion at line", i + 1, "after '.'");
                
                var completions = client.GetCompletion(openFile.uri, i, dotIndex + 1);
                if (completions.length > 0) {
                    console.log("  Found", completions.length, "completions:");
                    for (var j = 0; j < Math.min(3, completions.length); j++) {
                        console.log("    -", completions[j].label);
                    }
                }
                break; // Only test first occurrence
            }
        }
        
        // Close the file
        client.CloseFile(openFile.uri);
        console.log("\\nFile analysis completed");
        
    } catch (error) {
        console.error("Error analyzing file:", error.message);
    }
}

// Helper function to explore project structure
function exploreProject(client, rootPath) {
    console.log("\\n=== Project Structure Analysis ===");
    
    // This would typically involve reading directory contents
    // For demo purposes, we'll analyze known files
    var commonGoFiles = [
        rootPath + "/demo/pkg/models/user.go",
        rootPath + "/demo/pkg/calculator/calculator.go",
        rootPath + "/demo/cmd/app/main.go"
    ];
    
    for (var i = 0; i < commonGoFiles.length; i++) {
        var filePath = commonGoFiles[i];
        try {
            // Check if file exists by trying to read it
            readFile(filePath);
            analyzeGoFile(client, filePath);
        } catch (e) {
            console.log("Skipping non-existent file:", filePath);
        }
    }
}

// Main execution
try {
    // Create LSP client
    var client = createLSPClient({
        command: "gopls",
        args: [],
        rootPath: PROJECT_ROOT || "/home/ubuntu/goja-lsp-interface",
        debugMode: false
    });
    
    console.log("LSP client created");
    
    // Initialize
    client.Initialize();
    console.log("LSP client initialized");
    
    // Explore the project
    exploreProject(client, PROJECT_ROOT || "/home/ubuntu/goja-lsp-interface");
    
    // Show final statistics
    var openFiles = client.GetOpenFiles();
    console.log("\\n=== Final Statistics ===");
    console.log("Total files analyzed:", openFiles.length);
    
    var capabilities = client.GetCapabilities();
    console.log("LSP server capabilities utilized:");
    console.log("- Hover:", capabilities.hoverProvider);
    console.log("- Completion:", capabilities.completionProvider);
    console.log("- Definition:", capabilities.definitionProvider);
    console.log("- References:", capabilities.referencesProvider);
    
} catch (error) {
    console.error("Error in advanced example:", error.message);
} finally {
    if (typeof client !== 'undefined') {
        client.Close();
        console.log("LSP client closed");
    }
}

console.log("\\nAdvanced example completed!");

