// Example JavaScript code demonstrating LSP functionality
// This script shows how to use the JavaScript LSP interface

console.log("=== LSP JavaScript Example ===");

// Create an LSP client for Go language server (gopls)
var client = createLSPClient({
    command: "gopls",
    args: [],
    rootPath: "/path/to/your/go/project",
    debugMode: true
});

console.log("LSP client created");

try {
    // Initialize the LSP client
    client.Initialize();
    console.log("LSP client initialized");
    
    // Check server capabilities
    var capabilities = client.GetCapabilities();
    console.log("Server capabilities:");
    console.log("- Hover support:", capabilities.hoverProvider);
    console.log("- Completion support:", capabilities.completionProvider);
    console.log("- Definition support:", capabilities.definitionProvider);
    console.log("- References support:", capabilities.referencesProvider);
    
    // Open a Go file
    var filePath = "/path/to/your/file.go";
    var openFile = client.OpenFile(filePath);
    console.log("Opened file:", openFile.path);
    console.log("Language ID:", openFile.languageId);
    console.log("File URI:", openFile.uri);
    
    // Get hover information at a specific position
    var hover = client.GetHover(openFile.uri, 10, 5); // line 10, character 5
    if (hover) {
        console.log("Hover information:", hover.text);
    }
    
    // Get code completions at a specific position
    var completions = client.GetCompletion(openFile.uri, 15, 10);
    console.log("Found", completions.length, "completion items");
    
    // Show first few completions
    for (var i = 0; i < Math.min(5, completions.length); i++) {
        var item = completions[i];
        console.log("- " + item.label + (item.detail ? " (" + item.detail + ")" : ""));
    }
    
    // Find definition of symbol at position
    var definitions = client.GetDefinition(openFile.uri, 20, 8);
    console.log("Found", definitions.length, "definitions");
    for (var i = 0; i < definitions.length; i++) {
        console.log("Definition:", definitions[i].text);
    }
    
    // Find all references to symbol at position
    var references = client.GetReferences(openFile.uri, 20, 8, true);
    console.log("Found", references.length, "references");
    for (var i = 0; i < Math.min(10, references.length); i++) {
        console.log("Reference:", references[i].text);
    }
    
    // List all open files
    var openFiles = client.GetOpenFiles();
    console.log("Currently open files:");
    for (var i = 0; i < openFiles.length; i++) {
        console.log("- " + openFiles[i].path + " (" + openFiles[i].languageId + ")");
    }
    
    // Close the file when done
    client.CloseFile(openFile.uri);
    console.log("File closed");
    
} catch (error) {
    console.error("Error:", error.message);
} finally {
    // Always close the client when done
    client.Close();
    console.log("LSP client closed");
}

console.log("Example completed!");

// Utility functions available:
// - fileToURI(path) - Convert file path to URI
// - uriToFile(uri) - Convert URI to file path  
// - getLanguageID(path) - Get language ID from file extension
// - readFile(path) - Read file content as string

