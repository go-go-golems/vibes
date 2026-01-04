// Simple working JavaScript LSP example
// This demonstrates basic functionality without requiring full LSP server communication

console.log("=== Simple JavaScript LSP Demo ===");

// Demonstrate utility functions that work without LSP server
console.log("\\n--- Utility Functions ---");

// File path to URI conversion
var filePath = "/home/user/example.go";
var uri = fileToURI(filePath);
console.log("File path:", filePath);
console.log("URI:", uri);

// URI back to file path
var backToPath = uriToFile(uri);
console.log("Back to path:", backToPath);

// Language ID detection
var testFiles = [
    "main.go",
    "script.js", 
    "style.css",
    "document.md",
    "config.json",
    "unknown.xyz"
];

console.log("\\nLanguage ID detection:");
for (var i = 0; i < testFiles.length; i++) {
    var file = testFiles[i];
    var langId = getLanguageID(file);
    console.log("- " + file + " -> " + langId);
}

// Demonstrate creating LSP client (without initialization)
console.log("\\n--- LSP Client Creation ---");

try {
    var client = createLSPClient({
        command: "gopls",
        args: [],
        rootPath: "/tmp",
        debugMode: false
    });
    
    console.log("LSP client created successfully");
    console.log("Client type:", typeof client);
    console.log("Available methods:");
    console.log("- Initialize:", typeof client.Initialize);
    console.log("- OpenFile:", typeof client.OpenFile);
    console.log("- GetHover:", typeof client.GetHover);
    console.log("- GetCompletion:", typeof client.GetCompletion);
    console.log("- GetDefinition:", typeof client.GetDefinition);
    console.log("- GetReferences:", typeof client.GetReferences);
    console.log("- GetCapabilities:", typeof client.GetCapabilities);
    console.log("- Close:", typeof client.Close);
    
    // Check initialization status (should be false before Initialize() is called)
    console.log("Is initialized:", client.IsInitialized());
    
    // Close the client
    client.Close();
    console.log("Client closed successfully");
    
} catch (error) {
    console.error("Error creating LSP client:", error.message);
}

// Demonstrate setTimeout function
console.log("\\n--- Async Operations ---");
console.log("Setting timeout for 1 second...");

setTimeout(function() {
    console.log("Timeout callback executed!");
    console.log("Demo completed successfully!");
}, 1000);

console.log("Timeout set, waiting...");

// Note: In a real environment, you would:
// 1. Initialize the LSP client with client.Initialize()
// 2. Open files with client.OpenFile(path)
// 3. Use LSP features like hover, completion, etc.
// 4. Always close the client when done with client.Close()

