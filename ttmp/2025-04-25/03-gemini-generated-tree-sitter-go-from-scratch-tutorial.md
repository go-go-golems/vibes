## Intern Guide: Building the Go File Analyzer

**Project Vision:**

Welcome! You're about to build the "Go File Analyzer," a tool designed to parse and understand Go codebases. The primary goals are:

1.  **Analyze Go Code:** Accurately parse Go source files to identify functions, methods, packages, imports, and other relevant structures.
2.  **Provide Structured Data:** Generate detailed analysis results in a well-defined JSON format, suitable for both programmatic use and visualization.
3.  **Enable Exploration:** Offer a web-based user interface (UI) that allows users to easily explore the structure, relationships, and complexity within a Go codebase through interactive visualizations.

**Key Technology:**

At the heart of this tool is **Tree-sitter**. Tree-sitter is a powerful parser generator and incremental parsing library. Unlike simpler methods like regular expressions, Tree-sitter builds a complete Concrete Syntax Tree (CST) of the source code. This allows for much more accurate and robust analysis, correctly identifying code structures even in complex scenarios. We'll use the existing Go grammar for Tree-sitter.

**Architectural Overview:**

We will build this as a single, unified Go application with distinct internal components (packages):

1.  **`parser`:** Responsible for interacting with Tree-sitter, parsing Go source files, and extracting low-level information (functions, methods, imports, positions, comments).
2.  **`analysis`:** Contains the logic to orchestrate the analysis of a whole directory structure, recursively finding Go files, calling the parser, and aggregating the raw data.
3.  **`formatter`:** Takes the aggregated analysis data and transforms it into the final, rich JSON output formats (including the "Enhanced JSON Format" optimized for visualization).
4.  **`web`:** Implements the backend HTTP server, providing API endpoints for the frontend UI to request analysis and file content.
5.  **`cmd`:** Contains the main entry points for the application (e.g., one for the command-line analyzer, one for the web server).
6.  **`ui`:** Holds the static assets (HTML, CSS, JavaScript) for the web interface.

**Development Approach:**

We will build this project incrementally, phase by phase. Each phase builds upon the previous one, ensuring we have working pieces as we go.

---

### Phase 1: Project Setup & Core Parsing Foundation (Est. 2-3 days)

**Goal:** Set up the project structure, integrate Tree-sitter, and implement the basic ability to parse a single Go file and extract function names and positions.

**Tasks:**

1.  **Environment Setup:**
    - Ensure you have Go installed (version 1.18 or later). Check with `go version`.
    - Familiarize yourself with basic Go commands: `go build`, `go run`, `go mod init`, `go mod tidy`, `go test`.
2.  **Project Structure:**
    - Create the main project directory (e.g., `go-analyzer`).
    - Inside `go-analyzer`, initialize a Go module:
      ```bash
      go mod init github.com/your-org/go-analyzer # Or another appropriate module path
      ```
    - Create the initial directory structure:
      ```
      go-analyzer/
      ├── cmd/
      │   └── simple-parser/ # A temporary CLI tool for testing the parser
      │       └── main.go
      ├── internal/
      │   └── parser/
      │       └── parser.go
      ├── testdata/ # Create a simple Go file here for testing, e.g., testdata/simple.go
      │   └── simple.go
      ├── go.mod
      └── README.md (create a basic one)
      ```
3.  **Integrate Tree-sitter:**
    - Add the necessary Go bindings for Tree-sitter and the Go grammar:
      ```bash
      go get github.com/smacker/go-tree-sitter
      go get github.com/smacker/go-tree-sitter/golang
      ```
    - Run `go mod tidy`.
4.  **Implement Basic Parser (`internal/parser/parser.go`):**

    - Define a struct `FunctionInfo` to hold basic function data (Name, Filepath, StartLine, EndLine). Reference `go-file-analyzer/parser/treesitter.go` or `go-file-analyzer/simplified.go` for the basic fields.
    - Create a `Parser` struct that holds the Tree-sitter parser and language.
    - Implement a `NewParser()` function to initialize the Tree-sitter parser with the Go language.
    - Implement a method `ParseFile(filePath string) ([]FunctionInfo, error)`:
      - Read the file content.
      - Use `parser.ParseCtx` to get the syntax tree.
      - Define a Tree-sitter query string to find function declarations (e.g., `(function_declaration name: (identifier) @func_name) @function`). Reference `go-file-analyzer/parser/treesitter.go`.
      - Execute the query on the tree's root node.
      - Iterate through the query matches.
      - For each match, extract the function name (`@func_name`) and the overall function node (`@function`).
      - Get the start and end line/column numbers from the function node.
      - Create a `FunctionInfo` struct and add it to a slice.
      - Return the slice of `FunctionInfo`.
    - **Example Snippet (Conceptual):**

      ```go
      package parser

      import (
          "io/ioutil"
          sitter "github.com/smacker/go-tree-sitter"
          "github.com/smacker/go-tree-sitter/golang"
          // ... other imports
      )

      type FunctionInfo struct {
          Name      string `json:"name"`
          Filepath  string `json:"filepath"`
          StartLine int    `json:"startLine"`
          EndLine   int    `json:"endLine"`
      }

      type Parser struct {
          parser   *sitter.Parser
          language *sitter.Language
          // potentially store compiled queries here
      }

      func NewParser() *Parser {
          p := sitter.NewParser()
          lang := golang.GetLanguage()
          p.SetLanguage(lang)
          return &Parser{parser: p, language: lang}
      }

      func (p *Parser) ParseFile(filePath string) ([]FunctionInfo, error) {
          content, err := ioutil.ReadFile(filePath)
          // handle error
          tree, err := p.parser.ParseCtx(nil, nil, content)
          // handle error
          defer tree.Close()

          queryStr := `(function_declaration name: (identifier) @func_name) @function`
          query, err := sitter.NewQuery([]byte(queryStr), p.language)
          // handle error

          cursor := sitter.NewQueryCursor()
          cursor.Exec(query, tree.RootNode())

          var functions []FunctionInfo
          for {
              match, ok := cursor.NextMatch()
              if !ok { break }

              var funcName string
              var funcNode *sitter.Node

              for _, capture := range match.Captures {
                  captureName := query.CaptureNameForId(capture.Index)
                  if captureName == "func_name" {
                      funcName = string(content[capture.Node.StartByte():capture.Node.EndByte()])
                  } else if captureName == "function" {
                      funcNode = capture.Node
                  }
              }

              if funcNode != nil && funcName != "" {
                   start := funcNode.StartPoint()
                   end := funcNode.EndPoint()
                   functions = append(functions, FunctionInfo{
                       Name:      funcName,
                       Filepath:  filePath,
                       StartLine: int(start.Row) + 1, // Tree-sitter is 0-based
                       EndLine:   int(end.Row) + 1,
                   })
              }
          }
          return functions, nil
      }
      ```

5.  **Create Test File (`testdata/simple.go`):**

    - Create a simple Go file with a few functions to test the parser.

      ```go
      package simple

      import "fmt"

      // Add adds two numbers.
      func Add(a, b int) int {
          return a + b
      }

      func helper() {
          fmt.Println("Internal helper")
      }

      // Subtract subtracts b from a.
      func Subtract(a, b int) int {
          c := a - b
          if c < 0 {
               helper()
          }
          return c
      }
      ```

6.  **Implement Simple CLI (`cmd/simple-parser/main.go`):**
    - Create a basic command-line tool that takes a file path as an argument.
    - Instantiate your `parser.Parser`.
    - Call `ParseFile` on the provided path.
    - Print the extracted `FunctionInfo` (e.g., using `fmt.Printf` or basic JSON encoding).
7.  **Add Basic Tests:**
    - Create `internal/parser/parser_test.go`.
    - Write a test function (`TestParseFileSimple`) that uses your `testdata/simple.go` file.
    - Assert that the correct number of functions are found and their names/lines match expectations.

**Deliverable:** A Go project that can parse a single Go file and print the names and line numbers of the functions found within it. Includes a unit test for the parsing logic.

---

### Phase 2: Recursive Directory Analysis & Basic JSON Output (Est. 2-3 days)

**Goal:** Extend the tool to analyze an entire directory recursively, aggregate results from multiple files, and output a structured JSON containing basic file and function information.

**Tasks:**

1.  **Create `analysis` Package (`internal/analysis/analyzer.go`):**

    - Define structs for the basic analysis output, perhaps similar to the structure in `go-file-analyzer/recursive_analyzer/main.go` or `go-file-analyzer/analysis-sample.json`. Example:

      ```go
      package analysis

      import "github.com/your-org/go-analyzer/internal/parser" // Adjust import path

      type FileAnalysis struct {
          Path      string              `json:"path"`
          Package   string              `json:"package"` // Need to extract this in parser phase later or here
          Functions []parser.FunctionInfo `json:"functions"`
      }

      type AnalysisResult struct {
          BasePath  string         `json:"basePath"`
          Files     []FileAnalysis `json:"files"`
          // Add stats later if needed
      }
      ```

    - Implement a function `AnalyzeDirectory(rootDir string, parser *parser.Parser, excludeDirs []string) (*AnalysisResult, error)`:
      - Use `filepath.Walk` to traverse `rootDir`.
      - Inside the walk function:
        - Check for errors.
        - Skip directories listed in `excludeDirs` (e.g., "vendor", ".git"). Use `filepath.SkipDir`.
        - If it's a Go file (`.go` extension) and not a test file (`_test.go`), call `parser.ParseFile`.
        - Handle potential errors from `ParseFile`.
        - (Placeholder: Add logic to extract the package name, maybe by enhancing the parser or adding a separate step).
        - Aggregate results into a `FileAnalysis` struct.
      - Collect all `FileAnalysis` results. Use a mutex if implementing concurrency later.
      - Return an `AnalysisResult` struct containing the base path and the list of `FileAnalysis`.

2.  **Enhance Parser (Optional but recommended):**
    - Modify `internal/parser/parser.go` to also extract the package name from the file using a Tree-sitter query like `(package_clause (package_identifier) @package_name)`. Add `Package` to `FunctionInfo` or return it separately.
3.  **Update CLI (`cmd/analyzer/main.go` - Rename/replace `simple-parser`):**
    - Create a new main package `cmd/analyzer`.
    - Use the `flag` package to handle command-line arguments:
      - `-path`: Directory to analyze (default ".").
      - `-output`: Output JSON file path (default "analysis.json").
      - `-exclude`: Comma-separated list of directories to exclude (default "vendor,.git").
    - Instantiate the `parser.Parser`.
    - Call `analysis.AnalyzeDirectory` with the provided path and exclusions.
    - Marshal the `AnalysisResult` into JSON (using `json.MarshalIndent` for readability).
    - Write the JSON data to the specified output file or standard output.
4.  **Test:**
    - Create a more complex `testdata` structure with subdirectories and packages. Use the `go-file-analyzer/sample` directory structure as inspiration.
    - Run your `cmd/analyzer` on this test structure.
    - Verify the output JSON contains the expected files and functions.
    - Add tests for `analysis.AnalyzeDirectory` in `internal/analysis/analyzer_test.go`.

**Deliverable:** A CLI tool that recursively analyzes a Go project directory, excludes specified folders, and outputs a JSON file listing the analyzed files and the basic function info (name, path, lines) within them.

---

### Phase 3: Enhanced Parsing & Rich JSON Formatting (Est. 3-4 days)

**Goal:** Significantly enhance the parser to extract detailed information (methods, parameters, returns, comments) and implement a formatter to produce the "Enhanced JSON Format" designed for visualization.

**Tasks:**

1.  **Enhance Parser (`internal/parser/parser.go`):**
    - **Define Detailed Structs:** Update `FunctionInfo` (or create `EnhancedFunctionInfo`) to include fields for: `IsMethod`, `IsExported`, `Receiver` (struct with Name, Type, IsPointer), `Parameters` ([]ParameterInfo), `ReturnTypes` ([]string), `Comments`, `StartCol`, `EndCol`, `Signature`. Reference `enhanced-json-format-documentation.md` and `go-file-analyzer/parser/enhanced_parser.go`.
    - **Update Tree-sitter Queries:** Create or adapt queries to capture methods, receivers, parameters, return types, and comments. Reference the queries in `go-file-analyzer/parser/enhanced_parser.go` or `tree_sitter_parser_completion.md`.
      - Method query: `(method_declaration ...)`
      - Parameter query: `(parameter_declaration ...)`
      - Comment query: `(comment ...)`
    - **Refactor `ParseFile`:** Modify the parsing logic to use the new queries, extract the detailed information, and populate the `EnhancedFunctionInfo` structs.
      - Distinguish between functions and methods based on the matched query rule (`function_declaration` vs `method_declaration`).
      - Extract receiver details for methods.
      - Extract parameter names and types using the parameter query within the function/method parameter list node.
      - Extract return types (handle single, multiple, named returns).
      - Implement logic to associate preceding comments with functions/methods (check nodes immediately before the function/method node).
    - **Add Helper Functions:** Create helpers for extracting text from nodes, processing parameter/return lists, etc.
2.  **Define Enhanced JSON Structs (`internal/formatter/models.go`):**
    - Create a new package `internal/formatter`.
    - Define Go structs that precisely match _every_ field and section described in `enhanced-json-format-documentation.md`. This includes `EnhancedOutput`, `CodebaseMetadata`, `FileInfo` (with imports, size, lines), `FunctionInfo` (the detailed one), `PackageInfo`, `Graph`, `GraphNode`, `GraphEdge`, `VisualizationData`, `HierarchyNode`, `NetworkVisualization`, etc.
3.  **Implement Formatter (`internal/formatter/formatter.go`):**
    - Create a function `FormatEnhancedJSON(analysisResult *analysis.AnalysisResult) (*EnhancedOutput, error)` (adjust input type as needed based on your enhanced parser output).
    - **Populate Metadata:** Calculate or retrieve metadata like project name, Go version (placeholder for now), counts, timestamp.
    - **Populate Files:** Iterate through `analysisResult.Files`. For each, get detailed `FunctionInfo` (from the enhanced parser), extract imports (enhance parser if needed), get file size/lines (`os.Stat`, reading file). Ensure paths are relative to `basePath`.
    - **Populate Functions:** Collect all _detailed_ `FunctionInfo` objects from the files section. Add unique IDs (e.g., `package.FunctionName`). Generate signatures. (Placeholder: Calls/CalledBy will require more advanced analysis later).
    - **Populate Packages:** Group files by package, list files and function IDs for each package.
    - **Populate Call Graph (Basic):** Create nodes for packages and functions. Add "belongs_to" edges from functions to their packages. (Placeholder: Actual 'calls' edges need deeper analysis, perhaps leave empty or add dummy edges for now). Reference `json_formatter.go` or `enhanced-output-sample.json`.
    - **Populate VisualizationData:**
      - `packageHierarchy`: Build the tree structure based on package paths. Calculate `value` (e.g., number of functions).
      - `callNetwork`: Create nodes/links suitable for D3/force-directed graphs based on functions and the basic call graph. Include `group` (package) and `value` (e.g., lines of code).
      - `moduleDependencies`: (Placeholder/Dummy) Create a matrix based on package imports (requires import analysis).
      - `functionComplexity`: Create entries with function name, package, lines, and complexity (use lines as a proxy for now).
4.  **Update CLI (`cmd/analyzer/main.go`):**
    - Add a flag `-format` (e.g., `-format=basic` vs `-format=enhanced`).
    - If `enhanced` is selected:
      - Ensure the enhanced parser runs.
      - Call the `formatter.FormatEnhancedJSON`.
      - Marshal the resulting `EnhancedOutput` struct to JSON.
5.  **Testing:**
    - Update parser tests (`parser_test.go`) to verify the extraction of methods, params, returns, comments. Use `go-file-analyzer/sample/struct_sample.go` or `go-file-analyzer/sample/nested/` files as test data.
    - Add tests for the `formatter` package (`formatter_test.go`), ensuring the output structure matches the spec and data is populated correctly (at least for the parts implemented).

**Deliverable:** The CLI tool can now produce two JSON formats: a basic one and the detailed "Enhanced JSON Format". The enhanced format includes detailed function info and placeholder/basic visualization data.

---

### Phase 4: Web Server & API Implementation (Est. 3-4 days)

**Goal:** Create a backend web server that exposes an API to trigger analysis and retrieve code/results, using the Go packages directly.

**Tasks:**

1.  **Create `web` Package (`internal/web/server.go`, `internal/web/handlers.go`):**
    - Choose an HTTP router (stdlib `net/http` with `http.ServeMux` is fine, or `gorilla/mux` as seen in the original `webserver/go.mod`).
    - Implement `NewServer(...)` function to set up routes.
    - Implement `Start()` method to run the server.
2.  **Implement API Handlers (`internal/web/handlers.go`):**
    - **`handleAnalyze(w http.ResponseWriter, r *http.Request)`:**
      - Method: `POST`.
      - Decode JSON request body containing `path` and potentially exclusion options.
      - **Crucially:** Call the `analysis.AnalyzeDirectory` and `formatter.FormatEnhancedJSON` functions _directly_ from your Go packages (DO NOT use `os/exec` like the original `webserver`).
      - Implement optional caching: Check if results for the path exist in an in-memory map (use `sync.RWMutex` for safety). If hit, return cached JSON. If miss, run analysis, store result in cache, then return.
      - Marshal the `EnhancedOutput` to JSON and write the response. Handle errors appropriately (e.g., 400 for bad request, 500 for analysis errors).
    - **`handleGetFile(w http.ResponseWriter, r *http.Request)`:**
      - Method: `GET`.
      - Get `path` query parameter.
      - Validate the path (ensure it's within reasonable bounds, exists, is a `.go` file).
      - Read the file content using `ioutil.ReadFile`.
      - Set `Content-Type` to `text/plain`.
      - Write file content to the response. Handle errors (e.g., 404 Not Found).
    - **`handleGetCacheList(w http.ResponseWriter, r *http.Request)`:** (If caching implemented)
      - Return a list of cached analysis paths and perhaps some metadata (timestamp, size).
    - **`handleDeleteCache(w http.ResponseWriter, r *http.Request)`:** (If caching implemented)
      - Delete a specific entry from the cache.
3.  **Create Web Server Entry Point (`cmd/webserver/main.go`):**
    - Use the `flag` package for configuration: `-port`, `-static-dir` (path to UI files), `-default-path` (optional default path for analysis).
    - Instantiate the `parser.Parser`.
    - Instantiate and configure the `web.Server`.
    - Set up static file serving for the UI directory (e.g., using `http.FileServer` and `http.StripPrefix`).
    - Call `server.Start()`.
4.  **Dependencies:**
    - Add any necessary web framework/router dependencies (`gorilla/mux` if used) to the root `go.mod` and run `go mod tidy`.
5.  **Testing:**
    - Add tests for the API handlers (`internal/web/handlers_test.go`). Use `net/http/httptest` to create mock requests and record responses. Test success cases, error cases (bad path, analysis failure), and caching logic.

**Deliverable:** A runnable web server (`cmd/webserver`) that serves static files and provides API endpoints to trigger analysis (using the Go packages) and retrieve file content.

---

### Phase 5: Web UI Implementation (Est. 4-5 days)

**Goal:** Build the frontend interface using HTML, CSS, and JavaScript that interacts with the backend API to display analysis results and visualizations.

**Tasks:**

1.  **Setup UI Structure (`ui/static/`):**
    - Create directories: `css/`, `js/`.
    - Create `index.html`, `css/main.css`, `js/main.js`, `js/visualization.js`. Reference the original `go-file-analyzer/webserver/static/` files for structure and content inspiration.
2.  **HTML (`index.html`):**
    - Structure the page with sections for:
      - Analysis input form (path, exclusions).
      - Results display area (tabs for Summary, Functions, Packages, Raw JSON).
      - Visualization area (tabs for different viz types).
      - Loading indicators.
      - Help section (optional).
    - Include CSS and JS files. Include D3.js library (`<script src="https://d3js.org/d3.v7.min.js"></script>`).
3.  **CSS (`css/main.css`):**
    - Style the layout, form, results tables, tabs, code view, and visualization containers. Make it clean and usable. Start simple, refine later.
4.  **Core JavaScript (`js/main.js`):**
    - Add event listener to the analysis form submit button.
    - On submit:
      - Get form values.
      - Show loading indicator.
      - Make a `fetch` `POST` request to `/api/analyze` with the path and options.
      - Handle the JSON response. Store the `EnhancedOutput` data globally (e.g., `window.analysisData`).
      - Hide loading indicator.
      - Call functions to populate the different results tabs (Summary, Functions table, Packages tree, JSON view).
      - Call visualization functions (from `visualization.js`).
    - Implement tab switching logic.
    - Implement logic to populate the Functions table (filterable/sortable is a bonus).
    - Implement logic to display the package tree (can be simple list initially, enhanced later).
    - Implement logic to fetch file content via `/api/file` when a file is selected in the UI.
    - Display fetched code in a `<pre><code>` block.
    - Integrate a syntax highlighting library (like highlight.js, as seen in the original `index.html`). Initialize it after loading code.
    - Implement function highlighting: When a function is clicked in the list, add a CSS class to the corresponding lines in the code view and scroll to it.
5.  **Visualization JavaScript (`js/visualization.js`):**
    - Create functions to render each visualization type described in `enhanced-json-format-documentation.md`.
    - Use D3.js.
    - **Package Treemap/Hierarchy:** Use `d3.hierarchy` and `d3.treemap` or a similar layout (like `d3.tree` as seen in the original `visualization.js`) using `visualizationData.packageHierarchy`.
    - **Function Call Network:** Use `d3.forceSimulation` with `visualizationData.callNetwork.nodes` and `visualizationData.callNetwork.links`. Style nodes by package/type. Make it interactive (drag nodes).
    - **Complexity Scatter Plot:** Use `visualizationData.functionComplexity`. Plot complexity vs. lines of code. Add tooltips.
    - **Package Dependency Wheel:** Use `visualizationData.moduleDependencies`. Requires a D3 chord diagram implementation.
    - Add event listeners to visualization elements (e.g., clicking a node in the call graph could highlight the function in the main UI).
6.  **Integration:**
    - Ensure the web server (`cmd/webserver`) is configured to serve the `ui/static` directory.
    - Run the web server and test the UI thoroughly by analyzing the `testdata` project. Debug API calls and UI rendering.

**Deliverable:** A functional web interface served by the Go web server. Users can input a path, trigger analysis, view summary stats, browse files/functions, see highlighted code, and interact with at least 1-2 basic visualizations (e.g., package hierarchy, call network).

---

### Phase 6: Testing, Documentation, and Refinement (Ongoing + 2-3 days)

**Goal:** Ensure the application is robust, well-tested, and well-documented. Clean up any remaining rough edges.

**Tasks:**

1.  **Testing:**
    - **Expand Unit Tests:** Ensure good test coverage for `parser`, `analysis`, `formatter`, and `web` packages. Cover edge cases (empty files, files with syntax errors Tree-sitter might handle, complex signatures).
    - **Integration Tests:** Write tests that run the `cmd/analyzer` CLI tool on test data and verify the output JSON. Write tests that make HTTP requests to the running `cmd/webserver` API endpoints and verify the responses.
    - **Manual Testing:** Thoroughly test the web UI across different scenarios and maybe different browsers. Test the CLI with various flags. Use the test scripts (`.sh` files from original project) as inspiration if applicable.
2.  **Refinement:**
    - **Error Handling:** Review error handling throughout the application (parser, analyzer, formatter, API handlers). Provide informative error messages.
    - **Performance:** Identify any obvious performance bottlenecks (e.g., during analysis of large directories). Consider adding simple timing logs.
    - **Code Cleanup:** Run `go fmt` and `go vet`. Address any linting issues. Ensure code is readable and follows Go conventions. Remove any dead code.
    - **Concurrency (Review):** Ensure any concurrent operations (like file walking or caching) use mutexes correctly.
3.  **Documentation:**
    - **`README.md`:** Write a comprehensive README for the _new_ project. Explain:
      - What the tool does.
      - How to build it (`go build ./cmd/...`).
      - How to run the CLI (`./analyzer -path ...`).
      - How to run the web server (`./webserver -port ...`).
      - Explain command-line flags.
      - Briefly describe the architecture.
    - **JSON Format Documentation:** Create a document (e.g., `docs/json-format.md`) detailing the structure of the "Enhanced JSON Format", referencing `enhanced-json-format-documentation.md`.
    - **Code Comments:** Add clear comments to Go code explaining complex logic or public APIs.

**Deliverable:** A polished, tested, and documented Go File Analyzer application with both CLI and web interfaces, built from scratch following a clean structure.
