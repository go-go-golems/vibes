# Tasks

## TODO

- [x] Add tasks here

- [x] Create project structure (cmd/, internal/, web/, build/, assets/)
- [x] Copy prolog-ts.ts to web/ directory
- [x] Create web/app.ts entry point wrapper
- [x] Set up tsconfig.json and package.json
- [x] Create build/generate.go with esbuild pipeline
- [x] Implement cmd/prolog-repl/main.go with Goja VM setup
- [x] Test: Load module and create PrologDB instance
- [ ] Test: Add fact and query (basic functionality)
- [ ] Phase 1: Create PrologEvaluator struct with Goja runtime and module setup
- [ ] Phase 1: Implement NewPrologEvaluator() with Goja VM initialization and module loading
- [ ] Phase 1: Cache Goja function references (createDBFunc, parseClauseFunc, parseTermFunc, etc.)
- [ ] Phase 1: Implement GetPrompt(), GetName(), SupportsMultiline(), GetFileExtension() methods
- [ ] Phase 1: Implement basic EvaluateStream() with clause handling (parse and add facts)
- [ ] Phase 1: Test adding Prolog facts through REPL using tmux (send keys, capture output, verify)
- [ ] Phase 2: Implement query parsing (detect ?- or ? prefix)
- [ ] Phase 2: Implement handleQuery() method with prove() execution
- [ ] Phase 2: Format solutions as markdown events
- [ ] Phase 2: Handle empty solutions case
- [ ] Phase 2: Test queries with variables and multiple solutions using tmux (verify solutions displayed correctly)
- [ ] Phase 3: Implement formatBindings() to extract and format variable bindings
- [ ] Phase 3: Format bindings as tables using EventTable events
- [ ] Phase 3: Implement pretty-printing for Prolog terms
- [ ] Phase 3: Add structured log events for debugging
- [ ] Phase 3: Improve error messages with context
- [ ] Phase 4: Implement custom commands (/clear, /list, /help)
- [ ] Phase 4: Add multiline support for Prolog rules
- [ ] Phase 4: Test all features using tmux (keyboard shortcuts, multiline, history navigation)
- [ ] Phase 4: Implement history persistence
- [ ] Phase 5: Performance optimization (function caching, reduce allocations)
- [ ] Phase 5: Add comprehensive error handling and recovery
- [ ] Phase 5: Wire up main application with event bus and Bubble Tea program
- [ ] Phase 5: Add console integration (setupConsole with event emission)
- [ ] Phase 5: Create automated tmux test suite (script all test cases with key events and screenshot verification)
- [ ] Phase 3: Test formatting using tmux screenshots (verify visual output and layout)
