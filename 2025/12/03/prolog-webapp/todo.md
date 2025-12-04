# Prolog Webapp TODO

## Backend - Lisp Integration
- [x] Copy Prolog interpreter to project directory
- [x] Create Node.js wrapper to execute Lisp code via SBCL
- [x] Create database schema for saving sessions and presets
- [x] Create tRPC procedures for Prolog operations (query, assert, clear, load preset)
- [x] Add preset data for common Prolog examples

## Frontend - UI
- [x] Design and implement main UI layout
- [x] Create query input component with syntax highlighting
- [x] Create facts management interface
- [x] Create presets dropdown/selector
- [x] Display query results in a readable format
- [x] Add loading states and error handling
- [x] Style the application with a clean, modern design

## Testing
- [x] Test basic facts and queries
- [x] Test rules with multiple goals
- [x] Test list operations
- [x] Test complex queries

## Documentation
- [ ] Add usage instructions
- [ ] Document preset examples

## New Features
- [x] Add Project Management preset with organizational logic

## Docker Deployment
- [ ] Create Dockerfile with Node.js and SBCL
- [ ] Add .dockerignore file
- [ ] Test Docker build locally
- [ ] Verify SBCL works in container
- [ ] Test all presets in containerized environment

## Graph Visualization
- [ ] Add graph visualization library
- [ ] Parse facts to extract relationships
- [ ] Render directed graph of relationships
- [ ] Add interactive graph controls

## TypeScript Interpreter Improvements
- [x] Add support for rules with bodies (e.g., `(head) :- (goal1) (goal2)`)
- [x] Update parser to handle rule syntax with `:-`
- [x] Implement rule execution in proveAll
- [x] Fix Project Management preset in database (missing :- in rules)

## AI Chat Assistants
- [x] Create tRPC procedure for natural language to Prolog fact/rule conversion
- [x] Create tRPC procedure for natural language to Prolog query conversion
- [x] Add chat UI component for generating facts and rules
- [x] Add chat UI component for generating queries
- [x] Integrate chat components into the playground page
- [x] Test natural language conversion with various examples

## Improve AI Query Generation
- [x] Extract predicate signatures from current facts and rules
- [x] Update generateQuery prompt to include available predicates
- [x] Test query generation with predicate context

## Debug Features
- [x] Add debug button to show LLM prompts for fact/rule generation
- [x] Add debug button to show LLM prompts for query generation
- [x] Display full request and response in debug mode

## Atom Extraction and Display
- [x] Create function to extract all atoms (constants) from facts and rules
- [x] Track which predicates each atom appears in
- [x] Add atoms list to query generation prompt
- [x] Display atoms and their predicate associations in debug view
- [x] Test atom extraction with various presets

## Conjunctive Query Support
- [x] Update query parser to handle multiple goals (conjunctions)
- [x] Test that backend properly executes multi-goal queries
- [x] Update AI prompt to explain conjunction syntax
- [x] Add examples of conjunctive queries to AI prompt
- [x] Update frontend to display multi-goal queries properly
- [x] Test conjunctive queries with various examples

## AI Prompt Quality Improvements
- [x] Preserve original variable names in rule signatures (not ?arg1, ?arg2)
- [x] Add chain-of-thought reasoning to AI prompts
- [x] Update response parsing to extract reasoning
- [x] Display reasoning in UI
- [ ] Update database schema to store rule docstrings
- [ ] Update preset data to include docstrings for rules
- [ ] Update AI fact/rule generator to create docstrings
- [ ] Parse and store docstrings when rules are added
- [ ] Include rule docstrings in query generation prompt
- [x] Add copy-to-clipboard buttons in debug view
- [ ] Implement model selector dropdown (Gemini Flash, GPT-4, Claude, etc.)
- [ ] Add model parameter to backend AI procedures
- [ ] Test with different models

## Model Selector Implementation
- [x] Add model selector dropdown UI component
- [x] Update backend AI procedures to accept model parameter
- [ ] Test with different models (Gemini Flash, GPT-4, Claude)

## Docstring Implementation
- [x] Extract docstrings from AI-generated rules
- [x] Store docstrings in frontend state alongside facts
- [x] Include rule docstrings in query generation prompt
- [x] Display docstrings in UI when showing rules

## Docstring Implementation (Current)
- [x] Update PrologPlayground to store docstrings alongside facts
- [x] Extract docstrings from AI-generated rules response
- [x] Display docstrings in the facts list UI (tooltip or expandable)
- [x] Include rule docstrings in query generation prompt
- [ ] Test docstring generation and display end-to-end

## Docstring Bug Fix
- [ ] Debug why docstrings are not appearing in UI
- [ ] Debug why docstrings are not appearing in prompts
- [ ] Verify AI is generating docstrings in response
- [ ] Verify frontend is extracting docstrings correctly
- [ ] Verify UI is rendering docstrings
- [ ] Test complete docstring flow end-to-end

## Docstring Key Mismatch Fix
- [x] Identified root cause: docstring keys use rule head, but frontend expects full rule text
- [x] Updated JSON schema description to clarify keys should be complete rule text
- [x] Found real issue: LLM returns empty docstrings object {}
- [x] Updated system prompt with explicit requirement and example
- [x] Tried removing strict mode - still didn't work
- [x] Redesigned schema to use array of {fact, docstring} objects instead of separate docstrings object
- [x] Test with new schema - DOCSTRINGS NOW WORKING!
- [ ] Verify docstrings are included in query generation prompts
