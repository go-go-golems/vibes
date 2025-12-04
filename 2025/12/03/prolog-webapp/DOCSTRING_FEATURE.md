# Docstring Feature Documentation

## Overview

The Prolog interpreter web application now supports **docstrings** for rules. Docstrings are human-readable explanations that describe what a rule does, making the knowledge base more understandable and helping the AI generate better queries.

## Implementation Details

### Data Structure

Facts and rules are now stored as objects with optional docstrings:

```typescript
type FactWithDocstring = {
  fact: string;        // The Prolog fact or rule
  docstring?: string;  // Optional human-readable explanation
}
```

### AI Generation

When the AI generates facts and rules from natural language, it automatically creates docstrings for rules (but not simple facts). The AI response includes:

```json
{
  "reasoning": "Step-by-step thinking...",
  "facts": ["(parent tom bob)", "(grandparent ?gp ?gc) :- (parent ?gp ?p) (parent ?p ?gc)"],
  "docstrings": {
    "(grandparent ?gp ?gc) :- (parent ?gp ?p) (parent ?p ?gc)": "A grandparent is someone who is a parent of a parent"
  }
}
```

### UI Display

1. **Facts/Rules List**: Docstrings appear below their corresponding rules in italic gray text
2. **AI Preview**: When the AI generates rules, docstrings are shown in the preview before adding to the knowledge base
3. **Query Context**: Docstrings are included in the AI query generation prompt to help the AI understand what each rule does

### Query Generation Enhancement

When generating queries from natural language questions, the AI now receives:
- Available predicates with their signatures
- Available atoms (constants) and which predicates they appear in
- **Rule explanations (docstrings)** to understand what each rule does

This helps the AI generate more accurate queries by understanding the semantics of the rules, not just their syntax.

## How to Test

### Test 1: Generate Rules with Docstrings

1. Go to the playground
2. Click the "AI" tab under "Facts & Rules"
3. Enter a description like:
   ```
   Tom is the parent of Bob. Alice is the parent of Tom.
   A grandparent is someone who is a parent of a parent.
   ```
4. Click "Generate Facts/Rules"
5. Observe:
   - The AI shows chain-of-thought reasoning
   - Generated rules appear with docstrings below them
   - Docstrings explain what each rule does

### Test 2: View Docstrings in Knowledge Base

1. After generating rules, click "Add All"
2. Look at the "Facts & Rules" list
3. Observe:
   - Simple facts appear without docstrings
   - Rules appear with their docstrings in italic text below
   - Docstrings are clearly visible and readable

### Test 3: Docstrings Help Query Generation

1. Add some rules with docstrings (using AI or manually)
2. Switch to the "Query" section
3. Click the "AI" tab
4. Ask a natural language question like "Who are the grandparents?"
5. Click the debug icon (bug icon) to view the prompt
6. Observe:
   - The "User Prompt" section includes "Rule explanations:"
   - Each rule's docstring is shown to help the AI understand the rules
   - The AI can use this context to generate better queries

### Test 4: Complex Rules

Try generating more complex organizational rules:

```
John leads TeamA. Sarah leads TeamB.
A team member reports to a team leader if the leader leads the team and the member is on that team.
```

Observe how the AI creates appropriate docstrings for complex rules.

## Technical Implementation

### Frontend Changes

1. **PrologPlayground.tsx**:
   - Changed `facts` state from `string[]` to `FactWithDocstring[]`
   - Updated fact rendering to display docstrings
   - Extract fact strings when passing to query executor

2. **PrologAIChat.tsx**:
   - Updated interface to accept/return `FactWithDocstring[]`
   - Extract docstrings from AI response and attach to facts
   - Pass docstrings to query generation mutation

### Backend Changes

1. **routers.ts**:
   - `generateFactsFromNL`: Already generated docstrings in response
   - `generateQueryFromNL`: Updated to accept `docstrings` parameter and include them in the prompt

### Benefits

1. **Better Documentation**: Rules are self-documenting with human-readable explanations
2. **Improved AI Query Generation**: The AI understands rule semantics, not just syntax
3. **Learning Tool**: Users can see what each rule does without parsing Prolog syntax
4. **Maintainability**: Complex knowledge bases are easier to understand and modify

## Future Enhancements

Potential improvements to consider:

1. **Persistent Storage**: Store docstrings in the database for saved sessions
2. **Manual Editing**: Allow users to edit docstrings manually
3. **Preset Docstrings**: Add docstrings to existing presets
4. **Hover Tooltips**: Show docstrings on hover instead of always visible
5. **Export/Import**: Include docstrings when exporting/importing knowledge bases
