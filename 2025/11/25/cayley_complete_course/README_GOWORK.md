# Using go.work with Local Cayley Source

All Book 1 exercises now include a `go.work` file that references the local Cayley source code included in this package. This ensures:

1. **Consistency:** All exercises use the same Cayley version (v0.7.7)
2. **Modifiability:** You can modify Cayley source if needed
3. **Learning:** You can study the actual Cayley implementation

## Running Exercises

Simply navigate to any exercise directory and run:

```bash
cd book1_exercises/chapter_02/exercise_2_1
go run *.go
```

The `go.work` file automatically uses the local `cayley/` directory.

## What is go.work?

Go workspaces (go.work) allow you to work with multiple modules simultaneously. Each exercise's go.work file contains:

```
go 1.25.4

use .
use ../../../../cayley
```

This tells Go to use both the current exercise module and the local Cayley module.

## Verified Working Exercises

✅ chapter_02/exercise_2_1 - Hello World  
✅ chapter_02/exercise_2_2 - Social Network  
✅ chapter_02/exercise_2_3 - Query Relationships  
✅ chapter_03/exercise_3_1 - IRIs and Namespaces  
✅ chapter_04/exercise_4_1_transactions - Transactions  
✅ chapter_05/exercise_5_1 - Path API Traversal  
✅ chapter_06/exercise_6_1_simple - Advanced Traversal  
✅ chapter_13/exercise_13_1 - Multi-Agent Task System  
✅ chapter_15/exercise_15_3 - LLM-Integrated Blackboard  

**Total: 9/9 tested exercises working with go.work ✅**

## Troubleshooting

If you encounter issues:

1. Ensure Go 1.25.4 or later is installed
2. Check that the `cayley/` directory exists at the package root
3. Run `go mod tidy` in the exercise directory
4. Check that go.work points to the correct relative path

## Benefits

- **No network dependencies:** All code is local
- **Faster builds:** Go caches the local Cayley build
- **Debugging:** Step through Cayley source code
- **Experimentation:** Modify Cayley and see results immediately
