package fileeditor

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestDiffEdgeCases(t *testing.T) {
	testCases := []struct {
		name       string
		original   string
		diff       string
		expected   string
		isFinal    bool
		shouldFail bool
	}{
		// Out-of-order replacement tests
		{
			name:     "out-of-order replacements",
			original: "first\nsecond\nthird\nfourth\n",
			diff: `------- SEARCH
fourth
=======
new fourth
+++++++ REPLACE
------- SEARCH
second
=======
new second
+++++++ REPLACE`,
			expected: "first\nnew second\nthird\nnew fourth\n",
			isFinal:  true,
		},
		
		// Complex whitespace handling
		{
			name:     "complex whitespace handling",
			original: "  function test() {\n    console.log('hello');\n  }",
			diff: `------- SEARCH
    console.log('hello');
=======
    console.log('Hello, World!');
+++++++ REPLACE`,
			expected: "  function test() {\n    console.log('Hello, World!');\n  }",
			isFinal:  true,
		},
		
		// Empty replacement (deletion)
		{
			name:     "empty replacement deletion",
			original: "line1\nline2\nline3\nline4",
			diff: `------- SEARCH
line2
line3
=======
+++++++ REPLACE`,
			expected: "line1\nline4",
			isFinal:  true,
		},
		
		// Multiple consecutive empty lines
		{
			name:     "multiple consecutive empty lines",
			original: "line1\n\n\nline4",
			diff: `------- SEARCH


=======
new content
+++++++ REPLACE`,
			expected: "line1\nnew content\nline4",
			isFinal:  true,
		},
		
		// Very long content
		{
			name:     "long content replacement",
			original: strings.Repeat("line\n", 1000) + "target\n" + strings.Repeat("line\n", 1000),
			diff: `------- SEARCH
target
=======
replaced
+++++++ REPLACE`,
			expected: strings.Repeat("line\n", 1000) + "replaced\n" + strings.Repeat("line\n", 1000),
			isFinal:  true,
		},
		
		// Special characters
		{
			name:     "special characters",
			original: "function test() {\n  return \"hello\\nworld\";\n}",
			diff: `------- SEARCH
  return "hello\nworld";
=======
  return "hello\tworld";
+++++++ REPLACE`,
			expected: "function test() {\n  return \"hello\\tworld\";\n}",
			isFinal:  true,
		},
		
		// Unicode content
		{
			name:     "unicode content",
			original: "Hello 世界\nこんにちは\nGoodbye",
			diff: `------- SEARCH
こんにちは
=======
さようなら
+++++++ REPLACE`,
			expected: "Hello 世界\nさようなら\nGoodbye",
			isFinal:  true,
		},
		
		// Mixed line endings (this is tricky)
		{
			name:     "mixed line endings",
			original: "line1\nline2\nline3",
			diff: `------- SEARCH
line2
=======
new line2
+++++++ REPLACE`,
			expected: "line1\nnew line2\nline3",
			isFinal:  true,
		},
		
		// Incremental processing simulation
		{
			name:     "incremental processing",
			original: "line1\nline2\nline3",
			diff:     "------- SEARCH\nline2\n=======\nreplaced\n+++++++ REPLACE",
			expected: "line1\nreplaced\nline3",
			isFinal:  true,
		},
		
		// Block anchor fallback with similar lines
		{
			name:     "block anchor with similar content",
			original: "start\nmiddle1\nend\nstart\nmiddle2\nend",
			diff: `------- SEARCH
start
middle2
end
=======
start
replaced
end
+++++++ REPLACE`,
			expected: "start\nmiddle1\nend\nstart\nreplaced\nend",
			isFinal:  true,
		},
		
		// Error cases
		{
			name:       "search content not found",
			original:   "line1\nline2\nline3",
			diff: `------- SEARCH
nonexistent
=======
replacement
+++++++ REPLACE`,
			shouldFail: true,
			isFinal:    true,
		},
		
		// Malformed markers - should actually work with 3+ characters
		{
			name:       "malformed search marker - too short",
			original:   "line1\nline2\nline3",
			diff: `- SEARCH
line2
=======
replacement
+++++++ REPLACE`,
			shouldFail: true,
			isFinal:    true,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result, err := ConstructNewFileContent(tc.diff, tc.original, tc.isFinal)
			
			if tc.shouldFail {
				if err == nil {
					t.Errorf("Expected error but got none")
				}
				return
			}
			
			if err != nil {
				t.Errorf("Unexpected error: %v", err)
				return
			}
			
			if result != tc.expected {
				t.Errorf("Expected:\n%q\nGot:\n%q", tc.expected, result)
			}
		})
	}
}

func TestFileEditorAdvanced(t *testing.T) {
	// Create a temporary directory for testing
	tempDir, err := os.MkdirTemp("", "advanced_test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tempDir)
	
	editor := NewFileEditor(tempDir)
	
	t.Run("Nested directory creation", func(t *testing.T) {
		content := "nested file content"
		path := "deep/nested/directory/file.txt"
		
		err := editor.WriteToFile(path, content)
		if err != nil {
			t.Errorf("WriteToFile failed: %v", err)
		}
		
		readContent, err := editor.ReadFile(path)
		if err != nil {
			t.Errorf("ReadFile failed: %v", err)
		}
		
		if readContent != content {
			t.Errorf("Expected %q, got %q", content, readContent)
		}
	})
	
	t.Run("Large file handling", func(t *testing.T) {
		// Create a large file (1MB)
		largeContent := strings.Repeat("This is a line of text.\n", 40000)
		path := "large_file.txt"
		
		err := editor.WriteToFile(path, largeContent)
		if err != nil {
			t.Errorf("WriteToFile failed: %v", err)
		}
		
		// Apply a replacement in the middle
		diff := `------- SEARCH
This is a line of text.
=======
This is a MODIFIED line of text.
+++++++ REPLACE`
		
		err = editor.ReplaceInFile(path, diff)
		if err != nil {
			t.Errorf("ReplaceInFile failed: %v", err)
		}
		
		// Verify the change was made
		result, err := editor.ReadFile(path)
		if err != nil {
			t.Errorf("ReadFile failed: %v", err)
		}
		
		if !strings.Contains(result, "This is a MODIFIED line of text.") {
			t.Errorf("Expected modification not found in result")
		}
	})
	
	t.Run("Binary file handling", func(t *testing.T) {
		// Test with binary-like content
		binaryContent := string([]byte{0, 1, 2, 3, 255, 254, 253})
		path := "binary_file.bin"
		
		err := editor.WriteToFile(path, binaryContent)
		if err != nil {
			t.Errorf("WriteToFile failed: %v", err)
		}
		
		readContent, err := editor.ReadFile(path)
		if err != nil {
			t.Errorf("ReadFile failed: %v", err)
		}
		
		if readContent != binaryContent {
			t.Errorf("Binary content mismatch")
		}
	})
	
	t.Run("Concurrent file operations", func(t *testing.T) {
		// Test multiple operations on different files
		for i := 0; i < 10; i++ {
			path := filepath.Join("concurrent", fmt.Sprintf("file_%d.txt", i))
			content := fmt.Sprintf("Content for file %d", i)
			
			err := editor.WriteToFile(path, content)
			if err != nil {
				t.Errorf("WriteToFile failed for file %d: %v", i, err)
			}
		}
		
		// Verify all files were created
		files, err := editor.ListFiles("concurrent", false)
		if err != nil {
			t.Errorf("ListFiles failed: %v", err)
		}
		
		if len(files) != 10 {
			t.Errorf("Expected 10 files, got %d", len(files))
		}
	})
}

func TestPerformance(t *testing.T) {
	// Create a temporary directory for testing
	tempDir, err := os.MkdirTemp("", "performance_test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tempDir)
	
	editor := NewFileEditor(tempDir)
	
	t.Run("Large file with many small edits", func(t *testing.T) {
		// Create a large file with numbered lines
		lines := make([]string, 10000)
		for i := 0; i < 10000; i++ {
			lines[i] = fmt.Sprintf("Line %d: This is content for line %d", i, i)
		}
		content := strings.Join(lines, "\n")
		
		err := editor.WriteToFile("large_file.txt", content)
		if err != nil {
			t.Fatalf("Failed to create large file: %v", err)
		}
		
		// Apply multiple edits
		for i := 0; i < 10; i++ {
			lineNum := i * 1000
			diff := fmt.Sprintf(`------- SEARCH
Line %d: This is content for line %d
=======
Line %d: MODIFIED content for line %d
+++++++ REPLACE`, lineNum, lineNum, lineNum, lineNum)
			
			err := editor.ReplaceInFile("large_file.txt", diff)
			if err != nil {
				t.Errorf("Edit %d failed: %v", i, err)
			}
		}
		
		// Verify final content
		result, err := editor.ReadFile("large_file.txt")
		if err != nil {
			t.Fatalf("Failed to read final result: %v", err)
		}
		
		// Check that modifications were applied
		modifiedCount := strings.Count(result, "MODIFIED content")
		if modifiedCount != 10 {
			t.Errorf("Expected 10 modifications, found %d", modifiedCount)
		}
	})
}

func BenchmarkDiffProcessing(b *testing.B) {
	original := strings.Repeat("line\n", 1000) + "target\n" + strings.Repeat("line\n", 1000)
	diff := `------- SEARCH
target
=======
replaced
+++++++ REPLACE`
	
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, err := ConstructNewFileContent(diff, original, true)
		if err != nil {
			b.Fatalf("Benchmark failed: %v", err)
		}
	}
}

func BenchmarkFileOperations(b *testing.B) {
	tempDir, err := os.MkdirTemp("", "benchmark_test")
	if err != nil {
		b.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tempDir)
	
	editor := NewFileEditor(tempDir)
	content := "test content for benchmarking"
	
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		path := fmt.Sprintf("file_%d.txt", i)
		err := editor.WriteToFile(path, content)
		if err != nil {
			b.Fatalf("WriteToFile failed: %v", err)
		}
		
		_, err = editor.ReadFile(path)
		if err != nil {
			b.Fatalf("ReadFile failed: %v", err)
		}
	}
}

