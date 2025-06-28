package fileeditor

import (
	"os"
	"path/filepath"
	"testing"
)

func TestConstructNewFileContent(t *testing.T) {
	testCases := []struct {
		name       string
		original   string
		diff       string
		expected   string
		isFinal    bool
		shouldFail bool
	}{
		{
			name:     "empty file",
			original: "",
			diff: `------- SEARCH
=======
new content
+++++++ REPLACE`,
			expected: "new content\n",
			isFinal:  true,
		},
		{
			name:     "exact match replacement",
			original: "line1\nline2\nline3",
			diff: `------- SEARCH
line2
=======
replaced
+++++++ REPLACE`,
			expected: "line1\nreplaced\nline3",
			isFinal:  true,
		},
		{
			name:     "line-trimmed match replacement",
			original: "line1\n line2 \nline3",
			diff: `------- SEARCH
line2
=======
replaced
+++++++ REPLACE`,
			expected: "line1\nreplaced\nline3",
			isFinal:  true,
		},
		{
			name:     "block anchor match replacement",
			original: "line1\nstart\nmiddle\nend\nline5",
			diff: `------- SEARCH
start
middle
end
=======
replaced
+++++++ REPLACE`,
			expected: "line1\nreplaced\nline5",
			isFinal:  true,
		},
		{
			name:     "multiple ordered replacements",
			original: "First\nSecond\nThird\nFourth",
			diff: `------- SEARCH
First
=======
1st
+++++++ REPLACE

------- SEARCH
Third
=======
3rd
+++++++ REPLACE`,
			expected: "1st\nSecond\n3rd\nFourth",
			isFinal:  true,
		},
		{
			name:     "replace then delete",
			original: "line1\nline2\nline3\nline4",
			diff: `------- SEARCH
line2
=======
replaced
+++++++ REPLACE

------- SEARCH
line4
=======
+++++++ REPLACE`,
			expected: "line1\nreplaced\nline3\n",
			isFinal:  true,
		},
		{
			name:     "delete then replace",
			original: "line1\nline2\nline3\nline4",
			diff: `------- SEARCH
line1
=======
+++++++ REPLACE

------- SEARCH
line3
=======
replaced
+++++++ REPLACE`,
			expected: "line2\nreplaced\nline4",
			isFinal:  true,
		},
		{
			name:     "missing final REPLACE marker",
			original: "line1\nline2\nline3",
			diff: `------- SEARCH
line2
=======
replaced`,
			expected: "line1\nreplaced\nline3",
			isFinal:  true,
		},
		{
			name:     "flexible marker lengths - short",
			original: "before\ncontent\nafter",
			diff: `--- SEARCH
content
===
new content
+++ REPLACE`,
			expected: "before\nnew content\nafter",
			isFinal:  true,
		},
		{
			name:     "flexible marker lengths - long",
			original: "before\ncontent\nafter",
			diff: `----------- SEARCH
content
==========
new content
+++++++++++ REPLACE`,
			expected: "before\nnew content\nafter",
			isFinal:  true,
		},
		{
			name:     "legacy markers",
			original: "before\ncontent\nafter",
			diff: `<<<<<<< SEARCH
content
=======
new content
>>>>>>> REPLACE`,
			expected: "before\nnew content\nafter",
			isFinal:  true,
		},
		{
			name:       "no match found",
			original:   "line1\nline2\nline3",
			diff: `------- SEARCH
non-existent
=======
replaced
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

func TestFileEditor(t *testing.T) {
	// Create a temporary directory for testing
	tempDir, err := os.MkdirTemp("", "fileeditor_test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tempDir)
	
	editor := NewFileEditor(tempDir)
	
	t.Run("WriteToFile and ReadFile", func(t *testing.T) {
		content := "Hello, World!\nThis is a test file."
		path := "test.txt"
		
		// Write file
		err := editor.WriteToFile(path, content)
		if err != nil {
			t.Errorf("WriteToFile failed: %v", err)
		}
		
		// Read file
		readContent, err := editor.ReadFile(path)
		if err != nil {
			t.Errorf("ReadFile failed: %v", err)
		}
		
		if readContent != content {
			t.Errorf("Expected %q, got %q", content, readContent)
		}
	})
	
	t.Run("ReplaceInFile", func(t *testing.T) {
		// Create initial file
		initialContent := "function test() {\n\tconsole.log('hello');\n\treturn 42;\n}"
		path := "test.js"
		
		err := editor.WriteToFile(path, initialContent)
		if err != nil {
			t.Errorf("WriteToFile failed: %v", err)
		}
		
		// Apply replacement
		diff := `------- SEARCH
	console.log('hello');
=======
	console.log('Hello, World!');
+++++++ REPLACE`
		
		err = editor.ReplaceInFile(path, diff)
		if err != nil {
			t.Errorf("ReplaceInFile failed: %v", err)
		}
		
		// Verify result
		result, err := editor.ReadFile(path)
		if err != nil {
			t.Errorf("ReadFile failed: %v", err)
		}
		
		expected := "function test() {\n\tconsole.log('Hello, World!');\n\treturn 42;\n}"
		if result != expected {
			t.Errorf("Expected:\n%q\nGot:\n%q", expected, result)
		}
	})
	
	t.Run("CreateDirectory and ListFiles", func(t *testing.T) {
		// Create directory structure
		err := editor.CreateDirectory("subdir")
		if err != nil {
			t.Errorf("CreateDirectory failed: %v", err)
		}
		
		// Create files
		err = editor.WriteToFile("subdir/file1.txt", "content1")
		if err != nil {
			t.Errorf("WriteToFile failed: %v", err)
		}
		
		err = editor.WriteToFile("subdir/file2.txt", "content2")
		if err != nil {
			t.Errorf("WriteToFile failed: %v", err)
		}
		
		// List files non-recursively
		files, err := editor.ListFiles(".", false)
		if err != nil {
			t.Errorf("ListFiles failed: %v", err)
		}
		
		// Should contain subdir/ and test files
		found := false
		for _, file := range files {
			if file == "subdir/" {
				found = true
				break
			}
		}
		if !found {
			t.Errorf("Expected to find subdir/ in listing: %v", files)
		}
		
		// List files recursively
		files, err = editor.ListFiles(".", true)
		if err != nil {
			t.Errorf("ListFiles recursive failed: %v", err)
		}
		
		// Should contain subdir files
		foundFile1 := false
		for _, file := range files {
			if file == filepath.Join("subdir", "file1.txt") {
				foundFile1 = true
				break
			}
		}
		if !foundFile1 {
			t.Errorf("Expected to find subdir/file1.txt in recursive listing: %v", files)
		}
	})
	
	t.Run("FileExists", func(t *testing.T) {
		// Test existing file
		exists, err := editor.FileExists("test.txt")
		if err != nil {
			t.Errorf("FileExists failed: %v", err)
		}
		if !exists {
			t.Errorf("Expected test.txt to exist")
		}
		
		// Test non-existing file
		exists, err = editor.FileExists("nonexistent.txt")
		if err != nil {
			t.Errorf("FileExists failed: %v", err)
		}
		if exists {
			t.Errorf("Expected nonexistent.txt to not exist")
		}
	})
	
	t.Run("Path security", func(t *testing.T) {
		// Test absolute path rejection
		err := editor.WriteToFile("/etc/passwd", "malicious")
		if err == nil {
			t.Errorf("Expected error for absolute path")
		}
		
		// Test path traversal rejection
		err = editor.WriteToFile("../../../etc/passwd", "malicious")
		if err == nil {
			t.Errorf("Expected error for path traversal")
		}
	})
}

func TestMultipleSequentialEdits(t *testing.T) {
	// Create a temporary directory for testing
	tempDir, err := os.MkdirTemp("", "sequential_edits_test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tempDir)
	
	editor := NewFileEditor(tempDir)
	
	// Create initial file
	initialContent := `package main

import "fmt"

func main() {
	fmt.Println("Hello")
	x := 1
	y := 2
	fmt.Println(x + y)
}`
	
	err = editor.WriteToFile("main.go", initialContent)
	if err != nil {
		t.Fatalf("Failed to create initial file: %v", err)
	}
	
	// First edit: Change the greeting
	diff1 := `------- SEARCH
	fmt.Println("Hello")
=======
	fmt.Println("Hello, World!")
+++++++ REPLACE`
	
	err = editor.ReplaceInFile("main.go", diff1)
	if err != nil {
		t.Errorf("First edit failed: %v", err)
	}
	
	// Second edit: Change variable names
	diff2 := `------- SEARCH
	x := 1
	y := 2
=======
	a := 10
	b := 20
+++++++ REPLACE`
	
	err = editor.ReplaceInFile("main.go", diff2)
	if err != nil {
		t.Errorf("Second edit failed: %v", err)
	}
	
	// Third edit: Update the calculation
	diff3 := `------- SEARCH
	fmt.Println(x + y)
=======
	fmt.Println(a * b)
+++++++ REPLACE`
	
	err = editor.ReplaceInFile("main.go", diff3)
	if err != nil {
		t.Errorf("Third edit failed: %v", err)
	}
	
	// Verify final result
	result, err := editor.ReadFile("main.go")
	if err != nil {
		t.Errorf("Failed to read final result: %v", err)
	}
	
	expected := `package main

import "fmt"

func main() {
	fmt.Println("Hello, World!")
	a := 10
	b := 20
	fmt.Println(a * b)
}`
	
	if result != expected {
		t.Errorf("Final result mismatch.\nExpected:\n%s\nGot:\n%s", expected, result)
	}
}

