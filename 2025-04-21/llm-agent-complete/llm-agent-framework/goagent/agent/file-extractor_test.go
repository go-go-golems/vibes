package agent

import (
	"reflect"
	"testing"
)

func TestFileExtractor_Extract(t *testing.T) {
	testCases := []struct {
		name                      string
		responses                 []string
		expectedFiles             map[string]string
		expectedIncompleteFile    string
		expectedIncompleteContent string
	}{
		{
			name:          "No files",
			responses:     []string{"Just some text."},
			expectedFiles: map[string]string{},
		},
		{
			name:          "Single complete file",
			responses:     []string{`<file name="test.txt">Hello</file>`},
			expectedFiles: map[string]string{"test.txt": "Hello"},
		},
		{
			name:          "Single complete file with surrounding text",
			responses:     []string{"Intro text <file name=\"test.txt\">Hello</file> outro text"},
			expectedFiles: map[string]string{"test.txt": "Hello"},
		},
		{
			name:          "Multiple complete files in one response",
			responses:     []string{`<file name="a.txt">AAA</file> some text <file name="b.txt">BBB</file>`},
			expectedFiles: map[string]string{"a.txt": "AAA", "b.txt": "BBB"},
		},
		{
			name:                      "Incomplete file start",
			responses:                 []string{`<file name="incomplete.txt">Start of content`},
			expectedFiles:             map[string]string{},
			expectedIncompleteFile:    "incomplete.txt",
			expectedIncompleteContent: "Start of content",
		},
		{
			name: "Complete file across two responses",
			responses: []string{
				`<file name="split.txt">Part 1. `,
				`Part 2.</file>`,
			},
			expectedFiles: map[string]string{"split.txt": "Part 1. Part 2."},
		},
		{
			name: "Complete file across three responses",
			responses: []string{
				`<file name="long_split.txt">Segment 1. `,
				`Segment 2. `,
				`Segment 3.</file>`,
			},
			expectedFiles: map[string]string{"long_split.txt": "Segment 1. Segment 2. Segment 3."},
		},
		{
			name: "Multiple files split across responses",
			responses: []string{
				`Text <file name="one.txt">Content 1.</file> More <file name="two.txt">Content 2 part 1.`,
				`Content 2 part 2.</file> End <file name="three.txt">Content 3.`,
				`More content 3.</file>`,
			},
			expectedFiles: map[string]string{
				"one.txt":   "Content 1.",
				"two.txt":   "Content 2 part 1.Content 2 part 2.",
				"three.txt": "Content 3.More content 3.",
			},
		},
		{
			name: "Incomplete file followed by complete file",
			responses: []string{
				`<file name="partial.txt">Start `,
				`End.</file><file name="complete.txt">Full</file>`,
			},
			expectedFiles: map[string]string{
				"partial.txt":  "Start End.",
				"complete.txt": "Full",
			},
		},
		{
			name: "Complete file followed by incomplete file",
			responses: []string{
				`<file name="done.txt">Done</file><file name="not_done.txt">Beginning`,
			},
			expectedFiles:             map[string]string{"done.txt": "Done"},
			expectedIncompleteFile:    "not_done.txt",
			expectedIncompleteContent: "Beginning",
		},
		{
			name: "File remains incomplete",
			responses: []string{
				`<file name="unfinished.txt">Some data `,
				`More data `,
				`Still going`,
			},
			expectedFiles:             map[string]string{},
			expectedIncompleteFile:    "unfinished.txt",
			expectedIncompleteContent: "Some data More data Still going",
		},
		{
			name:          "Empty file tags",
			responses:     []string{`<file name="empty.txt"></file>`},
			expectedFiles: map[string]string{"empty.txt": ""},
		},
		{
			name:          "File with only whitespace content",
			responses:     []string{"<file name=\"ws.txt\">\n  \t\n</file>"},
			expectedFiles: map[string]string{"ws.txt": ""}, // TrimSpace removes it
		},
		{
			name:          "File with leading/trailing whitespace inside tags",
			responses:     []string{"<file name=\"padded.txt\">  \n Content \t\n </file>"},
			expectedFiles: map[string]string{"padded.txt": "Content"}, // TrimSpace handles this
		},
		{
			name: "Only end tag (should be ignored)",
			responses: []string{
				`</file>`,
			},
			expectedFiles: map[string]string{},
		},
		{
			name: "End tag after incomplete",
			responses: []string{
				`<file name="test.txt">abc`,
				`def</file>`,
			},
			expectedFiles: map[string]string{"test.txt": "abcdef"}, // Check newline handling
		},
		{
			name: "End tag after incomplete with newline",
			responses: []string{
				"<file name=\"test.txt\">abc\n",
				`def</file>`,
			},
			expectedFiles: map[string]string{"test.txt": "abc\ndef"}, // Check newline handling
		},
		{
			name: "Malformed start tag (missing quote)",
			responses: []string{
				`<file name="malformed.txt>Content</file>`,
			},
			expectedFiles: map[string]string{},
		},
		{
			name: "Malformed start tag (no name)",
			responses: []string{
				`<file>Content</file>`,
			},
			expectedFiles: map[string]string{},
		},
		{
			name: "Filename with special characters",
			responses: []string{
				`<file name="path/to/file-1.go">package main</file>`,
			},
			expectedFiles: map[string]string{"path/to/file-1.go": "package main"},
		},
		{
			name: "Start tag immediately followed by end tag across responses",
			responses: []string{
				`<file name="tricky.txt">`,
				`</file>`,
			},
			expectedFiles: map[string]string{"tricky.txt": ""},
		},
		{
			name: "Sequential files with incomplete middle file",
			responses: []string{
				`<file name="first.txt">Done</file><file name="middle.txt">Part 1`,
				`Part 2</file><file name="last.txt">Also Done</file>`,
			},
			expectedFiles: map[string]string{
				"first.txt":  "Done",
				"middle.txt": "Part 1Part 2",
				"last.txt":   "Also Done",
			},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			extractor := NewFileExtractor()
			actualFiles := make(map[string]string)

			for i, response := range tc.responses {
				completed := extractor.Extract(response)
				if len(completed) > 0 {
					t.Logf("Response %d (%q) completed files: %v", i+1, response, completed)
					for name, content := range completed {
						if _, exists := actualFiles[name]; exists {
							t.Logf("Warning: File '%s' completed again in response %d.", name, i+1)
						}
						actualFiles[name] = content
					}
				} else {
					t.Logf("Response %d (%q) completed no files.", i+1, response)
				}
				t.Logf("Extractor state after response %d: incompleteFile='%s', bufferLen=%d",
					i+1, extractor.incompleteFileName, extractor.incompleteFileContent.Len())
			}

			if !reflect.DeepEqual(actualFiles, tc.expectedFiles) {
				t.Errorf("Mismatch in completed files: Got:      %v Expected: %v", actualFiles, tc.expectedFiles)
			}

			if extractor.incompleteFileName != tc.expectedIncompleteFile {
				t.Errorf("Mismatch in incomplete file name: Got:      '%s' Expected: '%s'", extractor.incompleteFileName, tc.expectedIncompleteFile)
			}

			if extractor.incompleteFileContent.String() != tc.expectedIncompleteContent {
				t.Errorf("Mismatch in incomplete file content: Got:      '%s' Expected: '%s'",
					extractor.incompleteFileContent.String(), tc.expectedIncompleteContent)
			}
		})
	}
}
