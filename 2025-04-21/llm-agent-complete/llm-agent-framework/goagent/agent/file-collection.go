package agent

import (
	"context"
	"fmt"
	"strings"

	"github.com/go-go-golems/geppetto/pkg/conversation"
	"github.com/goagent/framework/goagent/llm"
	"github.com/goagent/framework/goagent/types"
	"golang.org/x/exp/maps"
)

// FileCollectionAgent implements an agent that can generate multiple files
// using XML tags to delimit them in the LLM response.
type FileCollectionAgent struct {
	*BaseAgent                   // Embed BaseAgent from goagent/agent
	extractor  *FileExtractor    // Handles file extraction logic
	files      map[string]string // Maps filenames to their complete content accumulated across calls
}

// NewFileCollectionAgent creates a new FileCollectionAgent.
func NewFileCollectionAgent(llmModel llm.LLM, maxIterations int) *FileCollectionAgent {
	return &FileCollectionAgent{
		BaseAgent: NewBaseAgent(llmModel, maxIterations), // Use constructor from goagent/agent
		extractor: NewFileExtractor(),
		files:     make(map[string]string),
	}
}

// GetFiles returns the generated files collected during the last Run.
func (a *FileCollectionAgent) GetFiles() map[string]string {
	return a.files
}

// buildSystemPrompt builds the system prompt for the FileCollectionAgent.
// It instructs the LLM to use specific XML tags for file content and a
// specific comment to signal completion.
func (a *FileCollectionAgent) buildSystemPrompt() string {
	// NOTE: This prompt is highly specific and relies on the LLM's ability
	// to follow structured instructions precisely. Model capabilities may vary.
	return `You are an AI assistant that generates complete, ready-to-use code files based on a request.

When asked to create files, you MUST follow these instructions:
1. Generate each file's complete content sequentially.
2. Wrap EACH file's content within <file name="filename.ext">...</file> XML tags. The filename MUST be included in the 'name' attribute.
3. Ensure each file is complete and ready to use (including imports, comments, etc.).
4. If a file's content is too long for one response, simply stop generating the content mid-file. Do NOT insert any placeholders like '...' or explanations within the <file>...</file> tags. You will be prompted to continue, and you should resume generating the file content directly after the cutoff point in the next response, starting immediately with the remaining file content without re-issuing the <file> tag.
5. After generating ALL the files requested and ensuring the implementation is complete, you MUST include the comment <!-- all files emitted --> on a new line AFTER the last </file> tag.
6. If the initial request can be fulfilled with a single response containing all files, include <!-- all files emitted --> at the end of that single response.
7. If no files are generated in a response (e.g., asking a question or providing an explanation), do NOT include the <!-- all files emitted --> comment.

Example of generating two files and finishing:
<file name="main.go">
package main

import "fmt"

func main() {
    fmt.Println("Hello, world!")
}
</file>
<file name="helper.go">
package main

// Helper function
func Helper() string {
	return "I'm helping!"
}
</file>
<!-- all files emitted -->

Example of a file split across responses:

Response 1:
<file name="long_file.txt">This is the first part of a very long file.
It continues on this line.

Response 2 (after being prompted to continue):
And it resumes here, directly continuing the content.
This is the final line.
</file>
<!-- all files emitted -->

If you need to generate more files in a subsequent response after finishing one, simply output the next set of <file>...</file> tags without the completion comment. Only add <!-- all files emitted --> when everything is truly finished.
`
}

// Run executes the FileCollectionAgent. It iteratively calls the LLM,
// extracting files based on XML tags until the LLM signals completion
// or the maximum iterations are reached.
// It returns a summary string of the files generated.
func (a *FileCollectionAgent) Run(ctx context.Context, input string) (string, error) {
	ctx, span := a.tracer.StartSpan(ctx, "FileCollectionAgent.Run")
	defer span.End()

	// Reset files map and extractor state for this run
	a.files = make(map[string]string)
	a.extractor = NewFileExtractor() // Ensure clean state for each run

	// Initialize conversation
	messages := []*conversation.Message{
		conversation.NewChatMessage(conversation.RoleSystem, a.buildSystemPrompt()),
		conversation.NewChatMessage(conversation.RoleUser, input),
	}

	// Iterate until max iterations or completion
	for i := 0; i < a.maxIter; i++ {
		responseMsg, err := a.llm.Generate(ctx, messages)
		if err != nil {
			return "", fmt.Errorf("LLM generation failed on iteration %d: %w", i, err)
		}

		response := responseMsg.Content.String()

		// Extract completed files from the current response using the extractor
		newCompleteFiles := a.extractor.Extract(response)

		// Add newly completed files to our final collection
		for name, content := range newCompleteFiles {
			a.files[name] = content // Overwrite if file is generated again (atomic update)
		}

		// Log the files completed in this iteration
		if len(newCompleteFiles) > 0 {
			a.tracer.LogEvent(ctx, types.Event{
				Type: "files_completed_in_iteration",
				Data: map[string]interface{}{
					"iteration": i,
					"files":     maps.Keys(newCompleteFiles), // Log names of files completed now
				},
				Timestamp: 0, // Will be set by the tracer
			})
		}

		// Check if the LLM signaled completion
		if strings.Contains(response, "<!-- all files emitted -->") {
			a.tracer.LogEvent(ctx, types.Event{
				Type:      "explicit_completion_detected",
				Data:      fmt.Sprintf("Stopping after iteration %d due to completion marker", i),
				Timestamp: 0,
			})
			break // Exit the loop
		}

		// Decide whether to continue based *only* on max iterations and explicit completion marker.
		// We always add the response and ask to continue if the marker isn't present,
		// even if no *new complete* files were extracted in this step,
		// because a file might still be in progress.

		// Add the LLM's response to the conversation history
		messages = append(messages, responseMsg)

		// Ask the LLM to continue generating files if needed
		// This prompt encourages the LLM to remember the context and finish the task.
		continuePrompt := "Continue generating the required files or content. If you were in the middle of generating a file, continue exactly where you left off. Remember to include '<!-- all files emitted -->' ONLY when all files are finished."
		// If a file is currently incomplete, give a more specific prompt
		if a.extractor.incompleteFileName != "" {
			continuePrompt = fmt.Sprintf("Continue generating the content for file '%s' exactly where you left off. Remember to include '<!-- all files emitted -->' ONLY when all files are finished.", a.extractor.incompleteFileName)
		}

		messages = append(messages, conversation.NewChatMessage(
			conversation.RoleUser,
			continuePrompt,
		))

		// Check if max iterations reached after this loop
		if i == a.maxIter-1 {
			a.tracer.LogEvent(ctx, types.Event{
				Type:      "max_iterations_reached",
				Data:      fmt.Sprintf("Stopping after %d iterations", a.maxIter),
				Timestamp: 0,
			})
			// Check if a file was still incomplete at max iterations
			if a.extractor.incompleteFileName != "" {
				a.tracer.LogEvent(ctx, types.Event{
					Type:      "warning_incomplete_file_at_max_iter",
					Data:      fmt.Sprintf("File '%s' might be incomplete.", a.extractor.incompleteFileName),
					Timestamp: 0,
				})
				// Store the potentially incomplete file content as is
				a.files[a.extractor.incompleteFileName] = strings.TrimSpace(a.extractor.incompleteFileContent.String()) + "\n... (truncated due to max iterations)"
			}
		}
	}

	// Build a summary string of the generated files
	var summary strings.Builder
	summary.WriteString(fmt.Sprintf("Agent finished. Collected %d files:\n", len(a.files)))
	if len(a.files) > 0 {
		fileNames := maps.Keys(a.files)
		for _, name := range fileNames {
			summary.WriteString(fmt.Sprintf("- %s\n", name))
		}
	} else {
		summary.WriteString("No files were collected.\n")
	}

	// Check if the last file might have been incomplete even if completion marker was found (edge case)
	if strings.Contains(messages[len(messages)-1].Content.String(), "<!-- all files emitted -->") && a.extractor.incompleteFileName != "" {
		a.tracer.LogEvent(ctx, types.Event{
			Type:      "warning_incomplete_file_despite_marker",
			Data:      fmt.Sprintf("Completion marker found, but extractor state indicates file '%s' might be incomplete. Storing partial content.", a.extractor.incompleteFileName),
			Timestamp: 0,
		})
		// Store the potentially incomplete file content
		a.files[a.extractor.incompleteFileName] = strings.TrimSpace(a.extractor.incompleteFileContent.String()) + "\n... (potentially truncated despite completion marker)"
	}

	return summary.String(), nil
}

// Ensure FileCollectionAgent implements the main Agent interface (Run method)
// Note: BaseAgent provides AddTool and SetMemory. The Run method here returns
// a summary string.
var _ Agent = (*FileCollectionAgent)(nil)
