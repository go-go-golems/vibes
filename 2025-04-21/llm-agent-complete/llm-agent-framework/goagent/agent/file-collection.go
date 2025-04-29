package agent

import (
	"context"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	"github.com/go-go-golems/geppetto/pkg/conversation"
	"github.com/go-go-golems/glazed/pkg/cmds/layers"
	"github.com/go-go-golems/glazed/pkg/cmds/parameters"
	"github.com/go-go-golems/glazed/pkg/middlewares"
	glazed_types "github.com/go-go-golems/glazed/pkg/types"
	"github.com/goagent/framework/goagent/llm"
	"github.com/goagent/framework/goagent/types"
	"github.com/pkg/errors"
	"golang.org/x/exp/maps"
)

// FileCollectionAgentFactory creates FileCollectionAgent instances.
type FileCollectionAgentFactory struct{}

var _ AgentFactory = &FileCollectionAgentFactory{}

const FileCollectionAgentType = "file-collection" // Define the type constant

// FileCollectionAgentSettings holds configuration for the FileCollectionAgent.
type FileCollectionAgentSettings struct {
	MaxIterations        int    `glazed.parameter:"max-iterations"`
	SaveToDisk           bool   `glazed.parameter:"save-to-disk"`
	DestinationDirectory string `glazed.parameter:"destination-directory"`
}

// NewAgent creates a new FileCollectionAgent.
func (f *FileCollectionAgentFactory) NewAgent(ctx context.Context, parsedLayers *layers.ParsedLayers, llmModel llm.LLM) (Agent, error) {
	var settings FileCollectionAgentSettings
	err := parsedLayers.InitializeStruct(FileCollectionAgentType, &settings)
	if err != nil {
		return nil, err
	}
	return NewFileCollectionAgent(llmModel, &settings), nil
}

// CreateLayers defines the Glazed parameter layers for the FileCollectionAgent.
func (f *FileCollectionAgentFactory) CreateLayers() ([]layers.ParameterLayer, error) {
	agentLayer, err := layers.NewParameterLayer(
		FileCollectionAgentType,
		"File Collection agent configuration",
		layers.WithParameterDefinitions(
			parameters.NewParameterDefinition(
				"max-iterations",
				parameters.ParameterTypeInteger,
				parameters.WithHelp("Maximum number of generation iterations"),
				parameters.WithDefault(5),
			),
			parameters.NewParameterDefinition(
				"save-to-disk",
				parameters.ParameterTypeBool,
				parameters.WithHelp("Save generated files to disk instead of printing content"),
				parameters.WithDefault(true),
			),
			parameters.NewParameterDefinition(
				"destination-directory",
				parameters.ParameterTypeString,
				parameters.WithHelp("Directory to save generated files (required if save-to-disk is true)"),
				parameters.WithDefault(""), // Default empty, validation happens at runtime
			),
		),
	)
	if err != nil {
		return nil, err
	}
	return []layers.ParameterLayer{agentLayer}, nil
}

// FileCollectionAgent implements an agent that can generate multiple files
// using XML tags to delimit them in the LLM response.
type FileCollectionAgent struct {
	*BaseAgent                              // Embed BaseAgent from goagent/agent
	extractor  *FileExtractor               // Handles file extraction logic
	files      map[string]string            // Maps filenames to their complete content accumulated across calls
	settings   *FileCollectionAgentSettings // Added settings field
}

// NewFileCollectionAgent creates a new FileCollectionAgent.
func NewFileCollectionAgent(llmModel llm.LLM, settings *FileCollectionAgentSettings) *FileCollectionAgent {
	return &FileCollectionAgent{
		BaseAgent: NewBaseAgent(llmModel, settings.MaxIterations), // Use maxIter from settings
		extractor: NewFileExtractor(),
		files:     make(map[string]string),
		settings:  settings, // Store settings
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

// buildSummary generates a string summarizing the outcome.
// If savedToDisk is true, it lists filenames and sizes.
// If savedToDisk is false, it shows filenames and their full content.
func (a *FileCollectionAgent) buildSummary(savedToDisk bool) string {
	if len(a.files) == 0 {
		return "No files were generated."
	}

	var sb strings.Builder

	if savedToDisk {
		sb.WriteString(fmt.Sprintf("Saved %d files to %s:\n", len(a.files), a.settings.DestinationDirectory))
		for name, content := range a.files {
			// Simple byte count for size
			sb.WriteString(fmt.Sprintf("- %s (%d bytes)\n", name, len(content)))
		}
	} else {
		sb.WriteString(fmt.Sprintf("Generated %d files:\n\n", len(a.files)))
		for name, content := range a.files {
			// Determine language for syntax highlighting if possible (simple check)
			lang := ""
			splitName := strings.Split(name, ".")
			if len(splitName) > 1 {
				lang = splitName[len(splitName)-1]
			}
			sb.WriteString(fmt.Sprintf("**%s**\n```%s\n", name, lang))
			sb.WriteString(content)
			sb.WriteString("\n```\n\n")
		}
	}

	return sb.String()
}

// runInternal executes the core file generation logic by interacting with the LLM.
// It populates the a.files map with the extracted files.
func (a *FileCollectionAgent) runInternal(ctx context.Context, input string) (map[string]string, error) {
	ctx, span := a.tracer.StartSpan(ctx, "FileCollectionAgent.runInternal")
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
			// Return the files collected so far along with the error
			return a.files, fmt.Errorf("LLM generation failed on iteration %d: %w", i, err)
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

	// Return the collected files map
	return a.files, nil
}

// Run executes the core file generation logic.
// If settings.SaveToDisk is true, it saves files to settings.DestinationDirectory
// and returns a summary of saved files (names and sizes).
// If settings.SaveToDisk is false, it returns a summary with full file content.
func (a *FileCollectionAgent) Run(ctx context.Context, input string) (string, error) {
	ctx, span := a.tracer.StartSpan(ctx, "FileCollectionAgent.Run")
	defer span.End()

	// Execute the LLM interaction to populate a.files
	files, runErr := a.runInternal(ctx, input)
	if runErr != nil {
		// Log the error but continue to potentially save/summarize partial results
		a.tracer.LogEvent(ctx, types.Event{
			Type: "run_internal_error",
			Data: runErr.Error(),
		})
	}

	saveErr := error(nil) // Variable to store potential saving errors
	savedToDisk := false  // Flag to indicate if saving was attempted and successful

	// Handle saving to disk if enabled
	if a.settings.SaveToDisk {
		if a.settings.DestinationDirectory == "" {
			saveErr = errors.New("--destination-directory is required when --save-to-disk is true")
		} else {
			// Create the directory if it doesn't exist
			err := os.MkdirAll(a.settings.DestinationDirectory, 0755)
			if err != nil {
				saveErr = errors.Wrapf(err, "failed to create destination directory: %s", a.settings.DestinationDirectory)
			} else {
				savedToDisk = true // Indicate saving is happening
				// Save each file
				for filename, content := range files {
					filePath := filepath.Join(a.settings.DestinationDirectory, filename)
					err := os.WriteFile(filePath, []byte(content), 0644)
					if err != nil {
						// Collect the first saving error encountered
						if saveErr == nil {
							saveErr = errors.Wrapf(err, "failed to write file: %s", filePath)
						}
						savedToDisk = false // If any file fails, mark as not fully saved
						a.tracer.LogEvent(ctx, types.Event{
							Type: "file_save_error",
							Data: map[string]interface{}{
								"filename": filePath,
								"error":    err.Error(),
							},
						})
						// Optionally, break here or try saving remaining files
					} else {
						a.tracer.LogEvent(ctx, types.Event{
							Type: "file_saved",
							Data: map[string]interface{}{
								"filename": filePath,
								"size":     len(content),
							},
						})
					}
				}
			}
		}
	}

	// Build summary based on whether files were (intended to be) saved
	summary := a.buildSummary(a.settings.SaveToDisk && saveErr == nil) // Show saved summary only if saving was intended and had no error

	// Log the final outcome
	a.tracer.LogEvent(ctx, types.Event{
		Type: "run_completed",
		Data: map[string]interface{}{
			"summary":   summary,
			"fileCount": len(a.files),
			"fileNames": maps.Keys(a.files),
			"saved":     savedToDisk,
			"saveError": nil, // Handle potential saveErr logging later
		},
	})
	if saveErr != nil {
		a.tracer.LogEvent(ctx, types.Event{
			Type: "run_completion_error",
			Data: map[string]interface{}{
				"saveError": saveErr.Error(),
			},
		})
	}

	// Prioritize returning errors: first saveErr, then runErr
	if saveErr != nil {
		return summary, saveErr // Return summary even with save error
	}
	return summary, runErr // Return summary and original LLM run error if any
}

// RunIntoWriter executes the agent and writes the summary string to the writer.
// It respects the save-to-disk logic performed by the Run method.
func (a *FileCollectionAgent) RunIntoWriter(ctx context.Context, input string, w io.Writer) error {
	ctx, span := a.tracer.StartSpan(ctx, "FileCollectionAgent.RunIntoWriter")
	defer span.End()

	summary, runErr := a.Run(ctx, input) // Call Run to get the summary and potential error

	_, writeErr := fmt.Fprintln(w, summary)
	if writeErr != nil {
		// Prioritize returning the write error if it occurs
		return errors.Wrap(writeErr, "failed to write summary")
	}

	// Return the error from the Run execution, if any
	return runErr
}

// RunIntoGlazeProcessor executes the agent and streams file data as rows
// into the Glazed processor. This method IGNORES the save-to-disk flags
// and ALWAYS outputs the full file content, as its purpose is structured data output.
func (a *FileCollectionAgent) RunIntoGlazeProcessor(
	ctx context.Context,
	input string,
	gp middlewares.Processor,
) error {
	ctx, span := a.tracer.StartSpan(ctx, "FileCollectionAgent.RunIntoGlazeProcessor")
	defer span.End()

	// Step 1: Execute the core logic via runInternal to populate a.files.
	// This bypasses the Run method's saving logic.
	files, runErr := a.runInternal(ctx, input)

	// Log if Run failed, but proceed to output any files collected before the error.
	if runErr != nil {
		a.tracer.LogEvent(ctx, types.Event{
			Type: "run_error_in_glazed_processor",
			Data: runErr.Error(),
		})
	}

	// Step 2: Process the collected files into the Glaze processor.
	for filename, content := range files { // Iterate over the files returned by runInternal
		row := glazed_types.NewRow(
			glazed_types.MRP("filename", filename),
			glazed_types.MRP("content", content),
		)
		if err := gp.AddRow(ctx, row); err != nil {
			// If adding a row fails, wrap and return the error immediately.
			// Log the underlying runErr as well if it exists.
			if runErr != nil {
				a.tracer.LogEvent(ctx, types.Event{
					Type: "add_row_failed_after_run_error",
					Data: map[string]interface{}{
						"add_row_error": err.Error(),
						"run_error":     runErr.Error(),
						"filename":      filename,
					},
				})
			}
			return errors.Wrapf(err, "failed to add row for file '%s'", filename)
		}
	}

	// Step 3: Return the original error from runInternal if it occurred.
	// If AddRow failed, that error was already returned.
	if runErr != nil {
		return errors.Wrap(runErr, "core agent run failed but partial results might have been processed")
	}
	return nil
}

// Ensure FileCollectionAgent implements the relevant agent interfaces
var _ Agent = (*FileCollectionAgent)(nil)       // Base interface
var _ WriterAgent = (*FileCollectionAgent)(nil) // For RunIntoWriter
var _ GlazedAgent = (*FileCollectionAgent)(nil) // For RunIntoGlazeProcessor
