package commands

import (
	"fmt"
	"strings"
	"time"

	"github.com/google/uuid"
	"github.com/spf13/cobra"

	"diary-cli/pkg/config"
	"diary-cli/pkg/storage"
	"diary-cli/pkg/types"
	"diary-cli/pkg/ui"
)

// NewTodoCommand creates the todo command group
func NewTodoCommand(cfg *config.Config) *cobra.Command {
	cmd := &cobra.Command{
		Use:   "todo [description]",
		Short: "Manage todos with Obsidian Tasks plugin format",
		Long: `Manage todos with Obsidian Tasks plugin format.

Examples:
  diary todo                           # Interactive todo creation
  diary todo "Review pull requests"    # Direct todo creation
  diary todo list                      # List active todos
  diary todo done <task-id>           # Mark todo as done
  diary todo edit <task-id>           # Edit existing todo`,
		RunE: func(cmd *cobra.Command, args []string) error {
			// If no arguments, run interactive mode
			if len(args) == 0 {
				return runInteractiveTodo(storage.NewMarkdownStorage(cfg), false)
			}

			// If first argument is a subcommand, let subcommands handle it
			switch args[0] {
			case "list", "done", "edit", "add":
				return cmd.Help()
			default:
				// Direct todo creation
				description := args[0]
				priority, _ := cmd.Flags().GetString("priority")
				due, _ := cmd.Flags().GetString("due")
				tags, _ := cmd.Flags().GetStringSlice("tags")

				return createTodo(cfg, description, priority, due, tags)
			}
		},
	}

	// Add flags for direct todo creation
	cmd.Flags().StringP("priority", "p", "medium", "Priority level (low, medium, high)")
	cmd.Flags().StringP("due", "d", "", "Due date (today, tomorrow, YYYY-MM-DD)")
	cmd.Flags().StringSliceP("tags", "t", []string{}, "Additional tags")

	// Add subcommands
	cmd.AddCommand(newTodoAddCommand(cfg))
	cmd.AddCommand(newTodoListCommand(cfg))
	cmd.AddCommand(newTodoDoneCommand(cfg))
	cmd.AddCommand(newTodoEditCommand(cfg))

	return cmd
}

// newTodoAddCommand creates the todo add command (default behavior)
func newTodoAddCommand(cfg *config.Config) *cobra.Command {
	var (
		priority string
		dueDate  string
		tags     string
		editor   bool
	)

	cmd := &cobra.Command{
		Use:   "add [description]",
		Short: "Add a new todo",
		Aliases: []string{"create", "new"},
		RunE: func(cmd *cobra.Command, args []string) error {
			storage := storage.NewMarkdownStorage(cfg)

			// Interactive mode if no arguments
			if len(args) == 0 {
				return runInteractiveTodo(storage, editor)
			}

			// Direct mode
			description := strings.Join(args, " ")
			return runDirectTodo(storage, description, priority, dueDate, tags, editor)
		},
	}

	cmd.Flags().StringVarP(&priority, "priority", "p", "medium", "Todo priority (high, medium, low)")
	cmd.Flags().StringVarP(&dueDate, "due", "d", "", "Due date (today, tomorrow, YYYY-MM-DD)")
	cmd.Flags().StringVarP(&tags, "tags", "t", "", "Additional tags (space-separated)")
	cmd.Flags().BoolVarP(&editor, "editor", "e", false, "Open in visual editor")

	return cmd
}

// newTodoListCommand creates the todo list command
func newTodoListCommand(cfg *config.Config) *cobra.Command {
	var (
		priority string
		dueDate  string
		showDone bool
	)

	cmd := &cobra.Command{
		Use:   "list",
		Short: "List todos",
		Aliases: []string{"ls"},
		RunE: func(cmd *cobra.Command, args []string) error {
			storage := storage.NewMarkdownStorage(cfg)
			return runTodoList(storage, priority, dueDate, showDone)
		},
	}

	cmd.Flags().StringVarP(&priority, "priority", "p", "", "Filter by priority")
	cmd.Flags().StringVarP(&dueDate, "due", "d", "", "Filter by due date")
	cmd.Flags().BoolVar(&showDone, "done", false, "Show completed todos")

	return cmd
}

// newTodoDoneCommand creates the todo done command
func newTodoDoneCommand(cfg *config.Config) *cobra.Command {
	cmd := &cobra.Command{
		Use:   "done <task-id>",
		Short: "Mark todo as done",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			storage := storage.NewMarkdownStorage(cfg)
			return runTodoDone(storage, args[0])
		},
	}

	return cmd
}

// newTodoEditCommand creates the todo edit command
func newTodoEditCommand(cfg *config.Config) *cobra.Command {
	cmd := &cobra.Command{
		Use:   "edit <task-id>",
		Short: "Edit existing todo",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			storage := storage.NewMarkdownStorage(cfg)
			return runTodoEdit(storage, args[0])
		},
	}

	return cmd
}

// runInteractiveTodo handles interactive todo creation
func runInteractiveTodo(storage *storage.MarkdownStorage, forceEditor bool) error {
	form, err := ui.ShowTodoForm()
	if err != nil {
		return fmt.Errorf("interactive form failed: %w", err)
	}

	// Parse due date if provided
	var dueDate *time.Time
	if form.DueDate != "" {
		parsed, err := parseDate(form.DueDate)
		if err != nil {
			return fmt.Errorf("invalid due date: %w", err)
		}
		dueDate = &parsed
	}

	// Parse additional tags
	var tags []string
	if form.Tags != "" {
		tags = strings.Fields(form.Tags)
	}

	// Create todo entry
	entry := &types.DiaryEntry{
		Type:     types.EntryTypeTodo,
		Content:  form.Description,
		Date:     time.Now(),
		Format:   types.FormatTask, // Always use task format for todos
		Priority: types.Priority(form.Priority),
		DueDate:  dueDate,
		Tags:     append([]string{"todo", "toProcess"}, tags...),
		TaskID:   uuid.New().String(),
	}

	// Handle editor if requested
	if form.UseEditor || forceEditor {
		if err := openEditor(entry); err != nil {
			return fmt.Errorf("editor failed: %w", err)
		}
	}

	// Add entry
	if err := storage.AddEntry(entry); err != nil {
		return fmt.Errorf("failed to add todo: %w", err)
	}

	fmt.Printf("✓ Added todo: %s (ID: %s)\n", entry.Content, entry.TaskID)
	return nil
}

// runDirectTodo handles direct todo creation
func runDirectTodo(storage *storage.MarkdownStorage, description, priority, dueDate, tags string, editor bool) error {
	// Validate priority
	if priority != "" && !types.Priority(priority).IsValid() {
		return fmt.Errorf("invalid priority: %s (valid: high, medium, low)", priority)
	}

	// Parse due date if provided
	var dueDatePtr *time.Time
	if dueDate != "" {
		parsed, err := parseDate(dueDate)
		if err != nil {
			return fmt.Errorf("invalid due date: %w", err)
		}
		dueDatePtr = &parsed
	}

	// Parse additional tags
	var tagList []string
	if tags != "" {
		tagList = strings.Fields(tags)
	}

	// Create todo entry
	entry := &types.DiaryEntry{
		Type:     types.EntryTypeTodo,
		Content:  description,
		Date:     time.Now(),
		Format:   types.FormatTask, // Always use task format for todos
		Priority: types.Priority(priority),
		DueDate:  dueDatePtr,
		Tags:     append([]string{"todo", "toProcess"}, tagList...),
		TaskID:   uuid.New().String(),
	}

	// Handle editor if requested
	if editor {
		if err := openEditor(entry); err != nil {
			return fmt.Errorf("editor failed: %w", err)
		}
	}

	// Add entry
	if err := storage.AddEntry(entry); err != nil {
		return fmt.Errorf("failed to add todo: %w", err)
	}

	fmt.Printf("✓ Added todo: %s (ID: %s)\n", entry.Content, entry.TaskID)
	return nil
}

// runTodoList handles listing todos
func runTodoList(storage *storage.MarkdownStorage, priority, dueDate string, showDone bool) error {
	// Get todos from last 30 days
	since := time.Now().AddDate(0, 0, -30)
	entries, err := storage.GetEntries(since, types.EntryTypeTodo)
	if err != nil {
		return fmt.Errorf("failed to get todos: %w", err)
	}

	// Filter todos
	var filteredTodos []*types.DiaryEntry
	for _, entry := range entries {
		// Skip completed todos unless requested
		if entry.Completed && !showDone {
			continue
		}

		// Filter by priority
		if priority != "" && string(entry.Priority) != priority {
			continue
		}

		// Filter by due date
		if dueDate != "" {
			filterDate, err := parseDate(dueDate)
			if err != nil {
				return fmt.Errorf("invalid due date filter: %w", err)
			}
			if entry.DueDate == nil || !entry.DueDate.Equal(filterDate) {
				continue
			}
		}

		filteredTodos = append(filteredTodos, entry)
	}

	// Display todos
	if len(filteredTodos) == 0 {
		fmt.Println("No todos found.")
		return nil
	}

	fmt.Printf("Found %d todo(s):\n\n", len(filteredTodos))
	for _, todo := range filteredTodos {
		status := "[ ]"
		if todo.Completed {
			status = "[x]"
		}

		fmt.Printf("%s %s", status, todo.Content)
		
		if todo.DueDate != nil {
			fmt.Printf(" 📅 %s", todo.DueDate.Format("2006-01-02"))
		}
		
		if todo.Priority != "" {
			fmt.Printf(" (Priority: %s)", todo.Priority)
		}
		
		fmt.Printf(" [ID: %s]\n", todo.TaskID)
	}

	return nil
}

// runTodoDone handles marking todos as done
func runTodoDone(storage *storage.MarkdownStorage, taskID string) error {
	// This is a simplified implementation
	// In a real implementation, you'd need to update the markdown file
	fmt.Printf("✓ Marked todo %s as done\n", taskID)
	fmt.Println("Note: File modification not yet implemented")
	return nil
}

// runTodoEdit handles editing todos
func runTodoEdit(storage *storage.MarkdownStorage, taskID string) error {
	// This is a simplified implementation
	// In a real implementation, you'd need to find and edit the todo in the markdown file
	fmt.Printf("✏️  Editing todo %s\n", taskID)
	fmt.Println("Note: Todo editing not yet implemented")
	return nil
}





// createTodo creates a new todo entry
func createTodo(cfg *config.Config, description, priority, due string, tags []string) error {
	entry := &types.DiaryEntry{
		Type:     types.EntryTypeTodo,
		Content:  description,
		Date:     time.Now(),
		Format:   types.FormatTask,
		Priority: types.Priority(priority),
		TaskID:   uuid.New().String(),
		Tags:     append([]string{"todo", "toProcess"}, tags...),
	}

	// Parse due date if provided
	if due != "" {
		dueDate, err := parseDate(due)
		if err != nil {
			return fmt.Errorf("invalid due date: %w", err)
		}
		entry.DueDate = &dueDate
	}

	storage := storage.NewMarkdownStorage(cfg)
	if err := storage.AddEntry(entry); err != nil {
		return fmt.Errorf("failed to add todo: %w", err)
	}

	fmt.Printf("✓ Added todo: %s (ID: %s)\n", description, entry.TaskID)
	return nil
}

