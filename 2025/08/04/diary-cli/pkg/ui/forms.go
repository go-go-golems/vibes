package ui

import (
	"fmt"

	"github.com/charmbracelet/huh"
	"diary-cli/pkg/types"
)

// ShowAddForm displays the interactive form for adding entries
func ShowAddForm() (*types.InteractiveForm, error) {
	form := &types.InteractiveForm{}

	err := huh.NewForm(
		huh.NewGroup(
			huh.NewSelect[string]().
				Title("Entry Type").
				Options(
					huh.NewOption("TIL (Today I Learned)", "til"),
					huh.NewOption("Thought", "thought"),
					huh.NewOption("Activity (Things I Did)", "did"),
					huh.NewOption("Link", "link"),
				).
				Value(&form.EntryType),

			huh.NewSelect[string]().
				Title("Format").
				Options(
					huh.NewOption("Default (simple markdown)", "default"),
					huh.NewOption("Markdown (enhanced with metadata)", "markdown"),
					huh.NewOption("Task (Obsidian Tasks plugin format)", "task"),
				).
				Value(&form.Format),
		),

		huh.NewGroup(
			huh.NewInput().
				Title("Entry Title (optional)").
				Value(&form.Title),

			huh.NewText().
				Title("Entry Content").
				Value(&form.Content),

			huh.NewInput().
				Title("Date (today/yesterday/YYYY-MM-DD)").
				Value(&form.Date).
				Placeholder("today"),
		),

		huh.NewGroup(
			huh.NewInput().
				Title("Subtitle slug (optional)").
				Value(&form.SubtitleSlug).
				Description("Creates a subtitle section"),

			huh.NewConfirm().
				Title("Use visual editor?").
				Value(&form.UseEditor),
		),
	).Run()

	if err != nil {
		return nil, fmt.Errorf("form cancelled or error: %w", err)
	}

	// Set defaults
	if form.Date == "" {
		form.Date = "today"
	}
	if form.Format == "" {
		form.Format = "default"
	}

	return form, nil
}

// ShowTodoForm displays the interactive form for creating todos
func ShowTodoForm() (*types.TodoForm, error) {
	form := &types.TodoForm{}

	err := huh.NewForm(
		huh.NewGroup(
			huh.NewInput().
				Title("Todo Description").
				Value(&form.Description),

			huh.NewSelect[string]().
				Title("Priority").
				Options(
					huh.NewOption("Low", "low"),
					huh.NewOption("Medium", "medium"),
					huh.NewOption("High", "high"),
				).
				Value(&form.Priority),
		),

		huh.NewGroup(
			huh.NewInput().
				Title("Due Date (optional)").
				Value(&form.DueDate).
				Placeholder("YYYY-MM-DD or 'tomorrow'"),

			huh.NewConfirm().
				Title("Use visual editor?").
				Value(&form.UseEditor),
		),
	).Run()

	if err != nil {
		return nil, fmt.Errorf("form cancelled or error: %w", err)
	}

	// Set defaults
	if form.Priority == "" {
		form.Priority = "medium"
	}

	return form, nil
}

// AppendForm holds the data for append operations
type AppendForm struct {
	SelectedIndex int
	SubtitleSlug  string
	Content       string
	UseEditor     bool
}

// ShowAppendForm displays the interactive form for appending to entries
func ShowAppendForm(entryOptions []string) (*AppendForm, error) {
	form := &AppendForm{}
	
	var selectedEntry string
	
	err := huh.NewForm(
		huh.NewGroup(
			huh.NewSelect[string]().
				Title("Select entry to append to").
				Options(func() []huh.Option[string] {
					options := make([]huh.Option[string], len(entryOptions))
					for i, entry := range entryOptions {
						options[i] = huh.NewOption(entry, entry)
					}
					return options
				}()...).
				Value(&selectedEntry),
		),
		
		huh.NewGroup(
			huh.NewInput().
				Title("Subtitle slug (optional)").
				Description("Create a new subtitle section").
				Value(&form.SubtitleSlug),
				
			huh.NewText().
				Title("Content to append").
				Value(&form.Content),
				
			huh.NewConfirm().
				Title("Use visual editor?").
				Value(&form.UseEditor),
		),
	).Run()
	
	if err != nil {
		return nil, err
	}
	
	// Find the selected entry index
	for i, option := range entryOptions {
		if option == selectedEntry {
			form.SelectedIndex = i
			break
		}
	}
	
	return form, nil
}

