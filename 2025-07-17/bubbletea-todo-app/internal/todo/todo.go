package todo

import (
	"time"
)

// TodoItem represents a single todo item
type TodoItem struct {
	ID          int       `json:"id"`
	Text        string    `json:"text"`
	Completed   bool      `json:"completed"`
	CreatedAt   time.Time `json:"created_at"`
	CompletedAt *time.Time `json:"completed_at,omitempty"`
}

// TodoList manages a collection of todo items
type TodoList struct {
	Items      []TodoItem `json:"items"`
	nextID     int
	selectedID int
}

// NewTodoList creates a new empty todo list
func NewTodoList() *TodoList {
	return &TodoList{
		Items:      make([]TodoItem, 0),
		nextID:     1,
		selectedID: 0,
	}
}

// AddItem adds a new todo item to the list
func (tl *TodoList) AddItem(text string) {
	item := TodoItem{
		ID:        tl.nextID,
		Text:      text,
		Completed: false,
		CreatedAt: time.Now(),
	}
	tl.Items = append(tl.Items, item)
	tl.nextID++
	tl.selectedID = len(tl.Items) - 1
}

// ToggleItem toggles the completion status of an item by index
func (tl *TodoList) ToggleItem(index int) {
	if index >= 0 && index < len(tl.Items) {
		tl.Items[index].Completed = !tl.Items[index].Completed
		if tl.Items[index].Completed {
			now := time.Now()
			tl.Items[index].CompletedAt = &now
		} else {
			tl.Items[index].CompletedAt = nil
		}
	}
}

// DeleteItem removes an item by index
func (tl *TodoList) DeleteItem(index int) {
	if index >= 0 && index < len(tl.Items) {
		tl.Items = append(tl.Items[:index], tl.Items[index+1:]...)
		if tl.selectedID >= len(tl.Items) && len(tl.Items) > 0 {
			tl.selectedID = len(tl.Items) - 1
		} else if len(tl.Items) == 0 {
			tl.selectedID = 0
		}
	}
}

// GetSelectedIndex returns the currently selected item index
func (tl *TodoList) GetSelectedIndex() int {
	return tl.selectedID
}

// SetSelectedIndex sets the currently selected item index
func (tl *TodoList) SetSelectedIndex(index int) {
	if index >= 0 && index < len(tl.Items) {
		tl.selectedID = index
	}
}

// MoveSelectionUp moves the selection up
func (tl *TodoList) MoveSelectionUp() {
	if tl.selectedID > 0 {
		tl.selectedID--
	}
}

// MoveSelectionDown moves the selection down
func (tl *TodoList) MoveSelectionDown() {
	if tl.selectedID < len(tl.Items)-1 {
		tl.selectedID++
	}
}

// GetCompletedCount returns the number of completed items
func (tl *TodoList) GetCompletedCount() int {
	count := 0
	for _, item := range tl.Items {
		if item.Completed {
			count++
		}
	}
	return count
}

// GetTotalCount returns the total number of items
func (tl *TodoList) GetTotalCount() int {
	return len(tl.Items)
}

