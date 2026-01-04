package models

// Config represents the top-level configuration
type Config struct {
	Auth         Auth   `yaml:"auth"`
	Organization string `yaml:"organization"`
}

// Auth represents authentication configuration
type Auth struct {
	Token string `yaml:"token"`
}

// Project represents a GitHub project
type Project struct {
	Name        string   `yaml:"name"`
	ID          string   `yaml:"id,omitempty"`
	Description string   `yaml:"description,omitempty"`
	Public      bool     `yaml:"public,omitempty"`
	Columns     []Column `yaml:"columns,omitempty"`
}

// Column represents a project column (status)
type Column struct {
	Name string `yaml:"name"`
	ID   string `yaml:"id,omitempty"`
}

// Issue represents a GitHub issue
type Issue struct {
	Title     string     `yaml:"title,omitempty"`
	ID        string     `yaml:"id,omitempty"`
	Body      string     `yaml:"body,omitempty"`
	Status    string     `yaml:"status,omitempty"`
	Assignees []string   `yaml:"assignees,omitempty"`
	Labels    []Label    `yaml:"labels,omitempty"`
	SubIssues []Issue    `yaml:"sub_issues,omitempty"`
	Comments  []Comment  `yaml:"comments,omitempty"`
}

// Label represents a GitHub issue label
type Label struct {
	Name  string `yaml:"name"`
	Color string `yaml:"color,omitempty"`
}

// Comment represents a GitHub issue comment
type Comment struct {
	Body string `yaml:"body"`
}

// ProjectManagement represents the complete YAML structure
type ProjectManagement struct {
	Config  Config   `yaml:"config"`
	Project Project  `yaml:"project,omitempty"`
	Issues  []Issue  `yaml:"issues,omitempty"`
}
