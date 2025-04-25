package model

import (
	"time"
)

// TTMPDocument represents a parsed TTMP document
type TTMPDocument struct {
	// Core Identity & Lifecycle
	ID        string    `yaml:"id" json:"id"`
	Title     string    `yaml:"title" json:"title"`
	Created   time.Time `yaml:"created" json:"created"`
	Updated   time.Time `yaml:"updated" json:"updated"`
	Status    string    `yaml:"status" json:"status"`
	Owner     string    `yaml:"owner" json:"owner"`
	Audience  string    `yaml:"audience" json:"audience"`
	
	// Discovery & Classification
	Tags        []string `yaml:"tags" json:"tags"`
	Category    string   `yaml:"category" json:"category"`
	Longevity   string   `yaml:"longevity" json:"longevity"`
	Keywords    string   `yaml:"keywords" json:"keywords"`
	
	// Code & Concept Links
	SourceFiles     []string `yaml:"source_files" json:"source_files"`
	TrackedFunctions []string `yaml:"tracked_functions" json:"tracked_functions"`
	Concepts        []string `yaml:"concepts" json:"concepts"`
	
	// Relationships to Other Docs
	SeeAlso    []string `yaml:"see_also" json:"see_also"`
	Predecessor string   `yaml:"predecessor" json:"predecessor"`
	Successor   string   `yaml:"successor" json:"successor"`
	Imports     []string `yaml:"imports" json:"imports"`
	
	// Revision & Governance
	SchemaVersion string   `yaml:"schema_version" json:"schema_version"`
	ApprovedBy    []string `yaml:"approved_by" json:"approved_by"`
	ReviewCycle   string   `yaml:"review_cycle" json:"review_cycle"`
	Changelog     []ChangelogEntry `yaml:"changelog" json:"changelog"`
	
	// Automation Hooks
	UpdateCommand  string `yaml:"update_command" json:"update_command"`
	NotifyOnChange string `yaml:"notify_on_change" json:"notify_on_change"`
	AutoSync       bool   `yaml:"auto_sync" json:"auto_sync"`
	
	// Content Metrics & Summaries
	WordCount int    `yaml:"word_count" json:"word_count"`
	Abstract  string `yaml:"abstract" json:"abstract"`
	TocDepth  int    `yaml:"toc_depth" json:"toc_depth"`
	
	// Execution-Time Context
	RunID  string `yaml:"run_id" json:"run_id"`
	Env    string `yaml:"env" json:"env"`
	Commit string `yaml:"commit" json:"commit"`
	
	// Document type-specific
	DocumentType string `yaml:"document_type" json:"document_type"`
	
	// The content of the document (excluding the frontmatter)
	Content string `yaml:"-" json:"content"`
	
	// Path to the file (if loaded from disk)
	FilePath string `yaml:"-" json:"file_path"`
}

// ChangelogEntry represents a single entry in a document's changelog
type ChangelogEntry struct {
	Date   time.Time `yaml:"date" json:"date"`
	Change string    `yaml:"change" json:"change"`
	Author string    `yaml:"author" json:"author"`
}