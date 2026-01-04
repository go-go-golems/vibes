package main

import (
	"encoding/binary"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"time"
)

// ClipData represents the data structure sent from the browser extension
type ClipData struct {
	Timestamp    string `json:"timestamp"`
	URL          string `json:"url"`
	Title        string `json:"title"`
	Category     string `json:"category"`
	SelectedText string `json:"selectedText"`
	Note         string `json:"note"`
	PageTitle    string `json:"pageTitle"`
	Domain       string `json:"domain"`
}

// Message represents the native messaging protocol message
type Message struct {
	Action string   `json:"action"`
	Data   ClipData `json:"data"`
}

// Response represents the response sent back to the extension
type Response struct {
	Success bool   `json:"success"`
	Error   string `json:"error,omitempty"`
	Message string `json:"message,omitempty"`
}

func main() {
	// Set up logging to a file for debugging
	logFile, err := os.OpenFile("clipper-backend.log", os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
	if err != nil {
		log.Fatalln("Failed to open log file:", err)
	}
	defer logFile.Close()
	log.SetOutput(logFile)
	
	log.Println("Clipper backend started")

	// Read messages from stdin (native messaging protocol)
	for {
		message, err := readMessage(os.Stdin)
		if err != nil {
			if err == io.EOF {
				log.Println("Input stream closed, exiting")
				break
			}
			log.Printf("Error reading message: %v", err)
			sendResponse(Response{Success: false, Error: err.Error()})
			continue
		}

		log.Printf("Received message: %s", string(message))

		var msg Message
		if err := json.Unmarshal(message, &msg); err != nil {
			log.Printf("Error unmarshaling message: %v", err)
			sendResponse(Response{Success: false, Error: "Invalid JSON message"})
			continue
		}

		switch msg.Action {
		case "saveClip":
			err := saveClip(msg.Data)
			if err != nil {
				log.Printf("Error saving clip: %v", err)
				sendResponse(Response{Success: false, Error: err.Error()})
			} else {
				log.Println("Clip saved successfully")
				sendResponse(Response{Success: true, Message: "Clip saved successfully"})
			}
		default:
			log.Printf("Unknown action: %s", msg.Action)
			sendResponse(Response{Success: false, Error: "Unknown action"})
		}
	}
}

// readMessage reads a message from the native messaging protocol
func readMessage(reader io.Reader) ([]byte, error) {
	// Read the message length (4 bytes, little-endian)
	var length uint32
	if err := binary.Read(reader, binary.LittleEndian, &length); err != nil {
		return nil, err
	}

	// Read the message content
	message := make([]byte, length)
	if _, err := io.ReadFull(reader, message); err != nil {
		return nil, err
	}

	return message, nil
}

// sendResponse sends a response using the native messaging protocol
func sendResponse(response Response) {
	responseJSON, err := json.Marshal(response)
	if err != nil {
		log.Printf("Error marshaling response: %v", err)
		return
	}

	// Write the message length (4 bytes, little-endian)
	length := uint32(len(responseJSON))
	if err := binary.Write(os.Stdout, binary.LittleEndian, length); err != nil {
		log.Printf("Error writing response length: %v", err)
		return
	}

	// Write the message content
	if _, err := os.Stdout.Write(responseJSON); err != nil {
		log.Printf("Error writing response: %v", err)
		return
	}

	os.Stdout.Sync()
}

// saveClip saves the clip data to a markdown file
func saveClip(data ClipData) error {
	// Parse timestamp
	timestamp, err := time.Parse(time.RFC3339, data.Timestamp)
	if err != nil {
		timestamp = time.Now()
	}

	// Create directory structure: clips/YYYY-MM-DD/
	dateDir := timestamp.Format("2006-01-02")
	clipsDir := filepath.Join("clips", dateDir)
	if err := os.MkdirAll(clipsDir, 0755); err != nil {
		return fmt.Errorf("failed to create clips directory: %v", err)
	}

	// Generate filename: category-sanitized-title.md
	sanitizedTitle := sanitizeFilename(data.Title)
	filename := fmt.Sprintf("%s-%s.md", data.Category, sanitizedTitle)
	filePath := filepath.Join(clipsDir, filename)

	// Generate markdown content
	content := generateMarkdown(data, timestamp)

	// Write to file
	if err := os.WriteFile(filePath, []byte(content), 0644); err != nil {
		return fmt.Errorf("failed to write clip file: %v", err)
	}

	log.Printf("Clip saved to: %s", filePath)
	return nil
}

// sanitizeFilename removes or replaces characters that are not safe for filenames
func sanitizeFilename(title string) string {
	// Replace spaces with hyphens
	title = strings.ReplaceAll(title, " ", "-")
	
	// Remove or replace unsafe characters
	reg := regexp.MustCompile(`[^a-zA-Z0-9\-_]`)
	title = reg.ReplaceAllString(title, "")
	
	// Limit length
	if len(title) > 50 {
		title = title[:50]
	}
	
	// Remove trailing hyphens
	title = strings.TrimRight(title, "-")
	
	if title == "" {
		title = "untitled"
	}
	
	return strings.ToLower(title)
}

// generateMarkdown creates the markdown content for the clip
func generateMarkdown(data ClipData, timestamp time.Time) string {
	var content strings.Builder
	
	// Header with metadata
	content.WriteString("# " + data.Title + "\n\n")
	
	// Metadata section
	content.WriteString("## Metadata\n\n")
	content.WriteString(fmt.Sprintf("- **Category**: %s\n", data.Category))
	content.WriteString(fmt.Sprintf("- **Date**: %s\n", timestamp.Format("2006-01-02 15:04:05")))
	content.WriteString(fmt.Sprintf("- **URL**: [%s](%s)\n", data.Domain, data.URL))
	content.WriteString(fmt.Sprintf("- **Page Title**: %s\n\n", data.PageTitle))
	
	// Selected text section (if any)
	if data.SelectedText != "" {
		content.WriteString("## Selected Text\n\n")
		content.WriteString("> " + strings.ReplaceAll(data.SelectedText, "\n", "\n> ") + "\n\n")
	}
	
	// Notes section (if any)
	if data.Note != "" {
		content.WriteString("## Notes\n\n")
		content.WriteString(data.Note + "\n\n")
	}
	
	// Source section
	content.WriteString("## Source\n\n")
	content.WriteString(fmt.Sprintf("Clipped from: [%s](%s)\n", data.URL, data.URL))
	content.WriteString(fmt.Sprintf("Original page title: %s\n", data.PageTitle))
	
	return content.String()
}

