package main

import (
	"fmt"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"

	"github.com/fsnotify/fsnotify"
)

type FileWatcher struct {
	watcher   *fsnotify.Watcher
	watchDir  string
	gitDir    string
}

func NewFileWatcher(watchDir string) (*FileWatcher, error) {
	watcher, err := fsnotify.NewWatcher()
	if err != nil {
		return nil, fmt.Errorf("failed to create watcher: %v", err)
	}

	// Ensure the watch directory exists
	if _, err := os.Stat(watchDir); os.IsNotExist(err) {
		return nil, fmt.Errorf("watch directory does not exist: %s", watchDir)
	}

	return &FileWatcher{
		watcher:  watcher,
		watchDir: watchDir,
		gitDir:   watchDir,
	}, nil
}

func (fw *FileWatcher) Start() error {
	// Add the directory to the watcher
	err := fw.watcher.Add(fw.watchDir)
	if err != nil {
		return fmt.Errorf("failed to add directory to watcher: %v", err)
	}

	log.Printf("Started watching directory: %s", fw.watchDir)

	// Start watching for events
	go fw.watchEvents()

	return nil
}

func (fw *FileWatcher) watchEvents() {
	for {
		select {
		case event, ok := <-fw.watcher.Events:
			if !ok {
				return
			}
			fw.handleEvent(event)

		case err, ok := <-fw.watcher.Errors:
			if !ok {
				return
			}
			log.Printf("Watcher error: %v", err)
		}
	}
}

func (fw *FileWatcher) handleEvent(event fsnotify.Event) {
	// Skip hidden files and git directory
	filename := filepath.Base(event.Name)
	if strings.HasPrefix(filename, ".") {
		return
	}

	log.Printf("File event: %s %s", event.Op.String(), event.Name)

	var commitMessage string
	relPath, _ := filepath.Rel(fw.watchDir, event.Name)

	switch {
	case event.Op&fsnotify.Create == fsnotify.Create:
		commitMessage = fmt.Sprintf("Created file: %s", relPath)
		fw.gitAdd(event.Name)
	case event.Op&fsnotify.Write == fsnotify.Write:
		commitMessage = fmt.Sprintf("Modified file: %s", relPath)
		fw.gitAdd(event.Name)
	case event.Op&fsnotify.Remove == fsnotify.Remove:
		commitMessage = fmt.Sprintf("Deleted file: %s", relPath)
		fw.gitRemove(event.Name)
	case event.Op&fsnotify.Rename == fsnotify.Rename:
		commitMessage = fmt.Sprintf("Renamed file: %s", relPath)
		fw.gitAdd(event.Name)
	default:
		return // Skip other events
	}

	// Small delay to handle rapid successive events
	time.Sleep(100 * time.Millisecond)

	// Create git commit
	fw.gitCommit(commitMessage)
}

func (fw *FileWatcher) gitAdd(filename string) {
	relPath, err := filepath.Rel(fw.watchDir, filename)
	if err != nil {
		log.Printf("Error getting relative path: %v", err)
		return
	}

	cmd := exec.Command("git", "add", relPath)
	cmd.Dir = fw.gitDir
	if err := cmd.Run(); err != nil {
		log.Printf("Error adding file to git: %v", err)
	}
}

func (fw *FileWatcher) gitRemove(filename string) {
	relPath, err := filepath.Rel(fw.watchDir, filename)
	if err != nil {
		log.Printf("Error getting relative path: %v", err)
		return
	}

	cmd := exec.Command("git", "rm", "--cached", relPath)
	cmd.Dir = fw.gitDir
	if err := cmd.Run(); err != nil {
		// If file is not in git, try to add it to track the deletion
		log.Printf("File not in git index, attempting to add: %v", err)
	}
}

func (fw *FileWatcher) gitCommit(message string) {
	// Check if there are any changes to commit
	cmd := exec.Command("git", "diff", "--cached", "--quiet")
	cmd.Dir = fw.gitDir
	if err := cmd.Run(); err == nil {
		// No changes to commit
		return
	}

	// Create commit
	cmd = exec.Command("git", "commit", "-m", message)
	cmd.Dir = fw.gitDir
	output, err := cmd.CombinedOutput()
	if err != nil {
		log.Printf("Error creating git commit: %v, output: %s", err, string(output))
	} else {
		log.Printf("Created commit: %s", message)
	}
}

func (fw *FileWatcher) Stop() {
	fw.watcher.Close()
}

func main() {
	if len(os.Args) < 2 {
		log.Fatal("Usage: go run main.go <directory-to-watch>")
	}

	watchDir := os.Args[1]
	absWatchDir, err := filepath.Abs(watchDir)
	if err != nil {
		log.Fatalf("Error getting absolute path: %v", err)
	}

	watcher, err := NewFileWatcher(absWatchDir)
	if err != nil {
		log.Fatalf("Error creating file watcher: %v", err)
	}
	defer watcher.Stop()

	err = watcher.Start()
	if err != nil {
		log.Fatalf("Error starting file watcher: %v", err)
	}

	log.Println("File watcher is running. Press Ctrl+C to stop.")

	// Keep the program running
	select {}
}

