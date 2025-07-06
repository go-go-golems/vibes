# YouTube Analyzer TUI Usage Guide

## Overview

The YouTube Analyzer Terminal User Interface (TUI) provides an interactive way to analyze YouTube videos using Google's Gemini AI. The TUI features multiple screens with smooth navigation, real-time progress updates, and comprehensive results display.

## Installation and Setup

1. **Build the application:**
   ```bash
   go build ./...
   ```

2. **Obtain a Google Gemini API key:**
   - Visit the [Google AI Studio](https://makersuite.google.com/app/apikey)
   - Create or use an existing API key

## Usage

### Starting the TUI

```bash
# Basic usage
go run ./cmd/tui --api-key YOUR_API_KEY

# Or using the main command
go run . tui --api-key YOUR_API_KEY

# With additional options
go run ./cmd/tui --api-key YOUR_API_KEY --mode comprehensive --verbose
```

### TUI Screens

#### 1. URL Input Screen
- **Purpose**: Enter YouTube URL for analysis
- **Features**:
  - URL validation
  - Clear input with Ctrl+L
  - Help system with ?
- **Navigation**:
  - `Enter`: Submit URL for analysis
  - `Esc`: Quit application
  - `Ctrl+C`: Force quit

#### 2. Loading Screen
- **Purpose**: Show analysis progress
- **Features**:
  - Animated spinner
  - Progress stages display
  - Elapsed time tracking
  - Real-time status updates
- **Navigation**:
  - `Esc`: Cancel analysis and return to input
  - `Ctrl+C`: Force quit

#### 3. Results Screen
- **Purpose**: Display comprehensive analysis results
- **Features**:
  - Scrollable content
  - Organized sections (Summary, Scores, Technologies, etc.)
  - Timestamps with importance levels
  - Platform-specific recommendations
- **Navigation**:
  - `↑/↓` or `j/k`: Scroll line by line
  - `u/d`: Page up/down
  - `g/G`: Go to top/bottom
  - `n`: New analysis (return to input)
  - `Esc`: Return to input
  - `?`: Toggle help
  - `Ctrl+C`: Quit

#### 4. Error Screen
- **Purpose**: Handle and display errors
- **Features**:
  - Error message display
  - Troubleshooting tips
  - Retry options
- **Navigation**:
  - `n/r`: New analysis/retry
  - `Esc`: Return to input
  - `Ctrl+C`: Quit

### Keyboard Shortcuts

#### Global Shortcuts
- `Ctrl+C`: Force quit application
- `?`: Toggle help display
- `Esc`: Go back or quit

#### Navigation
- `↑/↓` or `j/k`: Move up/down
- `←/→` or `h/l`: Move left/right
- `Enter`: Confirm/submit
- `Space`: Select

#### Results Screen Specific
- `u`: Page up
- `d`: Page down
- `g`: Go to top
- `G`: Go to bottom
- `n`: New analysis

### Configuration Options

#### Command Line Flags
- `--api-key` (required): Google Gemini API key
- `--mode`: Analysis mode (`quick` or `comprehensive`)
- `--model`: Specific Gemini model to use
- `--log-level`: Logging level (`debug`, `info`, `warn`, `error`)
- `--verbose`: Enable verbose output

#### Analysis Modes
- **Quick**: Fast analysis using gemini-2.5-flash
- **Comprehensive**: Detailed analysis using gemini-2.5-pro

## Features

### URL Validation
The TUI validates YouTube URLs and accepts various formats:
- `https://www.youtube.com/watch?v=VIDEO_ID`
- `https://youtu.be/VIDEO_ID`
- `https://youtube.com/embed/VIDEO_ID`
- `https://youtube.com/v/VIDEO_ID`

### Results Display
The results screen organizes information into sections:

1. **Summary**: Brief overview of the video content
2. **Scores & Metrics**: Technical score, viral potential, target audience
3. **Technologies/Topics**: Identified technologies and themes
4. **Key Timestamps**: Important moments with descriptions
5. **Technical Assessment**: Code quality, accuracy, educational value
6. **Social Media Tips**: Optimization recommendations
7. **Platform Recommendations**: Platform-specific advice
8. **Raw AI Response**: Complete AI response (truncated for readability)

### Error Handling
The TUI provides comprehensive error handling with:
- Network connection issues
- Invalid API keys
- Malformed URLs
- API quota exceeded
- Video accessibility problems

### Responsive Design
- Adapts to terminal width and height
- Minimum width support (40 characters)
- Centered content layout
- Dynamic text wrapping

## Troubleshooting

### Common Issues

1. **"Invalid API key" error**
   - Verify your API key is correct
   - Check API key permissions in Google Cloud Console
   - Ensure the API key has Gemini API access enabled

2. **"Network error" or timeout**
   - Check internet connection
   - Try again with a different video
   - Verify video is publicly accessible

3. **"Video not found" error**
   - Ensure the YouTube URL is correct
   - Check if the video is public and not restricted
   - Try a different video URL

4. **TUI display issues**
   - Ensure terminal supports ANSI colors
   - Try resizing terminal window
   - Use a modern terminal emulator

### Performance Tips
- Use `quick` mode for faster analysis
- Ensure stable internet connection
- Use videos under 30 minutes for best results
- Clear terminal before starting for best display

## Examples

### Basic Analysis
```bash
# Start TUI
go run ./cmd/tui --api-key sk-your-api-key

# Enter URL: https://www.youtube.com/watch?v=dQw4w9WgXcQ
# Press Enter to analyze
# View results and navigate with arrow keys
# Press 'n' for new analysis or 'q' to quit
```

### Comprehensive Analysis
```bash
# Start with comprehensive mode
go run ./cmd/tui --api-key sk-your-api-key --mode comprehensive

# Follow same steps as basic analysis
# Results will be more detailed but take longer
```

### Debug Mode
```bash
# Enable verbose logging
go run ./cmd/tui --api-key sk-your-api-key --verbose --log-level debug

# Useful for troubleshooting issues
```

## Technical Details

### Architecture
- Built using Bubble Tea framework
- Follows M→U→V (Model→Update→View) pattern
- Modular screen-based design
- Centralized styling with Lipgloss
- Unified key binding system

### Dependencies
- Bubble Tea: TUI framework
- Lipgloss: Styling and layout
- Bubbles: UI components (textinput, viewport, spinner)
- Gemini API: AI analysis backend

### File Structure
```
pkg/ui/
├── model/          # Screen models and state management
│   ├── screen.go   # Main model and screen coordination
│   ├── input.go    # URL input screen
│   ├── loading.go  # Analysis progress screen
│   ├── results.go  # Results display screen
│   └── error.go    # Error handling screen
├── view/           # Styling and rendering utilities
│   └── styles.go   # Centralized Lipgloss styles
├── keys/           # Keyboard shortcuts and help
│   └── keys.go     # Key bindings and help system
└── bubbles/        # Custom UI component wrappers
    └── textinput.go # Enhanced text input component
```

This TUI provides a user-friendly alternative to the command-line interface while maintaining all the powerful analysis capabilities of the YouTube Analyzer.
