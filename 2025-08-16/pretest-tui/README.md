# Pretest TUI - YAML DSL Terminal Interface

A terminal user interface (TUI) application built with Go, Bubbletea, and Bubbles that implements a YAML DSL for creating interactive pretesting questions. This tool supports both multiple choice questions (MCQ) and short answer questions with features like hints, references, and detailed feedback.

## Features

- **YAML DSL Support**: Define pretests using a simple, human-readable YAML format
- **Multiple Question Types**: Support for both multiple choice and short answer questions
- **Interactive UI**: Beautiful terminal interface with keyboard navigation
- **Rich Feedback**: Detailed explanations and rationales for each answer
- **Hints System**: Progressive hint disclosure to support learning
- **References**: Academic citations and sources for each question
- **Progress Tracking**: Visual progress bar and timing information
- **Comprehensive Scoring**: Detailed results with time spent per question

## Installation

### Prerequisites

- Go 1.23.4 or later
- Terminal with color support

### Building from Source

```bash
git clone <repository-url>
cd pretest-tui
go mod tidy
go build -o pretest-tui .
```

## Usage

```bash
./pretest-tui <pretest-file.yaml>
```

### Example

```bash
./pretest-tui examples/memory-pretest.yaml
```

## YAML DSL Specification

The YAML DSL follows this structure:

```yaml
pretest:
  title: <string>            # Title of the pretest
  questions:                 # List of questions
    - id: <string>           # Unique identifier
      type: mcq|short        # Multiple choice or short answer
      prompt: <string>       # The question text
      options:               # (MCQ only)
        - id: <string>       # e.g., "A"
          text: <string>     # option text
          rationale: <string> # why this option is right or wrong
      answer: <string>       # (MCQ only) correct option id
      hints:                 # Optional hints for scaffolding
        - <string>
      references:            # Optional sources/literature
        - <string>
```

### Multiple Choice Question Example

```yaml
pretest:
  title: "Memory Section Pretest"
  questions:
    - id: q1
      type: mcq
      prompt: "Which practice schedule usually improves long-term retention?"
      options:
        - id: A
          text: "Massed practice (cramming)"
          rationale: "Feels fluent but fades quickly."
        - id: B
          text: "Spaced practice"
          rationale: "Spacing strengthens memory consolidation and retrieval cues."
        - id: C
          text: "Copying notes"
          rationale: "Shallow processing, little retrieval effort."
      answer: B
      hints:
        - "Think about how time gaps influence memory strength."
      references:
        - "Bjork, R. A. (1994). Memory and metamemory considerations."
```

### Short Answer Question Example

```yaml
pretest:
  title: "Retrieval Practice Pretest"
  questions:
    - id: q2
      type: short
      prompt: "Why can effortful retrieval be beneficial for learning?"
      hints:
        - "Compare rereading a page with recalling from memory."
      references:
        - "Karpicke & Roediger (2008). The importance of retrieval."
```

## Controls

### General Navigation
- `q` or `Ctrl+C`: Quit the application
- `Enter` or `Space`: Confirm selection/continue

### Multiple Choice Questions
- `↑`/`↓` or `k`/`j`: Navigate between options
- `Enter` or `Space`: Select the highlighted option

### Short Answer Questions
- Type your answer in the text field
- `Ctrl+Enter`: Submit your answer

### Help and References
- `h`: Show next hint (if available)
- `r`: Toggle references display

## Example Files

The `examples/` directory contains several demonstration files:

- `memory-pretest.yaml`: Simple MCQ about memory and learning
- `retrieval-pretest.yaml`: Short answer question about retrieval practice
- `learning-strategies-pretest.yaml`: MCQ about interleaving
- `comprehensive-pretest.yaml`: Mixed question types with detailed content
- `short-answer-pretest.yaml`: Multiple short answer questions

## Architecture

The application is structured into several key components:

- **models.go**: YAML DSL parsing and data structures
- **session.go**: Session state management and progress tracking
- **ui.go**: Bubbletea model and user interface logic
- **styles.go**: Lipgloss styling and visual design
- **main.go**: Application entry point and CLI handling

## Dependencies

- [Bubbletea](https://github.com/charmbracelet/bubbletea): Terminal UI framework
- [Bubbles](https://github.com/charmbracelet/bubbles): UI components
- [Lipgloss](https://github.com/charmbracelet/lipgloss): Styling and layout
- [yaml.v3](https://gopkg.in/yaml.v3): YAML parsing

## Educational Background

This tool is designed to support learning strategies based on research in cognitive psychology, particularly:

- **Desirable Difficulties** (Bjork): Making learning appropriately challenging
- **Spacing Effect**: Distributed practice over time
- **Testing Effect**: Active retrieval strengthens memory
- **Interleaving**: Mixing different types of problems
- **Metacognition**: Awareness of one's own learning process

## License

This project is open source. See LICENSE file for details.

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.

