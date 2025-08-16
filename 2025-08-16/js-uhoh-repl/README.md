# JavaScript + Uhoh REPL

A powerful JavaScript REPL that integrates with the uhoh form DSL to create interactive terminal-based user interfaces. This project combines the go-go-goja JavaScript engine, uhoh form generation, and bobatea TUI framework to provide a unique development environment for creating and testing interactive forms through JavaScript.

## Features

- **JavaScript Execution**: Full JavaScript runtime powered by go-go-goja
- **Interactive Forms**: Create and display interactive forms using uhoh DSL
- **File Loading**: Load and execute JavaScript files from the command line or REPL
- **Multiple UI Themes**: Support for various uhoh themes (Default, Charm, Dracula, Catppuccin, Base16)
- **Comprehensive Logging**: Detailed execution logging for debugging
- **Form Field Types**: Support for input, text, select, multiselect, confirm, note, and filepicker fields

## Quick Start

### Prerequisites

- Go 1.24.3 or later
- Linux/macOS terminal environment

### Installation

1. Clone or extract the project
2. Build the application:
   ```bash
   go build -o repl ./cmd/
   ```

### Usage

#### REPL Mode
Start the interactive REPL:
```bash
./repl
```

In the REPL, you can:
- Execute JavaScript code directly
- Use `createUI(formDef)` to create interactive forms
- Use `/load <file>` to load JavaScript files

#### Direct File Execution
Execute a JavaScript file directly:
```bash
./repl examples/test_simple.js
```

## JavaScript API

### createUI(formDefinition)

Creates and displays an interactive form based on the provided form definition.

**Parameters:**
- `formDefinition` (Object): A JavaScript object defining the form structure

**Returns:**
- Object with form completion results

**Example:**
```javascript
const result = createUI({
    name: "Contact Form",
    theme: "Default",
    groups: [{
        name: "Personal Info",
        fields: [{
            type: "input",
            key: "name",
            title: "Your Name",
            value: "John Doe"
        }, {
            type: "confirm",
            key: "subscribe",
            title: "Subscribe to newsletter?",
            value: false
        }]
    }]
});
```

### loadFile(filePath)

Loads and executes a JavaScript file.

**Parameters:**
- `filePath` (String): Path to the JavaScript file

**Example:**
```javascript
loadFile("examples/simple_form.js");
```

## Form Definition Structure

Forms are defined using the uhoh DSL structure:

```javascript
{
    name: "Form Name",           // Optional form title
    theme: "Default",            // Theme: Default, Charm, Dracula, Catppuccin, Base16
    groups: [{                   // Array of field groups
        name: "Group Name",      // Optional group title
        fields: [{               // Array of form fields
            type: "input",       // Field type
            key: "field_key",    // Unique field identifier
            title: "Field Title", // Display label
            value: "default",    // Default value (optional)
            attributes: {        // Field-specific attributes (optional)
                placeholder: "Enter text...",
                char_limit: 100
            }
        }]
    }]
}
```

### Supported Field Types

#### Input Field
Single-line text input:
```javascript
{
    type: "input",
    key: "username",
    title: "Username",
    value: "default_user",
    attributes: {
        placeholder: "Enter username...",
        char_limit: 50
    }
}
```

#### Text Field
Multi-line text input:
```javascript
{
    type: "text",
    key: "description",
    title: "Description",
    attributes: {
        lines: 5,
        char_limit: 500,
        placeholder: "Enter description..."
    }
}
```

#### Select Field
Single selection dropdown:
```javascript
{
    type: "select",
    key: "category",
    title: "Category",
    options: [
        { label: "Option 1", value: "opt1" },
        { label: "Option 2", value: "opt2" }
    ],
    attributes: {
        filterable: true
    }
}
```

#### MultiSelect Field
Multiple selection field:
```javascript
{
    type: "multiselect",
    key: "features",
    title: "Select Features",
    options: [
        { label: "Feature A", value: "a" },
        { label: "Feature B", value: "b" },
        { label: "Feature C", value: "c" }
    ],
    attributes: {
        limit: 2
    }
}
```

#### Confirm Field
Yes/No confirmation:
```javascript
{
    type: "confirm",
    key: "agree",
    title: "Do you agree to the terms?",
    value: false,
    attributes: {
        affirmative: "Yes, I agree",
        negative: "No, I don't agree"
    }
}
```

#### Note Field
Informational display:
```javascript
{
    type: "note",
    title: "Important Information",
    description: "Please read this carefully before proceeding.",
    attributes: {
        show_next_button: true,
        next_label: "Continue"
    }
}
```

#### FilePicker Field
File selection:
```javascript
{
    type: "filepicker",
    key: "upload_file",
    title: "Select File",
    attributes: {
        current_directory: "/home/user",
        allowed_types: [".pdf", ".doc", ".txt"],
        show_hidden: false,
        show_size: true,
        file_allowed: true,
        dir_allowed: false
    }
}
```

## Examples

The `examples/` directory contains several demonstration files:

- `test_simple.js` - Basic form with input field
- `simple_form.js` - Contact form with multiple field types
- `product_order.js` - Product ordering form with selections
- `file_upload.js` - File upload form with file picker
- `survey_form.js` - Customer satisfaction survey
- `demo_all.js` - Comprehensive demonstration of all features

## Architecture

The application consists of several key components:

### Core Components

1. **Main Application** (`cmd/main.go`)
   - Entry point supporting both REPL and direct file execution modes
   - Command-line argument parsing

2. **JavaScript Evaluator** (`pkg/evaluator/js_uhoh_evaluator.go`)
   - go-go-goja JavaScript runtime integration
   - uhoh form generation and display
   - File loading and execution
   - Comprehensive logging

3. **REPL Model** (`cmd/repl_model.go`)
   - BubbleTea-based interactive REPL interface
   - Command history and navigation
   - Real-time JavaScript evaluation

### Integration Flow

1. JavaScript code is executed by go-go-goja runtime
2. `createUI()` calls convert JavaScript objects to YAML
3. uhoh processes YAML to create BubbleTea forms
4. Forms are displayed and handle user interaction
5. Results are returned to JavaScript context

## Logging

The application provides comprehensive logging to `repl.log`:

- JavaScript execution flow
- Form definition processing
- uhoh integration steps
- Error conditions and debugging information

## Known Limitations

1. **Validation**: Form validation rules are not yet supported in uhoh
2. **File Picker**: File picker fields may have limited functionality
3. **Form Completion**: Final value extraction logging may be incomplete

## Troubleshooting

### Common Issues

**Panic during form creation:**
- Remove validation rules from form definitions
- Ensure all required fields have proper structure

**File not found errors:**
- Check file paths are correct
- Use absolute paths when in doubt

**JavaScript errors:**
- Check syntax in JavaScript files
- Review `repl.log` for detailed error information

### Debug Mode

Enable detailed logging by checking the `repl.log` file after execution:
```bash
tail -f repl.log
```

## Development

### Building from Source

```bash
# Install dependencies
go mod tidy

# Build the application
go build -o repl ./cmd/

# Run tests
go test ./...
```

### Adding New JavaScript Functions

1. Add function to `createUIFunction()` or `loadFileFunction()` in the evaluator
2. Register the function in `NewJSUhohEvaluator()`
3. Add logging for debugging
4. Test with example files

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all examples work correctly
5. Submit a pull request

## License

This project is provided as-is for demonstration purposes.

## Dependencies

- [go-go-goja](https://github.com/go-go-golems/go-go-goja) - JavaScript engine integration
- [uhoh](https://github.com/go-go-golems/uhoh) - Form DSL and generation
- [bobatea](https://github.com/go-go-golems/bobatea) - TUI framework components
- [bubbletea](https://github.com/charmbracelet/bubbletea) - Terminal UI framework
- [huh](https://github.com/charmbracelet/huh) - Form components
- [goja](https://github.com/dop251/goja) - JavaScript runtime

