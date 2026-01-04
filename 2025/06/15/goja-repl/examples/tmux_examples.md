# Tmux Feature Examples

This document provides examples of using the tmux integration features in the Goja JavaScript REPL.

## Basic Tmux Commands

### Starting a Tmux Session

To start a new tmux session from within the REPL:

```
/tmux start
```

This will create a new tmux session with three windows:
- `repl`: The main REPL window
- `editor`: For editing JavaScript code or files
- `logs`: For console output and logs

### Killing a Tmux Session

To kill the current tmux session:

```
/tmux kill
```

## Editor Integration

### Editing a JavaScript Variable

To edit a JavaScript variable in the editor window:

```javascript
// First, define a variable
const myFunction = `function calculateArea(radius) {
  return Math.PI * radius * radius;
}`;

// Then edit it using the edit command
/edit variable myFunction
```

This will open the editor window with the variable content. After editing and saving, you can return to the REPL and the variable will be updated.

### Editing a File

To edit a file in the editor window:

```
/edit file path/to/file.js
```

If the file doesn't exist, it will be created.

## Console Log Management

### Viewing Logs

JavaScript `console.log()` output is automatically redirected to the log window when running in tmux mode. To view the log window:

```
/log view
```

### Sending Messages to Log Window

To manually send a message to the log window:

```
/log send This is a test message
```

### Returning to REPL

After viewing logs, return to the main REPL window:

```
/log return
```

## Complete Workflow Example

Here's a complete workflow example:

```javascript
// Start tmux session
/tmux start

// Define a function
const calculateArea = function(radius) {
  console.log(`Calculating area for radius: ${radius}`);
  return Math.PI * radius * radius;
};

// Use the function
const area = calculateArea(5);
console.log(`The area is: ${area}`);

// View the logs
/log view

// Return to REPL
/log return

// Edit the function
/edit variable calculateArea

// After editing, use the updated function
const newArea = calculateArea(10);
console.log(`The new area is: ${newArea}`);
```

## Notes

- The tmux integration requires tmux to be installed on your system
- You cannot use tmux commands if you're already inside a tmux session
- Editor commands use the $EDITOR environment variable, defaulting to vim if not set
