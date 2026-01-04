# Title Animator

A simple animated title presentation software built with Go and Charm libraries.

## Features

- Create animated titles with movement and color changes
- Define animations using a simple YAML DSL
- Export animations as GIFs
- Neon vibey styling

## YAML DSL Format

```yaml
title: "TITLE TEXT"  # The title to be displayed
duration: 3.0        # Total animation duration in seconds
size:
  width: 80          # Width of the animation canvas
  height: 20         # Height of the animation canvas
background: "#000000" # Background color (hex)
frames:
  - text: "TEXT"     # Text to display in this frame
    position:        # Position on screen
      x: 10
      y: 10
    color: "#FF00FF" # Text color (hex)
    duration: 0.5    # Duration of this frame in seconds
  # More frames...
```

## Usage

```
title-animator [command] [flags]
```

Commands:
- `play`: Play the animation in the terminal
- `export`: Export the animation as a GIF

Flags:
- `--file`: Path to the YAML animation file
- `--output`: Output file path for GIF export

---

https://manus.im/app/6MS8jSdSZh9qdUqv7zPkev
