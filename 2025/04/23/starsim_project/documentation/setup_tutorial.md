# Setting Up and Using the *Lisp (StarLisp) Simulator

This tutorial will guide you through the process of setting up and using our custom *Lisp (StarLisp) simulator. The simulator provides a simplified implementation of the *Lisp programming language that was originally developed for the Connection Machine by Thinking Machines Corporation.

## Table of Contents

1. [System Requirements](#system-requirements)
2. [Installation](#installation)
3. [Basic Usage](#basic-usage)
4. [Running the Matrix Multiplication Demo](#running-the-matrix-multiplication-demo)
5. [Creating Your Own *Lisp Programs](#creating-your-own-lisp-programs)
6. [Troubleshooting](#troubleshooting)
7. [Additional Resources](#additional-resources)

## System Requirements

To run the *Lisp simulator, you'll need:

- A Linux, macOS, or Windows system
- Steel Bank Common Lisp (SBCL) or another Common Lisp implementation
- Basic familiarity with Common Lisp and/or Lisp programming

## Installation

### Step 1: Install Steel Bank Common Lisp (SBCL)

#### On Ubuntu/Debian:
```bash
sudo apt-get update
sudo apt-get install -y sbcl
```

#### On macOS (using Homebrew):
```bash
brew install sbcl
```

#### On Windows:
Download the installer from the [SBCL website](http://www.sbcl.org/platform-table.html) and follow the installation instructions.

### Step 2: Download the Simulator Files

You can either download the zip file containing all the necessary files or clone the repository if available.

#### Option 1: Download the zip file
Extract the zip file to a directory of your choice:
```bash
unzip starsim_project.zip -d ~/starsim_project
```

#### Option 2: Manual setup
Create the necessary directory structure and copy the files:
```bash
mkdir -p ~/starsim_project/{simulator/starsim,demo,documentation}
# Copy the starsim.lisp file to ~/starsim_project/simulator/starsim/
# Copy the matrix_multiply.lisp file to ~/starsim_project/demo/
# Copy the documentation files to ~/starsim_project/documentation/
```

### Step 3: Verify the Installation

To verify that the simulator is installed correctly, run the following command:
```bash
cd ~/starsim_project/simulator
sbcl --load starsim/starsim.lisp --eval "(quit)"
```

You should see output similar to:
```
Initializing *Lisp simulator with grid dimensions: (8 8)
STARSIM: *Lisp simulator loaded successfully.
Default grid size: (8 8)
Use (starsim:with-simulator (:dimensions '(x y))) to change grid size.
```

## Basic Usage

The *Lisp simulator provides a set of functions and macros that mimic the behavior of the original *Lisp language. Here's how to use the simulator in your Common Lisp environment:

### Loading the Simulator

To load the simulator in your Common Lisp environment:
```lisp
(load "/path/to/starsim_project/simulator/starsim/starsim.lisp")
```

### Setting the Grid Size

The simulator uses a grid of virtual processors. You can set the grid size using the `*set-grid-size` function:
```lisp
(starsim:*set-grid-size '(16 16))  ; Sets a 16x16 grid
```

### Creating Parallel Variables (PVARs)

Parallel variables (PVARs) are the fundamental data structure in *Lisp. You can create a PVAR using the `*make-array` function:
```lisp
(defvar *my-pvar* (starsim:*make-array '(10 10) :initial-element 0))
```

### Accessing and Modifying PVARs

You can access and modify elements of a PVAR using the `*aref` and `*setf` functions:
```lisp
;; Set the value at position (2, 3) to 42
(starsim:*setf *my-pvar* 42 2 3)

;; Get the value at position (2, 3)
(starsim:*aref *my-pvar* 2 3)  ; Returns 42
```

### Parallel Operations

The simulator provides various parallel operations:

#### Reduction Operations
```lisp
(starsim:*sum *my-pvar*)  ; Sum of all elements
(starsim:*max *my-pvar*)  ; Maximum value
(starsim:*min *my-pvar*)  ; Minimum value
(starsim:*and *my-pvar*)  ; Logical AND of all elements
(starsim:*or *my-pvar*)   ; Logical OR of all elements
```

#### Communication Operations
```lisp
;; Copy values from one PVAR to another
(starsim:*pset *source-pvar* *dest-pvar*)

;; Shift values in a PVAR in the specified direction
(starsim:*news *my-pvar* '(1 0))  ; Shift right by 1
```

#### Mapping Operations
```lisp
;; Apply a function to each element of a PVAR
(starsim:*map #'1+ *my-pvar*)  ; Increment each element by 1
```

## Running the Matrix Multiplication Demo

The project includes a matrix multiplication demo that demonstrates the use of the *Lisp simulator. To run the demo:

```bash
cd ~/starsim_project/demo
sbcl --load matrix_multiply.lisp --eval "(matrix-demo:run-demo)" --eval "(quit)"
```

You should see output showing the multiplication of two matrices and some *Lisp operations on the result.

Alternatively, you can load the demo in your Common Lisp environment and run it interactively:
```lisp
(load "/path/to/starsim_project/demo/matrix_multiply.lisp")
(matrix-demo:run-demo)
```

## Creating Your Own *Lisp Programs

To create your own *Lisp programs using the simulator, follow these steps:

### Step 1: Create a new Lisp file

Create a new Lisp file for your program, e.g., `my_program.lisp`.

### Step 2: Load the simulator

At the beginning of your file, load the simulator:
```lisp
(require :starsim "/path/to/starsim_project/simulator/starsim/starsim")
```

### Step 3: Define your package

Define a package for your program:
```lisp
(defpackage :my-program
  (:use :common-lisp :starsim)
  (:export :run-program))

(in-package :my-program)
```

### Step 4: Write your program

Write your *Lisp program using the functions and macros provided by the simulator. Here's a simple example that creates a PVAR, fills it with values, and computes the sum:

```lisp
(defun run-program ()
  (format t "~%Running my *Lisp program~%")
  
  (starsim:with-simulator (:dimensions '(4 4))
    (let ((my-pvar (starsim:*make-array '(4 4) :initial-element 0)))
      
      ;; Fill the PVAR with values
      (dotimes (i 4)
        (dotimes (j 4)
          (starsim:*setf my-pvar (+ i j) i j)))
      
      ;; Print the PVAR
      (format t "~%My PVAR:~%")
      (starsim:*print my-pvar)
      
      ;; Compute and print the sum
      (format t "~%Sum of all elements: ~A~%" (starsim:*sum my-pvar))
      
      ;; Return the PVAR
      my-pvar)))
```

### Step 5: Run your program

Run your program using SBCL:
```bash
sbcl --load my_program.lisp --eval "(my-program:run-program)" --eval "(quit)"
```

## Troubleshooting

### Common Issues

#### Issue: "Package STARSIM not found"
Make sure you've loaded the simulator correctly using the correct path.

#### Issue: Style warnings during compilation
The simulator may generate some style warnings during compilation. These are generally harmless and don't affect the functionality of the simulator.

#### Issue: "Undefined function" errors
Make sure you're using the correct package prefix for functions. Most functions in the simulator are in the `starsim` package.

### Getting Help

If you encounter any issues not covered in this tutorial, please refer to the documentation or contact the project maintainers.

## Additional Resources

- [*Lisp Documentation](../documentation/star_lisp_documentation.md): Comprehensive documentation on the *Lisp language and programming model.
- [Original *Lisp Reference Manual](https://bitsavers.org/pdf/thinkingMachines/CM2/starLisp_Reference_Version_5.0_Sep1988.pdf): The original reference manual for *Lisp from Thinking Machines Corporation.
- [Connection Machine Documentation](https://bitsavers.org/pdf/thinkingMachines/CM2/): Documentation for the Connection Machine, the hardware platform for which *Lisp was originally developed.

---

This tutorial should help you get started with the *Lisp simulator. Happy parallel programming!
