# *Lisp (StarLisp) Programming Language Documentation

## Table of Contents
1. [Introduction](#introduction)
2. [Historical Context](#historical-context)
3. [Core Concepts](#core-concepts)
4. [Language Syntax and Features](#language-syntax-and-features)
5. [Parallel Programming Model](#parallel-programming-model)
6. [Data Structures](#data-structures)
7. [Communication Operations](#communication-operations)
8. [Reduction Operations](#reduction-operations)
9. [Mapping Operations](#mapping-operations)
10. [Comparison with Other Languages](#comparison-with-other-languages)
11. [Example Programs](#example-programs)
12. [References](#references)

## Introduction

*Lisp (pronounced "star-lisp"), also known as StarLisp, is a parallel dialect of the Lisp programming language developed by Thinking Machines Corporation for their Connection Machine parallel supercomputer. *Lisp was designed to provide a high-level programming interface for data-parallel computing, allowing programmers to express parallel algorithms in a familiar Lisp-like syntax while taking advantage of the massive parallelism offered by the Connection Machine architecture.

*Lisp extends Common Lisp with parallel data structures and operations, enabling programmers to work with collections of data distributed across thousands of processors simultaneously. This documentation provides a comprehensive overview of *Lisp, its features, programming model, and usage patterns.

## Historical Context

*Lisp has its origins in the early 1980s at the Massachusetts Institute of Technology (MIT). The original language, called URDU, was designed by Cliff Lasser in 1982 when the Connection Machine system was still in its design phase at MIT. While the hardware was being built at Thinking Machines Corporation, URDU evolved into SIMPL. Based on many users' experiences with SIMPL on the Connection Machine hardware, *Lisp emerged as the primary high-level programming language for the Connection Machine.

The Connection Machine, designed by Danny Hillis at MIT and later commercialized by Thinking Machines Corporation, was a massively parallel supercomputer that contained up to 65,536 simple processors working in parallel. Each processor had its own memory, and the processors were interconnected in a hypercube network topology. The Connection Machine was designed primarily for artificial intelligence applications and other problems that could benefit from massive parallelism.

*Lisp was implemented by Cliff Lasser and George Robertson for the hardware version, while JP Massar implemented the simulator (STARSIM) that allowed *Lisp programs to be developed and tested on conventional computers.

## Core Concepts

### Data Parallelism

*Lisp is based on the data-parallel programming model, where the same operation is performed simultaneously on multiple data elements. This is in contrast to control parallelism, where different operations are performed simultaneously. In *Lisp, data is distributed across the processors of the Connection Machine, and operations are applied to all data elements in parallel.

### Virtual Processors

The Connection Machine hardware provided a fixed number of physical processors, but *Lisp introduced the concept of virtual processors (VPs) that allowed programmers to work with a logical number of processors that could be larger than the physical count. The *Lisp runtime system automatically mapped virtual processors to physical processors, enabling programs to scale regardless of the actual hardware configuration.

### Parallel Variables (PVARs)

The fundamental data structure in *Lisp is the parallel variable, or PVAR. A PVAR represents a collection of values, one per virtual processor. Operations on PVARs are performed in parallel across all virtual processors. PVARs can hold scalar values, arrays, or more complex data structures.

## Language Syntax and Features

*Lisp extends Common Lisp syntax with parallel operations, typically denoted by an asterisk (*) prefix. Here are some of the key syntactic elements:

### Function Definitions

```lisp
(*defun function-name (parameters)
  "Optional documentation string"
  body)
```

The `*defun` macro defines a function that operates on parallel data.

### Variable Bindings

```lisp
(*let ((var1 value1)
       (var2 value2))
  body)
```

The `*let` macro creates local bindings for parallel variables.

### Conditional Execution

```lisp
(*when test
  body)

(*if test
    then-form
    else-form)
```

The `*when` and `*if` macros provide conditional execution in a parallel context.

### Sequential Execution

```lisp
(*progn
  form1
  form2
  ...)
```

The `*progn` macro evaluates forms sequentially in a parallel context.

### Assignment

```lisp
(*setf place value)
```

The `*setf` macro assigns a value to a parallel variable or a location within a parallel data structure.

## Parallel Programming Model

*Lisp follows the Single Instruction, Multiple Data (SIMD) programming model, where a single instruction stream is executed by multiple processors, each operating on different data. This model is well-suited for problems that exhibit data parallelism, where the same operation needs to be performed on many data elements.

### Processor Selection

*Lisp allows for selective activation of processors using the `*when` and `*if` constructs. Processors that do not meet the specified condition are temporarily deactivated and do not participate in the operations within the conditional block.

```lisp
(*when (> pvar-a 0)
  (*setf pvar-b (* pvar-a 2)))
```

In this example, only processors where `pvar-a` is greater than 0 will update `pvar-b`.

### Parallel Iteration

*Lisp provides constructs for parallel iteration over data:

```lisp
(*do-for-selected-processors (var start end)
  body)
```

This iterates over a range of processors, executing the body in parallel for each selected processor.

## Data Structures

### Creating Parallel Arrays

```lisp
(*make-array dimensions &key element-type initial-element)
```

The `*make-array` function creates a parallel array with the specified dimensions. Each processor holds a portion of the array.

### Accessing Array Elements

```lisp
(*aref array &rest indices)
```

The `*aref` function accesses elements of a parallel array.

### Parallel Structures

*Lisp allows for the definition of parallel structures, similar to Common Lisp structures but distributed across processors:

```lisp
(*defstruct name
  slot1
  slot2
  ...)
```

## Communication Operations

Communication operations in *Lisp allow data to be moved between processors, enabling complex parallel algorithms that require data exchange.

### Processor-to-Processor Communication

```lisp
(*pset source-pvar dest-pvar &key source-indices dest-indices)
```

The `*pset` function copies values from one parallel variable to another, optionally using the provided indices to specify the source and destination processors.

### Grid Communication

```lisp
(*news pvar direction &optional distance)
```

The `*news` function shifts values in a parallel variable in the specified direction by the given distance, implementing a nearest-neighbor communication pattern.

```lisp
(*scan pvar direction &key include-self)
```

The `*scan` function performs a scan operation (also known as a prefix sum) along the specified direction.

## Reduction Operations

Reduction operations combine values across processors to produce a single result.

```lisp
(*sum pvar)
(*max pvar)
(*min pvar)
(*and pvar)
(*or pvar)
```

These functions compute the sum, maximum, minimum, logical AND, and logical OR of all values in a parallel variable, respectively.

## Mapping Operations

Mapping operations apply a function to each element of one or more parallel variables.

```lisp
(*map function pvar &rest more-pvars)
```

The `*map` function applies the specified function element-wise to the given parallel variables.

## Comparison with Other Languages

### *Lisp vs. Common Lisp

*Lisp extends Common Lisp with parallel operations and data structures. While Common Lisp is designed for sequential computation, *Lisp is specifically tailored for data-parallel computation on the Connection Machine.

### *Lisp vs. Other Parallel Languages

Compared to other parallel programming languages of its era, such as Fortran 90 with array operations or C* (another language for the Connection Machine), *Lisp provided a higher-level, more expressive interface for parallel programming. Its Lisp heritage gave it powerful symbolic processing capabilities that were not present in more numerically-oriented languages.

## Example Programs

### Matrix Multiplication

```lisp
(defun matrix-multiply (matrix-a matrix-b rows-a cols-a cols-b)
  "Multiply matrix-a (rows-a x cols-a) by matrix-b (cols-a x cols-b)"
  (let ((result (*make-array (list rows-a cols-b) :initial-element 0)))
    ;; For each element in the result matrix
    (dotimes (i rows-a)
      (dotimes (j cols-b)
        (let ((sum 0))
          ;; Compute the dot product of row i from A and column j from B
          (dotimes (k cols-a)
            (let ((a-index (+ k (* i cols-a)))
                  (b-index (+ j (* k cols-b))))
              (incf sum (* (aref (pvar-data matrix-a) a-index)
                           (aref (pvar-data matrix-b) b-index)))))
          ;; Store the result
          (setf (aref (pvar-data result) (+ j (* i cols-b))) sum))))
    result))
```

### Image Processing

```lisp
(defun convolve-image (image kernel)
  "Apply a convolution kernel to an image"
  (let* ((width (first (*current-grid-size)))
         (height (second (*current-grid-size)))
         (result (*make-array (list width height) :initial-element 0)))
    ;; For each pixel in the image
    (dotimes (y height)
      (dotimes (x width)
        ;; Apply the kernel
        (let ((sum 0))
          (dotimes (ky 3)
            (dotimes (kx 3)
              (let ((ix (mod (+ x kx -1) width))
                    (iy (mod (+ y ky -1) height))
                    (k (+ kx (* ky 3))))
                (incf sum (* (aref (pvar-data image) (+ ix (* iy width)))
                             (aref (pvar-data kernel) k))))))
          ;; Store the result
          (setf (aref (pvar-data result) (+ x (* y width))) sum))))
    result))
```

## References

1. Lasser, C., & Omohundro, S. (1986). The Essential *Lisp Manual: Release 1, Revision 3. Technical Report 86.15, Thinking Machines Corporation.
2. Thinking Machines Corporation. (1988). *Lisp Reference Manual. Version 5.0.
3. Thinking Machines Corporation. (1988). *Lisp Compiler Guide. Version 5.0.
4. Thinking Machines Corporation. (1990). *Lisp Dictionary. Version 5.2.
5. Steele, G. L., & Hillis, W. D. (1986). Connection Machine Lisp: fine-grained parallel symbolic processing. Proceedings of the 1986 ACM conference on LISP and functional programming, 279-297.
6. Massar, J.P. (1988). STARSIM: Thinking Machines' *Lisp Simulator. Version F19.0.
