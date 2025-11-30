# Chapter 2: Exercises

These exercises will help you practice the fundamentals of creating and querying a Cayley graph programmatically in Go.

## Exercise 2.1: Hello World with Quads

This exercise is a warm-up to ensure your Go environment is correctly set up to work with Cayley. You will create a simple program that adds a single quad and then queries it back.

### Your Task:

Write a Go program that:
1.  Creates a new in-memory Cayley graph.
2.  Adds the quad `{"hello", "says", "world", nil}`.
3.  Queries for the object of the triple where the subject is `"hello"` and the predicate is `"says"`.
4.  Prints the result to the console.

### Function Signature to Implement:

```go
package main

import (
	"fmt"
	"log"

	"github.com/cayleygraph/cayley"
	"github.com/cayleygraph/quad"
)

func main() {
	// Your code here
}
```

## Exercise 2.2: Build a Simple Social Network Graph

Now for something a bit more interesting. You will model a small social network with a few people and their relationships.

### Your Task:

Write a Go program that creates an in-memory graph representing the following social network:

*   Alice knows Bob.
*   Alice is 25 years old.
*   Bob knows Charlie.
*   Bob is a developer.
*   Charlie is 30 years old.

Use the following predicates: `knows`, `age`, `occupation`.

### Function Signature to Implement:

```go
package main

import (
	"log"

	"github.com/cayleygraph/cayley"
	"github.com/cayleygraph/quad"
)

func buildSocialGraph(store *cayley.Handle) {
	// Your code here to add the quads for the social network
}

func main() {
	store, err := cayley.NewMemoryGraph()
	if err != nil {
		log.Fatalf("Failed to create graph: %v", err)
	}
	buildSocialGraph(store)
	log.Println("Social graph built successfully!")
}
```

## Exercise 2.3: Query Relationships

Building on the previous exercise, you will now write queries to extract information from the social network graph you created.

### Your Task:

Extend the program from Exercise 2.2 to perform the following queries and print the results:

1.  **Who does Alice know?**
2.  **What is Bob's occupation?**
3.  **Who are the friends of Alice's friends?** (i.e., who does Bob know?)

### Function Signature to Implement:

```go
package main

import (
	"fmt"
	"log"

	"github.com/cayleygraph/cayley"
	"github.com/cayleygraph/quad"
)

// (buildSocialGraph function from previous exercise)

func querySocialGraph(store *cayley.Handle) {
	// Your query code here
}

func main() {
	store, err := cayley.NewMemoryGraph()
	if err != nil {
		log.Fatalf("Failed to create graph: %v", err)
	}
	buildSocialGraph(store)
	querySocialGraph(store)
}
```

Completing these exercises will give you a solid foundation in the basic mechanics of working with Cayley in Go. In the next chapter, we will delve deeper into the quad data model and explore more advanced concepts.
