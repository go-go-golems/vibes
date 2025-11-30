# Chapter 1: Exercises

These exercises are designed to get you up and running with Cayley and to give you a first taste of interacting with a graph database.

## Exercise 1.1: Install Cayley and Verify Setup

In this exercise, you will download the pre-compiled Cayley binary, start the HTTP server, and verify that it is running correctly.

### Steps:

1.  **Download Cayley:**
    Open your terminal and run the following command to download the latest version of Cayley for Linux:

    ```bash
    wget https://github.com/cayleygraph/cayley/releases/download/v0.7.7/cayley_0.7.7_linux_amd64.tar.gz
    ```

2.  **Extract the Archive:**
    Extract the downloaded file:

    ```bash
    tar -xzf cayley_0.7.7_linux_amd64.tar.gz
    ```

3.  **Run Cayley:**
    Navigate into the extracted directory and start the Cayley HTTP server:

    ```bash
    cd cayley_0.7.7_linux_amd64
    ./cayley http
    ```

4.  **Verify Setup:**
    You should see output similar to this:

    ```
    Cayley version: 0.7.7
    using backend "memstore"
    listening on 127.0.0.1:64210, web interface at http://127.0.0.1:64210
    ```

    Open your web browser and navigate to `http://127.0.0.1:64210`. You should see the Cayley web interface. Congratulations, you have successfully installed Cayley!

## Exercise 1.2: Explore Sample Data with Cayley CLI

Now that you have Cayley running, let's load some data and run a few queries from the command line.

### Steps:

1.  **Create a Data File:**
    Create a file named `people.nq` and add the following lines. This file contains quads representing a few people and their relationships.

    ```nquads
    <alice> <knows> <bob> .
    <bob> <knows> <charlie> .
    <alice> <name> "Alice" .
    <bob> <name> "Bob" .
    <charlie> <name> "Charlie" .
    ```

2.  **Load the Data:**
    From your terminal, in the same directory as your `people.nq` file, run the following command to load the data into an in-memory Cayley instance:

    ```bash
    ./cayley load -i people.nq
    ```

3.  **Start the REPL:**
    Start the Cayley REPL (Read-Eval-Print Loop) to query the data you just loaded:

    ```bash
    ./cayley repl
    ```

4.  **Run Queries:**
    Now, let's run some Gizmo queries. Type the following queries into the REPL and press Enter. The `g.V()` function selects all vertices (nodes) in the graph.

    *   **Find everyone who Alice knows:**

        ```javascript
        g.V("<alice>").out("<knows>").all()
        ```

    *   **Find the name of the person Bob knows:**

        ```javascript
        g.V("<bob>").out("<knows>").out("<name>").all()
        ```

    *   **Find who knows Charlie:**

        ```javascript
        g.V("<charlie>").in("<knows>").all()
        ```

These simple exercises demonstrate the fundamental operations of loading data and querying it in Cayley. In the next chapter, we will start writing our own Go programs to interact with Cayley programmatically.
