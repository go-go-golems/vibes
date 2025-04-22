# Go LLM Frameworks Research

## natexcvi/go-llm

[GitHub Repository](https://github.com/natexcvi/go-llm)

### Overview
A framework for building LLM-based agents and integrating them into larger applications. It aims to abstract away much of the plumbing (free text to structured data, contextual memory, tool wrapping, retry logic, etc.) so developers can focus on the business logic of their agent.

### Key Features
1. **Agent System**: Implements a Chain-of-Thought agent pattern
2. **Memory Systems**:
   - `BufferMemory` - provides each step of the agent with a fixed buffer of recent messages
   - `SummarisedMemory` - provides each step with a summary of the conversation history

3. **Tool Integration**:
   - `PythonREPL` - allows agents to execute Python code
   - `IsolatedPythonREPL` - executes Python code in a Docker container
   - `BashTerminal` - allows agents to execute bash commands
   - `GoogleSearch` - enables web search capabilities
   - `WebpageSummary` - LLM-based tool for webpage summarization
   - `WolframAlpha` - for querying WolframAlpha's API
   - `KeyValueStore` - for storing and retrieving information
   - `AskUser` - enables agent-human interaction
   - `JSONAutoFixer` - fixes invalid JSON using a separate LLM chain
   - `GenericAgentTool` - allows an agent to run another agent

4. **LLM Engine Connectors**: Currently supports OpenAI's GPT chat completion API

5. **Function Calling**: Supports OpenAI's function call interface

### Architecture
The framework follows a task-based architecture where agents are created to solve specific tasks. Each task has:
- A description
- Examples (for few-shot learning)
- Input/output schemas
- Tools that the agent can use

### Code Structure
- `/agents` - Agent implementations
- `/engines` - LLM engine connectors
- `/evaluation` - Evaluation utilities
- `/memory` - Memory system implementations
- `/prebuilt` - Pre-built components
- `/tools` - Tool implementations
- `/go-llm-cli` - Command-line interface

### Example Usage
The framework allows defining structured tasks with input/output types, providing examples, and configuring the agent with specific tools and memory systems.

## cloudwego/eino

[GitHub Repository](https://github.com/cloudwego/eino)

### Overview
Eino (pronounced similarly to "I know") aims to be the ultimate LLM application development framework in Golang. It draws inspiration from frameworks like LangChain and LlamaIndex, focusing on simplicity, scalability, reliability, and effectiveness while aligning with Golang programming conventions.

### Key Features
1. **Component Abstractions**: Carefully curated abstractions and implementations that can be easily reused and combined
   - ChatModel, Tool, ChatTemplate, Retriever, Document Loader, Lambda, etc.
   - Each component has defined Input/Output Types, Option types, and appropriate streaming paradigms

2. **Composition Framework**: Handles type checking, stream processing, concurrency management, aspect injection, and option assignment
   - Two API sets for orchestration:
     - Chain API: Simple chained directed graph that can only go forward
     - Graph API: Cyclic or Acyclic directed graph with more power and flexibility

3. **Streaming Support**: Built-in handling of streaming responses from LLMs

4. **Concurrency Management**: Thread-safe state handling for complex agent workflows

5. **ReAct Agent Implementation**: Complete implementation using graph orchestration
   - Type checking at compile time
   - Stream processing for message concatenation
   - Concurrency-safe state handling
   - Aspect injection for callbacks
   - Flexible option assignment

### Architecture
Eino uses a component-based architecture where different components can be composed together using either Chain or Graph APIs. Components can be nested to capture complex business logic while remaining transparent from the outside.

### Code Structure
- `/callbacks` - Callback handling
- `/components` - Core component implementations
- `/compose` - Composition framework
- `/flow` - Pre-built flows including ReAct agent
- `/internal` - Internal utilities
- `/schema` - Schema definitions

### Example Usage
```go
// Simple chain example
chain, _ := NewChain[map[string]any, *Message]().
           AppendChatTemplate(prompt).
           AppendChatModel(model).
           Compile(ctx)

chain.Invoke(ctx, map[string]any{"query": "what's your name?"})

// Graph example with tool execution
graph := NewGraph[map[string]any, *schema.Message]()
_ = graph.AddChatTemplateNode("node_template", chatTpl)
_ = graph.AddChatModelNode("node_model", chatModel)
_ = graph.AddToolsNode("node_tools", toolsNode)
_ = graph.AddLambdaNode("node_converter", takeOne)

_ = graph.AddEdge(START, "node_template")
_ = graph.AddEdge("node_template", "node_model")
_ = graph.AddBranch("node_model", branch)
_ = graph.AddEdge("node_tools", "node_converter")
_ = graph.AddEdge("node_converter", END)

compiledGraph, err := graph.Compile(ctx)
out, err := r.Invoke(ctx, map[string]any{"query":"Beijing's weather this weekend"})
```
