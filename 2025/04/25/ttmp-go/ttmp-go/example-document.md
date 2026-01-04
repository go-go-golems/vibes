---
id: recursive-agent-debugging-guide
title: "Debugging Guide: Recursive Agent Architecture"
document_type: guide
longevity: long
tags: 
  - recursive
  - agent
  - debugging
created: 2025-04-07T00:00:00Z
updated: 2025-04-24T00:00:00Z
status: active
owner: "@manuel"
source_files:
  - recursive/engine.py
  - recursive/graph.py
tracked_functions:
  - GraphRunEngine.forward_one_step_not_parallel
  - AbstractNode.do_action
see_also:
  - 02-event-logging-system.md
schema_version: "1.0"
---

# Recursive Agent Debugging Guide

## Introduction

This guide provides detailed instructions for debugging the recursive agent system. It covers the key components and execution flow to help developers understand how the system works.

## Key Components

1. **Node System**: Represents tasks in a graph structure
2. **Agent System**: Handles the execution of tasks
3. **Memory System**: Manages context and state
4. **Execution Engine**: Orchestrates the entire process

## Debugging Tips

When debugging the recursive agent, it's important to understand the following:

1. Set breakpoints at critical locations
2. Monitor state transitions
3. Check task dependencies

## Code Examples

```python
# Example of setting a breakpoint in the engine
def forward_one_step_not_parallel(self, step=0):
    # Set breakpoint here to track execution
    need_next_step_node = self.get_need_next_step_node()
    if need_next_step_node:
        need_next_step_node.next_action_step()
```

## Conclusion

By following this guide, you should be able to effectively debug the recursive agent system and understand its internal workings.