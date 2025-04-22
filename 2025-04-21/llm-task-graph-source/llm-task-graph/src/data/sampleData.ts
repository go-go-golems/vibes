import { GraphData } from '../types/types';

export const sampleData: GraphData = {
  nodes: [
    {
      id: 'task-1',
      text: 'Analyze User Query',
      status: 'completed',
      timestamp: '2025-04-21T10:00:00Z',
      type: 'task',
      metadata: {
        description: 'Parse and understand the user query to determine required actions',
        dependencies: [],
      },
      events: [
        {
          id: 'event-1',
          type: 'user_input',
          timestamp: '2025-04-21T09:59:30Z',
          description: 'User submitted query',
          details: {
            query: 'How does photosynthesis work?',
            user_id: 'user-123',
          }
        },
        {
          id: 'event-2',
          type: 'llm_call',
          timestamp: '2025-04-21T10:00:00Z',
          description: 'Query analysis by LLM',
          details: {
            model: 'gpt-5',
            prompt_tokens: 42,
            completion_tokens: 128,
          }
        }
      ]
    },
    {
      id: 'task-2',
      text: 'Retrieve Information',
      status: 'completed',
      timestamp: '2025-04-21T10:01:30Z',
      type: 'task',
      metadata: {
        description: 'Search and retrieve relevant information about photosynthesis',
        dependencies: ['task-1'],
      },
      events: [
        {
          id: 'event-3',
          type: 'tool_call',
          timestamp: '2025-04-21T10:01:00Z',
          description: 'Search knowledge base',
          details: {
            tool: 'knowledge_search',
            query: 'photosynthesis process',
            results_count: 5,
          }
        },
        {
          id: 'event-4',
          type: 'memory_fetch',
          timestamp: '2025-04-21T10:01:15Z',
          description: 'Retrieve previous context',
          details: {
            memory_id: 'mem-456',
            relevance_score: 0.92,
          }
        }
      ]
    },
    {
      id: 'task-3',
      text: 'Synthesize Response',
      status: 'in_progress',
      timestamp: '2025-04-21T10:02:45Z',
      type: 'task',
      metadata: {
        description: 'Create comprehensive response about photosynthesis process',
        dependencies: ['task-2'],
      },
      events: [
        {
          id: 'event-5',
          type: 'llm_call',
          timestamp: '2025-04-21T10:02:45Z',
          description: 'Generate initial draft',
          details: {
            model: 'gpt-5',
            prompt_tokens: 1024,
            completion_tokens: 512,
          }
        }
      ]
    },
    {
      id: 'task-4',
      text: 'Create Visualization',
      status: 'pending',
      timestamp: '2025-04-21T10:03:00Z',
      type: 'task',
      metadata: {
        description: 'Generate diagram illustrating photosynthesis process',
        dependencies: ['task-2'],
      },
      events: []
    },
    {
      id: 'task-5',
      text: 'Format Final Answer',
      status: 'pending',
      timestamp: '2025-04-21T10:03:15Z',
      type: 'task',
      metadata: {
        description: 'Combine text explanation with visualization into final response',
        dependencies: ['task-3', 'task-4'],
      },
      events: []
    },
    {
      id: 'task-6',
      text: 'Deliver Response',
      status: 'pending',
      timestamp: '2025-04-21T10:03:30Z',
      type: 'task',
      metadata: {
        description: 'Present final answer to user',
        dependencies: ['task-5'],
      },
      events: []
    }
  ],
  edges: [
    {
      id: 'edge-1-2',
      from: 'task-1',
      to: 'task-2',
      text: 'provides context'
    },
    {
      id: 'edge-2-3',
      from: 'task-2',
      to: 'task-3',
      text: 'provides information'
    },
    {
      id: 'edge-2-4',
      from: 'task-2',
      to: 'task-4',
      text: 'provides information'
    },
    {
      id: 'edge-3-5',
      from: 'task-3',
      to: 'task-5',
      text: 'provides text'
    },
    {
      id: 'edge-4-5',
      from: 'task-4',
      to: 'task-5',
      text: 'provides visualization'
    },
    {
      id: 'edge-5-6',
      from: 'task-5',
      to: 'task-6',
      text: 'provides final answer'
    }
  ]
};
