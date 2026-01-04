export interface NodeData {
  id: string;
  text: string;
  status: 'pending' | 'in_progress' | 'completed' | 'failed';
  timestamp: string;
  type: 'task' | 'llm_call' | 'tool_call' | 'memory_fetch';
  metadata: {
    description: string;
    dependencies: string[];
    [key: string]: any;
  };
  events: Event[];
}

export interface Event {
  id: string;
  type: 'llm_call' | 'tool_call' | 'memory_fetch' | 'user_input' | 'system_event';
  timestamp: string;
  description: string;
  details: any;
}

export interface EdgeData {
  id: string;
  from: string;
  to: string;
  text?: string;
}

export interface GraphData {
  nodes: NodeData[];
  edges: EdgeData[];
}
