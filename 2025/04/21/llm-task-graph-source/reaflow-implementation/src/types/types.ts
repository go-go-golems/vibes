// src/types/types.ts

export interface NodeData {
  id: string;
  text?: string;
  width?: number;
  height?: number;
  data?: {
    type: string; // e.g., 'goal', 'subtask', 'action', etc.
    title: string;
    description?: string;
    icon?: string;
    stats?: Record<string, number>;
    showStats?: boolean;
    showError?: boolean;
  };
}

export interface NodeStyle {
  color: string;
  backgroundColor: string;
  icon: string;
}
