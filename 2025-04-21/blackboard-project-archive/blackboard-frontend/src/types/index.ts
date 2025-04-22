export interface Hypothesis {
  id: string;
  blackboard_id: number;
  content: string;
  confidence: number;
  time_range: string;
  owner: string | null;
  locked: boolean;
  created_at: string;
}

export interface KnowledgeSource {
  id: string;
  name: string;
  status: string;
  last_action: string;
  bid_value: number;
  activation_pattern: string;
  created_at: string;
}

export interface ActivityLog {
  id: number;
  time: string;
  knowledge_source_id: string;
  action: string;
  level: string;
  created_at: string;
}

export interface FocusControl {
  id: number;
  current_focus: string;
  created_at: string;
}

export interface BidQueue {
  id: number;
  knowledge_source_id: string;
  bid_value: number;
  target: string;
  action: string;
  created_at: string;
}

export interface BlackboardState {
  phrase: Hypothesis[];
  word: Hypothesis[];
  syllable: Hypothesis[];
  segment: Hypothesis[];
  parameter: Hypothesis[];
  knowledge_sources: KnowledgeSource[];
  focus: {
    current_focus: string;
    focus_priorities: Record<string, number>;
    bid_queue: BidQueue[];
  };
  activity_log: ActivityLog[];
}

export interface BlackboardEvent {
  type: string;
  knowledge_source?: string;
  level?: string;
  hypothesis_id?: string;
  action?: string;
  data?: any;
  timestamp: string;
}
