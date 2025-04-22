import { useEffect, useState } from 'react';
import { BlackboardState, BlackboardEvent } from '../types';

export const useBlackboardState = () => {
  const [state, setState] = useState<BlackboardState | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [cycle, setCycle] = useState(0);

  useEffect(() => {
    // Fetch initial state
    const fetchInitialState = async () => {
      try {
        const response = await fetch('/api/blackboard');
        if (!response.ok) {
          throw new Error(`Failed to fetch blackboard state: ${response.statusText}`);
        }
        const data = await response.json();
        setState(data);
        setLoading(false);
      } catch (err) {
        setError(err instanceof Error ? err.message : 'Unknown error');
        setLoading(false);
      }
    };

    fetchInitialState();

    // Set up SSE connection for real-time updates
    const eventSource = new EventSource('/events');
    
    eventSource.onmessage = (event) => {
      try {
        const eventData = JSON.parse(event.data) as BlackboardEvent;
        
        // Handle different event types
        switch (eventData.type) {
          case 'initial_state':
            setState(eventData.data);
            break;
          
          case 'hypothesis_added':
          case 'hypothesis_updated':
          case 'hypothesis_locked':
          case 'hypothesis_unlocked':
            if (state && eventData.data) {
              const level = eventData.level?.toLowerCase();
              if (level && level in state) {
                const updatedHypotheses = [...state[level as keyof typeof state] as any[]];
                const index = updatedHypotheses.findIndex(h => h.id === eventData.hypothesis_id);
                
                if (index !== -1) {
                  updatedHypotheses[index] = eventData.data;
                } else {
                  updatedHypotheses.push(eventData.data);
                }
                
                setState(prev => prev ? {
                  ...prev,
                  [level]: updatedHypotheses
                } : null);
              }
            }
            break;
          
          case 'knowledge_source_updated':
            if (state && eventData.data) {
              const updatedSources = [...state.knowledge_sources];
              const index = updatedSources.findIndex(ks => ks.id === eventData.knowledge_source);
              
              if (index !== -1) {
                updatedSources[index] = eventData.data;
                setState(prev => prev ? {
                  ...prev,
                  knowledge_sources: updatedSources
                } : null);
              }
            }
            break;
          
          case 'activity_log_added':
            if (state && eventData.data) {
              setState(prev => prev ? {
                ...prev,
                activity_log: [eventData.data, ...prev.activity_log.slice(0, 9)]
              } : null);
            }
            break;
          
          case 'focus_updated':
            if (state && eventData.level) {
              setState(prev => prev ? {
                ...prev,
                focus: {
                  ...prev.focus,
                  current_focus: eventData.level
                }
              } : null);
            }
            break;
          
          case 'bid_added':
            if (state && eventData.data) {
              setState(prev => prev ? {
                ...prev,
                focus: {
                  ...prev.focus,
                  bid_queue: [...prev.focus.bid_queue, eventData.data]
                }
              } : null);
            }
            break;
          
          case 'bid_removed':
            if (state && eventData.data) {
              const bidId = eventData.data;
              setState(prev => prev ? {
                ...prev,
                focus: {
                  ...prev.focus,
                  bid_queue: prev.focus.bid_queue.filter(bid => bid.id !== bidId)
                }
              } : null);
            }
            break;
          
          case 'cycle_incremented':
            if (eventData.data) {
              setCycle(eventData.data);
            }
            break;
          
          default:
            console.log('Unhandled event type:', eventData.type);
        }
      } catch (err) {
        console.error('Error processing event:', err);
      }
    };
    
    eventSource.onerror = () => {
      setError('EventSource connection error');
    };
    
    // Clean up
    return () => {
      eventSource.close();
    };
  }, []);

  return { state, loading, error, cycle };
};
