import React, { useMemo } from 'react';
import { useSelector } from 'react-redux';
import ReactFlow, {
  Background,
  Controls,
  MiniMap,
  useNodesState,
  useEdgesState,
  applyEdgeChanges,
  BackgroundVariant,
  Node,
  Edge,
  EdgeChange,
} from 'reactflow';
import 'reactflow/dist/style.css';
import TaskNode from './TaskNode';

// Register custom node types
const nodeTypes = {
  taskNode: TaskNode,
};

interface TaskState {
  tasks: {
    tasks: Task[];
  }
}

interface Task {
  id: string;
  title: string;
  description: string;
  status: string;
  parentId?: string;
  children: string[];
}

const TaskNetworkFlow = () => {
  const tasks = useSelector((state: TaskState) => state.tasks.tasks);
  
  // Convert tasks to nodes and edges for React Flow
  const { nodes, edges } = useMemo(() => {
    const nodes: Node[] = tasks.map((task) => {
      // Calculate position based on task hierarchy
      // This is a simple positioning algorithm that can be improved
      const taskDepth = getTaskDepth(task.id, tasks);
      const siblingIndex = getSiblingIndex(task.id, tasks);
      
      return {
        id: task.id,
        type: 'taskNode',
        position: { 
          x: taskDepth * 250, 
          y: siblingIndex * 150 
        },
        data: { 
          label: task.title,
          description: task.description,
          status: task.status,
          id: task.id
        },
      };
    });
    
    // Create edges between parent and child tasks
    // Use a Set to track edges we've already created
    const edgeIds = new Set<string>();
    const edges: Edge[] = [];
    
    // Create all edges from parent-child relationships
    tasks.forEach((task) => {
      // Add edge from parent to this task
      if (task.parentId) {
        const edgeId = `${task.parentId}-${task.id}`;
        if (!edgeIds.has(edgeId)) {
          edges.push({
            id: edgeId,
            source: task.parentId,
            target: task.id,
            type: 'smoothstep',
            animated: task.status === 'in-progress',
            style: { stroke: getEdgeColor(task.status) },
          });
          edgeIds.add(edgeId);
        }
      }
    });
    
    console.log('Created edges:', edges.map(e => e.id));
    return { nodes, edges };
  }, [tasks]);
  
  const [flowNodes, setNodes, onNodesChange] = useNodesState(nodes);
  const [flowEdges, setEdges, onEdgesChange] = useEdgesState(edges);
  
  // Update nodes and edges when tasks change
  React.useEffect(() => {
    console.log('--- EDGE UPDATE DEBUG ---');
    console.log('Current flowEdges:', flowEdges.map(e => e.id));
    console.log('New edges from tasks:', edges.map(e => e.id));
    
    // Apply changes to nodes directly
    setNodes(nodes);
    
    // Create a simpler mechanism to handle edge updates
    // First remove edges that don't exist in new edges
    const oldEdgeIds = flowEdges.map(e => e.id);
    const newEdgeIds = edges.map(e => e.id);
    
    // Determine which edges need to be removed
    const edgesToRemove = oldEdgeIds.filter(id => !newEdgeIds.includes(id));
    console.log('Edges to remove:', edgesToRemove);
    
    // Apply removals
    if (edgesToRemove.length > 0) {
      const removeChanges: EdgeChange[] = edgesToRemove.map(id => ({
        id,
        type: 'remove',
      }));
      console.log('Applying remove changes:', removeChanges);
      setEdges(oldEdges => {
        const result = applyEdgeChanges(removeChanges, oldEdges);
        console.log('After removal:', result.map(e => e.id));
        return result;
      });
    }
    
    // Then add or update remaining edges
    const edgesToAdd: Edge[] = [];
    const edgesToUpdate: Edge[] = [];
    
    edges.forEach(edge => {
      const existingEdge = flowEdges.find(e => e.id === edge.id);
      if (!existingEdge) {
        edgesToAdd.push(edge);
      } else if (
        existingEdge.animated !== edge.animated || 
        JSON.stringify(existingEdge.style) !== JSON.stringify(edge.style)
      ) {
        edgesToUpdate.push(edge);
      }
    });
    
    console.log('Edges to add:', edgesToAdd.map(e => e.id));
    console.log('Edges to update:', edgesToUpdate.map(e => e.id));
    
    // Process additions
    if (edgesToAdd.length > 0) {
      const addChanges: EdgeChange[] = edgesToAdd.map(edge => ({
        type: 'add',
        item: edge
      }));
      console.log('Applying add changes:', addChanges.length);
      setEdges(oldEdges => {
        const result = applyEdgeChanges(addChanges, oldEdges);
        console.log('After additions:', result.map(e => e.id));
        return result;
      });
    }
    
    // Process updates (remove then add)
    edgesToUpdate.forEach(edge => {
      console.log('Updating edge:', edge.id);
      setEdges(oldEdges => {
        // Remove first
        const removeChange: EdgeChange = {
          type: 'remove',
          id: edge.id
        };
        const afterRemove = applyEdgeChanges([removeChange], oldEdges);
        
        // Then add back with new properties
        const addChange: EdgeChange = {
          type: 'add',
          item: edge
        };
        const result = applyEdgeChanges([addChange], afterRemove);
        console.log('After update of edge', edge.id, result.map(e => e.id));
        return result;
      });
    });
    
    // Final log
    setTimeout(() => {
      console.log('Final edges state:', flowEdges.map(e => e.id));
      console.log('--- END DEBUG ---');
    }, 0);
    
  }, [nodes, edges, setNodes, setEdges, flowEdges]);
  
  return (
    <div style={{ width: '100%', height: '600px' }}>
      <ReactFlow
        nodes={flowNodes}
        edges={flowEdges}
        onNodesChange={onNodesChange}
        onEdgesChange={onEdgesChange}
        nodeTypes={nodeTypes}
        fitView
      >
        <Controls />
        <MiniMap />
        <Background variant={BackgroundVariant.Dots} gap={12} size={1} />
      </ReactFlow>
    </div>
  );
};

// Helper function to get task depth in hierarchy
const getTaskDepth = (taskId: string, allTasks: Task[]): number => {
  const task = allTasks.find(t => t.id === taskId);
  if (!task || !task.parentId) {
    return 0;
  }
  return 1 + getTaskDepth(task.parentId, allTasks);
};

// Helper function to get sibling index for vertical positioning
const getSiblingIndex = (taskId: string, allTasks: Task[]): number => {
  const task = allTasks.find(t => t.id === taskId);
  if (!task) return 0;
  
  const siblings = allTasks.filter(t => 
    (t.parentId === task.parentId) || 
    (!t.parentId && !task.parentId)
  );
  
  return siblings.findIndex(s => s.id === taskId);
};

// Helper function to get edge color based on task status
const getEdgeColor = (status?: string): string => {
  switch (status) {
    case 'completed':
      return '#22c55e';
    case 'in-progress':
      return '#3b82f6';
    case 'failed':
      return '#ef4444';
    default:
      return '#94a3b8';
  }
};

export default TaskNetworkFlow;
