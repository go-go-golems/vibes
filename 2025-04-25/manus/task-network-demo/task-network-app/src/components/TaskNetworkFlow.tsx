import React, { useMemo } from 'react';
import { useSelector } from 'react-redux';
import ReactFlow, {
  Background,
  Controls,
  MiniMap,
  useNodesState,
  useEdgesState,
} from 'reactflow';
import 'reactflow/dist/style.css';
import TaskNode from './TaskNode';

// Register custom node types
const nodeTypes = {
  taskNode: TaskNode,
};

const TaskNetworkFlow = () => {
  const tasks = useSelector((state) => state.tasks.tasks);
  
  // Convert tasks to nodes and edges for React Flow
  const { nodes, edges } = useMemo(() => {
    const nodes = tasks.map((task) => {
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
    const edges = [];
    tasks.forEach((task) => {
      if (task.parentId) {
        edges.push({
          id: `${task.parentId}-${task.id}`,
          source: task.parentId,
          target: task.id,
          type: 'smoothstep',
          animated: task.status === 'in-progress',
          style: { stroke: getEdgeColor(task.status) },
        });
      }
      
      // Add edges for each child
      task.children.forEach((childId) => {
        // Only add if not already added (avoid duplicates)
        if (!edges.some(edge => edge.id === `${task.id}-${childId}`)) {
          edges.push({
            id: `${task.id}-${childId}`,
            source: task.id,
            target: childId,
            type: 'smoothstep',
            animated: tasks.find(t => t.id === childId)?.status === 'in-progress',
            style: { stroke: getEdgeColor(tasks.find(t => t.id === childId)?.status) },
          });
        }
      });
    });
    
    return { nodes, edges };
  }, [tasks]);
  
  const [flowNodes, setNodes, onNodesChange] = useNodesState(nodes);
  const [flowEdges, setEdges, onEdgesChange] = useEdgesState(edges);
  
  // Update nodes and edges when tasks change
  React.useEffect(() => {
    setNodes(nodes);
    setEdges(edges);
  }, [nodes, edges, setNodes, setEdges]);
  
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
        <Background variant="dots" gap={12} size={1} />
      </ReactFlow>
    </div>
  );
};

// Helper function to get task depth in hierarchy
const getTaskDepth = (taskId, allTasks) => {
  const task = allTasks.find(t => t.id === taskId);
  if (!task || !task.parentId) {
    return 0;
  }
  return 1 + getTaskDepth(task.parentId, allTasks);
};

// Helper function to get sibling index for vertical positioning
const getSiblingIndex = (taskId, allTasks) => {
  const task = allTasks.find(t => t.id === taskId);
  if (!task) return 0;
  
  const siblings = allTasks.filter(t => 
    (t.parentId === task.parentId) || 
    (!t.parentId && !task.parentId)
  );
  
  return siblings.findIndex(s => s.id === taskId);
};

// Helper function to get edge color based on task status
const getEdgeColor = (status) => {
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
