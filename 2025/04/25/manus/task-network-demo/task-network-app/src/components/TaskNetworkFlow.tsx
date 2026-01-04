import React, { useMemo, useCallback, useState } from 'react';
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
  Panel,
  useReactFlow,
  ReactFlowProvider,
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

// Layout types
type LayoutType = 'default' | 'horizontal' | 'vertical' | 'radial' | 'force';

// Inner component that uses React Flow hooks
const TaskNetworkFlowInner = () => {
  const tasks = useSelector((state: TaskState) => state.tasks.tasks);
  const reactFlowInstance = useReactFlow();
  
  // Track current layout
  const [currentLayout, setCurrentLayout] = useState<LayoutType>('default');
  
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

  // Get node positions based on layout type
  const calculateNodePositions = useCallback((nodesToLayout: Node[], existingNodes: Node[] = []) => {
    // Create a copy of the nodes to layout
    const newNodes = [...nodesToLayout];
    
    if (currentLayout === 'default') {
      return newNodes;
    }
    
    if (currentLayout === 'horizontal') {
      // Group nodes by their depth level
      const nodesByLevel: {[key: number]: Node[]} = {};
      
      newNodes.forEach(node => {
        const taskDepth = getTaskDepth(node.id, tasks);
        if (!nodesByLevel[taskDepth]) {
          nodesByLevel[taskDepth] = [];
        }
        nodesByLevel[taskDepth].push(node);
      });
      
      // Position nodes in a horizontal tree layout
      Object.keys(nodesByLevel).forEach((level, levelIndex) => {
        const levelNodes = nodesByLevel[parseInt(level)];
        const levelCount = levelNodes.length;
        
        levelNodes.forEach((node, i) => {
          node.position = {
            x: parseInt(level) * 300,
            y: (i - (levelCount - 1) / 2) * 150 + 300
          };
        });
      });
    } else if (currentLayout === 'vertical') {
      // Group nodes by their depth level
      const nodesByLevel: {[key: number]: Node[]} = {};
      
      newNodes.forEach(node => {
        const taskDepth = getTaskDepth(node.id, tasks);
        if (!nodesByLevel[taskDepth]) {
          nodesByLevel[taskDepth] = [];
        }
        nodesByLevel[taskDepth].push(node);
      });
      
      // Position nodes in a vertical tree layout
      Object.keys(nodesByLevel).forEach((level, levelIndex) => {
        const levelNodes = nodesByLevel[parseInt(level)];
        const levelCount = levelNodes.length;
        
        levelNodes.forEach((node, i) => {
          node.position = {
            x: (i - (levelCount - 1) / 2) * 250 + 400,
            y: parseInt(level) * 150
          };
        });
      });
    } else if (currentLayout === 'radial') {
      const centerX = 600;
      const centerY = 400;
      const radius = 50;
      
      // Find root nodes (nodes without parents)
      const rootNodes = tasks.filter(task => !task.parentId)
        .filter(task => newNodes.some(node => node.id === task.id));
      
      // Position root nodes in the center
      rootNodes.forEach((rootTask, i) => {
        const rootNode = newNodes.find(node => node.id === rootTask.id);
        if (rootNode) {
          const angle = (i / Math.max(rootNodes.length, 1)) * 2 * Math.PI;
          rootNode.position = {
            x: centerX + Math.cos(angle) * radius,
            y: centerY + Math.sin(angle) * radius
          };
        }
      });
      
      // Position remaining nodes in concentric circles based on their depth
      const allNodesDepths = newNodes.map(node => getTaskDepth(node.id, tasks));
      const maxDepth = allNodesDepths.length > 0 ? Math.max(...allNodesDepths) : 0;
      
      for (let depth = 1; depth <= maxDepth; depth++) {
        const nodesAtDepth = tasks.filter(task => getTaskDepth(task.id, tasks) === depth)
          .filter(task => newNodes.some(node => node.id === task.id));
        
        nodesAtDepth.forEach((task, i) => {
          const node = newNodes.find(node => node.id === task.id);
          if (node) {
            const segmentAngle = (i / Math.max(nodesAtDepth.length, 1)) * 2 * Math.PI;
            // Increase radius for each level of depth
            const levelRadius = radius + depth * 150;
            
            node.position = {
              x: centerX + Math.cos(segmentAngle) * levelRadius,
              y: centerY + Math.sin(segmentAngle) * levelRadius
            };
          }
        });
      }
    } else if (currentLayout === 'force') {
      // For force layout with new nodes, we'll place them near their parent
      // or in a reasonable position relative to existing nodes
      newNodes.forEach(node => {
        const task = tasks.find(t => t.id === node.id);
        if (task && task.parentId) {
          // Find parent node position
          const parentNode = [...existingNodes, ...newNodes].find(n => n.id === task.parentId);
          if (parentNode) {
            // Position near parent with a small offset
            const angle = Math.random() * Math.PI * 2;
            const distance = 100 + Math.random() * 50;
            node.position = {
              x: parentNode.position.x + Math.cos(angle) * distance,
              y: parentNode.position.y + Math.sin(angle) * distance
            };
          }
        } else {
          // For root nodes without parents, position them in the center
          // with a slight random offset
          node.position = {
            x: 500 + (Math.random() - 0.5) * 200,
            y: 300 + (Math.random() - 0.5) * 200
          };
        }
      });
    }
    
    return newNodes;
  }, [currentLayout, tasks]);
  
  // Layout functions
  const applyHorizontalLayout = useCallback(() => {
    setCurrentLayout('horizontal');
    
    const newNodes = calculateNodePositions([...flowNodes], []);
    
    setNodes(newNodes);
    setTimeout(() => {
      reactFlowInstance.fitView({ padding: 0.2 });
    }, 10);
  }, [flowNodes, tasks, setNodes, reactFlowInstance, calculateNodePositions]);
  
  const applyVerticalLayout = useCallback(() => {
    setCurrentLayout('vertical');
    
    const newNodes = calculateNodePositions([...flowNodes], []);
    
    setNodes(newNodes);
    setTimeout(() => {
      reactFlowInstance.fitView({ padding: 0.2 });
    }, 10);
  }, [flowNodes, tasks, setNodes, reactFlowInstance, calculateNodePositions]);
  
  const applyRadialLayout = useCallback(() => {
    setCurrentLayout('radial');
    
    const newNodes = calculateNodePositions([...flowNodes], []);
    
    setNodes(newNodes);
    setTimeout(() => {
      reactFlowInstance.fitView({ padding: 0.2 });
    }, 10);
  }, [flowNodes, tasks, setNodes, reactFlowInstance, calculateNodePositions]);
  
  const applyForceLayout = useCallback(() => {
    setCurrentLayout('force');
    
    // Simple force-based layout simulation
    const simulation = {
      alpha: 1,
      nodes: flowNodes.map(node => ({
        ...node,
        vx: 0,
        vy: 0
      }))
    };
    
    const iterations = 50;
    
    // Simple repulsive forces between all nodes
    for (let i = 0; i < iterations; i++) {
      // Apply repulsive forces
      for (let a = 0; a < simulation.nodes.length; a++) {
        for (let b = a + 1; b < simulation.nodes.length; b++) {
          const nodeA = simulation.nodes[a];
          const nodeB = simulation.nodes[b];
          
          const dx = nodeB.position.x - nodeA.position.x;
          const dy = nodeB.position.y - nodeA.position.y;
          const dist = Math.sqrt(dx * dx + dy * dy);
          const force = (150 / (dist + 0.1)) * simulation.alpha;
          
          nodeA.vx -= dx * force / dist;
          nodeA.vy -= dy * force / dist;
          nodeB.vx += dx * force / dist;
          nodeB.vy += dy * force / dist;
        }
      }
      
      // Apply attractive forces along edges
      flowEdges.forEach(edge => {
        const source = simulation.nodes.find(n => n.id === edge.source);
        const target = simulation.nodes.find(n => n.id === edge.target);
        
        if (source && target) {
          const dx = target.position.x - source.position.x;
          const dy = target.position.y - source.position.y;
          const dist = Math.sqrt(dx * dx + dy * dy);
          const force = (dist / 20) * simulation.alpha;
          
          source.vx += dx * force / dist;
          source.vy += dy * force / dist;
          target.vx -= dx * force / dist;
          target.vy -= dy * force / dist;
        }
      });
      
      // Update positions
      simulation.nodes.forEach(node => {
        node.position.x += Math.min(10, Math.max(-10, node.vx));
        node.position.y += Math.min(10, Math.max(-10, node.vy));
      });
      
      // Cool down
      simulation.alpha *= 0.98;
    }
    
    setNodes(simulation.nodes);
    setTimeout(() => {
      reactFlowInstance.fitView({ padding: 0.2 });
    }, 10);
  }, [flowNodes, flowEdges, setNodes, reactFlowInstance]);
  
  // Reset layout to default
  const resetLayout = useCallback(() => {
    setCurrentLayout('default');
    
    const newNodes = [...nodes];
    setNodes(newNodes);
    setTimeout(() => {
      reactFlowInstance.fitView({ padding: 0.2 });
    }, 10);
  }, [nodes, setNodes, reactFlowInstance]);
  
  // Update nodes and edges when tasks change
  React.useEffect(() => {
    console.log('--- EDGE UPDATE DEBUG ---');
    console.log('Current flowEdges:', flowEdges.map(e => e.id));
    console.log('New edges from tasks:', edges.map(e => e.id));
    
    // Update nodes: preserve positions of existing nodes, layout new ones
    setNodes(prevNodes => {
      // Create a map of existing nodes by id for easy lookup
      const existingNodesMap = new Map(prevNodes.map(node => [node.id, node]));
      
      // Identify new nodes
      const newNodes: Node[] = [];
      const updatedNodes: Node[] = [];
      
      // Process each node
      nodes.forEach(node => {
        if (existingNodesMap.has(node.id)) {
          // Update existing node's data but keep position
          const existingNode = existingNodesMap.get(node.id)!;
          updatedNodes.push({
            ...existingNode,
            data: node.data // Update node data (label, description, status)
          } as Node);
        } else {
          // This is a new node
          newNodes.push(node);
        }
      });
      
      // Apply the current layout strategy to new nodes only
      const layoutedNewNodes = calculateNodePositions(newNodes, updatedNodes);
      
      // Combine updated and new nodes
      return [...updatedNodes, ...layoutedNewNodes];
    });
    
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
    
  }, [nodes, edges, setNodes, setEdges, flowEdges, calculateNodePositions]);
  
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
        
        {/* Layout control panel */}
        <Panel position="top-right" style={{ display: 'flex', flexDirection: 'column', gap: '8px' }}>
          <div style={{ fontWeight: 'bold', marginBottom: '4px' }}>Layout Options:</div>
          <button 
            onClick={resetLayout}
            style={{ 
              padding: '8px 12px', 
              background: currentLayout === 'default' ? '#2563eb' : '#3b82f6', 
              color: 'white', 
              border: 'none', 
              borderRadius: '4px', 
              cursor: 'pointer' 
            }}
          >
            Default Layout
          </button>
          <button 
            onClick={applyHorizontalLayout}
            style={{ 
              padding: '8px 12px', 
              background: currentLayout === 'horizontal' ? '#2563eb' : '#3b82f6', 
              color: 'white', 
              border: 'none', 
              borderRadius: '4px', 
              cursor: 'pointer' 
            }}
          >
            Horizontal Tree
          </button>
          <button 
            onClick={applyVerticalLayout}
            style={{ 
              padding: '8px 12px', 
              background: currentLayout === 'vertical' ? '#2563eb' : '#3b82f6', 
              color: 'white', 
              border: 'none', 
              borderRadius: '4px', 
              cursor: 'pointer' 
            }}
          >
            Vertical Tree
          </button>
          <button 
            onClick={applyRadialLayout}
            style={{ 
              padding: '8px 12px', 
              background: currentLayout === 'radial' ? '#2563eb' : '#3b82f6', 
              color: 'white', 
              border: 'none', 
              borderRadius: '4px', 
              cursor: 'pointer' 
            }}
          >
            Radial Layout
          </button>
          <button 
            onClick={applyForceLayout}
            style={{ 
              padding: '8px 12px', 
              background: currentLayout === 'force' ? '#2563eb' : '#3b82f6', 
              color: 'white', 
              border: 'none', 
              borderRadius: '4px', 
              cursor: 'pointer' 
            }}
          >
            Force Layout
          </button>
        </Panel>
      </ReactFlow>
    </div>
  );
};

// Wrapper component that provides the ReactFlowProvider
const TaskNetworkFlow = () => {
  return (
    <ReactFlowProvider>
      <TaskNetworkFlowInner />
    </ReactFlowProvider>
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
