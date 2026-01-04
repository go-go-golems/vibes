import React, { useState, useRef } from 'react';
import { Canvas, Node, Edge, NodeChildProps, ElkCanvasLayoutOptions } from 'reaflow';
import './App.css';
import { CustomNode } from './components/CustomNode';
import { nodeConfig } from './components/nodeConfig';
import { NodeData } from './types/types';

// Define Layout Options
const layoutOptions: ElkCanvasLayoutOptions = {
  'elk.algorithm': 'layered',
  'elk.direction': 'DOWN',
  'elk.spacing.nodeNode': '80',
  'elk.portAlignment.default': 'CENTER',
  'elk.layered.spacing.nodeNodeBetweenLayers': '80',
};

// Define node dimensions
const NODE_WIDTH = 260;
const NODE_HEIGHT = 164;

// Initial Data - Sample LLM Agent Planning Tasks
const initialNodes: NodeData[] = [
  {
    id: 'analyze',
    width: NODE_WIDTH,
    height: NODE_HEIGHT,
    data: {
      type: 'goal',
      title: 'Analyze User Request',
      description: "Understand user's needs and requirements"
    }
  },
  {
    id: 'retrieve',
    width: NODE_WIDTH,
    height: NODE_HEIGHT,
    data: {
      type: 'subtask',
      title: 'Retrieve Information',
      description: "Search for relevant data and context"
    }
  },
  {
    id: 'synthesize',
    width: NODE_WIDTH,
    height: NODE_HEIGHT,
    data: {
      type: 'subtask',
      title: 'Synthesize Response',
      description: "Generate appropriate answer"
    }
  },
  {
    id: 'create',
    width: NODE_WIDTH,
    height: NODE_HEIGHT,
    data: {
      type: 'action',
      title: 'Create Visualization',
      description: "Generate graph representation",
      icon: '📊',
      stats: { 'Time': 3, 'Nodes': 8 },
      showStats: true
    }
  },
  {
    id: 'format1',
    width: NODE_WIDTH,
    height: NODE_HEIGHT,
    data: {
      type: 'action',
      title: 'Format Response',
      description: "Structure the output for readability",
      icon: '📝'
    }
  },
  {
    id: 'format2',
    width: NODE_WIDTH,
    height: NODE_HEIGHT,
    data: {
      type: 'action',
      title: 'Apply Styling',
      description: "Add kawaii theme styling",
      icon: '🎨',
      stats: { 'Elements': 12, 'CSS': 250 },
      showStats: true
    }
  },
  {
    id: 'deliver1',
    width: NODE_WIDTH,
    height: NODE_HEIGHT,
    data: {
      type: 'llm',
      title: 'Generate Final Text',
      description: "Create final text content",
      icon: '🤖',
      showError: true
    }
  },
  {
    id: 'deliver2',
    width: NODE_WIDTH,
    height: NODE_HEIGHT,
    data: {
      type: 'tool',
      title: 'Deploy Website',
      description: "Make website publicly accessible",
      icon: '🔧'
    }
  }
];

// Edges: Represent hierarchy and dependencies
const initialEdges = [
  // Hierarchy Edges
  { id: 'analyze-retrieve', from: 'analyze', to: 'retrieve', className: 'edge-hierarchy' },
  { id: 'analyze-synthesize', from: 'analyze', to: 'synthesize', className: 'edge-hierarchy' },
  { id: 'synthesize-create', from: 'synthesize', to: 'create', className: 'edge-hierarchy' },
  { id: 'synthesize-format1', from: 'synthesize', to: 'format1', className: 'edge-hierarchy' },
  { id: 'synthesize-format2', from: 'synthesize', to: 'format2', className: 'edge-hierarchy' },
  { id: 'synthesize-deliver1', from: 'synthesize', to: 'deliver1', className: 'edge-hierarchy' },
  { id: 'synthesize-deliver2', from: 'synthesize', to: 'deliver2', className: 'edge-hierarchy' },
  
  // Dependency/Sequence Edges
  { id: 'retrieve-synthesize', from: 'retrieve', to: 'synthesize' },
  { id: 'create-format1', from: 'create', to: 'format1' },
  { id: 'format1-format2', from: 'format1', to: 'format2' },
  { id: 'format2-deliver1', from: 'format2', to: 'deliver1' },
  { id: 'deliver1-deliver2', from: 'deliver1', to: 'deliver2' }
];

function App() {
  // State for nodes and edges
  const [nodes, setNodes] = useState<NodeData[]>(initialNodes);
  const [edges, setEdges] = useState(initialEdges);
  const [selectedNode, setSelectedNode] = useState<string | null>(null);
  const [zoomLevel, setZoomLevel] = useState<number>(1);
  
  // Counter for generating unique IDs
  const nodeIdCounter = useRef(nodes.length + 1);

  // Function to add a new node
  const handleAddNode = (parentNode: NodeData) => {
    const newNodeId = `node-${nodeIdCounter.current++}`;
    const newEdgeId = `${parentNode.id}-${newNodeId}`;
    
    // Create a new node with random type
    const nodeTypes = ['goal', 'subtask', 'action', 'memory', 'tool', 'llm'];
    const randomType = nodeTypes[Math.floor(Math.random() * nodeTypes.length)];
    
    const newNode: NodeData = {
      id: newNodeId,
      width: NODE_WIDTH,
      height: NODE_HEIGHT,
      data: {
        type: randomType,
        title: `New ${randomType.charAt(0).toUpperCase() + randomType.slice(1)}`,
        description: 'Added at runtime',
        icon: nodeConfig(randomType).icon
      }
    };
    
    // Create the edge connecting to it
    const newEdge = {
      id: newEdgeId,
      from: parentNode.id,
      to: newNodeId,
      className: 'edge-hierarchy'
    };
    
    // Update state
    setNodes(prevNodes => [...prevNodes, newNode]);
    setEdges(prevEdges => [...prevEdges, newEdge]);
  };

  // Function to remove a random node
  const handleRemoveNode = () => {
    if (nodes.length <= 1) return;
    
    const randomIndex = Math.floor(Math.random() * nodes.length);
    const nodeToRemove = nodes[randomIndex];
    
    // Remove the node and all connected edges
    setNodes(prevNodes => prevNodes.filter(node => node.id !== nodeToRemove.id));
    setEdges(prevEdges => prevEdges.filter(edge => 
      edge.from !== nodeToRemove.id && edge.to !== nodeToRemove.id
    ));
  };

  // Function to add a random connection
  const handleAddConnection = () => {
    if (nodes.length < 2) return;
    
    // Pick two random nodes
    const fromIndex = Math.floor(Math.random() * nodes.length);
    let toIndex = Math.floor(Math.random() * nodes.length);
    
    // Make sure they're different nodes
    while (toIndex === fromIndex) {
      toIndex = Math.floor(Math.random() * nodes.length);
    }
    
    const fromNode = nodes[fromIndex];
    const toNode = nodes[toIndex];
    
    // Check if this edge already exists
    const edgeExists = edges.some(edge => 
      edge.from === fromNode.id && edge.to === toNode.id
    );
    
    if (!edgeExists) {
      const newEdge = {
        id: `${fromNode.id}-${toNode.id}`,
        from: fromNode.id,
        to: toNode.id
      };
      
      setEdges(prevEdges => [...prevEdges, newEdge]);
    }
  };

  // Function to change a random node's status
  const handleChangeStatus = () => {
    if (nodes.length === 0) return;
    
    const randomIndex = Math.floor(Math.random() * nodes.length);
    const nodeToUpdate = nodes[randomIndex];
    
    // Ensure nodeData exists and has required properties
    if (!nodeToUpdate.data) return;
    
    // Toggle error state or change type
    const updatedNodes = [...nodes];
    const nodeData = {...updatedNodes[randomIndex].data!}; // Non-null assertion
    
    if (Math.random() > 0.5) {
      // Toggle error state
      nodeData.showError = !nodeData.showError;
    } else {
      // Change node type
      const nodeTypes = ['goal', 'subtask', 'action', 'memory', 'tool', 'llm'];
      const currentTypeIndex = nodeTypes.indexOf(nodeData.type);
      const newTypeIndex = (currentTypeIndex + 1) % nodeTypes.length;
      nodeData.type = nodeTypes[newTypeIndex];
    }
    
    updatedNodes[randomIndex] = {
      ...nodeToUpdate,
      data: nodeData
    };
    
    setNodes(updatedNodes);
  };

  // Function to add a random event to a random node
  const handleAddEvent = () => {
    if (nodes.length === 0) return;
    
    const randomIndex = Math.floor(Math.random() * nodes.length);
    const nodeToUpdate = nodes[randomIndex];
    
    // Ensure nodeData exists
    if (!nodeToUpdate.data) return;
    
    // Add or update stats
    const updatedNodes = [...nodes];
    const nodeData = {...updatedNodes[randomIndex].data!}; // Non-null assertion
    
    // Create or update stats
    const stats = nodeData.stats || {};
    const eventTypes = ['Calls', 'Time', 'Memory', 'Tokens', 'Errors'];
    const randomEvent = eventTypes[Math.floor(Math.random() * eventTypes.length)];
    
    stats[randomEvent] = (stats[randomEvent] || 0) + Math.floor(Math.random() * 10) + 1;
    nodeData.stats = stats;
    nodeData.showStats = true;
    
    updatedNodes[randomIndex] = {
      ...nodeToUpdate,
      data: nodeData
    };
    
    setNodes(updatedNodes);
  };

  // Function to reset the graph
  const handleResetGraph = () => {
    setNodes(initialNodes);
    setEdges(initialEdges);
    setSelectedNode(null);
    nodeIdCounter.current = initialNodes.length + 1;
  };

  // Function to handle node selection
  const handleNodeClick = (id: string) => {
    setSelectedNode(id);
  };

  // Zoom control functions
  const handleZoomIn = () => {
    setZoomLevel(prev => Math.min(prev + 0.1, 2));
  };

  const handleZoomOut = () => {
    setZoomLevel(prev => Math.max(prev - 0.1, 0.5));
  };

  const handleZoomReset = () => {
    setZoomLevel(1);
  };

  return (
    <div className="app-container">
      <h1>LLM Agent Task Graph Visualization 🌸</h1>
      <p>A cute pink kawaii visualization of an LLM agent planning process</p>
      
      <div className="control-buttons">
        <button onClick={handleAddConnection}>Add Connection</button>
        <button onClick={handleRemoveNode}>Remove Node</button>
        <button onClick={handleChangeStatus}>Change Status</button>
        <button onClick={handleAddEvent}>Add Event</button>
        <button onClick={handleResetGraph}>Reset Graph</button>
      </div>
      
      <div className="canvas-container">
        <Canvas
          nodes={nodes}
          edges={edges}
          fit={true}
          direction="DOWN"
          layoutOptions={layoutOptions}
          zoomable={true}
          pannable={true}
          zoom={zoomLevel}
          maxWidth={1200}
          maxHeight={800}
          node={
            <Node>
              {(nodeProps: NodeChildProps) => (
                <CustomNode
                  nodeProps={nodeProps}
                  selectedNode={selectedNode}
                  onNodeClick={handleNodeClick}
                  onAddClick={handleAddNode}
                />
              )}
            </Node>
          }
          edge={
            <Edge
              style={{
                stroke: '#ff69b4',
                strokeWidth: 1.5,
              }}
            />
          }
        />
        
        <div className="zoom-controls">
          <button onClick={handleZoomIn}>+</button>
          <div className="zoom-level">{Math.round(zoomLevel * 100)}%</div>
          <button onClick={handleZoomOut}>-</button>
          <button onClick={handleZoomReset}>↻</button>
        </div>
      </div>
      
      {/* Info Panel */}
      <div className="info-panel">
        <h3>Selected Node</h3>
        {selectedNode ? (
          (() => {
            const node = nodes.find(n => n.id === selectedNode);
            const nodeData = node?.data;
            return node ? (
              <div>
                <p><strong>ID:</strong> {node.id}</p>
                <p><strong>Type:</strong> {nodeData?.type || 'N/A'}</p>
                <p><strong>Title:</strong> {nodeData?.title || node.text || 'N/A'}</p>
                <p><strong>Description:</strong> {nodeData?.description || 'N/A'}</p>
                {nodeData?.stats && (
                  <p><strong>Stats:</strong> {JSON.stringify(nodeData.stats)}</p>
                )}
              </div>
            ) : <p>Node not found</p>;
          })()
        ) : (
          <p>Click on a node to see its details</p>
        )}
      </div>
      
      {/* Legend */}
      <div className="legend">
        <h3>Legend</h3>
        <div className="legend-item">
          <div className="legend-color" style={{ 
            backgroundColor: nodeConfig('goal').backgroundColor, 
            borderLeft: `4px solid ${nodeConfig('goal').color}` 
          }}></div>
          <span>Goal</span>
        </div>
        <div className="legend-item">
          <div className="legend-color" style={{ 
            backgroundColor: nodeConfig('subtask').backgroundColor, 
            borderLeft: `4px solid ${nodeConfig('subtask').color}` 
          }}></div>
          <span>Subtask</span>
        </div>
        <div className="legend-item">
          <div className="legend-color" style={{ 
            backgroundColor: nodeConfig('action').backgroundColor, 
            borderLeft: `4px solid ${nodeConfig('action').color}` 
          }}></div>
          <span>Action</span>
        </div>
        <div className="legend-item">
          <div className="legend-color" style={{ 
            backgroundColor: nodeConfig('llm').backgroundColor, 
            borderLeft: `4px solid ${nodeConfig('llm').color}` 
          }}></div>
          <span>LLM Call</span>
        </div>
        <div className="legend-item">
          <div className="legend-color" style={{ 
            backgroundColor: nodeConfig('tool').backgroundColor, 
            borderLeft: `4px solid ${nodeConfig('tool').color}` 
          }}></div>
          <span>Tool Call</span>
        </div>
        <div className="legend-item">
          <div className="legend-color" style={{ 
            backgroundColor: nodeConfig('memory').backgroundColor, 
            borderLeft: `4px solid ${nodeConfig('memory').color}` 
          }}></div>
          <span>Memory Fetch</span>
        </div>
      </div>
    </div>
  );
}

export default App;
