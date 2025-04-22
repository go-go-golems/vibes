import React, { useState } from 'react';
import styled from 'styled-components';
import TaskGraph from './components/TaskGraph';
import NodeDetails from './components/NodeDetails';
import { sampleData } from './data/sampleData';
import { NodeData, EdgeData, GraphData, Event } from './types/types';

const AppContainer = styled.div`
  display: flex;
  flex-direction: column;
  width: 100vw;
  height: 100vh;
  background-color: #FFF0F5;
  font-family: 'Comic Sans MS', 'Segoe UI', sans-serif;
`;

const Header = styled.header`
  background-color: #FF69B4;
  color: white;
  padding: 16px 24px;
  text-align: center;
  box-shadow: 0 2px 8px rgba(255, 105, 180, 0.5);
`;

const Title = styled.h1`
  margin: 0;
  font-size: 24px;
  font-weight: 700;
`;

const GraphContainer = styled.div`
  flex: 1;
  position: relative;
  overflow: hidden;
`;

const ControlPanel = styled.div`
  position: absolute;
  top: 20px;
  left: 20px;
  background-color: white;
  border: 2px solid #FF69B4;
  border-radius: 12px;
  padding: 16px;
  box-shadow: 0 4px 8px rgba(255, 105, 180, 0.3);
  z-index: 100;
  display: flex;
  flex-direction: column;
  gap: 12px;
  max-width: 250px;
`;

const ControlTitle = styled.h3`
  margin: 0 0 8px 0;
  color: #8B008B;
  font-size: 16px;
  text-align: center;
  border-bottom: 1px solid #FFCCE5;
  padding-bottom: 8px;
`;

const ButtonGroup = styled.div`
  display: flex;
  flex-wrap: wrap;
  gap: 8px;
`;

const Button = styled.button`
  background-color: #FFCCE5;
  border: 2px solid #FF69B4;
  color: #8B008B;
  border-radius: 20px;
  padding: 8px 12px;
  font-size: 14px;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.2s ease;
  flex: 1;
  min-width: 100px;
  
  &:hover {
    background-color: #FF69B4;
    color: white;
    transform: translateY(-2px);
  }
  
  &:active {
    transform: translateY(0);
  }
`;

// Helper functions for graph modifications
const generateId = () => `node-${Math.floor(Math.random() * 10000)}`;
const generateEdgeId = (from: string, to: string) => `edge-${from}-${to}`;

const getRandomStatus = (): 'pending' | 'in_progress' | 'completed' | 'failed' => {
  const statuses: ('pending' | 'in_progress' | 'completed' | 'failed')[] = ['pending', 'in_progress', 'completed', 'failed'];
  return statuses[Math.floor(Math.random() * statuses.length)];
};

const getRandomType = (): 'task' | 'llm_call' | 'tool_call' | 'memory_fetch' => {
  const types: ('task' | 'llm_call' | 'tool_call' | 'memory_fetch')[] = ['task', 'llm_call', 'tool_call', 'memory_fetch'];
  return types[Math.floor(Math.random() * types.length)];
};

const getRandomEvent = (): Event => {
  const eventTypes: ('llm_call' | 'tool_call' | 'memory_fetch' | 'user_input' | 'system_event')[] = 
    ['llm_call', 'tool_call', 'memory_fetch', 'user_input', 'system_event'];
  const eventType = eventTypes[Math.floor(Math.random() * eventTypes.length)];
  
  const details: any = {};
  if (eventType === 'llm_call') {
    details.model = ['gpt-4', 'gpt-5', 'claude-3'][Math.floor(Math.random() * 3)];
    details.prompt_tokens = Math.floor(Math.random() * 1000) + 100;
    details.completion_tokens = Math.floor(Math.random() * 500) + 50;
  } else if (eventType === 'tool_call') {
    details.tool = ['search', 'calculator', 'knowledge_base', 'code_interpreter'][Math.floor(Math.random() * 4)];
    details.success = Math.random() > 0.2;
  } else if (eventType === 'memory_fetch') {
    details.memory_id = `mem-${Math.floor(Math.random() * 1000)}`;
    details.relevance_score = Math.random().toFixed(2);
  }
  
  return {
    id: `event-${Math.floor(Math.random() * 10000)}`,
    type: eventType,
    timestamp: new Date(Date.now() - Math.floor(Math.random() * 3600000)).toISOString(),
    description: `Random ${eventType.replace('_', ' ')} event`,
    details
  };
};

const generateRandomNode = (existingNodes: NodeData[]): NodeData => {
  const id = generateId();
  const randomTaskNames = [
    'Analyze User Query',
    'Search Knowledge Base',
    'Generate Response',
    'Create Visualization',
    'Verify Information',
    'Summarize Content',
    'Extract Key Points',
    'Translate Text',
    'Format Output',
    'Deliver Response'
  ];
  
  // Randomly select dependencies from existing nodes (0-2 dependencies)
  const dependencies: string[] = [];
  if (existingNodes.length > 0) {
    const numDeps = Math.floor(Math.random() * Math.min(3, existingNodes.length));
    for (let i = 0; i < numDeps; i++) {
      const randomNode = existingNodes[Math.floor(Math.random() * existingNodes.length)];
      if (!dependencies.includes(randomNode.id)) {
        dependencies.push(randomNode.id);
      }
    }
  }
  
  // Generate 0-3 random events
  const events: Event[] = [];
  const numEvents = Math.floor(Math.random() * 4);
  for (let i = 0; i < numEvents; i++) {
    events.push(getRandomEvent());
  }
  
  return {
    id,
    text: randomTaskNames[Math.floor(Math.random() * randomTaskNames.length)],
    status: getRandomStatus(),
    timestamp: new Date().toISOString(),
    type: getRandomType(),
    metadata: {
      description: `This is a randomly generated ${getRandomType()} node`,
      dependencies,
    },
    events
  };
};

function App() {
  const [graphData, setGraphData] = useState<GraphData>(sampleData);
  const [selectedNode, setSelectedNode] = useState<NodeData | null>(null);

  const handleNodeClick = (node: NodeData) => {
    setSelectedNode(node);
  };

  const handleCloseDetails = () => {
    setSelectedNode(null);
  };

  const addRandomNode = () => {
    const newNode = generateRandomNode(graphData.nodes);
    
    // Create edges from dependencies
    const newEdges: EdgeData[] = [];
    newNode.metadata.dependencies.forEach(depId => {
      newEdges.push({
        id: generateEdgeId(depId, newNode.id),
        from: depId,
        to: newNode.id,
        text: 'depends on'
      });
    });
    
    setGraphData({
      nodes: [...graphData.nodes, newNode],
      edges: [...graphData.edges, ...newEdges]
    });
    
    // Close details panel if open
    setSelectedNode(null);
  };

  const removeRandomNode = () => {
    if (graphData.nodes.length <= 1) return;
    
    const randomIndex = Math.floor(Math.random() * graphData.nodes.length);
    const nodeToRemove = graphData.nodes[randomIndex];
    
    // Remove the node and all connected edges
    const updatedNodes = graphData.nodes.filter(node => node.id !== nodeToRemove.id);
    const updatedEdges = graphData.edges.filter(
      edge => edge.from !== nodeToRemove.id && edge.to !== nodeToRemove.id
    );
    
    setGraphData({
      nodes: updatedNodes,
      edges: updatedEdges
    });
    
    // Close details panel if showing the removed node
    if (selectedNode && selectedNode.id === nodeToRemove.id) {
      setSelectedNode(null);
    }
  };

  const addRandomConnection = () => {
    if (graphData.nodes.length < 2) return;
    
    // Pick two random nodes
    let fromIndex = Math.floor(Math.random() * graphData.nodes.length);
    let toIndex = Math.floor(Math.random() * graphData.nodes.length);
    
    // Make sure they're different nodes
    while (fromIndex === toIndex) {
      toIndex = Math.floor(Math.random() * graphData.nodes.length);
    }
    
    const fromNode = graphData.nodes[fromIndex];
    const toNode = graphData.nodes[toIndex];
    
    // Check if this edge already exists
    const edgeExists = graphData.edges.some(
      edge => edge.from === fromNode.id && edge.to === toNode.id
    );
    
    if (!edgeExists) {
      const newEdge: EdgeData = {
        id: generateEdgeId(fromNode.id, toNode.id),
        from: fromNode.id,
        to: toNode.id,
        text: 'connects to'
      };
      
      // Update the target node's dependencies
      const updatedNodes = graphData.nodes.map(node => {
        if (node.id === toNode.id) {
          return {
            ...node,
            metadata: {
              ...node.metadata,
              dependencies: [...node.metadata.dependencies, fromNode.id]
            }
          };
        }
        return node;
      });
      
      setGraphData({
        nodes: updatedNodes,
        edges: [...graphData.edges, newEdge]
      });
    }
  };

  const changeRandomNodeStatus = () => {
    if (graphData.nodes.length === 0) return;
    
    const randomIndex = Math.floor(Math.random() * graphData.nodes.length);
    const nodeToUpdate = graphData.nodes[randomIndex];
    
    const updatedNodes = graphData.nodes.map(node => {
      if (node.id === nodeToUpdate.id) {
        return {
          ...node,
          status: getRandomStatus()
        };
      }
      return node;
    });
    
    setGraphData({
      nodes: updatedNodes,
      edges: graphData.edges
    });
    
    // Update details panel if showing the updated node
    if (selectedNode && selectedNode.id === nodeToUpdate.id) {
      const updatedNode = updatedNodes.find(node => node.id === nodeToUpdate.id);
      if (updatedNode) {
        setSelectedNode(updatedNode);
      }
    }
  };

  const addRandomEvent = () => {
    if (graphData.nodes.length === 0) return;
    
    const randomIndex = Math.floor(Math.random() * graphData.nodes.length);
    const nodeToUpdate = graphData.nodes[randomIndex];
    
    const newEvent = getRandomEvent();
    
    const updatedNodes = graphData.nodes.map(node => {
      if (node.id === nodeToUpdate.id) {
        return {
          ...node,
          events: [...node.events, newEvent]
        };
      }
      return node;
    });
    
    setGraphData({
      nodes: updatedNodes,
      edges: graphData.edges
    });
    
    // Update details panel if showing the updated node
    if (selectedNode && selectedNode.id === nodeToUpdate.id) {
      const updatedNode = updatedNodes.find(node => node.id === nodeToUpdate.id);
      if (updatedNode) {
        setSelectedNode(updatedNode);
      }
    }
  };

  const resetGraph = () => {
    setGraphData(sampleData);
    setSelectedNode(null);
  };

  return (
    <AppContainer>
      <Header>
        <Title>🌸 LLM Agent Task Graph Visualization 🌸</Title>
      </Header>
      <GraphContainer>
        <ControlPanel>
          <ControlTitle>Graph Modifications</ControlTitle>
          <ButtonGroup>
            <Button onClick={addRandomNode}>Add Node</Button>
            <Button onClick={removeRandomNode}>Remove Node</Button>
          </ButtonGroup>
          <ButtonGroup>
            <Button onClick={addRandomConnection}>Add Connection</Button>
            <Button onClick={changeRandomNodeStatus}>Change Status</Button>
          </ButtonGroup>
          <ButtonGroup>
            <Button onClick={addRandomEvent}>Add Event</Button>
            <Button onClick={resetGraph}>Reset Graph</Button>
          </ButtonGroup>
        </ControlPanel>
        <TaskGraph 
          data={graphData} 
          onNodeClick={handleNodeClick}
        />
        <NodeDetails 
          node={selectedNode} 
          onClose={handleCloseDetails}
        />
      </GraphContainer>
    </AppContainer>
  );
}

export default App;
