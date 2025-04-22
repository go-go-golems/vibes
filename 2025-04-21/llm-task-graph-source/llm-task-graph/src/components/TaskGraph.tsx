import React, { useState } from 'react';
import Tree from 'react-d3-tree';
import { GraphData, NodeData } from '../types/types';
import styled from 'styled-components';
import { TransformWrapper, TransformComponent } from 'react-zoom-pan-pinch';

// Kawaii pink theme colors
const colors = {
  background: '#FFF0F5', // LavenderBlush
  nodeBg: '#FFCCE5',     // Light pink
  nodeStroke: '#FF69B4', // Hot pink
  nodeTextColor: '#8B008B', // Dark magenta
  edgeStroke: '#FF1493', // Deep pink
  
  // Status colors
  pending: '#FFC0CB',    // Pink
  inProgress: '#FFB6C1', // LightPink
  completed: '#FF69B4',  // HotPink
  failed: '#DB7093',     // PaleVioletRed
  
  // Controls
  controlBg: '#FFFFFF',
  controlBorder: '#FF69B4',
  controlText: '#8B008B',
  controlHover: '#FFCCE5',
};

interface TaskGraphProps {
  data: GraphData;
  onNodeClick?: (node: NodeData) => void;
}

const StyledCanvas = styled.div`
  width: 100%;
  height: 100%;
  background-color: ${colors.background};
  border-radius: 12px;
  box-shadow: 0 4px 8px rgba(255, 105, 180, 0.2);
`;

const ControlsContainer = styled.div`
  position: absolute;
  bottom: 20px;
  left: 20px;
  display: flex;
  gap: 8px;
  z-index: 100;
`;

const ControlButton = styled.button`
  background-color: ${colors.controlBg};
  border: 2px solid ${colors.controlBorder};
  color: ${colors.controlText};
  border-radius: 50%;
  width: 40px;
  height: 40px;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 20px;
  cursor: pointer;
  box-shadow: 0 2px 4px rgba(255, 105, 180, 0.3);
  transition: all 0.2s ease;
  
  &:hover {
    background-color: ${colors.controlHover};
    transform: translateY(-2px);
  }
  
  &:active {
    transform: translateY(0);
  }
`;

const ZoomDisplay = styled.div`
  background-color: ${colors.controlBg};
  border: 2px solid ${colors.controlBorder};
  color: ${colors.controlText};
  border-radius: 20px;
  padding: 0 12px;
  height: 40px;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 14px;
  font-weight: 600;
  box-shadow: 0 2px 4px rgba(255, 105, 180, 0.3);
`;

// Convert our graph data to the format expected by react-d3-tree
const convertToTreeData = (graphData: GraphData) => {
  // Create a map of nodes by id for quick lookup
  const nodesMap = new Map();
  graphData.nodes.forEach(node => {
    nodesMap.set(node.id, {
      ...node,
      children: []
    });
  });
  
  // Build the tree structure based on edges
  graphData.edges.forEach(edge => {
    const parentNode = nodesMap.get(edge.from);
    const childNode = nodesMap.get(edge.to);
    
    if (parentNode && childNode) {
      parentNode.children.push(childNode);
    }
  });
  
  // Find root nodes (nodes with no incoming edges)
  const childIds = new Set(graphData.edges.map(edge => edge.to));
  const rootNodes = graphData.nodes.filter(node => !childIds.has(node.id));
  
  // If there are multiple root nodes, create a virtual root
  if (rootNodes.length > 1) {
    return {
      name: 'LLM Agent Planning',
      attributes: {
        status: 'root',
        type: 'root'
      },
      children: rootNodes.map(node => nodesMap.get(node.id))
    };
  } else if (rootNodes.length === 1) {
    return nodesMap.get(rootNodes[0].id);
  } else {
    // If there are no root nodes (circular graph), just pick the first node
    return nodesMap.get(graphData.nodes[0]?.id) || { name: 'Empty Graph', children: [] };
  }
};

// Custom node component for the tree
const CustomNode = ({ nodeDatum, onNodeClick }: any) => {
  const nodeData = nodeDatum as NodeData;
  
  const getNodeColor = (status: string) => {
    switch(status) {
      case 'pending': return colors.pending;
      case 'in_progress': return colors.inProgress;
      case 'completed': return colors.completed;
      case 'failed': return colors.failed;
      default: return colors.nodeBg;
    }
  };
  
  return (
    <g onClick={() => onNodeClick && onNodeClick(nodeData)}>
      <circle
        r={25}
        fill={getNodeColor(nodeData.status)}
        stroke={colors.nodeStroke}
        strokeWidth={2}
      />
      <text
        dy=".31em"
        fontSize={12}
        fontFamily="Arial"
        textAnchor="middle"
        fill={colors.nodeTextColor}
        style={{ pointerEvents: 'none' }}
      >
        {nodeData.text?.length > 15 ? `${nodeData.text.substring(0, 12)}...` : nodeData.text}
      </text>
    </g>
  );
};

const TaskGraph: React.FC<TaskGraphProps> = ({ data, onNodeClick }) => {
  const [zoomLevel, setZoomLevel] = useState(100);
  const treeData = convertToTreeData(data);
  
  return (
    <StyledCanvas>
      <TransformWrapper
        initialScale={1}
        minScale={0.1}
        maxScale={2}
        limitToBounds={false}
        onZoom={(ref) => {
          setZoomLevel(Math.round(ref.state.scale * 100));
        }}
      >
        {({ zoomIn, zoomOut, resetTransform }) => (
          <>
            <ControlsContainer>
              <ControlButton onClick={() => zoomIn()}>+</ControlButton>
              <ControlButton onClick={() => zoomOut()}>-</ControlButton>
              <ControlButton onClick={() => resetTransform()}>↻</ControlButton>
              <ZoomDisplay>{zoomLevel}%</ZoomDisplay>
            </ControlsContainer>
            <TransformComponent
              wrapperStyle={{
                width: '100%',
                height: '100%',
              }}
            >
              <div style={{ width: '100%', height: '100%' }}>
                <Tree
                  data={treeData}
                  orientation="vertical"
                  pathFunc="step"
                  renderCustomNodeElement={(rd3tProps) => 
                    CustomNode({ ...rd3tProps, onNodeClick })
                  }
                  separation={{ siblings: 2, nonSiblings: 2.5 }}
                  translate={{ x: window.innerWidth / 2, y: 100 }}
                  nodeSize={{ x: 200, y: 100 }}
                  collapsible={false}
                />
              </div>
            </TransformComponent>
          </>
        )}
      </TransformWrapper>
    </StyledCanvas>
  );
};

export default TaskGraph;
