// src/components/CustomNode.tsx

import React from 'react';
import { NodeChildProps } from 'reaflow';
import { NodeData } from '../types/types';
import { nodeConfig } from './nodeConfig';

// Define node dimensions
const NODE_WIDTH = 260;
const NODE_HEIGHT = 164;

interface NodeStatsProps {
  showStats?: boolean;
  stats?: Record<string, number>;
}

const NodeStats: React.FC<NodeStatsProps> = ({ showStats, stats }) => 
  showStats && stats ? (
    <ul className="node-stats">
      {Object.entries(stats).map(([label, count]) => (
        <li key={label}>
          <span>{label}</span>
          <strong>{count}</strong>
        </li>
      ))}
    </ul>
  ) : null;

interface NodeContentProps {
  node: NodeData;
  selected?: boolean;
  onClick?: () => void;
}

const NodeContent: React.FC<NodeContentProps> = ({ node, selected, onClick }) => {
  const nodeData = node.data || { type: 'default', title: 'Default Title' };
  const type = nodeData.type || 'default';
  const title = nodeData.title || 'Node Title';
  const description = nodeData.description || 'Node Description';
  const showError = nodeData.showError;
  const { color, icon, backgroundColor } = nodeConfig(type);

  const nodeContentStyle: React.CSSProperties = {
    borderLeft: `4px solid ${color}`,
    backgroundColor: backgroundColor,
    color: '#333'
  };

  const nodeIconStyle: React.CSSProperties = {
    color: color
  };

  return (
    <div 
      className="node-content" 
      style={nodeContentStyle} 
      onClick={onClick} 
      aria-selected={selected}
    >
      {showError && <div className="node-error-badge"></div>}
      <div className="node-icon" style={nodeIconStyle}>{icon}</div>
      <div className="node-details">
        <h1>{title} (ID: {node.id})</h1>
        <p>{description}</p>
      </div>
      <NodeStats stats={nodeData.stats} showStats={nodeData.showStats} />
    </div>
  );
};

export interface CustomNodeProps {
  nodeProps: NodeChildProps;
  selectedNode: string | null;
  onNodeClick: (id: string) => void;
  onAddClick: (node: NodeData) => void;
}

export const CustomNode: React.FC<CustomNodeProps> = ({ 
  nodeProps, 
  selectedNode, 
  onNodeClick, 
  onAddClick 
}) => {
  const { node } = nodeProps;
  const width = node.width || NODE_WIDTH;
  const height = node.height || NODE_HEIGHT;
  const isSelected = selectedNode === node.id;
  const isDisabled = false;
  const showAddButton = node.data?.type !== 'end';

  return (
    <foreignObject x={0} y={0} width={width} height={height}>
      <div className="node-style-reset node-wrapper">
        <NodeContent 
          node={node} 
          selected={isSelected} 
          onClick={onNodeClick ? () => onNodeClick(node.id) : undefined} 
        />
        {showAddButton && (
          <div className="add-button">
            <button 
              disabled={isDisabled}
              onClick={(e) => {
                e.stopPropagation();
                if (onAddClick) onAddClick(node);
              }}
            >
              +
            </button>
          </div>
        )}
      </div>
    </foreignObject>
  );
};
