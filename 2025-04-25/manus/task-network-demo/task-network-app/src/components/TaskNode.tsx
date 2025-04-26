import React, { memo } from 'react';
import { Handle, Position } from 'reactflow';
import { useDispatch } from 'react-redux';
import { updateTaskStatus, TaskStatus } from '../store/taskSlice';

const TaskNode = ({ data }) => {
  const dispatch = useDispatch();
  
  // Get background color based on task status
  const getBackgroundColor = () => {
    switch (data.status) {
      case TaskStatus.COMPLETED:
        return '#dcfce7'; // Light green
      case TaskStatus.IN_PROGRESS:
        return '#dbeafe'; // Light blue
      case TaskStatus.FAILED:
        return '#fee2e2'; // Light red
      default:
        return '#f8fafc'; // Light gray
    }
  };
  
  // Get border color based on task status
  const getBorderColor = () => {
    switch (data.status) {
      case TaskStatus.COMPLETED:
        return '#22c55e'; // Green
      case TaskStatus.IN_PROGRESS:
        return '#3b82f6'; // Blue
      case TaskStatus.FAILED:
        return '#ef4444'; // Red
      default:
        return '#94a3b8'; // Gray
    }
  };
  
  // Handle status change
  const handleStatusChange = (newStatus) => {
    dispatch(updateTaskStatus({ taskId: data.id, status: newStatus }));
  };
  
  return (
    <div
      style={{
        background: getBackgroundColor(),
        border: `2px solid ${getBorderColor()}`,
        borderRadius: '8px',
        padding: '10px',
        width: '200px',
        boxShadow: '0 4px 6px -1px rgba(0, 0, 0, 0.1)',
      }}
    >
      <Handle type="target" position={Position.Left} />
      
      <div style={{ fontWeight: 'bold', marginBottom: '5px' }}>{data.label}</div>
      <div style={{ fontSize: '0.8rem', marginBottom: '10px' }}>{data.description}</div>
      
      <div style={{ 
        display: 'flex', 
        justifyContent: 'space-between',
        backgroundColor: 'rgba(0, 0, 0, 0.05)',
        padding: '5px',
        borderRadius: '4px',
        fontSize: '0.75rem'
      }}>
        <span>Status: {data.status}</span>
        <div style={{ display: 'flex', gap: '5px' }}>
          <button
            onClick={() => handleStatusChange(TaskStatus.IN_PROGRESS)}
            style={{
              padding: '2px 4px',
              fontSize: '0.7rem',
              backgroundColor: data.status === TaskStatus.IN_PROGRESS ? '#3b82f6' : '#e2e8f0',
              color: data.status === TaskStatus.IN_PROGRESS ? 'white' : 'black',
              border: 'none',
              borderRadius: '3px',
              cursor: 'pointer'
            }}
          >
            Start
          </button>
          <button
            onClick={() => handleStatusChange(TaskStatus.COMPLETED)}
            style={{
              padding: '2px 4px',
              fontSize: '0.7rem',
              backgroundColor: data.status === TaskStatus.COMPLETED ? '#22c55e' : '#e2e8f0',
              color: data.status === TaskStatus.COMPLETED ? 'white' : 'black',
              border: 'none',
              borderRadius: '3px',
              cursor: 'pointer'
            }}
          >
            Complete
          </button>
        </div>
      </div>
      
      <Handle type="source" position={Position.Right} />
    </div>
  );
};

export default memo(TaskNode);
