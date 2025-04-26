import React from 'react';
import { useDispatch, useSelector } from 'react-redux';
import { simulateStep, resetTasks } from '../store/taskSlice';

const ControlPanel = () => {
  const dispatch = useDispatch();
  const history = useSelector((state) => state.tasks.history);
  const tasks = useSelector((state) => state.tasks.tasks);
  
  // Handle step button click
  const handleStep = () => {
    dispatch(simulateStep());
  };
  
  // Handle reset button click
  const handleReset = () => {
    dispatch(resetTasks());
  };
  
  return (
    <div style={{
      padding: '20px',
      marginBottom: '20px',
      backgroundColor: '#f8fafc',
      borderRadius: '8px',
      boxShadow: '0 2px 4px rgba(0, 0, 0, 0.1)'
    }}>
      <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '15px' }}>
        <h2 style={{ margin: 0 }}>Task Planning Network Simulation</h2>
        <div style={{ display: 'flex', gap: '10px' }}>
          <button
            onClick={handleStep}
            style={{
              padding: '8px 16px',
              backgroundColor: '#3b82f6',
              color: 'white',
              border: 'none',
              borderRadius: '4px',
              cursor: 'pointer',
              fontWeight: 'bold'
            }}
          >
            Step
          </button>
          <button
            onClick={handleReset}
            style={{
              padding: '8px 16px',
              backgroundColor: '#ef4444',
              color: 'white',
              border: 'none',
              borderRadius: '4px',
              cursor: 'pointer',
              fontWeight: 'bold'
            }}
          >
            Reset
          </button>
        </div>
      </div>
      
      <div style={{ display: 'flex', gap: '20px' }}>
        <div style={{ flex: 1 }}>
          <h3 style={{ fontSize: '1rem', marginBottom: '10px' }}>Statistics</h3>
          <div style={{ 
            display: 'grid', 
            gridTemplateColumns: 'repeat(2, 1fr)',
            gap: '10px'
          }}>
            <div style={{ 
              padding: '10px', 
              backgroundColor: 'white', 
              borderRadius: '4px',
              border: '1px solid #e2e8f0'
            }}>
              <div style={{ fontSize: '0.8rem', color: '#64748b' }}>Total Tasks</div>
              <div style={{ fontSize: '1.5rem', fontWeight: 'bold' }}>{tasks.length}</div>
            </div>
            <div style={{ 
              padding: '10px', 
              backgroundColor: 'white', 
              borderRadius: '4px',
              border: '1px solid #e2e8f0'
            }}>
              <div style={{ fontSize: '0.8rem', color: '#64748b' }}>Steps Taken</div>
              <div style={{ fontSize: '1.5rem', fontWeight: 'bold' }}>{history.length}</div>
            </div>
          </div>
        </div>
        
        <div style={{ flex: 2 }}>
          <h3 style={{ fontSize: '1rem', marginBottom: '10px' }}>Last Action</h3>
          <div style={{ 
            padding: '10px', 
            backgroundColor: 'white', 
            borderRadius: '4px',
            border: '1px solid #e2e8f0',
            maxHeight: '80px',
            overflow: 'auto'
          }}>
            {history.length > 0 ? (
              <div>
                <div style={{ fontSize: '0.9rem', fontWeight: 'bold' }}>
                  {history[history.length - 1].action}
                </div>
                <div style={{ fontSize: '0.8rem', color: '#64748b' }}>
                  {history[history.length - 1].taskId && `Task: ${history[history.length - 1].taskId}`}
                  {history[history.length - 1].oldStatus && history[history.length - 1].newStatus && 
                    ` • Status: ${history[history.length - 1].oldStatus} → ${history[history.length - 1].newStatus}`}
                </div>
                <div style={{ fontSize: '0.7rem', color: '#94a3b8' }}>
                  {new Date(history[history.length - 1].timestamp).toLocaleTimeString()}
                </div>
              </div>
            ) : (
              <div style={{ color: '#94a3b8', fontStyle: 'italic' }}>No actions yet</div>
            )}
          </div>
        </div>
      </div>
    </div>
  );
};

export default ControlPanel;
