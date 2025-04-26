// Representative screenshot of the initial application state
const initialScreenshot = {
  width: 800,
  height: 600,
  elements: [
    // Header
    {
      type: 'text',
      content: 'Hierarchical Task Planning Network',
      position: { x: 400, y: 50 },
      style: { fontSize: 24, fontWeight: 'bold', textAlign: 'center' }
    },
    // Control Panel
    {
      type: 'rectangle',
      position: { x: 400, y: 150 },
      size: { width: 700, height: 120 },
      style: { fill: '#f8fafc', stroke: '#e2e8f0', borderRadius: 8 }
    },
    {
      type: 'text',
      content: 'Task Planning Network Simulation',
      position: { x: 200, y: 120 },
      style: { fontSize: 18, fontWeight: 'bold' }
    },
    {
      type: 'rectangle',
      position: { x: 650, y: 120 },
      size: { width: 80, height: 30 },
      style: { fill: '#3b82f6', stroke: 'none', borderRadius: 4 }
    },
    {
      type: 'text',
      content: 'Step',
      position: { x: 650, y: 120 },
      style: { fontSize: 14, fontWeight: 'bold', color: 'white' }
    },
    {
      type: 'rectangle',
      position: { x: 740, y: 120 },
      size: { width: 80, height: 30 },
      style: { fill: '#ef4444', stroke: 'none', borderRadius: 4 }
    },
    {
      type: 'text',
      content: 'Reset',
      position: { x: 740, y: 120 },
      style: { fontSize: 14, fontWeight: 'bold', color: 'white' }
    },
    {
      type: 'text',
      content: 'Statistics',
      position: { x: 200, y: 160 },
      style: { fontSize: 14, fontWeight: 'bold' }
    },
    {
      type: 'rectangle',
      position: { x: 200, y: 190 },
      size: { width: 120, height: 60 },
      style: { fill: 'white', stroke: '#e2e8f0', borderRadius: 4 }
    },
    {
      type: 'text',
      content: 'Total Tasks',
      position: { x: 200, y: 180 },
      style: { fontSize: 12, color: '#64748b' }
    },
    {
      type: 'text',
      content: '0',
      position: { x: 200, y: 200 },
      style: { fontSize: 18, fontWeight: 'bold' }
    },
    {
      type: 'rectangle',
      position: { x: 330, y: 190 },
      size: { width: 120, height: 60 },
      style: { fill: 'white', stroke: '#e2e8f0', borderRadius: 4 }
    },
    {
      type: 'text',
      content: 'Steps Taken',
      position: { x: 330, y: 180 },
      style: { fontSize: 12, color: '#64748b' }
    },
    {
      type: 'text',
      content: '0',
      position: { x: 330, y: 200 },
      style: { fontSize: 18, fontWeight: 'bold' }
    },
    {
      type: 'text',
      content: 'Last Action',
      position: { x: 550, y: 160 },
      style: { fontSize: 14, fontWeight: 'bold' }
    },
    {
      type: 'rectangle',
      position: { x: 550, y: 190 },
      size: { width: 250, height: 60 },
      style: { fill: 'white', stroke: '#e2e8f0', borderRadius: 4 }
    },
    {
      type: 'text',
      content: 'No actions yet',
      position: { x: 550, y: 190 },
      style: { fontSize: 12, fontStyle: 'italic', color: '#94a3b8' }
    },
    // Flow Area
    {
      type: 'rectangle',
      position: { x: 400, y: 400 },
      size: { width: 700, height: 400 },
      style: { fill: '#ffffff', stroke: '#e2e8f0', borderRadius: 4 }
    },
    {
      type: 'text',
      content: 'No tasks yet. Click "Step" to add tasks.',
      position: { x: 400, y: 400 },
      style: { fontSize: 14, color: '#64748b', fontStyle: 'italic' }
    }
  ]
};

// Representative screenshot after a few steps
const afterStepsScreenshot = {
  width: 800,
  height: 600,
  elements: [
    // Header
    {
      type: 'text',
      content: 'Hierarchical Task Planning Network',
      position: { x: 400, y: 50 },
      style: { fontSize: 24, fontWeight: 'bold', textAlign: 'center' }
    },
    // Control Panel
    {
      type: 'rectangle',
      position: { x: 400, y: 150 },
      size: { width: 700, height: 120 },
      style: { fill: '#f8fafc', stroke: '#e2e8f0', borderRadius: 8 }
    },
    {
      type: 'text',
      content: 'Task Planning Network Simulation',
      position: { x: 200, y: 120 },
      style: { fontSize: 18, fontWeight: 'bold' }
    },
    {
      type: 'rectangle',
      position: { x: 650, y: 120 },
      size: { width: 80, height: 30 },
      style: { fill: '#3b82f6', stroke: 'none', borderRadius: 4 }
    },
    {
      type: 'text',
      content: 'Step',
      position: { x: 650, y: 120 },
      style: { fontSize: 14, fontWeight: 'bold', color: 'white' }
    },
    {
      type: 'rectangle',
      position: { x: 740, y: 120 },
      size: { width: 80, height: 30 },
      style: { fill: '#ef4444', stroke: 'none', borderRadius: 4 }
    },
    {
      type: 'text',
      content: 'Reset',
      position: { x: 740, y: 120 },
      style: { fontSize: 14, fontWeight: 'bold', color: 'white' }
    },
    {
      type: 'text',
      content: 'Statistics',
      position: { x: 200, y: 160 },
      style: { fontSize: 14, fontWeight: 'bold' }
    },
    {
      type: 'rectangle',
      position: { x: 200, y: 190 },
      size: { width: 120, height: 60 },
      style: { fill: 'white', stroke: '#e2e8f0', borderRadius: 4 }
    },
    {
      type: 'text',
      content: 'Total Tasks',
      position: { x: 200, y: 180 },
      style: { fontSize: 12, color: '#64748b' }
    },
    {
      type: 'text',
      content: '5',
      position: { x: 200, y: 200 },
      style: { fontSize: 18, fontWeight: 'bold' }
    },
    {
      type: 'rectangle',
      position: { x: 330, y: 190 },
      size: { width: 120, height: 60 },
      style: { fill: 'white', stroke: '#e2e8f0', borderRadius: 4 }
    },
    {
      type: 'text',
      content: 'Steps Taken',
      position: { x: 330, y: 180 },
      style: { fontSize: 12, color: '#64748b' }
    },
    {
      type: 'text',
      content: '7',
      position: { x: 330, y: 200 },
      style: { fontSize: 18, fontWeight: 'bold' }
    },
    {
      type: 'text',
      content: 'Last Action',
      position: { x: 550, y: 160 },
      style: { fontSize: 14, fontWeight: 'bold' }
    },
    {
      type: 'rectangle',
      position: { x: 550, y: 190 },
      size: { width: 250, height: 60 },
      style: { fill: 'white', stroke: '#e2e8f0', borderRadius: 4 }
    },
    {
      type: 'text',
      content: 'SIMULATE_UPDATE_STATUS',
      position: { x: 550, y: 180 },
      style: { fontSize: 12, fontWeight: 'bold' }
    },
    {
      type: 'text',
      content: 'Task: task-3 • Status: pending → in-progress',
      position: { x: 550, y: 200 },
      style: { fontSize: 10, color: '#64748b' }
    },
    // Flow Area with Tasks
    {
      type: 'rectangle',
      position: { x: 400, y: 400 },
      size: { width: 700, height: 400 },
      style: { fill: '#ffffff', stroke: '#e2e8f0', borderRadius: 4 }
    },
    // Task 1 (Root)
    {
      type: 'rectangle',
      position: { x: 100, y: 350 },
      size: { width: 200, height: 100 },
      style: { fill: '#dbeafe', stroke: '#3b82f6', borderRadius: 8 }
    },
    {
      type: 'text',
      content: 'Task 1',
      position: { x: 100, y: 330 },
      style: { fontSize: 14, fontWeight: 'bold' }
    },
    {
      type: 'text',
      content: 'This is a simulated task 1',
      position: { x: 100, y: 350 },
      style: { fontSize: 12 }
    },
    {
      type: 'text',
      content: 'Status: in-progress',
      position: { x: 100, y: 370 },
      style: { fontSize: 10, color: '#64748b' }
    },
    // Task 2 (Child of Task 1)
    {
      type: 'rectangle',
      position: { x: 350, y: 300 },
      size: { width: 200, height: 100 },
      style: { fill: '#dcfce7', stroke: '#22c55e', borderRadius: 8 }
    },
    {
      type: 'text',
      content: 'Task 2',
      position: { x: 350, y: 280 },
      style: { fontSize: 14, fontWeight: 'bold' }
    },
    {
      type: 'text',
      content: 'This is a simulated task 2',
      position: { x: 350, y: 300 },
      style: { fontSize: 12 }
    },
    {
      type: 'text',
      content: 'Status: completed',
      position: { x: 350, y: 320 },
      style: { fontSize: 10, color: '#64748b' }
    },
    // Task 3 (Child of Task 1)
    {
      type: 'rectangle',
      position: { x: 350, y: 450 },
      size: { width: 200, height: 100 },
      style: { fill: '#dbeafe', stroke: '#3b82f6', borderRadius: 8 }
    },
    {
      type: 'text',
      content: 'Task 3',
      position: { x: 350, y: 430 },
      style: { fontSize: 14, fontWeight: 'bold' }
    },
    {
      type: 'text',
      content: 'This is a simulated task 3',
      position: { x: 350, y: 450 },
      style: { fontSize: 12 }
    },
    {
      type: 'text',
      content: 'Status: in-progress',
      position: { x: 350, y: 470 },
      style: { fontSize: 10, color: '#64748b' }
    },
    // Task 4 (Child of Task 2)
    {
      type: 'rectangle',
      position: { x: 600, y: 300 },
      size: { width: 200, height: 100 },
      style: { fill: '#f8fafc', stroke: '#94a3b8', borderRadius: 8 }
    },
    {
      type: 'text',
      content: 'Task 4',
      position: { x: 600, y: 280 },
      style: { fontSize: 14, fontWeight: 'bold' }
    },
    {
      type: 'text',
      content: 'This is a simulated task 4',
      position: { x: 600, y: 300 },
      style: { fontSize: 12 }
    },
    {
      type: 'text',
      content: 'Status: pending',
      position: { x: 600, y: 320 },
      style: { fontSize: 10, color: '#64748b' }
    },
    // Task 5 (Child of Task 3)
    {
      type: 'rectangle',
      position: { x: 600, y: 450 },
      size: { width: 200, height: 100 },
      style: { fill: '#fee2e2', stroke: '#ef4444', borderRadius: 8 }
    },
    {
      type: 'text',
      content: 'Task 5',
      position: { x: 600, y: 430 },
      style: { fontSize: 14, fontWeight: 'bold' }
    },
    {
      type: 'text',
      content: 'This is a simulated task 5',
      position: { x: 600, y: 450 },
      style: { fontSize: 12 }
    },
    {
      type: 'text',
      content: 'Status: failed',
      position: { x: 600, y: 470 },
      style: { fontSize: 10, color: '#64748b' }
    },
    // Edges
    {
      type: 'edge',
      source: { x: 200, y: 350 },
      target: { x: 350, y: 300 },
      style: { stroke: '#3b82f6', animated: true }
    },
    {
      type: 'edge',
      source: { x: 200, y: 350 },
      target: { x: 350, y: 450 },
      style: { stroke: '#3b82f6', animated: true }
    },
    {
      type: 'edge',
      source: { x: 450, y: 300 },
      target: { x: 600, y: 300 },
      style: { stroke: '#22c55e' }
    },
    {
      type: 'edge',
      source: { x: 450, y: 450 },
      target: { x: 600, y: 450 },
      style: { stroke: '#ef4444' }
    }
  ]
};

// Export the screenshots
export { initialScreenshot, afterStepsScreenshot };
