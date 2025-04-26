// Generate PNG images from the JavaScript representation
const fs = require('fs');
const { createCanvas } = require('canvas');

// Create canvas
const canvas = createCanvas(800, 600);
const ctx = canvas.getContext('2d');

// Initial state screenshot
function drawInitialState() {
  // Clear canvas
  ctx.fillStyle = '#ffffff';
  ctx.fillRect(0, 0, canvas.width, canvas.height);
  
  // Draw header
  ctx.fillStyle = '#0f172a';
  ctx.font = 'bold 24px sans-serif';
  ctx.textAlign = 'center';
  ctx.fillText('Hierarchical Task Planning Network', 400, 50);
  
  // Draw control panel background
  ctx.fillStyle = '#f8fafc';
  ctx.strokeStyle = '#e2e8f0';
  ctx.lineWidth = 1;
  ctx.beginPath();
  ctx.roundRect(50, 80, 700, 120, 8);
  ctx.fill();
  ctx.stroke();
  
  // Draw control panel title
  ctx.fillStyle = '#0f172a';
  ctx.font = 'bold 18px sans-serif';
  ctx.textAlign = 'left';
  ctx.fillText('Task Planning Network Simulation', 70, 110);
  
  // Draw step button
  ctx.fillStyle = '#3b82f6';
  ctx.beginPath();
  ctx.roundRect(600, 95, 60, 30, 4);
  ctx.fill();
  ctx.fillStyle = '#ffffff';
  ctx.font = 'bold 14px sans-serif';
  ctx.textAlign = 'center';
  ctx.fillText('Step', 630, 115);
  
  // Draw reset button
  ctx.fillStyle = '#ef4444';
  ctx.beginPath();
  ctx.roundRect(670, 95, 60, 30, 4);
  ctx.fill();
  ctx.fillStyle = '#ffffff';
  ctx.font = 'bold 14px sans-serif';
  ctx.textAlign = 'center';
  ctx.fillText('Reset', 700, 115);
  
  // Draw statistics section
  ctx.fillStyle = '#0f172a';
  ctx.font = 'bold 14px sans-serif';
  ctx.textAlign = 'left';
  ctx.fillText('Statistics', 70, 150);
  
  // Draw total tasks box
  ctx.fillStyle = '#ffffff';
  ctx.strokeStyle = '#e2e8f0';
  ctx.beginPath();
  ctx.roundRect(70, 160, 100, 60, 4);
  ctx.fill();
  ctx.stroke();
  ctx.fillStyle = '#64748b';
  ctx.font = '12px sans-serif';
  ctx.fillText('Total Tasks', 80, 180);
  ctx.fillStyle = '#0f172a';
  ctx.font = 'bold 18px sans-serif';
  ctx.fillText('0', 80, 205);
  
  // Draw steps taken box
  ctx.fillStyle = '#ffffff';
  ctx.strokeStyle = '#e2e8f0';
  ctx.beginPath();
  ctx.roundRect(180, 160, 100, 60, 4);
  ctx.fill();
  ctx.stroke();
  ctx.fillStyle = '#64748b';
  ctx.font = '12px sans-serif';
  ctx.fillText('Steps Taken', 190, 180);
  ctx.fillStyle = '#0f172a';
  ctx.font = 'bold 18px sans-serif';
  ctx.fillText('0', 190, 205);
  
  // Draw last action section
  ctx.fillStyle = '#0f172a';
  ctx.font = 'bold 14px sans-serif';
  ctx.textAlign = 'left';
  ctx.fillText('Last Action', 300, 150);
  
  // Draw last action box
  ctx.fillStyle = '#ffffff';
  ctx.strokeStyle = '#e2e8f0';
  ctx.beginPath();
  ctx.roundRect(300, 160, 430, 60, 4);
  ctx.fill();
  ctx.stroke();
  ctx.fillStyle = '#94a3b8';
  ctx.font = 'italic 12px sans-serif';
  ctx.textAlign = 'center';
  ctx.fillText('No actions yet', 515, 190);
  
  // Draw flow area
  ctx.fillStyle = '#ffffff';
  ctx.strokeStyle = '#e2e8f0';
  ctx.beginPath();
  ctx.roundRect(50, 220, 700, 350, 4);
  ctx.fill();
  ctx.stroke();
  
  // Draw empty state message
  ctx.fillStyle = '#64748b';
  ctx.font = 'italic 14px sans-serif';
  ctx.textAlign = 'center';
  ctx.fillText('No tasks yet. Click "Step" to add tasks.', 400, 400);
  
  return canvas;
}

// After steps screenshot
function drawAfterSteps() {
  // Clear canvas
  ctx.fillStyle = '#ffffff';
  ctx.fillRect(0, 0, canvas.width, canvas.height);
  
  // Draw header
  ctx.fillStyle = '#0f172a';
  ctx.font = 'bold 24px sans-serif';
  ctx.textAlign = 'center';
  ctx.fillText('Hierarchical Task Planning Network', 400, 50);
  
  // Draw control panel background
  ctx.fillStyle = '#f8fafc';
  ctx.strokeStyle = '#e2e8f0';
  ctx.lineWidth = 1;
  ctx.beginPath();
  ctx.roundRect(50, 80, 700, 120, 8);
  ctx.fill();
  ctx.stroke();
  
  // Draw control panel title
  ctx.fillStyle = '#0f172a';
  ctx.font = 'bold 18px sans-serif';
  ctx.textAlign = 'left';
  ctx.fillText('Task Planning Network Simulation', 70, 110);
  
  // Draw step button
  ctx.fillStyle = '#3b82f6';
  ctx.beginPath();
  ctx.roundRect(600, 95, 60, 30, 4);
  ctx.fill();
  ctx.fillStyle = '#ffffff';
  ctx.font = 'bold 14px sans-serif';
  ctx.textAlign = 'center';
  ctx.fillText('Step', 630, 115);
  
  // Draw reset button
  ctx.fillStyle = '#ef4444';
  ctx.beginPath();
  ctx.roundRect(670, 95, 60, 30, 4);
  ctx.fill();
  ctx.fillStyle = '#ffffff';
  ctx.font = 'bold 14px sans-serif';
  ctx.textAlign = 'center';
  ctx.fillText('Reset', 700, 115);
  
  // Draw statistics section
  ctx.fillStyle = '#0f172a';
  ctx.font = 'bold 14px sans-serif';
  ctx.textAlign = 'left';
  ctx.fillText('Statistics', 70, 150);
  
  // Draw total tasks box
  ctx.fillStyle = '#ffffff';
  ctx.strokeStyle = '#e2e8f0';
  ctx.beginPath();
  ctx.roundRect(70, 160, 100, 60, 4);
  ctx.fill();
  ctx.stroke();
  ctx.fillStyle = '#64748b';
  ctx.font = '12px sans-serif';
  ctx.fillText('Total Tasks', 80, 180);
  ctx.fillStyle = '#0f172a';
  ctx.font = 'bold 18px sans-serif';
  ctx.fillText('5', 80, 205);
  
  // Draw steps taken box
  ctx.fillStyle = '#ffffff';
  ctx.strokeStyle = '#e2e8f0';
  ctx.beginPath();
  ctx.roundRect(180, 160, 100, 60, 4);
  ctx.fill();
  ctx.stroke();
  ctx.fillStyle = '#64748b';
  ctx.font = '12px sans-serif';
  ctx.fillText('Steps Taken', 190, 180);
  ctx.fillStyle = '#0f172a';
  ctx.font = 'bold 18px sans-serif';
  ctx.fillText('7', 190, 205);
  
  // Draw last action section
  ctx.fillStyle = '#0f172a';
  ctx.font = 'bold 14px sans-serif';
  ctx.textAlign = 'left';
  ctx.fillText('Last Action', 300, 150);
  
  // Draw last action box
  ctx.fillStyle = '#ffffff';
  ctx.strokeStyle = '#e2e8f0';
  ctx.beginPath();
  ctx.roundRect(300, 160, 430, 60, 4);
  ctx.fill();
  ctx.stroke();
  ctx.fillStyle = '#0f172a';
  ctx.font = 'bold 12px sans-serif';
  ctx.textAlign = 'left';
  ctx.fillText('SIMULATE_UPDATE_STATUS', 310, 180);
  ctx.fillStyle = '#64748b';
  ctx.font = '12px sans-serif';
  ctx.fillText('Task: task-3 • Status: pending → in-progress', 310, 200);
  ctx.fillStyle = '#94a3b8';
  ctx.font = '10px sans-serif';
  ctx.fillText('10:24:30 PM', 310, 215);
  
  // Draw flow area
  ctx.fillStyle = '#ffffff';
  ctx.strokeStyle = '#e2e8f0';
  ctx.beginPath();
  ctx.roundRect(50, 220, 700, 350, 4);
  ctx.fill();
  ctx.stroke();
  
  // Draw task nodes and edges
  
  // Task 1 (Root)
  ctx.fillStyle = '#dbeafe';
  ctx.strokeStyle = '#3b82f6';
  ctx.lineWidth = 2;
  ctx.beginPath();
  ctx.roundRect(100, 300, 150, 80, 8);
  ctx.fill();
  ctx.stroke();
  ctx.fillStyle = '#0f172a';
  ctx.font = 'bold 14px sans-serif';
  ctx.textAlign = 'left';
  ctx.fillText('Task 1', 110, 320);
  ctx.fillStyle = '#334155';
  ctx.font = '12px sans-serif';
  ctx.fillText('This is a simulated task 1', 110, 340);
  ctx.fillStyle = '#64748b';
  ctx.font = '10px sans-serif';
  ctx.fillText('Status: in-progress', 110, 360);
  
  // Task 2 (Child of Task 1)
  ctx.fillStyle = '#dcfce7';
  ctx.strokeStyle = '#22c55e';
  ctx.lineWidth = 2;
  ctx.beginPath();
  ctx.roundRect(350, 250, 150, 80, 8);
  ctx.fill();
  ctx.stroke();
  ctx.fillStyle = '#0f172a';
  ctx.font = 'bold 14px sans-serif';
  ctx.textAlign = 'left';
  ctx.fillText('Task 2', 360, 270);
  ctx.fillStyle = '#334155';
  ctx.font = '12px sans-serif';
  ctx.fillText('This is a simulated task 2', 360, 290);
  ctx.fillStyle = '#64748b';
  ctx.font = '10px sans-serif';
  ctx.fillText('Status: completed', 360, 310);
  
  // Task 3 (Child of Task 1)
  ctx.fillStyle = '#dbeafe';
  ctx.strokeStyle = '#3b82f6';
  ctx.lineWidth = 2;
  ctx.beginPath();
  ctx.roundRect(350, 350, 150, 80, 8);
  ctx.fill();
  ctx.stroke();
  ctx.fillStyle = '#0f172a';
  ctx.font = 'bold 14px sans-serif';
  ctx.textAlign = 'left';
  ctx.fillText('Task 3', 360, 370);
  ctx.fillStyle = '#334155';
  ctx.font = '12px sans-serif';
  ctx.fillText('This is a simulated task 3', 360, 390);
  ctx.fillStyle = '#64748b';
  ctx.font = '10px sans-serif';
  ctx.fillText('Status: in-progress', 360, 410);
  
  // Task 4 (Child of Task 2)
  ctx.fillStyle = '#f8fafc';
  ctx.strokeStyle = '#94a3b8';
  ctx.lineWidth = 2;
  ctx.beginPath();
  ctx.roundRect(600, 250, 150, 80, 8);
  ctx.fill();
  ctx.stroke();
  ctx.fillStyle = '#0f172a';
  ctx.font = 'bold 14px sans-serif';
  ctx.textAlign = 'left';
  ctx.fillText('Task 4', 610, 270);
  ctx.fillStyle = '#334155';
  ctx.font = '12px sans-serif';
  ctx.fillText('This is a simulated task 4', 610, 290);
  ctx.fillStyle = '#64748b';
  ctx.font = '10px sans-serif';
  ctx.fillText('Status: pending', 610, 310);
  
  // Task 5 (Child of Task 3)
  ctx.fillStyle = '#fee2e2';
  ctx.strokeStyle = '#ef4444';
  ctx.lineWidth = 2;
  ctx.beginPath();
  ctx.roundRect(600, 350, 150, 80, 8);
  ctx.fill();
  ctx.stroke();
  ctx.fillStyle = '#0f172a';
  ctx.font = 'bold 14px sans-serif';
  ctx.textAlign = 'left';
  ctx.fillText('Task 5', 610, 370);
  ctx.fillStyle = '#334155';
  ctx.font = '12px sans-serif';
  ctx.fillText('This is a simulated task 5', 610, 390);
  ctx.fillStyle = '#64748b';
  ctx.font = '10px sans-serif';
  ctx.fillText('Status: failed', 610, 410);
  
  // Draw edges
  // Edge from Task 1 to Task 2
  ctx.strokeStyle = '#3b82f6';
  ctx.lineWidth = 2;
  ctx.beginPath();
  ctx.moveTo(250, 320);
  ctx.bezierCurveTo(300, 320, 300, 290, 350, 290);
  ctx.stroke();
  
  // Edge from Task 1 to Task 3
  ctx.strokeStyle = '#3b82f6';
  ctx.lineWidth = 2;
  ctx.beginPath();
  ctx.moveTo(250, 340);
  ctx.bezierCurveTo(300, 340, 300, 390, 350, 390);
  ctx.stroke();
  
  // Edge from Task 2 to Task 4
  ctx.strokeStyle = '#22c55e';
  ctx.lineWidth = 2;
  ctx.beginPath();
  ctx.moveTo(500, 290);
  ctx.bezierCurveTo(550, 290, 550, 290, 600, 290);
  ctx.stroke();
  
  // Edge from Task 3 to Task 5
  ctx.strokeStyle = '#ef4444';
  ctx.lineWidth = 2;
  ctx.beginPath();
  ctx.moveTo(500, 390);
  ctx.bezierCurveTo(550, 390, 550, 390, 600, 390);
  ctx.stroke();
  
  return canvas;
}

// Generate and save the images
const initialCanvas = drawInitialState();
const initialBuffer = initialCanvas.toBuffer('image/png');
fs.writeFileSync('/home/ubuntu/task-network-demo/docs/screenshots/initial_state.png', initialBuffer);

const afterStepsCanvas = drawAfterSteps();
const afterStepsBuffer = afterStepsCanvas.toBuffer('image/png');
fs.writeFileSync('/home/ubuntu/task-network-demo/docs/screenshots/after_steps.png', afterStepsBuffer);

console.log('Screenshots generated successfully!');
