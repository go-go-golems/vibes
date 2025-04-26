import { createSlice } from '@reduxjs/toolkit';

// Define task status constants
export const TaskStatus = {
  PENDING: 'pending',
  IN_PROGRESS: 'in-progress',
  COMPLETED: 'completed',
  FAILED: 'failed'
};

// Initial state for the task slice
const initialState = {
  tasks: [],
  nextId: 1,
  history: []
};

// Create the task slice
export const taskSlice = createSlice({
  name: 'tasks',
  initialState,
  reducers: {
    // Add a new task to the network
    addTask: (state, action) => {
      const { title, description, parentId } = action.payload;
      const newTask = {
        id: `task-${state.nextId}`,
        title,
        description,
        status: TaskStatus.PENDING,
        parentId,
        children: [],
        createdAt: new Date().toISOString()
      };
      
      state.tasks.push(newTask);
      state.nextId += 1;
      
      // If this task has a parent, add it to the parent's children
      if (parentId) {
        const parent = state.tasks.find(task => task.id === parentId);
        if (parent) {
          parent.children.push(newTask.id);
        }
      }
      
      // Add to history
      state.history.push({
        action: 'ADD_TASK',
        taskId: newTask.id,
        timestamp: new Date().toISOString()
      });
    },
    
    // Update a task's status
    updateTaskStatus: (state, action) => {
      const { taskId, status } = action.payload;
      const task = state.tasks.find(task => task.id === taskId);
      
      if (task) {
        task.status = status;
        task.updatedAt = new Date().toISOString();
        
        // Add to history
        state.history.push({
          action: 'UPDATE_STATUS',
          taskId,
          oldStatus: task.status,
          newStatus: status,
          timestamp: new Date().toISOString()
        });
      }
    },
    
    // Simulate a step in the agent's planning process
    simulateStep: (state) => {
      // Logic to simulate an agent step:
      // 1. Either add a new task
      // 2. Or update an existing task's status
      
      const pendingTasks = state.tasks.filter(task => task.status === TaskStatus.PENDING);
      const inProgressTasks = state.tasks.filter(task => task.status === TaskStatus.IN_PROGRESS);
      
      // Randomly decide what to do
      const action = Math.random();
      
      if (action < 0.4 || state.tasks.length === 0) {
        // Add a new task (40% chance or if no tasks exist)
        const parentCandidates = [...pendingTasks, ...inProgressTasks];
        const parentId = parentCandidates.length > 0 && Math.random() > 0.3 
          ? parentCandidates[Math.floor(Math.random() * parentCandidates.length)].id 
          : null;
        
        const taskNumber = state.nextId;
        const newTask = {
          id: `task-${taskNumber}`,
          title: `Task ${taskNumber}`,
          description: `This is a simulated task ${taskNumber}`,
          status: TaskStatus.PENDING,
          parentId,
          children: [],
          createdAt: new Date().toISOString()
        };
        
        state.tasks.push(newTask);
        state.nextId += 1;
        
        // If this task has a parent, add it to the parent's children
        if (parentId) {
          const parent = state.tasks.find(task => task.id === parentId);
          if (parent) {
            parent.children.push(newTask.id);
          }
        }
        
        // Add to history
        state.history.push({
          action: 'SIMULATE_ADD_TASK',
          taskId: newTask.id,
          timestamp: new Date().toISOString()
        });
      } else if (action < 0.8 && pendingTasks.length > 0) {
        // Update a pending task to in-progress (40% chance)
        const taskToUpdate = pendingTasks[Math.floor(Math.random() * pendingTasks.length)];
        taskToUpdate.status = TaskStatus.IN_PROGRESS;
        taskToUpdate.updatedAt = new Date().toISOString();
        
        // Add to history
        state.history.push({
          action: 'SIMULATE_UPDATE_STATUS',
          taskId: taskToUpdate.id,
          oldStatus: TaskStatus.PENDING,
          newStatus: TaskStatus.IN_PROGRESS,
          timestamp: new Date().toISOString()
        });
      } else if (inProgressTasks.length > 0) {
        // Complete an in-progress task (20% chance)
        const taskToUpdate = inProgressTasks[Math.floor(Math.random() * inProgressTasks.length)];
        const newStatus = Math.random() > 0.8 ? TaskStatus.FAILED : TaskStatus.COMPLETED;
        taskToUpdate.status = newStatus;
        taskToUpdate.updatedAt = new Date().toISOString();
        
        // Add to history
        state.history.push({
          action: 'SIMULATE_UPDATE_STATUS',
          taskId: taskToUpdate.id,
          oldStatus: TaskStatus.IN_PROGRESS,
          newStatus: newStatus,
          timestamp: new Date().toISOString()
        });
      }
    },
    
    // Reset the task network
    resetTasks: (state) => {
      state.tasks = [];
      state.nextId = 1;
      state.history.push({
        action: 'RESET',
        timestamp: new Date().toISOString()
      });
    }
  }
});

// Export actions and reducer
export const { addTask, updateTaskStatus, simulateStep, resetTasks } = taskSlice.actions;
export default taskSlice.reducer;
