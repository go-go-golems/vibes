# Event-Driven Hierarchical Task Network with State Machine Architecture

This document presents an enhanced design that combines a hierarchical task network (HTN) with event-driven architecture and state machine principles for a report writing agent. This hybrid approach leverages the strengths of all three paradigms to create a highly flexible, decoupled, and robust system.

## Table of Contents

1. [Introduction](#introduction)
2. [Architectural Overview](#architectural-overview)
3. [Event System Design](#event-system-design)
4. [State Machine Integration](#state-machine-integration)
5. [Hierarchical Task Network](#hierarchical-task-network)
6. [Implementation](#implementation)
7. [Example Execution](#example-execution)
8. [Advanced Features](#advanced-features)

## Introduction

Our enhanced architecture combines three powerful paradigms:

1. **Hierarchical Task Network (HTN)**: Provides a structured approach to decomposing complex tasks into simpler subtasks, creating a tree-like structure that represents the plan.

2. **Event-Driven Architecture**: Enables loose coupling between components, allowing them to communicate through events without direct dependencies.

3. **State Machine**: Formalizes the lifecycle of tasks and the system as a whole, making state transitions explicit and manageable.

By integrating these approaches, we create a system where:
- Tasks are organized hierarchically but communicate through events
- Each task operates as a state machine with well-defined transitions
- The entire system is reactive, responding to events rather than following procedural logic

## Architectural Overview

The architecture consists of the following components:

```
┌─────────────────────────────────────────────────────────────┐
│                     Report Writing Agent                     │
└─────────────────────────────────────────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────┐
│                       Event Queue                            │
└─────────────────────────────────────────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────┐
│                  Task Network Manager                        │
├─────────────────┬─────────────────────┬─────────────────────┤
│  Task Planner   │   Task Executor     │   Task Generator    │
└─────────────────┴─────────────────────┴─────────────────────┘
          │                 │                     │
          ▼                 ▼                     ▼
┌─────────────────┐ ┌─────────────────┐ ┌─────────────────────┐
│   LLM Service   │ │  Tool Registry  │ │   Memory System     │
└─────────────────┘ └─────────────────┘ └─────────────────────┘
                               │
                               ▼
┌─────────────────┬─────────────────────┬─────────────────────┐
│  Search Tool    │   Writing Tool      │   Combining Tool    │
└─────────────────┴─────────────────────┴─────────────────────┘
```

## Event System Design

### Event Types

The system uses a comprehensive set of events to drive all operations:

```go
// EventType represents the type of an event
type EventType string

const (
    // Task lifecycle events
    EventTaskCreated       EventType = "task.created"
    EventTaskPlanned       EventType = "task.planned"
    EventTaskStarted       EventType = "task.started"
    EventTaskCompleted     EventType = "task.completed"
    EventTaskFailed        EventType = "task.failed"
    EventTaskCancelled     EventType = "task.cancelled"
    
    // Planning events
    EventPlanRequested     EventType = "plan.requested"
    EventPlanCreated       EventType = "plan.created"
    
    // Execution events
    EventExecutionRequested EventType = "execution.requested"
    EventExecutionStarted   EventType = "execution.started"
    EventExecutionCompleted EventType = "execution.completed"
    EventExecutionFailed    EventType = "execution.failed"
    
    // Tool events
    EventSearchRequested    EventType = "search.requested"
    EventSearchCompleted    EventType = "search.completed"
    EventSearchFailed       EventType = "search.failed"
    
    EventWriteRequested     EventType = "write.requested"
    EventWriteCompleted     EventType = "write.completed"
    EventWriteFailed        EventType = "write.failed"
    
    EventCombineRequested   EventType = "combine.requested"
    EventCombineCompleted   EventType = "combine.completed"
    EventCombineFailed      EventType = "combine.failed"
    
    // LLM events
    EventLLMRequested       EventType = "llm.requested"
    EventLLMCompleted       EventType = "llm.completed"
    EventLLMFailed          EventType = "llm.failed"
    
    // Memory events
    EventMemoryStoreRequested  EventType = "memory.store.requested"
    EventMemoryStoreCompleted  EventType = "memory.store.completed"
    EventMemoryRetrieveRequested EventType = "memory.retrieve.requested"
    EventMemoryRetrieveCompleted EventType = "memory.retrieve.completed"
    
    // System events
    EventSystemStarted      EventType = "system.started"
    EventSystemStopped      EventType = "system.stopped"
    EventSystemError        EventType = "system.error"
)
```

### Event Structure

Events have a consistent structure with metadata and payload:

```go
// Event represents an event in the system
type Event interface {
    ID() string
    Type() EventType
    Timestamp() time.Time
    Payload() interface{}
    ParentID() string
    TaskID() string
    Metadata() map[string]string
}

// BaseEvent provides a base implementation of Event
type BaseEvent struct {
    id        string
    eventType EventType
    timestamp time.Time
    payload   interface{}
    parentID  string
    taskID    string
    metadata  map[string]string
}

// NewEvent creates a new event
func NewEvent(eventType EventType, payload interface{}, taskID, parentID string) Event {
    return &BaseEvent{
        id:        uuid.New().String(),
        eventType: eventType,
        timestamp: time.Now(),
        payload:   payload,
        taskID:    taskID,
        parentID:  parentID,
        metadata:  make(map[string]string),
    }
}

// ID returns the event ID
func (e *BaseEvent) ID() string {
    return e.id
}

// Type returns the event type
func (e *BaseEvent) Type() EventType {
    return e.eventType
}

// Timestamp returns the event timestamp
func (e *BaseEvent) Timestamp() time.Time {
    return e.timestamp
}

// Payload returns the event payload
func (e *BaseEvent) Payload() interface{} {
    return e.payload
}

// ParentID returns the parent event ID
func (e *BaseEvent) ParentID() string {
    return e.parentID
}

// TaskID returns the associated task ID
func (e *BaseEvent) TaskID() string {
    return e.taskID
}

// Metadata returns the event metadata
func (e *BaseEvent) Metadata() map[string]string {
    return e.metadata
}
```

### Event Queue

The event queue is the central communication mechanism:

```go
// EventQueue manages events in the system
type EventQueue struct {
    queue     chan Event
    handlers  map[EventType][]EventHandler
    mutex     sync.RWMutex
    isRunning bool
    wg        sync.WaitGroup
}

// EventHandler handles events
type EventHandler func(Event) error

// NewEventQueue creates a new event queue
func NewEventQueue(bufferSize int) *EventQueue {
    return &EventQueue{
        queue:    make(chan Event, bufferSize),
        handlers: make(map[EventType][]EventHandler),
    }
}

// RegisterHandler registers a handler for an event type
func (eq *EventQueue) RegisterHandler(eventType EventType, handler EventHandler) {
    eq.mutex.Lock()
    defer eq.mutex.Unlock()
    
    eq.handlers[eventType] = append(eq.handlers[eventType], handler)
}

// Publish publishes an event to the queue
func (eq *EventQueue) Publish(event Event) {
    eq.queue <- event
}

// Start starts processing events
func (eq *EventQueue) Start() {
    eq.mutex.Lock()
    if eq.isRunning {
        eq.mutex.Unlock()
        return
    }
    
    eq.isRunning = true
    eq.mutex.Unlock()
    
    eq.wg.Add(1)
    go eq.processEvents()
}

// Stop stops processing events
func (eq *EventQueue) Stop() {
    eq.mutex.Lock()
    eq.isRunning = false
    eq.mutex.Unlock()
    
    eq.wg.Wait()
}

// processEvents processes events from the queue
func (eq *EventQueue) processEvents() {
    defer eq.wg.Done()
    
    for {
        eq.mutex.RLock()
        isRunning := eq.isRunning
        eq.mutex.RUnlock()
        
        if !isRunning {
            return
        }
        
        select {
        case event := <-eq.queue:
            eq.handleEvent(event)
        default:
            // No events, sleep briefly
            time.Sleep(10 * time.Millisecond)
        }
    }
}

// handleEvent handles an event
func (eq *EventQueue) handleEvent(event Event) {
    eq.mutex.RLock()
    handlers := eq.handlers[event.Type()]
    eq.mutex.RUnlock()
    
    for _, handler := range handlers {
        err := handler(event)
        if err != nil {
            // Log error but continue processing
            log.Printf("Error handling event %s: %v", event.ID(), err)
        }
    }
}
```

## State Machine Integration

### Task States

Each task operates as a state machine with well-defined states and transitions:

```go
// TaskState represents the state of a task in the state machine
type TaskState string

const (
    // Task states
    StateCreated    TaskState = "created"
    StatePlanning   TaskState = "planning"
    StatePlanned    TaskState = "planned"
    StateReady      TaskState = "ready"
    StateExecuting  TaskState = "executing"
    StateWaiting    TaskState = "waiting"
    StateCompleted  TaskState = "completed"
    StateFailed     TaskState = "failed"
    StateCancelled  TaskState = "cancelled"
)

// TaskTransition represents a transition between task states
type TaskTransition struct {
    From      TaskState
    To        TaskState
    EventType EventType
    Guard     func(*Task) bool
    Action    func(*Task, Event) error
}

// TaskStateMachine manages task state transitions
type TaskStateMachine struct {
    transitions map[TaskState][]TaskTransition
}

// NewTaskStateMachine creates a new task state machine
func NewTaskStateMachine() *TaskStateMachine {
    sm := &TaskStateMachine{
        transitions: make(map[TaskState][]TaskTransition),
    }
    
    // Define transitions
    sm.addTransition(StateCreated, StatePlanning, EventPlanRequested, nil, nil)
    sm.addTransition(StatePlanning, StatePlanned, EventPlanCreated, nil, nil)
    sm.addTransition(StatePlanned, StateReady, EventTaskPlanned, isReadyToExecute, nil)
    sm.addTransition(StateReady, StateExecuting, EventExecutionRequested, nil, nil)
    sm.addTransition(StateExecuting, StateWaiting, EventExecutionStarted, nil, nil)
    sm.addTransition(StateWaiting, StateCompleted, EventExecutionCompleted, nil, nil)
    sm.addTransition(StateWaiting, StateFailed, EventExecutionFailed, nil, nil)
    sm.addTransition(StateExecuting, StateFailed, EventExecutionFailed, nil, nil)
    sm.addTransition(StateReady, StateCancelled, EventTaskCancelled, nil, nil)
    sm.addTransition(StateExecuting, StateCancelled, EventTaskCancelled, nil, nil)
    sm.addTransition(StateWaiting, StateCancelled, EventTaskCancelled, nil, nil)
    
    return sm
}

// addTransition adds a transition to the state machine
func (sm *TaskStateMachine) addTransition(from, to TaskState, eventType EventType, 
                                         guard func(*Task) bool, action func(*Task, Event) error) {
    sm.transitions[from] = append(sm.transitions[from], TaskTransition{
        From:      from,
        To:        to,
        EventType: eventType,
        Guard:     guard,
        Action:    action,
    })
}

// HandleEvent handles an event for a task
func (sm *TaskStateMachine) HandleEvent(task *Task, event Event) error {
    // Get transitions for current state
    transitions, ok := sm.transitions[task.State]
    if !ok {
        return fmt.Errorf("no transitions defined for state: %s", task.State)
    }
    
    // Find matching transition
    for _, transition := range transitions {
        if transition.EventType == event.Type() {
            // Check guard condition
            if transition.Guard != nil && !transition.Guard(task) {
                return fmt.Errorf("guard condition failed for transition from %s to %s", 
                                 transition.From, transition.To)
            }
            
            // Execute action
            if transition.Action != nil {
                err := transition.Action(task, event)
                if err != nil {
                    return fmt.Errorf("action failed: %w", err)
                }
            }
            
            // Update task state
            task.State = transition.To
            
            return nil
        }
    }
    
    return fmt.Errorf("no matching transition for event type: %s in state: %s", 
                     event.Type(), task.State)
}

// isReadyToExecute checks if a task is ready to execute
func isReadyToExecute(task *Task) bool {
    // Check if all dependencies are completed
    for _, depID := range task.Dependencies {
        depTask, ok := task.taskNetwork.GetTask(depID)
        if !ok || depTask.State != StateCompleted {
            return false
        }
    }
    
    return true
}
```

### System State Machine

The overall system also operates as a state machine:

```go
// SystemState represents the state of the system
type SystemState string

const (
    // System states
    SystemStateInitializing SystemState = "initializing"
    SystemStateIdle         SystemState = "idle"
    SystemStateProcessing   SystemState = "processing"
    SystemStateCompleting   SystemState = "completing"
    SystemStateCompleted    SystemState = "completed"
    SystemStateError        SystemState = "error"
)

// SystemStateMachine manages system state transitions
type SystemStateMachine struct {
    currentState SystemState
    transitions  map[SystemState]map[EventType]SystemState
    actions      map[SystemState]func()
    mutex        sync.RWMutex
}

// NewSystemStateMachine creates a new system state machine
func NewSystemStateMachine() *SystemStateMachine {
    sm := &SystemStateMachine{
        currentState: SystemStateInitializing,
        transitions:  make(map[SystemState]map[EventType]SystemState),
        actions:      make(map[SystemState]func()),
    }
    
    // Define transitions
    sm.addTransition(SystemStateInitializing, EventSystemStarted, SystemStateIdle)
    sm.addTransition(SystemStateIdle, EventTaskCreated, SystemStateProcessing)
    sm.addTransition(SystemStateProcessing, EventTaskCompleted, SystemStateProcessing)
    sm.addTransition(SystemStateProcessing, EventSystemError, SystemStateError)
    sm.addTransition(SystemStateError, EventSystemStarted, SystemStateIdle)
    sm.addTransition(SystemStateProcessing, EventTaskCompleted, SystemStateCompleting)
    sm.addTransition(SystemStateCompleting, EventSystemStopped, SystemStateCompleted)
    
    return sm
}

// addTransition adds a transition to the state machine
func (sm *SystemStateMachine) addTransition(from SystemState, eventType EventType, to SystemState) {
    if _, ok := sm.transitions[from]; !ok {
        sm.transitions[from] = make(map[EventType]SystemState)
    }
    
    sm.transitions[from][eventType] = to
}

// SetAction sets an action for a state
func (sm *SystemStateMachine) SetAction(state SystemState, action func()) {
    sm.actions[state] = action
}

// HandleEvent handles an event
func (sm *SystemStateMachine) HandleEvent(event Event) error {
    sm.mutex.Lock()
    defer sm.mutex.Unlock()
    
    // Get transitions for current state
    transitions, ok := sm.transitions[sm.currentState]
    if !ok {
        return fmt.Errorf("no transitions defined for state: %s", sm.currentState)
    }
    
    // Find matching transition
    to, ok := transitions[event.Type()]
    if !ok {
        return fmt.Errorf("no transition defined for event type: %s in state: %s", 
                         event.Type(), sm.currentState)
    }
    
    // Update state
    oldState := sm.currentState
    sm.currentState = to
    
    // Execute action for new state
    if action, ok := sm.actions[to]; ok {
        action()
    }
    
    log.Printf("System state transition: %s -> %s triggered by event: %s", 
              oldState, sm.currentState, event.Type())
    
    return nil
}

// GetCurrentState returns the current system state
func (sm *SystemStateMachine) GetCurrentState() SystemState {
    sm.mutex.RLock()
    defer sm.mutex.RUnlock()
    
    return sm.currentState
}
```

## Hierarchical Task Network

### Task Representation

Tasks in our HTN are represented as nodes in a tree structure, now enhanced with state machine integration:

```go
// TaskType represents the type of a task
type TaskType string

const (
    TaskTypeCompound TaskType = "compound"
    TaskTypePrimitive TaskType = "primitive"
)

// TaskCategory represents the category of a task
type TaskCategory string

const (
    TaskCategorySearch    TaskCategory = "search"
    TaskCategoryWrite     TaskCategory = "write"
    TaskCategoryCombine   TaskCategory = "combine"
    TaskCategoryPlan      TaskCategory = "plan"
)

// Task represents a task in the hierarchical task network
type Task struct {
    ID           string
    ParentID     string
    Type         TaskType
    Category     TaskCategory
    Name         string
    Description  string
    State        TaskState
    Dependencies []string
    Children     []*Task
    Parameters   map[string]interface{}
    Result       interface{}
    CreatedAt    time.Time
    StartedAt    time.Time
    CompletedAt  time.Time
    taskNetwork  *TaskNetwork
    eventQueue   *EventQueue
}

// NewTask creates a new task
func NewTask(parentID string, taskType TaskType, category TaskCategory, 
            name, description string, taskNetwork *TaskNetwork, eventQueue *EventQueue) *Task {
    task := &Task{
        ID:          uuid.New().String(),
        ParentID:    parentID,
        Type:        taskType,
        Category:    category,
        Name:        name,
        Description: description,
        State:       StateCreated,
        Dependencies: []string{},
        Children:    []*Task{},
        Parameters:  make(map[string]interface{}),
        CreatedAt:   time.Now(),
        taskNetwork: taskNetwork,
        eventQueue:  eventQueue,
    }
    
    // Publish task created event
    task.eventQueue.Publish(NewEvent(EventTaskCreated, task, task.ID, parentID))
    
    return task
}

// RequestPlanning requests planning for this task
func (t *Task) RequestPlanning() {
    t.eventQueue.Publish(NewEvent(EventPlanRequested, t, t.ID, t.ParentID))
}

// RequestExecution requests execution of this task
func (t *Task) RequestExecution() {
    t.eventQueue.Publish(NewEvent(EventExecutionRequested, t, t.ID, t.ParentID))
}

// Complete marks the task as completed
func (t *Task) Complete(result interface{}) {
    t.Result = result
    t.CompletedAt = time.Now()
    
    t.eventQueue.Publish(NewEvent(EventExecutionCompleted, result, t.ID, t.ParentID))
}

// Fail marks the task as failed
func (t *Task) Fail(err error) {
    t.CompletedAt = time.Now()
    
    t.eventQueue.Publish(NewEvent(EventExecutionFailed, err.Error(), t.ID, t.ParentID))
}

// Cancel cancels the task
func (t *Task) Cancel(reason string) {
    t.CompletedAt = time.Now()
    
    t.eventQueue.Publish(NewEvent(EventTaskCancelled, reason, t.ID, t.ParentID))
}

// AddChild adds a child task
func (t *Task) AddChild(child *Task) {
    t.Children = append(t.Children, child)
}

// AddDependency adds a dependency
func (t *Task) AddDependency(depID string) {
    t.Dependencies = append(t.Dependencies, depID)
}
```

### Task Network

The task network manages the hierarchical structure of tasks, now with event-driven updates:

```go
// TaskNetwork represents a hierarchical task network
type TaskNetwork struct {
    rootTask    *Task
    allTasks    map[string]*Task
    mutex       sync.RWMutex
    eventQueue  *EventQueue
    stateMachine *TaskStateMachine
}

// NewTaskNetwork creates a new task network
func NewTaskNetwork(eventQueue *EventQueue, rootTaskName, rootTaskDescription string) *TaskNetwork {
    tn := &TaskNetwork{
        allTasks:    make(map[string]*Task),
        eventQueue:  eventQueue,
        stateMachine: NewTaskStateMachine(),
    }
    
    // Create root task
    rootTask := NewTask("", TaskTypeCompound, TaskCategoryPlan, 
                      rootTaskName, rootTaskDescription, tn, eventQueue)
    
    tn.rootTask = rootTask
    tn.allTasks[rootTask.ID] = rootTask
    
    // Register event handlers
    eventQueue.RegisterHandler(EventTaskCreated, tn.handleTaskCreated)
    eventQueue.RegisterHandler(EventPlanRequested, tn.handlePlanRequested)
    eventQueue.RegisterHandler(EventPlanCreated, tn.handlePlanCreated)
    eventQueue.RegisterHandler(EventTaskPlanned, tn.handleTaskPlanned)
    eventQueue.RegisterHandler(EventExecutionRequested, tn.handleExecutionRequested)
    eventQueue.RegisterHandler(EventExecutionStarted, tn.handleExecutionStarted)
    eventQueue.RegisterHandler(EventExecutionCompleted, tn.handleExecutionCompleted)
    eventQueue.RegisterHandler(EventExecutionFailed, tn.handleExecutionFailed)
    eventQueue.RegisterHandler(EventTaskCancelled, tn.handleTaskCancelled)
    
    return tn
}

// AddTask adds a task to the network
func (tn *TaskNetwork) AddTask(parentID string, task *Task) error {
    tn.mutex.Lock()
    defer tn.mutex.Unlock()
    
    parent, ok := tn.allTasks[parentID]
    if !ok {
        return fmt.Errorf("parent task not found: %s", parentID)
    }
    
    parent.AddChild(task)
    tn.allTasks[task.ID] = task
    
    return nil
}

// GetTask retrieves a task by ID
func (tn *TaskNetwork) GetTask(id string) (*Task, bool) {
    tn.mutex.RLock()
    defer tn.mutex.RUnlock()
    
    task, ok := tn.allTasks[id]
    return task, ok
}

// GetPendingTasks returns all tasks in the ready state
func (tn *TaskNetwork) GetPendingTasks() []*Task {
    tn.mutex.RLock()
    defer tn.mutex.RUnlock()
    
    var pendingTasks []*Task
    
    for _, task := range tn.allTasks {
        if task.State == StateReady {
            pendingTasks = append(pendingTasks, task)
        }
    }
    
    return pendingTasks
}

// GetTaskHierarchy returns the task hierarchy as a tree
func (tn *TaskNetwork) GetTaskHierarchy() *Task {
    tn.mutex.RLock()
    defer tn.mutex.RUnlock()
    
    return tn.rootTask
}

// Event handlers

// handleTaskCreated handles task created events
func (tn *TaskNetwork) handleTaskCreated(event Event) error {
    task, ok := event.Payload().(*Task)
    if !ok {
        return fmt.Errorf("invalid payload type for task created event")
    }
    
    // If it's a compound task, request planning
    if task.Type == TaskTypeCompound {
        task.RequestPlanning()
    } else {
        // For primitive tasks, check if dependencies are met
        allDependenciesMet := true
        
        for _, depID := range task.Dependencies {
            depTask, ok := tn.GetTask(depID)
            if !ok || depTask.State != StateCompleted {
                allDependenciesMet = false
                break
            }
        }
        
        if allDependenciesMet {
            // Transition to ready state
            tn.eventQueue.Publish(NewEvent(EventTaskPlanned, task, task.ID, task.ParentID))
        }
    }
    
    return nil
}

// handlePlanRequested handles plan requested events
func (tn *TaskNetwork) handlePlanRequested(event Event) error {
    taskID := event.TaskID()
    
    task, ok := tn.GetTask(taskID)
    if !ok {
        return fmt.Errorf("task not found: %s", taskID)
    }
    
    // Update task state
    err := tn.stateMachine.HandleEvent(task, event)
    if err != nil {
        return fmt.Errorf("state transition failed: %w", err)
    }
    
    // Forward to task planner
    tn.eventQueue.Publish(NewEvent(EventLLMRequested, 
                                  map[string]string{
                                      "purpose": "task_planning",
                                      "taskID": taskID,
                                  }, 
                                  taskID, event.ID()))
    
    return nil
}

// handlePlanCreated handles plan created events
func (tn *TaskNetwork) handlePlanCreated(event Event) error {
    taskID := event.TaskID()
    
    task, ok := tn.GetTask(taskID)
    if !ok {
        return fmt.Errorf("task not found: %s", taskID)
    }
    
    // Update task state
    err := tn.stateMachine.HandleEvent(task, event)
    if err != nil {
        return fmt.Errorf("state transition failed: %w", err)
    }
    
    // Check if all children are planned
    allChildrenPlanned := true
    for _, child := range task.Children {
        if child.State != StatePlanned && child.State != StateReady && 
           child.State != StateCompleted {
            allChildrenPlanned = false
            break
        }
    }
    
    if allChildrenPlanned {
        // Transition to planned state
        tn.eventQueue.Publish(NewEvent(EventTaskPlanned, task, task.ID, task.ParentID))
    }
    
    return nil
}

// handleTaskPlanned handles task planned events
func (tn *TaskNetwork) handleTaskPlanned(event Event) error {
    taskID := event.TaskID()
    
    task, ok := tn.GetTask(taskID)
    if !ok {
        return fmt.Errorf("task not found: %s", taskID)
    }
    
    // Update task state
    err := tn.stateMachine.HandleEvent(task, event)
    if err != nil {
        return fmt.Errorf("state transition failed: %w", err)
    }
    
    return nil
}

// handleExecutionRequested handles execution requested events
func (tn *TaskNetwork) handleExecutionRequested(event Event) error {
    taskID := event.TaskID()
    
    task, ok := tn.GetTask(taskID)
    if !ok {
        return fmt.Errorf("task not found: %s", taskID)
    }
    
    // Update task state
    err := tn.stateMachine.HandleEvent(task, event)
    if err != nil {
        return fmt.Errorf("state transition failed: %w", err)
    }
    
    // Start execution
    task.StartedAt = time.Now()
    
    // Publish execution started event
    tn.eventQueue.Publish(NewEvent(EventExecutionStarted, task, task.ID, event.ID()))
    
    return nil
}

// handleExecutionStarted handles execution started events
func (tn *TaskNetwork) handleExecutionStarted(event Event) error {
    taskID := event.TaskID()
    
    task, ok := tn.GetTask(taskID)
    if !ok {
        return fmt.Errorf("task not found: %s", taskID)
    }
    
    // Update task state
    err := tn.stateMachine.HandleEvent(task, event)
    if err != nil {
        return fmt.Errorf("state transition failed: %w", err)
    }
    
    // Forward to appropriate handler based on task category
    switch task.Category {
    case TaskCategorySearch:
        tn.eventQueue.Publish(NewEvent(EventSearchRequested, 
                                      map[string]string{"query": task.Description}, 
                                      task.ID, event.ID()))
    case TaskCategoryWrite:
        tn.eventQueue.Publish(NewEvent(EventWriteRequested, 
                                      map[string]string{"topic": task.Description}, 
                                      task.ID, event.ID()))
    case TaskCategoryCombine:
        tn.eventQueue.Publish(NewEvent(EventCombineRequested, 
                                      map[string]string{"sections": task.Description}, 
                                      task.ID, event.ID()))
    default:
        return fmt.Errorf("unknown task category: %s", task.Category)
    }
    
    return nil
}

// handleExecutionCompleted handles execution completed events
func (tn *TaskNetwork) handleExecutionCompleted(event Event) error {
    taskID := event.TaskID()
    
    task, ok := tn.GetTask(taskID)
    if !ok {
        return fmt.Errorf("task not found: %s", taskID)
    }
    
    // Update task state
    err := tn.stateMachine.HandleEvent(task, event)
    if err != nil {
        return fmt.Errorf("state transition failed: %w", err)
    }
    
    // Check if parent task is completed
    if task.ParentID != "" {
        parentTask, ok := tn.GetTask(task.ParentID)
        if ok {
            // Check if all children are completed
            allChildrenCompleted := true
            for _, child := range parentTask.Children {
                if child.State != StateCompleted && child.State != StateCancelled {
                    allChildrenCompleted = false
                    break
                }
            }
            
            if allChildrenCompleted {
                // Complete parent task
                parentTask.Complete(nil)
            }
        }
    }
    
    // Check if any dependent tasks can now be executed
    for _, otherTask := range tn.allTasks {
        if otherTask.State == StateCreated || otherTask.State == StatePlanned {
            // Check if this task is a dependency
            isDependency := false
            for _, depID := range otherTask.Dependencies {
                if depID == taskID {
                    isDependency = true
                    break
                }
            }
            
            if isDependency {
                // Check if all dependencies are now met
                allDependenciesMet := true
                for _, depID := range otherTask.Dependencies {
                    depTask, ok := tn.GetTask(depID)
                    if !ok || depTask.State != StateCompleted {
                        allDependenciesMet = false
                        break
                    }
                }
                
                if allDependenciesMet {
                    // Transition to ready state
                    tn.eventQueue.Publish(NewEvent(EventTaskPlanned, otherTask, 
                                                 otherTask.ID, otherTask.ParentID))
                }
            }
        }
    }
    
    return nil
}

// handleExecutionFailed handles execution failed events
func (tn *TaskNetwork) handleExecutionFailed(event Event) error {
    taskID := event.TaskID()
    
    task, ok := tn.GetTask(taskID)
    if !ok {
        return fmt.Errorf("task not found: %s", taskID)
    }
    
    // Update task state
    err := tn.stateMachine.HandleEvent(task, event)
    if err != nil {
        return fmt.Errorf("state transition failed: %w", err)
    }
    
    // Publish system error event
    tn.eventQueue.Publish(NewEvent(EventSystemError, 
                                  fmt.Sprintf("Task failed: %s - %s", task.ID, event.Payload()), 
                                  task.ID, event.ID()))
    
    return nil
}

// handleTaskCancelled handles task cancelled events
func (tn *TaskNetwork) handleTaskCancelled(event Event) error {
    taskID := event.TaskID()
    
    task, ok := tn.GetTask(taskID)
    if !ok {
        return fmt.Errorf("task not found: %s", taskID)
    }
    
    // Update task state
    err := tn.stateMachine.HandleEvent(task, event)
    if err != nil {
        return fmt.Errorf("state transition failed: %w", err)
    }
    
    // Cancel all children
    for _, child := range task.Children {
        if child.State != StateCompleted && child.State != StateFailed && 
           child.State != StateCancelled {
            child.Cancel(fmt.Sprintf("Parent task cancelled: %s", task.ID))
        }
    }
    
    return nil
}
```

## Implementation

### Task Planner

The task planner uses LLM to decompose compound tasks into subtasks, now event-driven:

```go
// TaskPlanner plans tasks using LLM
type TaskPlanner struct {
    llmService  *LLMService
    taskNetwork *TaskNetwork
    eventQueue  *EventQueue
}

// NewTaskPlanner creates a new task planner
func NewTaskPlanner(llmService *LLMService, taskNetwork *TaskNetwork, 
                   eventQueue *EventQueue) *TaskPlanner {
    tp := &TaskPlanner{
        llmService:  llmService,
        taskNetwork: taskNetwork,
        eventQueue:  eventQueue,
    }
    
    // Register event handlers
    eventQueue.RegisterHandler(EventLLMCompleted, tp.handleLLMCompleted)
    
    return tp
}

// handleLLMCompleted handles LLM completed events for planning
func (tp *TaskPlanner) handleLLMCompleted(event Event) error {
    // Check if this is a planning result
    metadata := event.Metadata()
    purpose, ok := metadata["purpose"]
    if !ok || purpose != "task_planning" {
        // Not a planning result, ignore
        return nil
    }
    
    taskID := event.TaskID()
    task, ok := tp.taskNetwork.GetTask(taskID)
    if !ok {
        return fmt.Errorf("task not found: %s", taskID)
    }
    
    // Parse subtasks from LLM response
    planResult, ok := event.Payload().(string)
    if !ok {
        return fmt.Errorf("invalid payload type for LLM completed event")
    }
    
    // Parse subtasks
    subtasks, err := tp.parseSubtasks(planResult, task.ID)
    if err != nil {
        return fmt.Errorf("failed to parse subtasks: %w", err)
    }
    
    // Add subtasks to the network
    for _, subtask := range subtasks {
        err := tp.taskNetwork.AddTask(task.ID, subtask)
        if err != nil {
            return fmt.Errorf("failed to add subtask: %w", err)
        }
    }
    
    // Publish plan created event
    tp.eventQueue.Publish(NewEvent(EventPlanCreated, subtasks, task.ID, event.ID()))
    
    return nil
}

// createPlanningPrompt creates a prompt for LLM planning
func (tp *TaskPlanner) createPlanningPrompt(task *Task) string {
    return fmt.Sprintf(`You are a task planner for a report writing agent. 
Your job is to decompose the following task into subtasks:

Task: %s
Description: %s

The report writing process involves alternating between searching for information, 
writing content, and combining content into cohesive sections.

Please provide a list of subtasks in the following format:
1. [SEARCH/WRITE/COMBINE] Task name: Task description
2. [SEARCH/WRITE/COMBINE] Task name: Task description
...

Each subtask should be atomic and achievable. Specify dependencies between tasks if necessary
by adding "Depends on: X, Y, Z" after the task description.`, task.Name, task.Description)
}

// parseSubtasks parses subtasks from LLM response
func (tp *TaskPlanner) parseSubtasks(planResult, parentID string) ([]*Task, error) {
    var subtasks []*Task
    
    // Parse LLM response to extract subtasks
    // This is a simplified implementation - in a real system, you would use regex or a more robust parser
    lines := strings.Split(planResult, "\n")
    for _, line := range lines {
        if strings.HasPrefix(line, "1. ") || strings.HasPrefix(line, "2. ") || 
           strings.HasPrefix(line, "3. ") || strings.HasPrefix(line, "4. ") || 
           strings.HasPrefix(line, "5. ") {
            
            // Extract task category
            var category TaskCategory
            if strings.Contains(line, "[SEARCH]") {
                category = TaskCategorySearch
            } else if strings.Contains(line, "[WRITE]") {
                category = TaskCategoryWrite
            } else if strings.Contains(line, "[COMBINE]") {
                category = TaskCategoryCombine
            } else {
                continue
            }
            
            // Extract task name and description
            parts := strings.SplitN(line, ":", 2)
            if len(parts) < 2 {
                continue
            }
            
            namePart := parts[0]
            descPart := strings.TrimSpace(parts[1])
            
            // Extract name
            nameMatch := regexp.MustCompile(`\[(SEARCH|WRITE|COMBINE)\] (.*)`).FindStringSubmatch(namePart)
            if len(nameMatch) < 3 {
                continue
            }
            name := strings.TrimSpace(nameMatch[2])
            
            // Extract dependencies
            var dependencies []string
            var description string
            if strings.Contains(descPart, "Depends on:") {
                depParts := strings.SplitN(descPart, "Depends on:", 2)
                description = strings.TrimSpace(depParts[0])
                
                if len(depParts) > 1 {
                    depList := strings.Split(depParts[1], ",")
                    for _, dep := range depList {
                        depName := strings.TrimSpace(dep)
                        // In a real implementation, you would resolve dependency names to IDs
                        // For simplicity, we'll just use the names as IDs
                        dependencies = append(dependencies, depName)
                    }
                }
            } else {
                description = descPart
            }
            
            // Create task
            task := NewTask(parentID, TaskTypePrimitive, category, name, description, 
                          tp.taskNetwork, tp.eventQueue)
            task.Dependencies = dependencies
            
            subtasks = append(subtasks, task)
        }
    }
    
    return subtasks, nil
}
```

### Task Executor

The task executor executes primitive tasks, now event-driven:

```go
// TaskExecutor executes tasks
type TaskExecutor struct {
    taskNetwork *TaskNetwork
    toolRegistry *ToolRegistry
    llmService  *LLMService
    memorySystem *MemorySystem
    eventQueue  *EventQueue
}

// NewTaskExecutor creates a new task executor
func NewTaskExecutor(taskNetwork *TaskNetwork, toolRegistry *ToolRegistry, 
                    llmService *LLMService, memorySystem *MemorySystem, 
                    eventQueue *EventQueue) *TaskExecutor {
    te := &TaskExecutor{
        taskNetwork:  taskNetwork,
        toolRegistry: toolRegistry,
        llmService:   llmService,
        memorySystem: memorySystem,
        eventQueue:   eventQueue,
    }
    
    // Register event handlers
    eventQueue.RegisterHandler(EventSearchRequested, te.handleSearchRequested)
    eventQueue.RegisterHandler(EventSearchCompleted, te.handleSearchCompleted)
    eventQueue.RegisterHandler(EventSearchFailed, te.handleSearchFailed)
    
    eventQueue.RegisterHandler(EventWriteRequested, te.handleWriteRequested)
    eventQueue.RegisterHandler(EventWriteCompleted, te.handleWriteCompleted)
    eventQueue.RegisterHandler(EventWriteFailed, te.handleWriteFailed)
    
    eventQueue.RegisterHandler(EventCombineRequested, te.handleCombineRequested)
    eventQueue.RegisterHandler(EventCombineCompleted, te.handleCombineCompleted)
    eventQueue.RegisterHandler(EventCombineFailed, te.handleCombineFailed)
    
    eventQueue.RegisterHandler(EventLLMCompleted, te.handleLLMCompleted)
    eventQueue.RegisterHandler(EventLLMFailed, te.handleLLMFailed)
    
    eventQueue.RegisterHandler(EventMemoryRetrieveCompleted, te.handleMemoryRetrieveCompleted)
    
    return te
}

// handleSearchRequested handles search requested events
func (te *TaskExecutor) handleSearchRequested(event Event) error {
    taskID := event.TaskID()
    
    task, ok := te.taskNetwork.GetTask(taskID)
    if !ok {
        return fmt.Errorf("task not found: %s", taskID)
    }
    
    // Get search tool
    searchTool, ok := te.toolRegistry.GetTool("search")
    if !ok {
        te.eventQueue.Publish(NewEvent(EventSearchFailed, 
                                      "search tool not found", 
                                      taskID, event.ID()))
        return nil
    }
    
    // Get query
    payload, ok := event.Payload().(map[string]string)
    if !ok {
        te.eventQueue.Publish(NewEvent(EventSearchFailed, 
                                      "invalid payload type", 
                                      taskID, event.ID()))
        return nil
    }
    
    query, ok := payload["query"]
    if !ok {
        query = task.Description
    }
    
    // Execute search asynchronously
    go func() {
        result, err := searchTool.Execute(query)
        if err != nil {
            te.eventQueue.Publish(NewEvent(EventSearchFailed, 
                                          err.Error(), 
                                          taskID, event.ID()))
            return
        }
        
        // Store result in memory
        err = te.memorySystem.Store(taskID, "search_result", result)
        if err != nil {
            te.eventQueue.Publish(NewEvent(EventSearchFailed, 
                                          fmt.Sprintf("failed to store search result: %v", err), 
                                          taskID, event.ID()))
            return
        }
        
        // Publish search completed event
        te.eventQueue.Publish(NewEvent(EventSearchCompleted, 
                                      result, 
                                      taskID, event.ID()))
    }()
    
    return nil
}

// handleSearchCompleted handles search completed events
func (te *TaskExecutor) handleSearchCompleted(event Event) error {
    taskID := event.TaskID()
    
    task, ok := te.taskNetwork.GetTask(taskID)
    if !ok {
        return fmt.Errorf("task not found: %s", taskID)
    }
    
    // Complete task
    task.Complete(event.Payload())
    
    return nil
}

// handleSearchFailed handles search failed events
func (te *TaskExecutor) handleSearchFailed(event Event) error {
    taskID := event.TaskID()
    
    task, ok := te.taskNetwork.GetTask(taskID)
    if !ok {
        return fmt.Errorf("task not found: %s", taskID)
    }
    
    // Fail task
    task.Fail(fmt.Errorf("%v", event.Payload()))
    
    return nil
}

// handleWriteRequested handles write requested events
func (te *TaskExecutor) handleWriteRequested(event Event) error {
    taskID := event.TaskID()
    
    task, ok := te.taskNetwork.GetTask(taskID)
    if !ok {
        return fmt.Errorf("task not found: %s", taskID)
    }
    
    // Retrieve dependencies from memory
    var depIDs []string
    for _, depID := range task.Dependencies {
        depIDs = append(depIDs, depID)
    }
    
    if len(depIDs) > 0 {
        // Request memory retrieval
        te.eventQueue.Publish(NewEvent(EventMemoryRetrieveRequested, 
                                      map[string]interface{}{
                                          "keys": depIDs,
                                          "purpose": "write",
                                      }, 
                                      taskID, event.ID()))
    } else {
        // No dependencies, create writing prompt directly
        prompt := te.createWritingPrompt(task, nil)
        
        // Request LLM generation
        te.eventQueue.Publish(NewEvent(EventLLMRequested, 
                                      map[string]string{
                                          "prompt": prompt,
                                          "purpose": "content_writing",
                                      }, 
                                      taskID, event.ID()))
    }
    
    return nil
}

// handleWriteCompleted handles write completed events
func (te *TaskExecutor) handleWriteCompleted(event Event) error {
    taskID := event.TaskID()
    
    task, ok := te.taskNetwork.GetTask(taskID)
    if !ok {
        return fmt.Errorf("task not found: %s", taskID)
    }
    
    // Complete task
    task.Complete(event.Payload())
    
    return nil
}

// handleWriteFailed handles write failed events
func (te *TaskExecutor) handleWriteFailed(event Event) error {
    taskID := event.TaskID()
    
    task, ok := te.taskNetwork.GetTask(taskID)
    if !ok {
        return fmt.Errorf("task not found: %s", taskID)
    }
    
    // Fail task
    task.Fail(fmt.Errorf("%v", event.Payload()))
    
    return nil
}

// handleCombineRequested handles combine requested events
func (te *TaskExecutor) handleCombineRequested(event Event) error {
    taskID := event.TaskID()
    
    task, ok := te.taskNetwork.GetTask(taskID)
    if !ok {
        return fmt.Errorf("task not found: %s", taskID)
    }
    
    // Retrieve dependencies from memory
    var depIDs []string
    for _, depID := range task.Dependencies {
        depIDs = append(depIDs, depID)
    }
    
    if len(depIDs) > 0 {
        // Request memory retrieval
        te.eventQueue.Publish(NewEvent(EventMemoryRetrieveRequested, 
                                      map[string]interface{}{
                                          "keys": depIDs,
                                          "purpose": "combine",
                                      }, 
                                      taskID, event.ID()))
    } else {
        // No dependencies, fail task
        te.eventQueue.Publish(NewEvent(EventCombineFailed, 
                                      "no dependencies to combine", 
                                      taskID, event.ID()))
    }
    
    return nil
}

// handleCombineCompleted handles combine completed events
func (te *TaskExecutor) handleCombineCompleted(event Event) error {
    taskID := event.TaskID()
    
    task, ok := te.taskNetwork.GetTask(taskID)
    if !ok {
        return fmt.Errorf("task not found: %s", taskID)
    }
    
    // Complete task
    task.Complete(event.Payload())
    
    return nil
}

// handleCombineFailed handles combine failed events
func (te *TaskExecutor) handleCombineFailed(event Event) error {
    taskID := event.TaskID()
    
    task, ok := te.taskNetwork.GetTask(taskID)
    if !ok {
        return fmt.Errorf("task not found: %s", taskID)
    }
    
    // Fail task
    task.Fail(fmt.Errorf("%v", event.Payload()))
    
    return nil
}

// handleLLMCompleted handles LLM completed events
func (te *TaskExecutor) handleLLMCompleted(event Event) error {
    // Check if this is a content writing or combining result
    metadata := event.Metadata()
    purpose, ok := metadata["purpose"]
    if !ok || (purpose != "content_writing" && purpose != "content_combining") {
        // Not a writing or combining result, ignore
        return nil
    }
    
    taskID := event.TaskID()
    task, ok := te.taskNetwork.GetTask(taskID)
    if !ok {
        return fmt.Errorf("task not found: %s", taskID)
    }
    
    // Get content
    content, ok := event.Payload().(string)
    if !ok {
        return fmt.Errorf("invalid payload type for LLM completed event")
    }
    
    // Store content in memory
    var memoryKey string
    var eventType EventType
    if purpose == "content_writing" {
        memoryKey = "written_content"
        eventType = EventWriteCompleted
    } else {
        memoryKey = "combined_content"
        eventType = EventCombineCompleted
    }
    
    err := te.memorySystem.Store(taskID, memoryKey, content)
    if err != nil {
        return fmt.Errorf("failed to store content: %w", err)
    }
    
    // Publish completed event
    te.eventQueue.Publish(NewEvent(eventType, content, taskID, event.ID()))
    
    return nil
}

// handleLLMFailed handles LLM failed events
func (te *TaskExecutor) handleLLMFailed(event Event) error {
    // Check if this is a content writing or combining result
    metadata := event.Metadata()
    purpose, ok := metadata["purpose"]
    if !ok || (purpose != "content_writing" && purpose != "content_combining") {
        // Not a writing or combining result, ignore
        return nil
    }
    
    taskID := event.TaskID()
    
    // Publish failed event
    var eventType EventType
    if purpose == "content_writing" {
        eventType = EventWriteFailed
    } else {
        eventType = EventCombineFailed
    }
    
    te.eventQueue.Publish(NewEvent(eventType, event.Payload(), taskID, event.ID()))
    
    return nil
}

// handleMemoryRetrieveCompleted handles memory retrieve completed events
func (te *TaskExecutor) handleMemoryRetrieveCompleted(event Event) error {
    // Check if this is for writing or combining
    metadata := event.Metadata()
    purpose, ok := metadata["purpose"]
    if !ok || (purpose != "write" && purpose != "combine") {
        // Not for writing or combining, ignore
        return nil
    }
    
    taskID := event.TaskID()
    task, ok := te.taskNetwork.GetTask(taskID)
    if !ok {
        return fmt.Errorf("task not found: %s", taskID)
    }
    
    // Get retrieved data
    data, ok := event.Payload().(map[string]interface{})
    if !ok {
        return fmt.Errorf("invalid payload type for memory retrieve completed event")
    }
    
    if purpose == "write" {
        // Create writing prompt
        prompt := te.createWritingPrompt(task, data)
        
        // Request LLM generation
        te.eventQueue.Publish(NewEvent(EventLLMRequested, 
                                      map[string]string{
                                          "prompt": prompt,
                                          "purpose": "content_writing",
                                      }, 
                                      taskID, event.ID()))
    } else {
        // Create combining prompt
        prompt := te.createCombiningPrompt(task, data)
        
        // Request LLM generation
        te.eventQueue.Publish(NewEvent(EventLLMRequested, 
                                      map[string]string{
                                          "prompt": prompt,
                                          "purpose": "content_combining",
                                      }, 
                                      taskID, event.ID()))
    }
    
    return nil
}

// createWritingPrompt creates a prompt for LLM writing
func (te *TaskExecutor) createWritingPrompt(task *Task, context map[string]interface{}) string {
    var sb strings.Builder
    
    sb.WriteString(fmt.Sprintf(`You are a content writer for a report. 
Your task is to write content for the following section:

Section: %s
Description: %s

`, task.Name, task.Description))
    
    if context != nil && len(context) > 0 {
        sb.WriteString("Here is the relevant information to use:\n\n")
        
        for depID, value := range context {
            depTask, ok := te.taskNetwork.GetTask(depID)
            if ok {
                sb.WriteString(fmt.Sprintf("--- %s ---\n%v\n\n", depTask.Name, value))
            }
        }
    }
    
    sb.WriteString(`Please write a well-structured, informative section that incorporates 
the provided information. Use a professional tone and ensure the content flows naturally.`)
    
    return sb.String()
}

// createCombiningPrompt creates a prompt for LLM combining
func (te *TaskExecutor) createCombiningPrompt(task *Task, context map[string]interface{}) string {
    var sb strings.Builder
    
    sb.WriteString(fmt.Sprintf(`You are a content editor for a report. 
Your task is to combine the following content into a cohesive section:

Section: %s
Description: %s

Here are the pieces of content to combine:

`, task.Name, task.Description))
    
    i := 1
    for depID, value := range context {
        depTask, ok := te.taskNetwork.GetTask(depID)
        if ok {
            sb.WriteString(fmt.Sprintf("--- %s ---\n%v\n\n", depTask.Name, value))
            i++
        }
    }
    
    sb.WriteString(`Please combine these pieces into a cohesive, well-structured section. 
Ensure that the content flows naturally, eliminate redundancies, and maintain a consistent tone. 
The combined content should be comprehensive and read as a unified piece.`)
    
    return sb.String()
}
```

### Task Generator

The task generator creates new tasks based on the results of executed tasks, now event-driven:

```go
// TaskGenerator generates new tasks based on task results
type TaskGenerator struct {
    taskNetwork *TaskNetwork
    llmService  *LLMService
    eventQueue  *EventQueue
}

// NewTaskGenerator creates a new task generator
func NewTaskGenerator(taskNetwork *TaskNetwork, llmService *LLMService, 
                     eventQueue *EventQueue) *TaskGenerator {
    tg := &TaskGenerator{
        taskNetwork: taskNetwork,
        llmService:  llmService,
        eventQueue:  eventQueue,
    }
    
    // Register event handlers
    eventQueue.RegisterHandler(EventTaskCompleted, tg.handleTaskCompleted)
    eventQueue.RegisterHandler(EventLLMCompleted, tg.handleLLMCompleted)
    
    return tg
}

// handleTaskCompleted handles task completed events
func (tg *TaskGenerator) handleTaskCompleted(event Event) error {
    taskID := event.TaskID()
    
    task, ok := tg.taskNetwork.GetTask(taskID)
    if !ok {
        return fmt.Errorf("task not found: %s", taskID)
    }
    
    // Only generate new tasks for certain categories
    if task.Category != TaskCategorySearch && task.Category != TaskCategoryWrite {
        return nil
    }
    
    // Create generation prompt
    prompt := tg.createGenerationPrompt(task)
    
    // Request LLM generation
    tg.eventQueue.Publish(NewEvent(EventLLMRequested, 
                                  map[string]string{
                                      "prompt": prompt,
                                      "purpose": "task_generation",
                                      "taskID": taskID,
                                  }, 
                                  taskID, event.ID()))
    
    return nil
}

// handleLLMCompleted handles LLM completed events for task generation
func (tg *TaskGenerator) handleLLMCompleted(event Event) error {
    // Check if this is a task generation result
    metadata := event.Metadata()
    purpose, ok := metadata["purpose"]
    if !ok || purpose != "task_generation" {
        // Not a task generation result, ignore
        return nil
    }
    
    completedTaskID, ok := metadata["taskID"]
    if !ok {
        return fmt.Errorf("taskID not found in metadata")
    }
    
    completedTask, ok := tg.taskNetwork.GetTask(completedTaskID)
    if !ok {
        return fmt.Errorf("completed task not found: %s", completedTaskID)
    }
    
    // Parse new tasks from LLM response
    genResult, ok := event.Payload().(string)
    if !ok {
        return fmt.Errorf("invalid payload type for LLM completed event")
    }
    
    // Parse new tasks
    newTasks, err := tg.parseNewTasks(genResult, completedTaskID, completedTask.ParentID)
    if err != nil {
        return fmt.Errorf("failed to parse new tasks: %w", err)
    }
    
    // Add new tasks to the network
    for _, newTask := range newTasks {
        err := tg.taskNetwork.AddTask(completedTask.ParentID, newTask)
        if err != nil {
            return fmt.Errorf("failed to add new task: %w", err)
        }
    }
    
    return nil
}

// createGenerationPrompt creates a prompt for LLM task generation
func (tg *TaskGenerator) createGenerationPrompt(task *Task) string {
    var resultStr string
    if result, ok := task.Result.(string); ok {
        resultStr = result
    } else {
        resultStr = fmt.Sprintf("%v", task.Result)
    }
    
    return fmt.Sprintf(`You are a task planner for a report writing agent. 
Based on the result of a completed task, suggest new tasks that should be created.

Completed Task: %s
Description: %s
Category: %s
Result: 
%s

Please suggest new tasks that should be created based on this result. 
These could be follow-up research tasks, writing tasks to elaborate on specific points, 
or combining tasks to integrate this with other content.

Provide a list of new tasks in the following format:
1. [SEARCH/WRITE/COMBINE] Task name: Task description
2. [SEARCH/WRITE/COMBINE] Task name: Task description
...

Each task should be atomic and achievable. Specify dependencies between tasks if necessary
by adding "Depends on: X, Y, Z" after the task description.`, task.Name, task.Description, task.Category, resultStr)
}

// parseNewTasks parses new tasks from LLM response
func (tg *TaskGenerator) parseNewTasks(genResult, completedTaskID, parentID string) ([]*Task, error) {
    var newTasks []*Task
    
    // Parse LLM response to extract new tasks
    // This is a simplified implementation - in a real system, you would use regex or a more robust parser
    lines := strings.Split(genResult, "\n")
    for _, line := range lines {
        if strings.HasPrefix(line, "1. ") || strings.HasPrefix(line, "2. ") || 
           strings.HasPrefix(line, "3. ") || strings.HasPrefix(line, "4. ") || 
           strings.HasPrefix(line, "5. ") {
            
            // Extract task category
            var category TaskCategory
            if strings.Contains(line, "[SEARCH]") {
                category = TaskCategorySearch
            } else if strings.Contains(line, "[WRITE]") {
                category = TaskCategoryWrite
            } else if strings.Contains(line, "[COMBINE]") {
                category = TaskCategoryCombine
            } else {
                continue
            }
            
            // Extract task name and description
            parts := strings.SplitN(line, ":", 2)
            if len(parts) < 2 {
                continue
            }
            
            namePart := parts[0]
            descPart := strings.TrimSpace(parts[1])
            
            // Extract name
            nameMatch := regexp.MustCompile(`\[(SEARCH|WRITE|COMBINE)\] (.*)`).FindStringSubmatch(namePart)
            if len(nameMatch) < 3 {
                continue
            }
            name := strings.TrimSpace(nameMatch[2])
            
            // Extract dependencies
            var dependencies []string
            var description string
            if strings.Contains(descPart, "Depends on:") {
                depParts := strings.SplitN(descPart, "Depends on:", 2)
                description = strings.TrimSpace(depParts[0])
                
                if len(depParts) > 1 {
                    depList := strings.Split(depParts[1], ",")
                    for _, dep := range depList {
                        depName := strings.TrimSpace(dep)
                        // In a real implementation, you would resolve dependency names to IDs
                        // For simplicity, we'll just use the names as IDs
                        dependencies = append(dependencies, depName)
                    }
                }
            } else {
                description = descPart
            }
            
            // Always add the completed task as a dependency
            dependencies = append(dependencies, completedTaskID)
            
            // Create task
            task := NewTask(parentID, TaskTypePrimitive, category, name, description, 
                          tg.taskNetwork, tg.eventQueue)
            
            // Add dependencies
            for _, depID := range dependencies {
                task.AddDependency(depID)
            }
            
            newTasks = append(newTasks, task)
        }
    }
    
    return newTasks, nil
}
```

### Report Writing Agent

The report writing agent orchestrates the entire process, now fully event-driven:

```go
// ReportWritingAgent is an agent for writing reports
type ReportWritingAgent struct {
    taskNetwork    *TaskNetwork
    taskPlanner    *TaskPlanner
    taskExecutor   *TaskExecutor
    taskGenerator  *TaskGenerator
    llmService     *LLMService
    toolRegistry   *ToolRegistry
    memorySystem   *MemorySystem
    eventQueue     *EventQueue
    systemStateMachine *SystemStateMachine
}

// NewReportWritingAgent creates a new report writing agent
func NewReportWritingAgent(llmService *LLMService, toolRegistry *ToolRegistry, 
                          memorySystem *MemorySystem) *ReportWritingAgent {
    // Create event queue
    eventQueue := NewEventQueue(100)
    
    // Create task network
    taskNetwork := NewTaskNetwork(eventQueue, "Write Report", 
                                "Write a comprehensive report on the given topic")
    
    // Create components
    taskPlanner := NewTaskPlanner(llmService, taskNetwork, eventQueue)
    taskExecutor := NewTaskExecutor(taskNetwork, toolRegistry, llmService, 
                                   memorySystem, eventQueue)
    taskGenerator := NewTaskGenerator(taskNetwork, llmService, eventQueue)
    
    // Create system state machine
    systemStateMachine := NewSystemStateMachine()
    
    agent := &ReportWritingAgent{
        taskNetwork:      taskNetwork,
        taskPlanner:      taskPlanner,
        taskExecutor:     taskExecutor,
        taskGenerator:    taskGenerator,
        llmService:       llmService,
        toolRegistry:     toolRegistry,
        memorySystem:     memorySystem,
        eventQueue:       eventQueue,
        systemStateMachine: systemStateMachine,
    }
    
    // Register event handlers
    eventQueue.RegisterHandler(EventSystemStarted, agent.handleSystemStarted)
    eventQueue.RegisterHandler(EventTaskCompleted, agent.handleTaskCompleted)
    
    // Set state machine actions
    systemStateMachine.SetAction(SystemStateIdle, func() {
        log.Println("System is idle, waiting for tasks")
    })
    
    systemStateMachine.SetAction(SystemStateProcessing, func() {
        log.Println("System is processing tasks")
        
        // Start processing pending tasks
        go agent.processPendingTasks()
    })
    
    systemStateMachine.SetAction(SystemStateCompleting, func() {
        log.Println("System is completing final report")
        
        // Create final report
        go agent.createFinalReport()
    })
    
    systemStateMachine.SetAction(SystemStateCompleted, func() {
        log.Println("System has completed all tasks")
    })
    
    systemStateMachine.SetAction(SystemStateError, func() {
        log.Println("System encountered an error")
    })
    
    return agent
}

// WriteReport writes a report on the given topic
func (a *ReportWritingAgent) WriteReport(topic string) (string, error) {
    // Set topic parameter on root task
    rootTask := a.taskNetwork.GetTaskHierarchy()
    rootTask.Parameters["topic"] = topic
    rootTask.Description = fmt.Sprintf("Write a comprehensive report on %s", topic)
    
    // Start event queue
    a.eventQueue.Start()
    
    // Start system
    a.eventQueue.Publish(NewEvent(EventSystemStarted, nil, "", ""))
    
    // Wait for completion
    for {
        state := a.systemStateMachine.GetCurrentState()
        if state == SystemStateCompleted {
            break
        }
        
        time.Sleep(100 * time.Millisecond)
    }
    
    // Stop event queue
    a.eventQueue.Stop()
    
    // Get final report
    result, ok := a.memorySystem.Retrieve(rootTask.ID, "final_report")
    if !ok {
        return "", fmt.Errorf("no final report found")
    }
    
    if report, ok := result.(string); ok {
        return report, nil
    }
    
    return "", fmt.Errorf("invalid final report type")
}

// handleSystemStarted handles system started events
func (a *ReportWritingAgent) handleSystemStarted(event Event) error {
    // Update system state
    err := a.systemStateMachine.HandleEvent(event)
    if err != nil {
        return fmt.Errorf("state transition failed: %w", err)
    }
    
    // Get root task
    rootTask := a.taskNetwork.GetTaskHierarchy()
    
    // Request planning for root task
    rootTask.RequestPlanning()
    
    return nil
}

// handleTaskCompleted handles task completed events
func (a *ReportWritingAgent) handleTaskCompleted(event Event) error {
    taskID := event.TaskID()
    
    // Check if root task is completed
    rootTask := a.taskNetwork.GetTaskHierarchy()
    if taskID == rootTask.ID {
        // Publish system stopped event
        a.eventQueue.Publish(NewEvent(EventSystemStopped, nil, "", ""))
    }
    
    return nil
}

// processPendingTasks processes pending tasks
func (a *ReportWritingAgent) processPendingTasks() {
    // Get pending tasks
    pendingTasks := a.taskNetwork.GetPendingTasks()
    
    // Request execution for each pending task
    for _, task := range pendingTasks {
        task.RequestExecution()
    }
}

// createFinalReport creates the final report
func (a *ReportWritingAgent) createFinalReport() {
    rootTask := a.taskNetwork.GetTaskHierarchy()
    
    // Collect all completed task results
    var sections []string
    
    // Find all COMBINE tasks that are completed
    for _, task := range a.taskNetwork.allTasks {
        if task.Category == TaskCategoryCombine && task.State == StateCompleted {
            if result, ok := task.Result.(string); ok {
                sections = append(sections, fmt.Sprintf("## %s\n\n%s", task.Name, result))
            }
        }
    }
    
    // Create final report prompt
    topic := ""
    if topicParam, ok := rootTask.Parameters["topic"]; ok {
        if topicStr, ok := topicParam.(string); ok {
            topic = topicStr
        }
    }
    
    prompt := fmt.Sprintf(`You are a report editor. Your task is to create a final, 
cohesive report from the following sections:

Topic: %s

%s

Please create a complete report with the following:
1. An executive summary
2. An introduction
3. The main content (using the provided sections)
4. A conclusion
5. References (if applicable)

Ensure the report flows naturally, has consistent formatting, and reads as a unified document.`, 
        topic, strings.Join(sections, "\n\n"))
    
    // Call LLM for final report
    finalReport, err := a.llmService.Generate(prompt, "final_report")
    if err != nil {
        a.eventQueue.Publish(NewEvent(EventSystemError, 
                                     fmt.Sprintf("LLM final report generation failed: %v", err), 
                                     rootTask.ID, ""))
        return
    }
    
    // Store final report
    err = a.memorySystem.Store(rootTask.ID, "final_report", finalReport)
    if err != nil {
        a.eventQueue.Publish(NewEvent(EventSystemError, 
                                     fmt.Sprintf("Failed to store final report: %v", err), 
                                     rootTask.ID, ""))
        return
    }
    
    // Complete root task
    rootTask.Complete(finalReport)
}
```

## Example Execution

Here's a step-by-step example of how the event-driven hierarchical task network processes a request to write a report on climate change:

1. **Initialization**:
   - Create event queue, task network, and other components
   - Register event handlers
   - Set up system state machine

2. **System Start**:
   - Publish `EventSystemStarted` event
   - System state transitions to `SystemStateIdle`
   - Root task is created with state `StateCreated`

3. **Initial Planning**:
   - Root task publishes `EventPlanRequested` event
   - Task state transitions to `StatePlanning`
   - LLM service receives request to plan the task
   - LLM service publishes `EventLLMCompleted` event with planning result
   - Task planner parses subtasks and adds them to the network
   - Task planner publishes `EventPlanCreated` event
   - Root task state transitions to `StatePlanned`
   - Root task publishes `EventTaskPlanned` event
   - Root task state transitions to `StateReady`
   - System state transitions to `SystemStateProcessing`

4. **Task Execution - Research**:
   - System processes pending tasks
   - "Research climate change basics" task publishes `EventExecutionRequested` event
   - Task state transitions to `StateExecuting`
   - Task publishes `EventExecutionStarted` event
   - Task state transitions to `StateWaiting`
   - Task executor publishes `EventSearchRequested` event
   - Search tool executes search
   - Search tool publishes `EventSearchCompleted` event
   - Task executor stores result in memory
   - Task executor publishes `EventExecutionCompleted` event
   - Task state transitions to `StateCompleted`

5. **Task Generation**:
   - Task generator receives `EventTaskCompleted` event
   - Task generator requests LLM to generate new tasks
   - LLM service publishes `EventLLMCompleted` event with generation result
   - Task generator parses new tasks and adds them to the network
   - New tasks are created with state `StateCreated`
   - New tasks with satisfied dependencies transition to `StateReady`

6. **Parallel Execution**:
   - Multiple ready tasks publish `EventExecutionRequested` events
   - Tasks execute in parallel
   - Each task follows its own state transitions
   - As tasks complete, they publish `EventTaskCompleted` events
   - Dependent tasks become ready as their dependencies are met

7. **Writing Tasks**:
   - Writing tasks publish `EventWriteRequested` events
   - Task executor retrieves dependency results from memory
   - Task executor requests LLM to write content
   - LLM service publishes `EventLLMCompleted` events with written content
   - Task executor stores content in memory
   - Task executor publishes `EventWriteCompleted` events
   - Writing tasks transition to `StateCompleted`

8. **Combining Tasks**:
   - Combining tasks publish `EventCombineRequested` events
   - Task executor retrieves dependency results from memory
   - Task executor requests LLM to combine content
   - LLM service publishes `EventLLMCompleted` events with combined content
   - Task executor stores combined content in memory
   - Task executor publishes `EventCombineCompleted` events
   - Combining tasks transition to `StateCompleted`

9. **Final Report Creation**:
   - When all tasks are completed, system state transitions to `SystemStateCompleting`
   - System collects all combined sections
   - System requests LLM to create final report
   - System stores final report in memory
   - Root task transitions to `StateCompleted`
   - System publishes `EventSystemStopped` event
   - System state transitions to `SystemStateCompleted`

10. **Result Retrieval**:
    - System retrieves final report from memory
    - System returns final report to caller

## Advanced Features

### Event Tracing

The system can trace events for debugging and monitoring:

```go
// EventTracer traces events in the system
type EventTracer struct {
    traces    []EventTrace
    mutex     sync.RWMutex
    isEnabled bool
}

// EventTrace represents a trace of an event
type EventTrace struct {
    Event       Event
    HandlerName string
    StartTime   time.Time
    EndTime     time.Time
    Duration    time.Duration
    Error       error
}

// NewEventTracer creates a new event tracer
func NewEventTracer() *EventTracer {
    return &EventTracer{
        traces:    []EventTrace{},
        isEnabled: true,
    }
}

// Enable enables event tracing
func (et *EventTracer) Enable() {
    et.mutex.Lock()
    defer et.mutex.Unlock()
    
    et.isEnabled = true
}

// Disable disables event tracing
func (et *EventTracer) Disable() {
    et.mutex.Lock()
    defer et.mutex.Unlock()
    
    et.isEnabled = false
}

// TraceEvent traces an event
func (et *EventTracer) TraceEvent(event Event, handlerName string, 
                                 startTime time.Time, endTime time.Time, err error) {
    et.mutex.Lock()
    defer et.mutex.Unlock()
    
    if !et.isEnabled {
        return
    }
    
    et.traces = append(et.traces, EventTrace{
        Event:       event,
        HandlerName: handlerName,
        StartTime:   startTime,
        EndTime:     endTime,
        Duration:    endTime.Sub(startTime),
        Error:       err,
    })
}

// GetTraces returns all traces
func (et *EventTracer) GetTraces() []EventTrace {
    et.mutex.RLock()
    defer et.mutex.RUnlock()
    
    return append([]EventTrace{}, et.traces...)
}

// GetTracesByEventType returns traces for a specific event type
func (et *EventTracer) GetTracesByEventType(eventType EventType) []EventTrace {
    et.mutex.RLock()
    defer et.mutex.RUnlock()
    
    var result []EventTrace
    
    for _, trace := range et.traces {
        if trace.Event.Type() == eventType {
            result = append(result, trace)
        }
    }
    
    return result
}

// GetTracesByTaskID returns traces for a specific task ID
func (et *EventTracer) GetTracesByTaskID(taskID string) []EventTrace {
    et.mutex.RLock()
    defer et.mutex.RUnlock()
    
    var result []EventTrace
    
    for _, trace := range et.traces {
        if trace.Event.TaskID() == taskID {
            result = append(result, trace)
        }
    }
    
    return result
}
```

### Event Visualization

The system can visualize the event flow:

```go
// EventVisualizer visualizes events
type EventVisualizer struct {
    tracer *EventTracer
}

// NewEventVisualizer creates a new event visualizer
func NewEventVisualizer(tracer *EventTracer) *EventVisualizer {
    return &EventVisualizer{
        tracer: tracer,
    }
}

// GenerateSequenceDiagram generates a sequence diagram of events
func (ev *EventVisualizer) GenerateSequenceDiagram() string {
    traces := ev.tracer.GetTraces()
    
    // Sort traces by start time
    sort.Slice(traces, func(i, j int) bool {
        return traces[i].StartTime.Before(traces[j].StartTime)
    })
    
    // Generate PlantUML sequence diagram
    var sb strings.Builder
    
    sb.WriteString("@startuml\n")
    sb.WriteString("title Event Sequence Diagram\n\n")
    
    // Define participants
    sb.WriteString("participant \"Agent\" as Agent\n")
    sb.WriteString("participant \"Task Network\" as TaskNetwork\n")
    sb.WriteString("participant \"Task Planner\" as TaskPlanner\n")
    sb.WriteString("participant \"Task Executor\" as TaskExecutor\n")
    sb.WriteString("participant \"Task Generator\" as TaskGenerator\n")
    sb.WriteString("participant \"LLM Service\" as LLMService\n")
    sb.WriteString("participant \"Memory System\" as MemorySystem\n")
    sb.WriteString("participant \"Search Tool\" as SearchTool\n")
    sb.WriteString("\n")
    
    // Generate sequence
    for _, trace := range traces {
        // Determine source and target based on handler name
        var source, target string
        
        if strings.Contains(trace.HandlerName, "Agent") {
            source = "Agent"
        } else if strings.Contains(trace.HandlerName, "TaskNetwork") {
            source = "TaskNetwork"
        } else if strings.Contains(trace.HandlerName, "TaskPlanner") {
            source = "TaskPlanner"
        } else if strings.Contains(trace.HandlerName, "TaskExecutor") {
            source = "TaskExecutor"
        } else if strings.Contains(trace.HandlerName, "TaskGenerator") {
            source = "TaskGenerator"
        } else if strings.Contains(trace.HandlerName, "LLMService") {
            source = "LLMService"
        } else if strings.Contains(trace.HandlerName, "MemorySystem") {
            source = "MemorySystem"
        } else if strings.Contains(trace.HandlerName, "SearchTool") {
            source = "SearchTool"
        } else {
            source = "Agent"
        }
        
        // Determine target based on event type
        switch trace.Event.Type() {
        case EventTaskCreated, EventTaskPlanned, EventTaskCompleted:
            target = "TaskNetwork"
        case EventPlanRequested, EventPlanCreated:
            target = "TaskPlanner"
        case EventExecutionRequested, EventExecutionStarted, 
             EventExecutionCompleted, EventExecutionFailed:
            target = "TaskExecutor"
        case EventSearchRequested, EventSearchCompleted, EventSearchFailed:
            target = "SearchTool"
        case EventLLMRequested, EventLLMCompleted, EventLLMFailed:
            target = "LLMService"
        case EventMemoryStoreRequested, EventMemoryStoreCompleted, 
             EventMemoryRetrieveRequested, EventMemoryRetrieveCompleted:
            target = "MemorySystem"
        default:
            target = "Agent"
        }
        
        // Add sequence arrow
        sb.WriteString(fmt.Sprintf("%s -> %s : %s", source, target, trace.Event.Type()))
        
        // Add task ID if available
        if trace.Event.TaskID() != "" {
            sb.WriteString(fmt.Sprintf(" (Task: %s)", trace.Event.TaskID()))
        }
        
        // Add duration
        sb.WriteString(fmt.Sprintf(" [%.2fms]", float64(trace.Duration.Microseconds())/1000.0))
        
        sb.WriteString("\n")
        
        // Add error if any
        if trace.Error != nil {
            sb.WriteString(fmt.Sprintf("note right of %s: Error: %v\n", target, trace.Error))
        }
    }
    
    sb.WriteString("@enduml\n")
    
    return sb.String()
}

// GenerateTaskStateDiagram generates a state diagram of tasks
func (ev *EventVisualizer) GenerateTaskStateDiagram() string {
    traces := ev.tracer.GetTraces()
    
    // Sort traces by start time
    sort.Slice(traces, func(i, j int) bool {
        return traces[i].StartTime.Before(traces[j].StartTime)
    })
    
    // Track task states
    taskStates := make(map[string][]struct {
        State     TaskState
        Timestamp time.Time
    })
    
    for _, trace := range traces {
        taskID := trace.Event.TaskID()
        if taskID == "" {
            continue
        }
        
        var state TaskState
        
        switch trace.Event.Type() {
        case EventTaskCreated:
            state = StateCreated
        case EventPlanRequested:
            state = StatePlanning
        case EventPlanCreated, EventTaskPlanned:
            state = StatePlanned
        case EventExecutionRequested:
            state = StateReady
        case EventExecutionStarted:
            state = StateExecuting
        case EventSearchRequested, EventWriteRequested, EventCombineRequested:
            state = StateWaiting
        case EventExecutionCompleted, EventTaskCompleted:
            state = StateCompleted
        case EventExecutionFailed, EventTaskFailed:
            state = StateFailed
        case EventTaskCancelled:
            state = StateCancelled
        default:
            continue
        }
        
        if _, ok := taskStates[taskID]; !ok {
            taskStates[taskID] = []struct {
                State     TaskState
                Timestamp time.Time
            }{}
        }
        
        taskStates[taskID] = append(taskStates[taskID], struct {
            State     TaskState
            Timestamp time.Time
        }{
            State:     state,
            Timestamp: trace.StartTime,
        })
    }
    
    // Generate PlantUML state diagram
    var sb strings.Builder
    
    sb.WriteString("@startuml\n")
    sb.WriteString("title Task State Diagram\n\n")
    
    // Define states
    sb.WriteString("state \"Created\" as Created\n")
    sb.WriteString("state \"Planning\" as Planning\n")
    sb.WriteString("state \"Planned\" as Planned\n")
    sb.WriteString("state \"Ready\" as Ready\n")
    sb.WriteString("state \"Executing\" as Executing\n")
    sb.WriteString("state \"Waiting\" as Waiting\n")
    sb.WriteString("state \"Completed\" as Completed\n")
    sb.WriteString("state \"Failed\" as Failed\n")
    sb.WriteString("state \"Cancelled\" as Cancelled\n")
    sb.WriteString("\n")
    
    // Define transitions
    sb.WriteString("Created --> Planning\n")
    sb.WriteString("Planning --> Planned\n")
    sb.WriteString("Planned --> Ready\n")
    sb.WriteString("Ready --> Executing\n")
    sb.WriteString("Executing --> Waiting\n")
    sb.WriteString("Waiting --> Completed\n")
    sb.WriteString("Waiting --> Failed\n")
    sb.WriteString("Executing --> Failed\n")
    sb.WriteString("Ready --> Cancelled\n")
    sb.WriteString("Executing --> Cancelled\n")
    sb.WriteString("Waiting --> Cancelled\n")
    sb.WriteString("\n")
    
    // Add task state histories
    for taskID, states := range taskStates {
        sb.WriteString(fmt.Sprintf("note right of Created : Task %s\n", taskID))
        
        for i, state := range states {
            if i > 0 {
                prevState := states[i-1].State
                duration := state.Timestamp.Sub(states[i-1].Timestamp)
                
                sb.WriteString(fmt.Sprintf("note right of %s : %s -> %s (%.2fms)\n", 
                                         prevState, prevState, state.State, 
                                         float64(duration.Microseconds())/1000.0))
            }
        }
        
        sb.WriteString("\n")
    }
    
    sb.WriteString("@enduml\n")
    
    return sb.String()
}
```

This enhanced design combines the strengths of hierarchical task networks, event-driven architecture, and state machines to create a highly flexible, decoupled, and robust system for report writing. The event-driven approach allows for loose coupling between components, while the state machine integration makes the system's behavior explicit and manageable. The hierarchical task structure provides a clear organization of the work to be done, making the system both powerful and maintainable.
