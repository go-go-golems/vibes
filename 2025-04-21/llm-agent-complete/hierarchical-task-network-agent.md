# Dynamic Hierarchical Task Network Agent for Report Writing

This document presents the design of an agent with a dynamic hierarchical task network (HTN) that uses LLM calls for planning a report-writing process. The agent alternates between searching, writing, and combining content, with each step potentially generating new dependent task nodes.

## Table of Contents

1. [Introduction](#introduction)
2. [Architectural Overview](#architectural-overview)
3. [Hierarchical Task Network Design](#hierarchical-task-network-design)
4. [Task Planning with LLM](#task-planning-with-llm)
5. [Task Execution](#task-execution)
6. [Dynamic Task Generation](#dynamic-task-generation)
7. [Implementation](#implementation)
8. [Example Execution](#example-execution)
9. [Advanced Features](#advanced-features)

## Introduction

A Hierarchical Task Network (HTN) is a planning methodology where complex tasks are decomposed into simpler subtasks until reaching primitive actions that can be directly executed. In our design, we combine HTN planning with LLM capabilities to create a dynamic system that can adapt its task structure as it progresses through report writing.

The key features of our design include:

1. **Dynamic Task Decomposition**: Tasks are decomposed into subtasks based on LLM planning
2. **Adaptive Planning**: New tasks are generated based on the results of executed tasks
3. **Multi-modal Operation**: The agent alternates between searching, writing, and combining content
4. **Hierarchical Structure**: Tasks are organized in a hierarchical tree structure
5. **Dependency Management**: Tasks can depend on the completion of other tasks

## Architectural Overview

The architecture consists of the following components:

```
┌─────────────────────────────────────────────────────────────┐
│                     Report Writing Agent                     │
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

## Hierarchical Task Network Design

### Task Representation

Tasks in our HTN are represented as nodes in a tree structure:

```go
// TaskType represents the type of a task
type TaskType string

const (
    TaskTypeCompound TaskType = "compound"
    TaskTypePrimitive TaskType = "primitive"
)

// TaskStatus represents the status of a task
type TaskStatus string

const (
    TaskStatusPending   TaskStatus = "pending"
    TaskStatusActive    TaskStatus = "active"
    TaskStatusCompleted TaskStatus = "completed"
    TaskStatusFailed    TaskStatus = "failed"
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
    Status       TaskStatus
    Dependencies []string
    Children     []*Task
    Parameters   map[string]interface{}
    Result       interface{}
    CreatedAt    time.Time
    StartedAt    time.Time
    CompletedAt  time.Time
}

// NewTask creates a new task
func NewTask(parentID string, taskType TaskType, category TaskCategory, 
            name, description string) *Task {
    return &Task{
        ID:          uuid.New().String(),
        ParentID:    parentID,
        Type:        taskType,
        Category:    category,
        Name:        name,
        Description: description,
        Status:      TaskStatusPending,
        Dependencies: []string{},
        Children:    []*Task{},
        Parameters:  make(map[string]interface{}),
        CreatedAt:   time.Now(),
    }
}
```

### Task Network

The task network manages the hierarchical structure of tasks:

```go
// TaskNetwork represents a hierarchical task network
type TaskNetwork struct {
    rootTask    *Task
    allTasks    map[string]*Task
    mutex       sync.RWMutex
}

// NewTaskNetwork creates a new task network
func NewTaskNetwork(rootTaskName, rootTaskDescription string) *TaskNetwork {
    rootTask := NewTask("", TaskTypeCompound, TaskCategoryPlan, 
                      rootTaskName, rootTaskDescription)
    
    return &TaskNetwork{
        rootTask: rootTask,
        allTasks: map[string]*Task{rootTask.ID: rootTask},
    }
}

// AddTask adds a task to the network
func (tn *TaskNetwork) AddTask(parentID string, task *Task) error {
    tn.mutex.Lock()
    defer tn.mutex.Unlock()
    
    parent, ok := tn.allTasks[parentID]
    if !ok {
        return fmt.Errorf("parent task not found: %s", parentID)
    }
    
    parent.Children = append(parent.Children, task)
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

// UpdateTaskStatus updates the status of a task
func (tn *TaskNetwork) UpdateTaskStatus(id string, status TaskStatus) error {
    tn.mutex.Lock()
    defer tn.mutex.Unlock()
    
    task, ok := tn.allTasks[id]
    if !ok {
        return fmt.Errorf("task not found: %s", id)
    }
    
    task.Status = status
    
    if status == TaskStatusActive {
        task.StartedAt = time.Now()
    } else if status == TaskStatusCompleted || status == TaskStatusFailed {
        task.CompletedAt = time.Now()
    }
    
    return nil
}

// SetTaskResult sets the result of a task
func (tn *TaskNetwork) SetTaskResult(id string, result interface{}) error {
    tn.mutex.Lock()
    defer tn.mutex.Unlock()
    
    task, ok := tn.allTasks[id]
    if !ok {
        return fmt.Errorf("task not found: %s", id)
    }
    
    task.Result = result
    
    return nil
}

// GetPendingTasks returns all pending tasks that have no pending dependencies
func (tn *TaskNetwork) GetPendingTasks() []*Task {
    tn.mutex.RLock()
    defer tn.mutex.RUnlock()
    
    var pendingTasks []*Task
    
    for _, task := range tn.allTasks {
        if task.Status == TaskStatusPending && tn.areDependenciesMet(task) {
            pendingTasks = append(pendingTasks, task)
        }
    }
    
    return pendingTasks
}

// areDependenciesMet checks if all dependencies of a task are met
func (tn *TaskNetwork) areDependenciesMet(task *Task) bool {
    for _, depID := range task.Dependencies {
        dep, ok := tn.allTasks[depID]
        if !ok || dep.Status != TaskStatusCompleted {
            return false
        }
    }
    
    return true
}

// GetTaskHierarchy returns the task hierarchy as a tree
func (tn *TaskNetwork) GetTaskHierarchy() *Task {
    tn.mutex.RLock()
    defer tn.mutex.RUnlock()
    
    return tn.rootTask
}
```

## Task Planning with LLM

The task planner uses LLM to decompose compound tasks into subtasks:

```go
// TaskPlanner plans tasks using LLM
type TaskPlanner struct {
    llmService  *LLMService
    taskNetwork *TaskNetwork
}

// NewTaskPlanner creates a new task planner
func NewTaskPlanner(llmService *LLMService, taskNetwork *TaskNetwork) *TaskPlanner {
    return &TaskPlanner{
        llmService:  llmService,
        taskNetwork: taskNetwork,
    }
}

// PlanTask plans a compound task by decomposing it into subtasks
func (tp *TaskPlanner) PlanTask(task *Task) error {
    if task.Type != TaskTypeCompound {
        return fmt.Errorf("cannot plan primitive task: %s", task.ID)
    }
    
    // Create planning prompt
    prompt := tp.createPlanningPrompt(task)
    
    // Call LLM for planning
    planResult, err := tp.llmService.Generate(prompt, "task_planning")
    if err != nil {
        return fmt.Errorf("LLM planning failed: %w", err)
    }
    
    // Parse subtasks from LLM response
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
            if strings.Contains(descPart, "Depends on:") {
                depParts := strings.SplitN(descPart, "Depends on:", 2)
                description := strings.TrimSpace(depParts[0])
                
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
            task := NewTask(parentID, TaskTypePrimitive, category, name, description)
            task.Dependencies = dependencies
            
            subtasks = append(subtasks, task)
        }
    }
    
    return subtasks, nil
}
```

## Task Execution

The task executor executes primitive tasks:

```go
// TaskExecutor executes tasks
type TaskExecutor struct {
    taskNetwork *TaskNetwork
    toolRegistry *ToolRegistry
    llmService  *LLMService
    memorySystem *MemorySystem
}

// NewTaskExecutor creates a new task executor
func NewTaskExecutor(taskNetwork *TaskNetwork, toolRegistry *ToolRegistry, 
                    llmService *LLMService, memorySystem *MemorySystem) *TaskExecutor {
    return &TaskExecutor{
        taskNetwork:  taskNetwork,
        toolRegistry: toolRegistry,
        llmService:   llmService,
        memorySystem: memorySystem,
    }
}

// ExecuteTask executes a primitive task
func (te *TaskExecutor) ExecuteTask(task *Task) error {
    if task.Type != TaskTypePrimitive {
        return fmt.Errorf("cannot execute compound task: %s", task.ID)
    }
    
    // Update task status
    err := te.taskNetwork.UpdateTaskStatus(task.ID, TaskStatusActive)
    if err != nil {
        return fmt.Errorf("failed to update task status: %w", err)
    }
    
    var result interface{}
    var execErr error
    
    // Execute based on task category
    switch task.Category {
    case TaskCategorySearch:
        result, execErr = te.executeSearchTask(task)
    case TaskCategoryWrite:
        result, execErr = te.executeWriteTask(task)
    case TaskCategoryCombine:
        result, execErr = te.executeCombineTask(task)
    default:
        execErr = fmt.Errorf("unknown task category: %s", task.Category)
    }
    
    if execErr != nil {
        te.taskNetwork.UpdateTaskStatus(task.ID, TaskStatusFailed)
        return fmt.Errorf("task execution failed: %w", execErr)
    }
    
    // Set task result
    err = te.taskNetwork.SetTaskResult(task.ID, result)
    if err != nil {
        return fmt.Errorf("failed to set task result: %w", err)
    }
    
    // Update task status
    err = te.taskNetwork.UpdateTaskStatus(task.ID, TaskStatusCompleted)
    if err != nil {
        return fmt.Errorf("failed to update task status: %w", err)
    }
    
    return nil
}

// executeSearchTask executes a search task
func (te *TaskExecutor) executeSearchTask(task *Task) (interface{}, error) {
    // Get search tool
    searchTool, ok := te.toolRegistry.GetTool("search")
    if !ok {
        return nil, fmt.Errorf("search tool not found")
    }
    
    // Execute search
    query := task.Description
    if queryParam, ok := task.Parameters["query"]; ok {
        if queryStr, ok := queryParam.(string); ok {
            query = queryStr
        }
    }
    
    result, err := searchTool.Execute(query)
    if err != nil {
        return nil, fmt.Errorf("search failed: %w", err)
    }
    
    // Store result in memory
    err = te.memorySystem.Store(task.ID, "search_result", result)
    if err != nil {
        return nil, fmt.Errorf("failed to store search result: %w", err)
    }
    
    return result, nil
}

// executeWriteTask executes a write task
func (te *TaskExecutor) executeWriteTask(task *Task) (interface{}, error) {
    // Prepare context for writing
    context := make(map[string]interface{})
    
    // Get dependencies
    for _, depID := range task.Dependencies {
        depTask, ok := te.taskNetwork.GetTask(depID)
        if !ok {
            return nil, fmt.Errorf("dependency task not found: %s", depID)
        }
        
        context[depTask.Name] = depTask.Result
    }
    
    // Create writing prompt
    prompt := te.createWritingPrompt(task, context)
    
    // Call LLM for writing
    content, err := te.llmService.Generate(prompt, "content_writing")
    if err != nil {
        return nil, fmt.Errorf("LLM writing failed: %w", err)
    }
    
    // Store content in memory
    err = te.memorySystem.Store(task.ID, "written_content", content)
    if err != nil {
        return nil, fmt.Errorf("failed to store written content: %w", err)
    }
    
    return content, nil
}

// executeCombineTask executes a combine task
func (te *TaskExecutor) executeCombineTask(task *Task) (interface{}, error) {
    // Prepare content to combine
    var contents []string
    
    // Get dependencies
    for _, depID := range task.Dependencies {
        depTask, ok := te.taskNetwork.GetTask(depID)
        if !ok {
            return nil, fmt.Errorf("dependency task not found: %s", depID)
        }
        
        if content, ok := depTask.Result.(string); ok {
            contents = append(contents, content)
        }
    }
    
    // Create combining prompt
    prompt := te.createCombiningPrompt(task, contents)
    
    // Call LLM for combining
    combinedContent, err := te.llmService.Generate(prompt, "content_combining")
    if err != nil {
        return nil, fmt.Errorf("LLM combining failed: %w", err)
    }
    
    // Store combined content in memory
    err = te.memorySystem.Store(task.ID, "combined_content", combinedContent)
    if err != nil {
        return nil, fmt.Errorf("failed to store combined content: %w", err)
    }
    
    return combinedContent, nil
}

// createWritingPrompt creates a prompt for LLM writing
func (te *TaskExecutor) createWritingPrompt(task *Task, context map[string]interface{}) string {
    var sb strings.Builder
    
    sb.WriteString(fmt.Sprintf(`You are a content writer for a report. 
Your task is to write content for the following section:

Section: %s
Description: %s

`, task.Name, task.Description))
    
    if len(context) > 0 {
        sb.WriteString("Here is the relevant information to use:\n\n")
        
        for name, value := range context {
            sb.WriteString(fmt.Sprintf("--- %s ---\n%v\n\n", name, value))
        }
    }
    
    sb.WriteString(`Please write a well-structured, informative section that incorporates 
the provided information. Use a professional tone and ensure the content flows naturally.`)
    
    return sb.String()
}

// createCombiningPrompt creates a prompt for LLM combining
func (te *TaskExecutor) createCombiningPrompt(task *Task, contents []string) string {
    var sb strings.Builder
    
    sb.WriteString(fmt.Sprintf(`You are a content editor for a report. 
Your task is to combine the following content into a cohesive section:

Section: %s
Description: %s

Here are the pieces of content to combine:

`, task.Name, task.Description))
    
    for i, content := range contents {
        sb.WriteString(fmt.Sprintf("--- Content %d ---\n%s\n\n", i+1, content))
    }
    
    sb.WriteString(`Please combine these pieces into a cohesive, well-structured section. 
Ensure that the content flows naturally, eliminate redundancies, and maintain a consistent tone. 
The combined content should be comprehensive and read as a unified piece.`)
    
    return sb.String()
}
```

## Dynamic Task Generation

The task generator creates new tasks based on the results of executed tasks:

```go
// TaskGenerator generates new tasks based on task results
type TaskGenerator struct {
    taskNetwork *TaskNetwork
    llmService  *LLMService
}

// NewTaskGenerator creates a new task generator
func NewTaskGenerator(taskNetwork *TaskNetwork, llmService *LLMService) *TaskGenerator {
    return &TaskGenerator{
        taskNetwork: taskNetwork,
        llmService:  llmService,
    }
}

// GenerateTasksFromResult generates new tasks based on a completed task's result
func (tg *TaskGenerator) GenerateTasksFromResult(task *Task) error {
    // Only generate new tasks for certain categories
    if task.Category != TaskCategorySearch && task.Category != TaskCategoryWrite {
        return nil
    }
    
    // Create generation prompt
    prompt := tg.createGenerationPrompt(task)
    
    // Call LLM for task generation
    genResult, err := tg.llmService.Generate(prompt, "task_generation")
    if err != nil {
        return fmt.Errorf("LLM task generation failed: %w", err)
    }
    
    // Parse new tasks from LLM response
    newTasks, err := tg.parseNewTasks(genResult, task.ID)
    if err != nil {
        return fmt.Errorf("failed to parse new tasks: %w", err)
    }
    
    // Add new tasks to the network
    for _, newTask := range newTasks {
        err := tg.taskNetwork.AddTask(task.ParentID, newTask)
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
func (tg *TaskGenerator) parseNewTasks(genResult, completedTaskID string) ([]*Task, error) {
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
            task := NewTask("", TaskTypePrimitive, category, name, description)
            task.Dependencies = dependencies
            
            newTasks = append(newTasks, task)
        }
    }
    
    return newTasks, nil
}
```

## Implementation

### Report Writing Agent

The report writing agent orchestrates the entire process:

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
}

// NewReportWritingAgent creates a new report writing agent
func NewReportWritingAgent(llmService *LLMService, toolRegistry *ToolRegistry, 
                          memorySystem *MemorySystem) *ReportWritingAgent {
    // Create task network
    taskNetwork := NewTaskNetwork("Write Report", "Write a comprehensive report on the given topic")
    
    // Create components
    taskPlanner := NewTaskPlanner(llmService, taskNetwork)
    taskExecutor := NewTaskExecutor(taskNetwork, toolRegistry, llmService, memorySystem)
    taskGenerator := NewTaskGenerator(taskNetwork, llmService)
    
    return &ReportWritingAgent{
        taskNetwork:   taskNetwork,
        taskPlanner:   taskPlanner,
        taskExecutor:  taskExecutor,
        taskGenerator: taskGenerator,
        llmService:    llmService,
        toolRegistry:  toolRegistry,
        memorySystem:  memorySystem,
    }
}

// WriteReport writes a report on the given topic
func (a *ReportWritingAgent) WriteReport(topic string) (string, error) {
    // Set topic parameter on root task
    rootTask := a.taskNetwork.GetTaskHierarchy()
    rootTask.Parameters["topic"] = topic
    rootTask.Description = fmt.Sprintf("Write a comprehensive report on %s", topic)
    
    // Plan root task
    err := a.taskPlanner.PlanTask(rootTask)
    if err != nil {
        return "", fmt.Errorf("initial planning failed: %w", err)
    }
    
    // Execute tasks until completion
    for {
        // Get pending tasks
        pendingTasks := a.taskNetwork.GetPendingTasks()
        if len(pendingTasks) == 0 {
            // Check if root task is completed
            if rootTask.Status == TaskStatusCompleted {
                break
            }
            
            // No pending tasks but root not completed - check if all children are completed
            allChildrenCompleted := true
            for _, child := range rootTask.Children {
                if child.Status != TaskStatusCompleted && child.Status != TaskStatusFailed {
                    allChildrenCompleted = false
                    break
                }
            }
            
            if allChildrenCompleted {
                // All children completed, combine results into final report
                finalReport, err := a.createFinalReport(rootTask)
                if err != nil {
                    return "", fmt.Errorf("failed to create final report: %w", err)
                }
                
                // Set root task result and mark as completed
                a.taskNetwork.SetTaskResult(rootTask.ID, finalReport)
                a.taskNetwork.UpdateTaskStatus(rootTask.ID, TaskStatusCompleted)
                
                return finalReport, nil
            }
            
            // Wait for tasks to complete
            time.Sleep(100 * time.Millisecond)
            continue
        }
        
        // Execute each pending task
        for _, task := range pendingTasks {
            // If it's a compound task, plan it
            if task.Type == TaskTypeCompound {
                err := a.taskPlanner.PlanTask(task)
                if err != nil {
                    return "", fmt.Errorf("task planning failed: %w", err)
                }
                continue
            }
            
            // Execute primitive task
            err := a.taskExecutor.ExecuteTask(task)
            if err != nil {
                // Log error but continue with other tasks
                log.Printf("Task execution failed: %v", err)
                continue
            }
            
            // Generate new tasks based on result
            err = a.taskGenerator.GenerateTasksFromResult(task)
            if err != nil {
                // Log error but continue
                log.Printf("Task generation failed: %v", err)
            }
        }
    }
    
    // Get final report
    if result, ok := rootTask.Result.(string); ok {
        return result, nil
    }
    
    return "", fmt.Errorf("no report generated")
}

// createFinalReport creates the final report by combining all results
func (a *ReportWritingAgent) createFinalReport(rootTask *Task) (string, error) {
    // Collect all completed task results
    var sections []string
    
    // Find all COMBINE tasks that are completed
    var combineTasks []*Task
    for _, task := range a.taskNetwork.allTasks {
        if task.Category == TaskCategoryCombine && task.Status == TaskStatusCompleted {
            combineTasks = append(combineTasks, task)
        }
    }
    
    // Sort combine tasks by completion time
    sort.Slice(combineTasks, func(i, j int) bool {
        return combineTasks[i].CompletedAt.Before(combineTasks[j].CompletedAt)
    })
    
    // Add results to sections
    for _, task := range combineTasks {
        if result, ok := task.Result.(string); ok {
            sections = append(sections, fmt.Sprintf("## %s\n\n%s", task.Name, result))
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
        return "", fmt.Errorf("LLM final report generation failed: %w", err)
    }
    
    return finalReport, nil
}
```

### Tool Registry

The tool registry manages the tools available to the agent:

```go
// Tool represents a tool that can be used by the agent
type Tool interface {
    Name() string
    Description() string
    Execute(input string) (interface{}, error)
}

// ToolRegistry manages available tools
type ToolRegistry struct {
    tools map[string]Tool
    mutex sync.RWMutex
}

// NewToolRegistry creates a new tool registry
func NewToolRegistry() *ToolRegistry {
    return &ToolRegistry{
        tools: make(map[string]Tool),
    }
}

// RegisterTool registers a tool
func (tr *ToolRegistry) RegisterTool(tool Tool) {
    tr.mutex.Lock()
    defer tr.mutex.Unlock()
    
    tr.tools[tool.Name()] = tool
}

// GetTool retrieves a tool by name
func (tr *ToolRegistry) GetTool(name string) (Tool, bool) {
    tr.mutex.RLock()
    defer tr.mutex.RUnlock()
    
    tool, ok := tr.tools[name]
    return tool, ok
}

// SearchTool is a tool for searching information
type SearchTool struct {
    searchClient *SearchClient
}

// NewSearchTool creates a new search tool
func NewSearchTool(searchClient *SearchClient) *SearchTool {
    return &SearchTool{
        searchClient: searchClient,
    }
}

// Name returns the name of the tool
func (t *SearchTool) Name() string {
    return "search"
}

// Description returns the description of the tool
func (t *SearchTool) Description() string {
    return "Searches for information on a given topic"
}

// Execute executes the tool
func (t *SearchTool) Execute(input string) (interface{}, error) {
    // Call search client
    results, err := t.searchClient.Search(input)
    if err != nil {
        return nil, fmt.Errorf("search failed: %w", err)
    }
    
    return results, nil
}
```

### Memory System

The memory system stores and retrieves information:

```go
// MemorySystem stores and retrieves information
type MemorySystem struct {
    storage map[string]map[string]interface{}
    mutex   sync.RWMutex
}

// NewMemorySystem creates a new memory system
func NewMemorySystem() *MemorySystem {
    return &MemorySystem{
        storage: make(map[string]map[string]interface{}),
    }
}

// Store stores a value
func (ms *MemorySystem) Store(id, key string, value interface{}) error {
    ms.mutex.Lock()
    defer ms.mutex.Unlock()
    
    // Initialize ID storage if needed
    if _, ok := ms.storage[id]; !ok {
        ms.storage[id] = make(map[string]interface{})
    }
    
    // Store value
    ms.storage[id][key] = value
    
    return nil
}

// Retrieve retrieves a value
func (ms *MemorySystem) Retrieve(id, key string) (interface{}, bool) {
    ms.mutex.RLock()
    defer ms.mutex.RUnlock()
    
    // Check if ID exists
    idStorage, ok := ms.storage[id]
    if !ok {
        return nil, false
    }
    
    // Retrieve value
    value, ok := idStorage[key]
    return value, ok
}

// RetrieveAll retrieves all values for an ID
func (ms *MemorySystem) RetrieveAll(id string) (map[string]interface{}, bool) {
    ms.mutex.RLock()
    defer ms.mutex.RUnlock()
    
    // Check if ID exists
    idStorage, ok := ms.storage[id]
    if !ok {
        return nil, false
    }
    
    // Copy values
    result := make(map[string]interface{})
    for k, v := range idStorage {
        result[k] = v
    }
    
    return result, true
}
```

### LLM Service

The LLM service handles interactions with the language model:

```go
// LLMService handles interactions with the language model
type LLMService struct {
    provider LLMProvider
}

// LLMProvider represents an LLM API provider
type LLMProvider interface {
    Generate(prompt, purpose string) (string, error)
}

// NewLLMService creates a new LLM service
func NewLLMService(provider LLMProvider) *LLMService {
    return &LLMService{
        provider: provider,
    }
}

// Generate generates text using the LLM
func (s *LLMService) Generate(prompt, purpose string) (string, error) {
    return s.provider.Generate(prompt, purpose)
}

// MockLLMProvider is a mock LLM provider for testing
type MockLLMProvider struct {
    responses map[string]string
}

// NewMockLLMProvider creates a new mock LLM provider
func NewMockLLMProvider() *MockLLMProvider {
    return &MockLLMProvider{
        responses: map[string]string{
            "task_planning": `1. [SEARCH] Research climate change basics: Find general information about climate change, its causes, and effects.
2. [SEARCH] Research climate change impacts: Find specific information about the impacts of climate change on ecosystems, economies, and human societies.
3. [SEARCH] Research climate change mitigation: Find information about strategies and policies to mitigate climate change.
4. [WRITE] Write introduction to climate change: Write an introductory section explaining what climate change is and why it matters. Depends on: Research climate change basics.
5. [WRITE] Write section on climate change impacts: Write a section detailing the various impacts of climate change. Depends on: Research climate change impacts.
6. [WRITE] Write section on climate change mitigation: Write a section on strategies to mitigate climate change. Depends on: Research climate change mitigation.
7. [COMBINE] Combine introduction and impacts: Combine the introduction and impacts sections into a cohesive first half of the report. Depends on: Write introduction to climate change, Write section on climate change impacts.
8. [COMBINE] Create final report: Combine all sections into a final, cohesive report. Depends on: Combine introduction and impacts, Write section on climate change mitigation.`,
            
            "task_generation": `1. [SEARCH] Research recent climate change statistics: Find the most recent data and statistics on climate change indicators.
2. [SEARCH] Research climate change in developing countries: Find specific information about how climate change affects developing countries.
3. [WRITE] Write section on recent climate change data: Write a section presenting the most recent climate change data and what it means. Depends on: Research recent climate change statistics.
4. [COMBINE] Update impacts section with new data: Combine the existing impacts section with the new data on developing countries. Depends on: Write section on climate change impacts, Research climate change in developing countries.`,
            
            "content_writing": `# Climate Change: An Introduction

Climate change refers to long-term shifts in temperatures and weather patterns. These shifts may be natural, such as through variations in the solar cycle. But since the 1800s, human activities have been the main driver of climate change, primarily due to burning fossil fuels like coal, oil, and gas, which produces heat-trapping gases.

## Key Causes of Climate Change

1. **Greenhouse Gas Emissions**: The primary driver of current climate change is the release of greenhouse gases, particularly carbon dioxide (CO2), methane, and nitrous oxide, from human activities.

2. **Deforestation**: Forests act as carbon sinks, absorbing CO2 from the atmosphere. When forests are cleared, this carbon is released back into the atmosphere, contributing to the greenhouse effect.

3. **Industrial Processes**: Various industrial processes, including cement production and chemical manufacturing, release greenhouse gases as byproducts.

## Evidence of Climate Change

The evidence for rapid climate change is compelling:

- Global temperature rise: The planet's average surface temperature has risen about 1.1°C since the late 19th century.
- Warming oceans: The oceans have absorbed much of this increased heat, with the top 100 meters warming by more than 0.33°C since 1969.
- Shrinking ice sheets: The Greenland and Antarctic ice sheets have decreased in mass.
- Glacial retreat: Glaciers are retreating almost everywhere around the world.
- Decreased snow cover: Satellite observations reveal that the amount of spring snow cover in the Northern Hemisphere has decreased over the past five decades.
- Sea level rise: Global sea level rose about 8-9 inches in the last century.
- Extreme events: The number of record high temperature events has been increasing, while the number of record low temperature events has been decreasing.

Understanding climate change is crucial for developing effective strategies to mitigate its effects and adapt to the changes that are already occurring.`,
            
            "content_combining": `# Climate Change: Understanding the Crisis

## Introduction

Climate change represents one of the most significant challenges facing humanity in the 21st century. It refers to long-term shifts in temperatures and weather patterns, primarily driven by human activities since the 1800s, especially the burning of fossil fuels. This comprehensive report examines the causes, impacts, and potential solutions to climate change, providing a foundation for informed decision-making and action.

The scientific consensus is clear: our planet is warming at an unprecedented rate due to human activities. The Intergovernmental Panel on Climate Change (IPCC) has stated with high confidence that human-induced warming reached approximately 1.1°C above pre-industrial levels in 2019. Without significant mitigation efforts, this warming is projected to continue, potentially reaching 1.5°C between 2030 and 2052.

## The Science of Climate Change

Climate change occurs when greenhouse gases (GHGs) such as carbon dioxide, methane, and nitrous oxide trap heat in the Earth's atmosphere, creating a greenhouse effect. While this effect is natural and necessary for life on Earth, human activities have significantly increased the concentration of these gases in the atmosphere.

Key causes of anthropogenic (human-caused) climate change include:

1. **Fossil Fuel Combustion**: Burning coal, oil, and natural gas for electricity, heat, and transportation releases significant amounts of CO2.
2. **Deforestation**: Forests act as carbon sinks, absorbing CO2. When cleared, this carbon is released back into the atmosphere.
3. **Industrial Processes**: Manufacturing, particularly cement production, releases CO2 as a byproduct.
4. **Agriculture**: Livestock production releases methane, while fertilizer use releases nitrous oxide.

The evidence for climate change is compelling and multifaceted:

- Rising global temperatures (approximately 1.1°C since pre-industrial times)
- Warming oceans (absorbing over 90% of the excess heat)
- Melting ice sheets and glaciers
- Rising sea levels (about 8-9 inches in the last century)
- Increasing frequency and intensity of extreme weather events
- Shifting wildlife populations and habitats
- Changes in plant flowering and growing seasons

## Impacts of Climate Change

The impacts of climate change are far-reaching, affecting natural systems, human societies, and economies worldwide. These impacts vary by region but generally include:

### Environmental Impacts

- **Ecosystem Disruption**: Changes in temperature and precipitation patterns are altering habitats and disrupting ecological relationships.
- **Biodiversity Loss**: Many species are struggling to adapt to rapid changes, leading to population declines and increased extinction risk.
- **Ocean Acidification**: As oceans absorb CO2, they become more acidic, threatening marine life, particularly shellfish and coral reefs.
- **Water Cycle Changes**: Altered precipitation patterns are causing more frequent and severe droughts in some regions and flooding in others.

### Socioeconomic Impacts

- **Food Security**: Changing growing conditions and extreme weather events threaten agricultural productivity.
- **Water Scarcity**: Many regions are experiencing reduced water availability due to changing precipitation patterns and glacier melt.
- **Human Health**: Climate change exacerbates health risks through increased heat stress, air pollution, and vector-borne diseases.
- **Infrastructure Damage**: Rising sea levels and extreme weather events damage buildings, transportation systems, and energy infrastructure.
- **Economic Costs**: The combined effects of climate change could reduce global GDP by up to 18% by 2050 if no action is taken.

### Disproportionate Effects

Climate change disproportionately affects vulnerable populations, including:

- Low-income communities with fewer resources to adapt
- Coastal and island populations facing sea-level rise
- Indigenous peoples whose livelihoods depend on natural resources
- Developing nations with limited infrastructure and adaptive capacity
- Future generations who will inherit increasingly severe climate conditions

This inequity raises important questions of climate justice and the responsibility of high-emitting nations to support adaptation and mitigation efforts globally.`,
            
            "final_report": `# COMPREHENSIVE REPORT ON CLIMATE CHANGE

## Executive Summary

Climate change represents one of the most significant challenges facing humanity in the 21st century. This report provides a comprehensive overview of climate change, examining its causes, wide-ranging impacts, and potential mitigation strategies. The scientific evidence is unequivocal: Earth's climate is warming at an unprecedented rate due to human activities, primarily the burning of fossil fuels and deforestation. Without immediate and substantial action to reduce greenhouse gas emissions, the world faces severe and potentially irreversible consequences, including rising sea levels, extreme weather events, biodiversity loss, and threats to food and water security. However, there are viable pathways to mitigate climate change through renewable energy transition, improved energy efficiency, sustainable land management, and policy interventions. This report aims to inform decision-makers and the public about the urgency of the climate crisis and the solutions available to address it.

## Introduction

Climate change refers to long-term shifts in temperatures and weather patterns. While these shifts can occur naturally through variations in the solar cycle and volcanic activity, human activities have been the primary driver of climate change since the industrial revolution, particularly through the burning of fossil fuels like coal, oil, and gas, which produces heat-trapping greenhouse gases.

The scientific consensus is clear: our planet is warming at an unprecedented rate due to human activities. The Intergovernmental Panel on Climate Change (IPCC) has stated with high confidence that human-induced warming reached approximately 1.1°C above pre-industrial levels in 2019. Without significant mitigation efforts, this warming is projected to continue, potentially reaching 1.5°C between 2030 and 2052.

Understanding climate change is crucial for developing effective strategies to mitigate its effects and adapt to the changes that are already occurring. This report provides a comprehensive examination of climate change, its causes, impacts, and potential solutions.

## The Science of Climate Change

### Key Causes of Climate Change

1. **Greenhouse Gas Emissions**: The primary driver of current climate change is the release of greenhouse gases, particularly carbon dioxide (CO2), methane, and nitrous oxide, from human activities.

2. **Deforestation**: Forests act as carbon sinks, absorbing CO2 from the atmosphere. When forests are cleared, this carbon is released back into the atmosphere, contributing to the greenhouse effect.

3. **Industrial Processes**: Various industrial processes, including cement production and chemical manufacturing, release greenhouse gases as byproducts.

4. **Agriculture**: Livestock production releases methane, while fertilizer use releases nitrous oxide.

### Evidence of Climate Change

The evidence for rapid climate change is compelling and multifaceted:

- **Global temperature rise**: The planet's average surface temperature has risen about 1.1°C since the late 19th century.
- **Warming oceans**: The oceans have absorbed much of this increased heat, with the top 100 meters warming by more than 0.33°C since 1969.
- **Shrinking ice sheets**: The Greenland and Antarctic ice sheets have decreased in mass.
- **Glacial retreat**: Glaciers are retreating almost everywhere around the world.
- **Decreased snow cover**: Satellite observations reveal that the amount of spring snow cover in the Northern Hemisphere has decreased over the past five decades.
- **Sea level rise**: Global sea level rose about 8-9 inches in the last century.
- **Extreme events**: The number of record high temperature events has been increasing, while the number of record low temperature events has been decreasing.

## Impacts of Climate Change

The impacts of climate change are far-reaching, affecting natural systems, human societies, and economies worldwide. These impacts vary by region but generally include:

### Environmental Impacts

- **Ecosystem Disruption**: Changes in temperature and precipitation patterns are altering habitats and disrupting ecological relationships.
- **Biodiversity Loss**: Many species are struggling to adapt to rapid changes, leading to population declines and increased extinction risk.
- **Ocean Acidification**: As oceans absorb CO2, they become more acidic, threatening marine life, particularly shellfish and coral reefs.
- **Water Cycle Changes**: Altered precipitation patterns are causing more frequent and severe droughts in some regions and flooding in others.

### Socioeconomic Impacts

- **Food Security**: Changing growing conditions and extreme weather events threaten agricultural productivity.
- **Water Scarcity**: Many regions are experiencing reduced water availability due to changing precipitation patterns and glacier melt.
- **Human Health**: Climate change exacerbates health risks through increased heat stress, air pollution, and vector-borne diseases.
- **Infrastructure Damage**: Rising sea levels and extreme weather events damage buildings, transportation systems, and energy infrastructure.
- **Economic Costs**: The combined effects of climate change could reduce global GDP by up to 18% by 2050 if no action is taken.

### Disproportionate Effects

Climate change disproportionately affects vulnerable populations, including:

- Low-income communities with fewer resources to adapt
- Coastal and island populations facing sea-level rise
- Indigenous peoples whose livelihoods depend on natural resources
- Developing nations with limited infrastructure and adaptive capacity
- Future generations who will inherit increasingly severe climate conditions

This inequity raises important questions of climate justice and the responsibility of high-emitting nations to support adaptation and mitigation efforts globally.

## Recent Climate Change Data

The most recent climate data continues to confirm and strengthen the scientific understanding of climate change:

- 2020 tied with 2016 as the warmest year on record.
- The last decade (2011-2020) was the warmest on record.
- Atmospheric CO2 concentrations reached 419 parts per million in 2021, the highest level in at least 2 million years.
- Arctic sea ice is declining at a rate of 13.1% per decade.
- The rate of sea level rise has accelerated to 3.7 mm per year.

In developing countries, climate change impacts are often more severe due to:

- Greater dependence on climate-sensitive sectors like agriculture
- Limited financial resources for adaptation measures
- Higher exposure to extreme weather events
- Less robust infrastructure and emergency response systems

For example, in sub-Saharan Africa, climate change is projected to reduce crop yields by up to 30% by 2050, threatening food security in a region already struggling with hunger and malnutrition.

## Climate Change Mitigation Strategies

Mitigating climate change requires a multi-faceted approach to reduce greenhouse gas emissions and enhance carbon sinks. Key strategies include:

### Energy Transition

- **Renewable Energy**: Rapidly scaling up solar, wind, hydroelectric, and geothermal power to replace fossil fuels.
- **Energy Efficiency**: Improving efficiency in buildings, transportation, and industry to reduce overall energy demand.
- **Electrification**: Converting systems that currently use fossil fuels (e.g., vehicles, heating) to electricity that can be generated from renewable sources.

### Land Use Changes

- **Reforestation and Afforestation**: Planting trees and restoring forests to sequester carbon.
- **Sustainable Agriculture**: Implementing practices that reduce emissions and increase soil carbon storage.
- **Wetland and Peatland Protection**: Preserving these ecosystems, which store large amounts of carbon.

### Policy and Economic Measures

- **Carbon Pricing**: Implementing carbon taxes or cap-and-trade systems to internalize the cost of carbon emissions.
- **Regulations and Standards**: Setting emissions limits and efficiency standards for various sectors.
- **Research and Development**: Investing in new technologies for clean energy, carbon capture, and other mitigation solutions.

### International Cooperation

- **Paris Agreement**: Strengthening nationally determined contributions to meet the goal of limiting warming to well below 2°C.
- **Climate Finance**: Providing financial support to developing countries for mitigation and adaptation.
- **Technology Transfer**: Sharing clean energy and adaptation technologies with developing nations.

## Conclusion

Climate change represents one of the most significant challenges of our time, with far-reaching implications for ecosystems, human societies, and future generations. The scientific evidence is clear: human activities are causing rapid warming of the Earth's climate system, leading to a cascade of impacts from rising sea levels to extreme weather events.

However, there is still time to prevent the most catastrophic consequences of climate change through decisive action. Mitigation strategies, including transitioning to renewable energy, improving energy efficiency, and implementing sustainable land management practices, can significantly reduce greenhouse gas emissions. At the same time, adaptation measures can help communities prepare for and respond to the changes that are already occurring.

Addressing climate change requires coordinated efforts at all levels—from individual actions to international agreements. It also demands recognition of the disproportionate impacts on vulnerable populations and a commitment to climate justice.

The choices we make in the coming decades will determine the world we leave for future generations. By acting with urgency, innovation, and solidarity, we can build a more sustainable and resilient future in the face of climate change.

## References

1. Intergovernmental Panel on Climate Change (IPCC). (2021). Sixth Assessment Report.
2. NASA Global Climate Change. (2021). Vital Signs of the Planet.
3. World Meteorological Organization (WMO). (2021). State of the Global Climate 2020.
4. United Nations Environment Programme (UNEP). (2020). Emissions Gap Report 2020.
5. World Bank. (2021). Climate Change Action Plan 2021-2025.
6. International Energy Agency (IEA). (2021). Net Zero by 2050: A Roadmap for the Global Energy Sector.
7. Food and Agriculture Organization (FAO). (2020). Climate Change and Food Security.
8. World Health Organization (WHO). (2021). Climate Change and Health.`,
        },
    }
}

// Generate generates text using the mock LLM
func (p *MockLLMProvider) Generate(prompt, purpose string) (string, error) {
    // Check if we have a predefined response for this purpose
    if response, ok := p.responses[purpose]; ok {
        return response, nil
    }
    
    // Default response
    return "This is a mock response for: " + purpose, nil
}
```

## Example Execution

Here's a step-by-step example of how the report writing agent processes a request to write a report on climate change:

1. **Initialization**:
   - Create a new report writing agent
   - Initialize task network with root task "Write Report"
   - Set topic parameter to "climate change"

2. **Initial Planning**:
   - Call LLM to plan the root task
   - LLM decomposes the task into subtasks:
     - Research climate change basics (SEARCH)
     - Research climate change impacts (SEARCH)
     - Research climate change mitigation (SEARCH)
     - Write introduction (WRITE, depends on research basics)
     - Write impacts section (WRITE, depends on research impacts)
     - Write mitigation section (WRITE, depends on research mitigation)
     - Combine introduction and impacts (COMBINE, depends on introduction and impacts)
     - Create final report (COMBINE, depends on combined intro/impacts and mitigation)

3. **Task Execution - Research**:
   - Execute "Research climate change basics" task
     - Call search tool to find information
     - Store results in memory
     - Mark task as completed
   - Generate new tasks based on research results:
     - Research recent climate change statistics (SEARCH)
     - Write section on recent data (WRITE, depends on new research)

4. **Task Execution - More Research**:
   - Execute "Research climate change impacts" task
   - Execute "Research climate change mitigation" task
   - Execute "Research recent climate change statistics" task
   - Execute "Research climate change in developing countries" task
   - All research tasks store results in memory

5. **Task Execution - Writing**:
   - Execute "Write introduction to climate change" task
     - Retrieve research results from memory
     - Call LLM to write introduction
     - Store written content in memory
   - Execute "Write section on climate change impacts" task
   - Execute "Write section on climate change mitigation" task
   - Execute "Write section on recent climate change data" task

6. **Task Execution - Combining**:
   - Execute "Combine introduction and impacts" task
     - Retrieve written sections from memory
     - Call LLM to combine sections
     - Store combined content in memory
   - Execute "Update impacts section with new data" task
   - Execute "Create final report" task
     - Retrieve all combined sections
     - Call LLM to create final report
     - Store final report in memory

7. **Completion**:
   - All tasks completed
   - Return final report

The hierarchical task network at the end of execution might look like this:

```
Write Report on Climate Change (COMPOUND, COMPLETED)
├── Research climate change basics (PRIMITIVE, SEARCH, COMPLETED)
├── Research climate change impacts (PRIMITIVE, SEARCH, COMPLETED)
├── Research climate change mitigation (PRIMITIVE, SEARCH, COMPLETED)
├── Research recent climate change statistics (PRIMITIVE, SEARCH, COMPLETED)
├── Research climate change in developing countries (PRIMITIVE, SEARCH, COMPLETED)
├── Write introduction to climate change (PRIMITIVE, WRITE, COMPLETED)
├── Write section on climate change impacts (PRIMITIVE, WRITE, COMPLETED)
├── Write section on climate change mitigation (PRIMITIVE, WRITE, COMPLETED)
├── Write section on recent climate change data (PRIMITIVE, WRITE, COMPLETED)
├── Combine introduction and impacts (PRIMITIVE, COMBINE, COMPLETED)
├── Update impacts section with new data (PRIMITIVE, COMBINE, COMPLETED)
└── Create final report (PRIMITIVE, COMBINE, COMPLETED)
```

## Advanced Features

### Task Prioritization

Tasks can be prioritized based on various factors:

```go
// TaskPriority represents the priority of a task
type TaskPriority int

const (
    PriorityLow    TaskPriority = 1
    PriorityNormal TaskPriority = 2
    PriorityHigh   TaskPriority = 3
)

// Add priority to Task struct
type Task struct {
    // ... existing fields
    Priority TaskPriority
}

// GetPendingTasksByPriority returns pending tasks sorted by priority
func (tn *TaskNetwork) GetPendingTasksByPriority() []*Task {
    pendingTasks := tn.GetPendingTasks()
    
    // Sort by priority (highest first)
    sort.Slice(pendingTasks, func(i, j int) bool {
        return pendingTasks[i].Priority > pendingTasks[j].Priority
    })
    
    return pendingTasks
}
```

### Parallel Execution

Multiple tasks can be executed in parallel if they don't depend on each other:

```go
// ParallelTaskExecutor executes tasks in parallel
type ParallelTaskExecutor struct {
    taskExecutor *TaskExecutor
    maxWorkers   int
    workerPool   chan struct{}
}

// NewParallelTaskExecutor creates a new parallel task executor
func NewParallelTaskExecutor(taskExecutor *TaskExecutor, maxWorkers int) *ParallelTaskExecutor {
    return &ParallelTaskExecutor{
        taskExecutor: taskExecutor,
        maxWorkers:   maxWorkers,
        workerPool:   make(chan struct{}, maxWorkers),
    }
}

// ExecuteTasks executes multiple tasks in parallel
func (pte *ParallelTaskExecutor) ExecuteTasks(tasks []*Task) {
    var wg sync.WaitGroup
    
    for _, task := range tasks {
        wg.Add(1)
        
        // Acquire worker
        pte.workerPool <- struct{}{}
        
        go func(t *Task) {
            defer wg.Done()
            defer func() { <-pte.workerPool }() // Release worker
            
            pte.taskExecutor.ExecuteTask(t)
        }(task)
    }
    
    wg.Wait()
}
```

### Error Recovery

The agent can recover from task failures:

```go
// TaskRecoveryStrategy represents a strategy for recovering from task failures
type TaskRecoveryStrategy interface {
    RecoverFromFailure(task *Task) (*Task, error)
}

// RetryStrategy retries failed tasks
type RetryStrategy struct {
    maxRetries int
    taskPlanner *TaskPlanner
}

// RecoverFromFailure retries a failed task
func (rs *RetryStrategy) RecoverFromFailure(task *Task) (*Task, error) {
    // Check if task has been retried too many times
    retryCount := 0
    if retryCountParam, ok := task.Parameters["retry_count"]; ok {
        if count, ok := retryCountParam.(int); ok {
            retryCount = count
        }
    }
    
    if retryCount >= rs.maxRetries {
        return nil, fmt.Errorf("max retries exceeded for task: %s", task.ID)
    }
    
    // Create a new task as a retry
    retryTask := NewTask(task.ParentID, task.Type, task.Category, 
                        fmt.Sprintf("%s (Retry %d)", task.Name, retryCount+1), 
                        task.Description)
    
    // Copy parameters
    for k, v := range task.Parameters {
        retryTask.Parameters[k] = v
    }
    
    // Set retry count
    retryTask.Parameters["retry_count"] = retryCount + 1
    
    // Copy dependencies
    retryTask.Dependencies = append([]string{}, task.Dependencies...)
    
    return retryTask, nil
}

// AlternativeStrategy tries an alternative approach for failed tasks
type AlternativeStrategy struct {
    llmService *LLMService
    taskPlanner *TaskPlanner
}

// RecoverFromFailure creates an alternative task
func (as *AlternativeStrategy) RecoverFromFailure(task *Task) (*Task, error) {
    // Create prompt for alternative approach
    prompt := fmt.Sprintf(`The following task has failed:
    
Task: %s
Description: %s
Category: %s

Please suggest an alternative approach to accomplish the same goal.
Provide a new task description that takes a different approach.`, 
        task.Name, task.Description, task.Category)
    
    // Call LLM for alternative approach
    alternativeDesc, err := as.llmService.Generate(prompt, "alternative_approach")
    if err != nil {
        return nil, fmt.Errorf("failed to generate alternative approach: %w", err)
    }
    
    // Create a new task with alternative approach
    alternativeTask := NewTask(task.ParentID, task.Type, task.Category, 
                             fmt.Sprintf("%s (Alternative)", task.Name), 
                             alternativeDesc)
    
    // Copy dependencies
    alternativeTask.Dependencies = append([]string{}, task.Dependencies...)
    
    return alternativeTask, nil
}
```

This design demonstrates how a dynamic hierarchical task network agent can be implemented for report writing. The agent uses LLM calls for planning, alternates between searching, writing, and combining content, and dynamically generates new tasks based on the results of executed tasks.
