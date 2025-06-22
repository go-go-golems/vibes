package main

import (
	"encoding/json"
	"fmt"
	"os"
	"regexp"
	"strings"
	"time"

	"github.com/dop251/goja"
	"github.com/example/goja-watermill/pkg/watermill"
	"github.com/rs/zerolog"
)

// LogEntry represents a parsed log entry
type LogEntry struct {
	Level       string                 `json:"level"`
	Time        string                 `json:"time"`
	Message     string                 `json:"message"`
	Component   string                 `json:"component,omitempty"`
	MessageUUID string                 `json:"message_uuid,omitempty"`
	Topic       string                 `json:"topic,omitempty"`
	HandlerID   string                 `json:"handler_id,omitempty"`
	PubSubID    string                 `json:"pubsub_id,omitempty"`
	Fields      map[string]interface{} `json:"-"`
}

// ValidationResult represents the result of validation
type ValidationResult struct {
	TestName    string                 `json:"test_name"`
	Success     bool                   `json:"success"`
	Duration    time.Duration          `json:"duration"`
	LogEntries  []LogEntry             `json:"log_entries"`
	Metrics     map[string]interface{} `json:"metrics"`
	Errors      []string               `json:"errors,omitempty"`
	MessageFlow map[string][]LogEntry  `json:"message_flow,omitempty"`
}

// LogCapture captures logs for analysis
type LogCapture struct {
	entries []LogEntry
	buffer  strings.Builder
}

// Write implements io.Writer
func (lc *LogCapture) Write(p []byte) (n int, err error) {
	lc.buffer.Write(p)
	
	// Parse JSON log entries
	lines := strings.Split(strings.TrimSpace(lc.buffer.String()), "\n")
	lc.buffer.Reset()
	
	for _, line := range lines {
		if strings.TrimSpace(line) == "" {
			continue
		}
		
		var entry LogEntry
		if err := json.Unmarshal([]byte(line), &entry); err == nil {
			lc.entries = append(lc.entries, entry)
		} else {
			// Handle non-JSON log lines (like Watermill logs)
			if strings.Contains(line, "level=") {
				entry = parseWatermillLog(line)
				lc.entries = append(lc.entries, entry)
			}
		}
	}
	
	return len(p), nil
}

// parseWatermillLog parses Watermill log format
func parseWatermillLog(line string) LogEntry {
	entry := LogEntry{
		Fields: make(map[string]interface{}),
	}
	
	// Extract level
	if levelMatch := regexp.MustCompile(`level=(\w+)`).FindStringSubmatch(line); len(levelMatch) > 1 {
		entry.Level = strings.ToLower(levelMatch[1])
	}
	
	// Extract message
	if msgMatch := regexp.MustCompile(`msg="([^"]+)"`).FindStringSubmatch(line); len(msgMatch) > 1 {
		entry.Message = msgMatch[1]
	}
	
	// Extract message UUID
	if uuidMatch := regexp.MustCompile(`message_uuid=([^\s]+)`).FindStringSubmatch(line); len(uuidMatch) > 1 {
		entry.MessageUUID = uuidMatch[1]
	}
	
	// Extract topic
	if topicMatch := regexp.MustCompile(`topic=([^\s]+)`).FindStringSubmatch(line); len(topicMatch) > 1 {
		entry.Topic = topicMatch[1]
	}
	
	entry.Component = "watermill"
	entry.Time = time.Now().Format(time.RFC3339)
	
	return entry
}

// GetEntries returns captured log entries
func (lc *LogCapture) GetEntries() []LogEntry {
	return lc.entries
}

// Clear clears captured entries
func (lc *LogCapture) Clear() {
	lc.entries = nil
	lc.buffer.Reset()
}

func main() {
	fmt.Println("🚀 Starting Goja-Watermill Comprehensive Validation")
	fmt.Println(strings.Repeat("=", 60))
	
	// Set up log capture
	logCapture := &LogCapture{}
	
	// Configure zerolog to write to our capture
	logger := zerolog.New(logCapture).With().Timestamp().Logger()
	
	// Run validation tests
	results := []ValidationResult{}
	
	// Test 1: Basic Functionality
	fmt.Println("📋 Test 1: Basic Functionality")
	result1 := validateBasicFunctionality(logger, logCapture)
	results = append(results, result1)
	printResult(result1)
	
	// Test 2: Message Flow
	fmt.Println("\n📋 Test 2: Message Flow Validation")
	result2 := validateMessageFlow(logger, logCapture)
	results = append(results, result2)
	printResult(result2)
	
	// Test 3: Middleware Chain
	fmt.Println("\n📋 Test 3: Middleware Chain")
	result3 := validateMiddleware(logger, logCapture)
	results = append(results, result3)
	printResult(result3)
	
	// Test 4: Error Handling
	fmt.Println("\n📋 Test 4: Error Handling")
	result4 := validateErrorHandling(logger, logCapture)
	results = append(results, result4)
	printResult(result4)
	
	// Test 5: Performance
	fmt.Println("\n📋 Test 5: Performance Validation")
	result5 := validatePerformance(logger, logCapture)
	results = append(results, result5)
	printResult(result5)
	
	// Generate summary
	fmt.Println("\n" + strings.Repeat("=", 60))
	fmt.Println("📊 VALIDATION SUMMARY")
	fmt.Println(strings.Repeat("=", 60))
	
	totalTests := len(results)
	passedTests := 0
	totalDuration := time.Duration(0)
	
	for _, result := range results {
		if result.Success {
			passedTests++
		}
		totalDuration += result.Duration
	}
	
	fmt.Printf("✅ Tests Passed: %d/%d (%.1f%%)\n", passedTests, totalTests, float64(passedTests)/float64(totalTests)*100)
	fmt.Printf("⏱️  Total Duration: %v\n", totalDuration)
	
	// Analyze logs across all tests
	fmt.Println("\n📈 LOG ANALYSIS")
	fmt.Println(strings.Repeat("-", 30))
	
	allEntries := []LogEntry{}
	for _, result := range results {
		allEntries = append(allEntries, result.LogEntries...)
	}
	
	analyzeLogEntries(allEntries)
	
	// Save detailed results
	saveResults(results)
	
	if passedTests == totalTests {
		fmt.Println("\n🎉 ALL TESTS PASSED! Implementation is working correctly.")
		os.Exit(0)
	} else {
		fmt.Printf("\n❌ %d tests failed. Check the detailed results.\n", totalTests-passedTests)
		os.Exit(1)
	}
}

func validateBasicFunctionality(logger zerolog.Logger, logCapture *LogCapture) ValidationResult {
	start := time.Now()
	logCapture.Clear()
	
	result := ValidationResult{
		TestName: "Basic Functionality",
		Metrics:  make(map[string]interface{}),
	}
	
	defer func() {
		result.Duration = time.Since(start)
		result.LogEntries = logCapture.GetEntries()
	}()
	
	// Create Goja runtime and module
	vm := goja.New()
	module, err := watermill.NewModule(vm, logger)
	if err != nil {
		result.Errors = append(result.Errors, fmt.Sprintf("Failed to create module: %v", err))
		return result
	}
	defer module.Close()
	
	// Set up console
	setupConsole(vm)
	
	// Test basic pub/sub
	jsCode := `
		const bus = watermill.createPubSub("memory", { enable_metrics: true });
		let messageReceived = false;
		let receivedPayload = "";
		
		bus.subscribe("test.basic", function(msg) {
			messageReceived = true;
			receivedPayload = msg.payload;
		});
		
		watermill.start();
		bus.publish("test.basic", "Hello, Validation!");
		
		// Return test results
		{ messageReceived, receivedPayload, busId: bus.getId() }
	`
	
	// Wait for processing
	time.Sleep(200 * time.Millisecond)
	
	_, err = vm.RunString(jsCode)
	if err != nil {
		result.Errors = append(result.Errors, fmt.Sprintf("JavaScript execution failed: %v", err))
		return result
	}
	
	// Wait for message processing
	time.Sleep(200 * time.Millisecond)
	
	// Check results
	testResult, err := vm.RunString("{ messageReceived, receivedPayload, busId: bus.getId() }")
	if err != nil {
		result.Errors = append(result.Errors, fmt.Sprintf("Failed to get test results: %v", err))
		return result
	}
	
	resultObj := testResult.Export().(map[string]interface{})
	
	if !resultObj["messageReceived"].(bool) {
		result.Errors = append(result.Errors, "Message was not received")
		return result
	}
	
	if resultObj["receivedPayload"].(string) != "Hello, Validation!" {
		result.Errors = append(result.Errors, "Incorrect payload received")
		return result
	}
	
	result.Success = true
	result.Metrics["message_received"] = true
	result.Metrics["payload_correct"] = true
	result.Metrics["bus_id"] = resultObj["busId"]
	
	return result
}

func validateMessageFlow(logger zerolog.Logger, logCapture *LogCapture) ValidationResult {
	start := time.Now()
	logCapture.Clear()
	
	result := ValidationResult{
		TestName:    "Message Flow",
		Metrics:     make(map[string]interface{}),
		MessageFlow: make(map[string][]LogEntry),
	}
	
	defer func() {
		result.Duration = time.Since(start)
		result.LogEntries = logCapture.GetEntries()
		
		// Analyze message flow
		for _, entry := range result.LogEntries {
			if entry.MessageUUID != "" {
				result.MessageFlow[entry.MessageUUID] = append(result.MessageFlow[entry.MessageUUID], entry)
			}
		}
	}()
	
	vm := goja.New()
	module, err := watermill.NewModule(vm, logger)
	if err != nil {
		result.Errors = append(result.Errors, fmt.Sprintf("Failed to create module: %v", err))
		return result
	}
	defer module.Close()
	
	setupConsole(vm)
	
	// Test message flow tracking
	_, err = vm.RunString(`
		const bus = watermill.createPubSub("memory");
		let processedMessages = [];
		
		bus.subscribe("flow.test", function(msg) {
			processedMessages.push({
				uuid: msg.uuid,
				payload: msg.payload,
				timestamp: new Date().toISOString()
			});
		});
		
		watermill.start();
		
		// Publish multiple messages
		bus.publish("flow.test", "Message 1");
		bus.publish("flow.test", "Message 2");
		bus.publish("flow.test", "Message 3");
	`)
	
	if err != nil {
		result.Errors = append(result.Errors, fmt.Sprintf("JavaScript execution failed: %v", err))
		return result
	}
	
	// Wait for processing
	time.Sleep(300 * time.Millisecond)
	
	// Check results
	jsResult, err := vm.RunString("processedMessages.length")
	if err != nil {
		result.Errors = append(result.Errors, fmt.Sprintf("Failed to get results: %v", err))
		return result
	}
	
	messageCount := int(jsResult.ToInteger())
	if messageCount != 3 {
		result.Errors = append(result.Errors, fmt.Sprintf("Expected 3 messages, got %d", messageCount))
		return result
	}
	
	// Validate that we have complete message flows
	expectedStages := []string{"Publishing message", "Message published successfully", "Processing message in JS handler"}
	completeFlows := 0
	
	for uuid, flow := range result.MessageFlow {
		stagesSeen := make(map[string]bool)
		for _, entry := range flow {
			stagesSeen[entry.Message] = true
		}
		
		complete := true
		for _, stage := range expectedStages {
			if !stagesSeen[stage] {
				complete = false
				break
			}
		}
		
		if complete {
			completeFlows++
		} else {
			result.Errors = append(result.Errors, fmt.Sprintf("Incomplete flow for message %s", uuid))
		}
	}
	
	result.Success = completeFlows >= 3
	result.Metrics["messages_processed"] = messageCount
	result.Metrics["complete_flows"] = completeFlows
	result.Metrics["total_flows"] = len(result.MessageFlow)
	
	return result
}

func validateMiddleware(logger zerolog.Logger, logCapture *LogCapture) ValidationResult {
	start := time.Now()
	logCapture.Clear()
	
	result := ValidationResult{
		TestName: "Middleware Chain",
		Metrics:  make(map[string]interface{}),
	}
	
	defer func() {
		result.Duration = time.Since(start)
		result.LogEntries = logCapture.GetEntries()
	}()
	
	vm := goja.New()
	module, err := watermill.NewModule(vm, logger)
	if err != nil {
		result.Errors = append(result.Errors, fmt.Sprintf("Failed to create module: %v", err))
		return result
	}
	defer module.Close()
	
	setupConsole(vm)
	
	// Test middleware chain
	_, err = vm.RunString(`
		const bus = watermill.createPubSub("memory");
		let middlewareLog = [];
		let handlerLog = [];
		
		// Add multiple middleware
		bus.useMiddleware(function(msg, next) {
			middlewareLog.push("middleware1:before:" + msg.uuid);
			msg.setMetadata("middleware1", "processed");
			const result = next(msg);
			middlewareLog.push("middleware1:after:" + msg.uuid);
			return result;
		});
		
		bus.useMiddleware(function(msg, next) {
			middlewareLog.push("middleware2:before:" + msg.uuid);
			msg.setMetadata("middleware2", "processed");
			const result = next(msg);
			middlewareLog.push("middleware2:after:" + msg.uuid);
			return result;
		});
		
		bus.subscribe("middleware.test", function(msg) {
			handlerLog.push({
				uuid: msg.uuid,
				middleware1: msg.getMetadata("middleware1"),
				middleware2: msg.getMetadata("middleware2")
			});
		});
		
		watermill.start();
		bus.publish("middleware.test", "Middleware Test");
	`)
	
	if err != nil {
		result.Errors = append(result.Errors, fmt.Sprintf("JavaScript execution failed: %v", err))
		return result
	}
	
	// Wait for processing
	time.Sleep(300 * time.Millisecond)
	
	// Check middleware execution
	middlewareResult, err := vm.RunString("middlewareLog")
	if err != nil {
		result.Errors = append(result.Errors, fmt.Sprintf("Failed to get middleware log: %v", err))
		return result
	}
	
	handlerResult, err := vm.RunString("handlerLog")
	if err != nil {
		result.Errors = append(result.Errors, fmt.Sprintf("Failed to get handler log: %v", err))
		return result
	}
	
	middlewareLog := middlewareResult.Export().([]interface{})
	handlerLog := handlerResult.Export().([]interface{})
	
	// Validate middleware execution order
	expectedOrder := []string{"middleware2:before:", "middleware1:before:", "middleware1:after:", "middleware2:after:"}
	
	if len(middlewareLog) != 4 {
		result.Errors = append(result.Errors, fmt.Sprintf("Expected 4 middleware log entries, got %d", len(middlewareLog)))
		return result
	}
	
	for i, expected := range expectedOrder {
		actual := middlewareLog[i].(string)
		if !strings.Contains(actual, expected) {
			result.Errors = append(result.Errors, fmt.Sprintf("Middleware order incorrect at position %d: expected %s, got %s", i, expected, actual))
			return result
		}
	}
	
	// Validate handler received processed metadata
	if len(handlerLog) != 1 {
		result.Errors = append(result.Errors, fmt.Sprintf("Expected 1 handler log entry, got %d", len(handlerLog)))
		return result
	}
	
	handler := handlerLog[0].(map[string]interface{})
	if handler["middleware1"] != "processed" || handler["middleware2"] != "processed" {
		result.Errors = append(result.Errors, "Middleware metadata not properly set")
		return result
	}
	
	result.Success = true
	result.Metrics["middleware_executions"] = len(middlewareLog)
	result.Metrics["handler_executions"] = len(handlerLog)
	result.Metrics["middleware_order_correct"] = true
	
	return result
}

func validateErrorHandling(logger zerolog.Logger, logCapture *LogCapture) ValidationResult {
	start := time.Now()
	logCapture.Clear()
	
	result := ValidationResult{
		TestName: "Error Handling",
		Metrics:  make(map[string]interface{}),
	}
	
	defer func() {
		result.Duration = time.Since(start)
		result.LogEntries = logCapture.GetEntries()
	}()
	
	vm := goja.New()
	module, err := watermill.NewModule(vm, logger)
	if err != nil {
		result.Errors = append(result.Errors, fmt.Sprintf("Failed to create module: %v", err))
		return result
	}
	defer module.Close()
	
	setupConsole(vm)
	
	// Test error handling
	_, err = vm.RunString(`
		const bus = watermill.createPubSub("memory");
		let successCount = 0;
		let errorCount = 0;
		
		bus.subscribe("error.test", function(msg) {
			if (msg.payload === "error") {
				errorCount++;
				throw new Error("Intentional test error");
			} else {
				successCount++;
			}
		});
		
		watermill.start();
		
		// Publish messages that will succeed and fail
		bus.publish("error.test", "success1");
		bus.publish("error.test", "error");
		bus.publish("error.test", "success2");
		bus.publish("error.test", "error");
		bus.publish("error.test", "success3");
	`)
	
	if err != nil {
		result.Errors = append(result.Errors, fmt.Sprintf("JavaScript execution failed: %v", err))
		return result
	}
	
	// Wait for processing
	time.Sleep(300 * time.Millisecond)
	
	// Check results
	successResult, err := vm.RunString("successCount")
	if err != nil {
		result.Errors = append(result.Errors, fmt.Sprintf("Failed to get success count: %v", err))
		return result
	}
	
	errorResult, err := vm.RunString("errorCount")
	if err != nil {
		result.Errors = append(result.Errors, fmt.Sprintf("Failed to get error count: %v", err))
		return result
	}
	
	successCount := int(successResult.ToInteger())
	errorCount := int(errorResult.ToInteger())
	
	if successCount != 3 {
		result.Errors = append(result.Errors, fmt.Sprintf("Expected 3 successful messages, got %d", successCount))
		return result
	}
	
	if errorCount != 2 {
		result.Errors = append(result.Errors, fmt.Sprintf("Expected 2 error messages, got %d", errorCount))
		return result
	}
	
	// Check that errors were logged
	errorLogs := 0
	for _, entry := range result.LogEntries {
		if entry.Level == "error" && strings.Contains(entry.Message, "JS handler failed") {
			errorLogs++
		}
	}
	
	if errorLogs < 2 {
		result.Errors = append(result.Errors, fmt.Sprintf("Expected at least 2 error log entries, got %d", errorLogs))
		return result
	}
	
	result.Success = true
	result.Metrics["success_count"] = successCount
	result.Metrics["error_count"] = errorCount
	result.Metrics["error_logs"] = errorLogs
	
	return result
}

func validatePerformance(logger zerolog.Logger, logCapture *LogCapture) ValidationResult {
	start := time.Now()
	logCapture.Clear()
	
	result := ValidationResult{
		TestName: "Performance",
		Metrics:  make(map[string]interface{}),
	}
	
	defer func() {
		result.Duration = time.Since(start)
		result.LogEntries = logCapture.GetEntries()
	}()
	
	vm := goja.New()
	module, err := watermill.NewModule(vm, logger)
	if err != nil {
		result.Errors = append(result.Errors, fmt.Sprintf("Failed to create module: %v", err))
		return result
	}
	defer module.Close()
	
	setupConsole(vm)
	
	// Performance test
	const messageCount = 100
	
	_, err = vm.RunString(fmt.Sprintf(`
		const bus = watermill.createPubSub("memory", { enable_metrics: true });
		let processedCount = 0;
		const startTime = Date.now();
		
		bus.subscribe("perf.test", function(msg) {
			processedCount++;
		});
		
		watermill.start();
		
		// Publish many messages
		for (let i = 0; i < %d; i++) {
			bus.publish("perf.test", "Message " + i);
		}
		
		const publishTime = Date.now();
	`, messageCount))
	
	if err != nil {
		result.Errors = append(result.Errors, fmt.Sprintf("JavaScript execution failed: %v", err))
		return result
	}
	
	// Wait for processing
	time.Sleep(1 * time.Second)
	
	// Check results
	processedResult, err := vm.RunString("processedCount")
	if err != nil {
		result.Errors = append(result.Errors, fmt.Sprintf("Failed to get processed count: %v", err))
		return result
	}
	
	processedCount := int(processedResult.ToInteger())
	
	if processedCount != messageCount {
		result.Errors = append(result.Errors, fmt.Sprintf("Expected %d processed messages, got %d", messageCount, processedCount))
		return result
	}
	
	// Calculate throughput
	throughput := float64(processedCount) / result.Duration.Seconds()
	
	result.Success = true
	result.Metrics["messages_processed"] = processedCount
	result.Metrics["duration_seconds"] = result.Duration.Seconds()
	result.Metrics["throughput_msg_per_sec"] = throughput
	result.Metrics["performance_acceptable"] = throughput > 50 // At least 50 msg/sec
	
	return result
}

func setupConsole(vm *goja.Runtime) {
	console := vm.NewObject()
	console.Set("log", func(call goja.FunctionCall) goja.Value {
		return goja.Undefined()
	})
	vm.Set("console", console)
}

func printResult(result ValidationResult) {
	if result.Success {
		fmt.Printf("✅ %s: PASSED (%.2fs)\n", result.TestName, result.Duration.Seconds())
	} else {
		fmt.Printf("❌ %s: FAILED (%.2fs)\n", result.TestName, result.Duration.Seconds())
		for _, err := range result.Errors {
			fmt.Printf("   • %s\n", err)
		}
	}
	
	// Print key metrics
	for key, value := range result.Metrics {
		fmt.Printf("   📊 %s: %v\n", key, value)
	}
}

func analyzeLogEntries(entries []LogEntry) {
	levelCounts := make(map[string]int)
	componentCounts := make(map[string]int)
	
	for _, entry := range entries {
		levelCounts[entry.Level]++
		if entry.Component != "" {
			componentCounts[entry.Component]++
		}
	}
	
	fmt.Printf("Total Log Entries: %d\n", len(entries))
	fmt.Println("By Level:")
	for level, count := range levelCounts {
		fmt.Printf("  %s: %d\n", level, count)
	}
	
	fmt.Println("By Component:")
	for component, count := range componentCounts {
		fmt.Printf("  %s: %d\n", component, count)
	}
}

func saveResults(results []ValidationResult) {
	file, err := os.Create("validation_results.json")
	if err != nil {
		fmt.Printf("Warning: Could not save results: %v\n", err)
		return
	}
	defer file.Close()
	
	encoder := json.NewEncoder(file)
	encoder.SetIndent("", "  ")
	encoder.Encode(map[string]interface{}{
		"timestamp": time.Now().Format(time.RFC3339),
		"results":   results,
	})
	
	fmt.Println("📄 Detailed results saved to validation_results.json")
}

