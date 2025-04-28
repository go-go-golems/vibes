# How to Use Event Router to Output Agent LLM to Stdout

## Overview

This document outlines a plan to enhance the `GeppettoLLM` implementation to leverage Geppetto's event system for streaming LLM output to stdout. Currently, the LLM only returns the final result without providing any streaming or progress updates during generation.

## Background

Geppetto implements a sophisticated event system using the Watermill library for pub/sub messaging. The system allows:

1. Steps (like AI chat completions) to publish events during their execution via `AddPublishedTopic`.
2. Subscribers to receive these events via an `EventRouter`.
3. Different event types for different stages (start, partial completion, tool calls, final result, errors).

By connecting this event system to stdout using `events.StepPrinterFunc`, we can display real-time streaming output and provide a better user experience in the LLM agent framework.

## Implementation Plan

### 1. Extend `GeppettoLLM` to Support Event Publishing Configuration [-]

-   [-] Add `message.Publisher` and `topicID` fields to `GeppettoLLM` struct.
-   [-] Add a `WithPublisherAndTopic` option to the constructor to set these fields.

```go
// Add to GeppettoLLM struct
type GeppettoLLM struct {
    // existing fields
    publisher message.Publisher
    topicID   string
}

// Option pattern for configuration
type GeppettoLLMOption func(*GeppettoLLM) error

// WithPublisherAndTopic adds a publisher and topic for event emission
func WithPublisherAndTopic(publisher message.Publisher, topicID string) GeppettoLLMOption {
    return func(llm *GeppettoLLM) error {
        if publisher == nil {
            return errors.New("publisher cannot be nil")
        }
        if topicID == "" {
            return errors.New("topicID cannot be empty")
        }
        llm.publisher = publisher
        llm.topicID = topicID
        return nil
    }
}

// Update constructor
func NewGeppettoLLM(
    stepSettings *settings.StepSettings,
    opts ...GeppettoLLMOption,
) (*GeppettoLLM, error) {
    // ... existing code ...
    llm := &GeppettoLLM{
        stepSettings:      stepSettings,
        embeddingProvider: embeddingProvider,
    }

    // Apply options
    for _, opt := range opts {
        if err := opt(llm); err != nil {
            return nil, err
        }
    }

    return llm, nil
}

```

### 2. Modify Internal Step Creation to Inject Publisher [-]

-   [-] Update the internal `createChatStep` helper function to accept the `publisher` and `topicID`.
-   [-] Inside `createChatStep`, if `publisher` and `topicID` are provided, call `AddPublishedTopic` on the created chat step instance.
-   [-] Update the `Generate` method to call this modified `createChatStep` using the stored `publisher` and `topicID`. The `Generate` signature itself remains unchanged.

```go
// Modified helper function
func createChatStep(
    stepSettings *settings.StepSettings,
    publisher message.Publisher,
    topic string,
) (steps.Step[conversation.Conversation, *conversation.Message], error) {
    factory := &ai.StandardStepFactory{
        Settings: stepSettings,
    }
    step, err := factory.NewStep()
    // ... error handling ...

    // Add publisher if provided
    if publisher != nil && topic != "" {
        // Use type assertion to get AddPublishedTopic method
        type publisherAdder interface {
            AddPublishedTopic(publisher message.Publisher, topic string) error
        }
        paStep, ok := step.(publisherAdder)
        if ok {
            err = paStep.AddPublishedTopic(publisher, topic)
            // ... error handling ...
        } else {
             // error: step doesn't support adding publisher
        }
    }
    return step, nil
}

// Generate method calls the modified helper
func (g *GeppettoLLM) Generate(
    ctx context.Context,
    messages []*conversation.Message,
) (*conversation.Message, error) {
    // Pass stored publisher/topic to helper
    chatStep, err := createChatStep(g.stepSettings, g.publisher, g.topicID)
    // ... rest of Generate implementation ...
}
```

### 3. Update the Simple Agent Example in `main.go` [-]

-   [-] Modify `main.go` to:
    -   Create an `events.EventRouter` and start it in a goroutine.
    -   Generate a unique `topicID` for the LLM call.
    -   Create the `GeppettoLLM` using `llm.WithPublisherAndTopic`, passing the router's publisher and the `topicID`.
    -   Register `events.StepPrinterFunc` as a handler for the `topicID` on the router.
    -   Call `geppettoLLM.Generate` (no `topicID` argument needed here).
    -   Remove manual `fmt.Println` for the final response.
    -   Ensure proper shutdown of the router using context cancellation and `errgroup.Wait()`.

```go
func main() {
    // ... existing setup ...

    // 1. Create and start an event router
    ctx, cancel := context.WithCancel(context.Background())
    defer cancel() // Ensure context is cancelled on exit

    router, err := events.NewEventRouter()
    if err != nil {
        log.Fatal().Err(err).Msg("Failed to create event router")
    }

    // Start the router in a background goroutine
    eg, routerCtx := errgroup.WithContext(ctx)
    eg.Go(func() error {
        log.Info().Msg("Starting event router")
        runErr := router.Run(routerCtx) // Use derived context for cancellation
        log.Info().Err(runErr).Msg("Event router stopped")
        // Don't return context.Canceled as a fatal error
        if runErr != nil && !errors.Is(runErr, context.Canceled) {
            return runErr // Return other errors
        }
        return nil
    })

    // 2. Generate a unique topic ID
    topicID := fmt.Sprintf("simple-agent-llm-%s", uuid.New().String())

    // 3. Create the LLM with publisher and topic configuration
    geppettoLLM, err := llm.NewGeppettoLLM(
        stepSettings,
        llm.WithPublisherAndTopic(router.Publisher, topicID), // Configure publisher
    )
    if err != nil {
        log.Fatal().Err(err).Msg("Failed to create Geppetto LLM")
    }

    // 4. Register the StepPrinterFunc handler
    handlerName := "stdout-printer-" + topicID
    log.Info().Str("handler", handlerName).Str("topic", topicID).Msg("Registering stdout handler")
    err = router.AddHandler(
        handlerName,                   // Unique handler name
        topicID,                       // Topic to subscribe to
        events.StepPrinterFunc("LLM", os.Stdout), // Use StepPrinterFunc directly
    )
    if err != nil {
        log.Fatal().Err(err).Msg("Failed to register stdout handler")
    }

    // Prepare prompt
    prompt := "What is the size of the planet Earth? Provide standard metrics like diameter, circumference, mass."
    messages := []*conversation.Message{
        conversation.NewChatMessage(conversation.RoleUser, prompt),
    }

    // 5. Call the LLM (Generate doesn't need topicID anymore)
    log.Info().Msg("Calling LLM Generate")
    _, err = geppettoLLM.Generate(ctx, messages) // No topicID argument
    if err != nil {
        log.Error().Err(err).Msg("LLM Generate failed")
        // Consider cancelling context and exiting if LLM fails critically
        // cancel()
        // os.Exit(1)
    } else {
        log.Info().Msg("LLM Generate completed")
    }


    // 6. Signal router to stop and wait for shutdown
    log.Info().Msg("Signalling router to stop")
    cancel() // Cancel the main context, which stops the router via routerCtx

    log.Info().Msg("Waiting for event router to shut down")
    if err := eg.Wait(); err != nil {
        // Log error from router.Run (if it wasn't context.Canceled)
        log.Error().Err(err).Msg("Event router shutdown failed")
    } else {
        log.Info().Msg("Event router shut down successfully")
    }


    // No need to print response manually anymore
    // fmt.Println("\nLLM Response:")
    // fmt.Println(responseMsg.Content.String())

    log.Info().Msg("Simple agent finished.")
    // ... rest of main (if any) ...
}
```

## Next Steps

After implementing these changes:

1. Test the streaming output with different LLM providers.
2. Add support for tool call events to visualize agent reasoning (already handled by `StepPrinterFunc`).
3. Enhance the stdout handler with better formatting if needed (could wrap `StepPrinterFunc` or create a custom one).
4. Create a more sophisticated TUI (terminal UI) using libraries like Bubble Tea, potentially leveraging `bobatea`.

## References

- Geppetto Steps, PubSub, and Watermill in Geppetto (04-events.md)
- Working with Steps for AI Inference and Tool Calling (07-steps.md)
- Pinocchio ChatRunner API Documentation (01-chat-runner-events.md)
- Current `GeppettoLLM` implementation (`goagent/llm/geppetto.go`)
- Simple Agent example (`cmd/simple/main.go`)
- `events.StepPrinterFunc` (`geppetto/pkg/events/step-printer-func.go`) 