package pipeline

import (
	"context"
	"errors"
	"fmt"
	"log"
	"sync"
	"time"

	"github.com/ThreeDotsLabs/watermill/message"
)

// Stage represents a pipeline processing stage
type Stage func(ctx context.Context, data interface{}) (interface{}, error)

// Pipeline for processing data through a series of stages
type Pipeline struct {
	name   string
	stages []Stage
	mutex  sync.RWMutex
}

// Result of a pipeline execution
type Result struct {
	Data      interface{}
	Error     error
	Duration  time.Duration
	Completed bool
}

// NewPipeline creates a new pipeline with the given name
func NewPipeline(name string) *Pipeline {
	return &Pipeline{
		name:   name,
		stages: []Stage{},
	}
}

// AddStage adds a stage to the pipeline
func (p *Pipeline) AddStage(stage Stage) *Pipeline {
	p.mutex.Lock()
	defer p.mutex.Unlock()
	p.stages = append(p.stages, stage)
	return p
}

// Process processes the data through all stages
func (p *Pipeline) Process(ctx context.Context, data interface{}) Result {
	start := time.Now()
	result := Result{
		Data:      data,
		Completed: false,
	}

	p.mutex.RLock()
	stages := p.stages
	p.mutex.RUnlock()

	for i, stage := range stages {
		select {
		case <-ctx.Done():
			result.Error = ctx.Err()
			result.Duration = time.Since(start)
			return result
		default:
			stageData, err := stage(ctx, result.Data)
			if err != nil {
				result.Error = fmt.Errorf("error at stage %d: %w", i, err)
				result.Duration = time.Since(start)
				return result
			}
			result.Data = stageData
		}
	}

	result.Duration = time.Since(start)
	result.Completed = true
	return result
}

// BatchProcessor processes batches of messages using pipelines
type BatchProcessor struct {
	pipeline       *Pipeline
	batchSize      int
	maxConcurrency int
}

// NewBatchProcessor creates a new batch processor
func NewBatchProcessor(pipeline *Pipeline, batchSize, maxConcurrency int) *BatchProcessor {
	return &BatchProcessor{
		pipeline:       pipeline,
		batchSize:      batchSize,
		maxConcurrency: maxConcurrency,
	}
}

// ProcessBatch processes a batch of messages
func (b *BatchProcessor) ProcessBatch(
	ctx context.Context,
	messages []*message.Message,
	messageToData func(msg *message.Message) (interface{}, error),
	resultHandler func(msg *message.Message, result Result) error,
) error {
	if len(messages) == 0 {
		return nil
	}

	// Process in batches
	for i := 0; i < len(messages); i += b.batchSize {
		end := i + b.batchSize
		if end > len(messages) {
			end = len(messages)
		}
		batch := messages[i:end]

		errs := b.processBatchConcurrently(ctx, batch, messageToData, resultHandler)
		if len(errs) > 0 {
			return errors.Join(errs...)
		}
	}

	return nil
}

// processBatchConcurrently processes a batch of messages concurrently
func (b *BatchProcessor) processBatchConcurrently(
	ctx context.Context,
	batch []*message.Message,
	messageToData func(msg *message.Message) (interface{}, error),
	resultHandler func(msg *message.Message, result Result) error,
) []error {
	sem := make(chan struct{}, b.maxConcurrency)
	var wg sync.WaitGroup
	errChan := make(chan error, len(batch))

	for _, msg := range batch {
		wg.Add(1)
		sem <- struct{}{}

		go func(msg *message.Message) {
			defer func() {
				<-sem
				wg.Done()
			}()

			// Convert message to data
			data, err := messageToData(msg)
			if err != nil {
				errChan <- fmt.Errorf("error converting message to data: %w", err)
				return
			}

			// Process the data through the pipeline
			result := b.pipeline.Process(ctx, data)

			// Handle the result
			if err := resultHandler(msg, result); err != nil {
				errChan <- fmt.Errorf("error handling result: %w", err)
			}
		}(msg)
	}

	// Wait for all goroutines to finish
	wg.Wait()
	close(errChan)

	// Collect errors
	var errs []error
	for err := range errChan {
		errs = append(errs, err)
		log.Printf("Error processing message in batch: %v", err)
	}

	return errs
}

// PipelineBuilder helps build complex pipelines
type PipelineBuilder struct {
	name   string
	stages []Stage
}

// NewPipelineBuilder creates a new pipeline builder
func NewPipelineBuilder(name string) *PipelineBuilder {
	return &PipelineBuilder{
		name:   name,
		stages: []Stage{},
	}
}

// AddStage adds a stage to the pipeline
func (b *PipelineBuilder) AddStage(stage Stage) *PipelineBuilder {
	b.stages = append(b.stages, stage)
	return b
}

// AddConditionalStage adds a stage that only executes if the condition is met
func (b *PipelineBuilder) AddConditionalStage(
	condition func(data interface{}) bool,
	stage Stage,
) *PipelineBuilder {
	conditionalStage := func(ctx context.Context, data interface{}) (interface{}, error) {
		if condition(data) {
			return stage(ctx, data)
		}
		return data, nil
	}
	b.stages = append(b.stages, conditionalStage)
	return b
}

// AddErrorHandler adds an error handling stage
func (b *PipelineBuilder) AddErrorHandler(handler func(err error) error) *PipelineBuilder {
	errorHandlerStage := func(ctx context.Context, data interface{}) (interface{}, error) {
		// Create a child context that we can cancel
		childCtx, cancel := context.WithCancel(ctx)
		defer cancel()

		// Create a channel to receive the result
		resultCh := make(chan struct {
			data interface{}
			err  error
		}, 1)

		// Run the next stage in a goroutine
		go func() {
			nextStage := b.stages[len(b.stages)-1]
			result, err := nextStage(childCtx, data)
			resultCh <- struct {
				data interface{}
				err  error
			}{result, err}
		}()

		// Wait for the result or context cancellation
		select {
		case <-ctx.Done():
			return nil, ctx.Err()
		case result := <-resultCh:
			if result.err != nil {
				return result.data, handler(result.err)
			}
			return result.data, nil
		}
	}
	b.stages = append(b.stages, errorHandlerStage)
	return b
}

// Build creates a new pipeline with the configured stages
func (b *PipelineBuilder) Build() *Pipeline {
	pipeline := NewPipeline(b.name)
	for _, stage := range b.stages {
		pipeline.AddStage(stage)
	}
	return pipeline
}