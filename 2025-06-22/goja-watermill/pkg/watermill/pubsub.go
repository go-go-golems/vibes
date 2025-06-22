package watermill

import (
	"encoding/json"
	"fmt"
	"time"

	"github.com/ThreeDotsLabs/watermill/message"
	"github.com/dop251/goja"
	"github.com/google/uuid"
)

// subscribe registers a JavaScript handler for a topic
func (p *PubSubInstance) subscribe(call goja.FunctionCall) goja.Value {
	if len(call.Arguments) < 2 {
		panic(p.module.vm.NewTypeError("subscribe requires 2 arguments (topic, handler)"))
	}
	
	topic := call.Arguments[0].String()
	jsFunc, ok := goja.AssertFunction(call.Arguments[1])
	if !ok {
		panic(p.module.vm.NewTypeError("second argument must be a function"))
	}
	
	handlerID := fmt.Sprintf("%s_%s_%d", p.id, topic, time.Now().UnixNano())
	
	// Create the Go handler function that will call the JS function
	handlerFunc := func(msg *message.Message) error {
		p.module.logger.Debug().
			Str("handler_id", handlerID).
			Str("topic", topic).
			Str("message_uuid", msg.UUID).
			Msg("Processing message in JS handler")
		
		// Convert message to JS object
		jsMsg := &JSMessage{
			UUID:     msg.UUID,
			Payload:  string(msg.Payload),
			Metadata: msg.Metadata,
			rawMsg:   msg,
			module:   p.module,
		}
		
		// Execute JS function in the JS thread
		var jsErr error
		p.module.executeInJS(func() {
			defer func() {
				if r := recover(); r != nil {
					jsErr = fmt.Errorf("JS handler panic: %v", r)
					p.module.logger.Error().
						Str("handler_id", handlerID).
						Interface("panic", r).
						Msg("Panic in JS handler")
				}
			}()
			
			// Call the JS function
			jsResult, err := jsFunc(goja.Undefined(), jsMsg.ToJSObject(p.module.vm))
			if err != nil {
				jsErr = fmt.Errorf("JS handler error: %w", err)
				return
			}
			
			// Check if the result indicates an error
			if !goja.IsUndefined(jsResult) && !goja.IsNull(jsResult) {
				if errStr := jsResult.String(); errStr != "" && errStr != "undefined" {
					jsErr = fmt.Errorf("JS handler returned error: %s", errStr)
				}
			}
		})
		
		if jsErr != nil {
			p.module.logger.Error().
				Err(jsErr).
				Str("handler_id", handlerID).
				Str("topic", topic).
				Msg("JS handler failed")
			return jsErr
		}
		
		p.module.logger.Debug().
			Str("handler_id", handlerID).
			Str("topic", topic).
			Msg("JS handler completed successfully")
		
		return nil
	}
	
	// Apply middleware to the handler
	finalHandler := p.applyMiddleware(handlerFunc)
	
	// Add handler to router
	handler := p.module.router.AddNoPublisherHandler(
		handlerID,
		topic,
		p.subscriber,
		finalHandler,
	)
	
	// Store handler info
	handlerInfo := &HandlerInfo{
		id:      handlerID,
		topic:   topic,
		jsFunc:  jsFunc,
		handler: handler,
		stopped: false,
	}
	
	p.handlersMutex.Lock()
	p.handlers[handlerID] = handlerInfo
	p.handlersMutex.Unlock()
	
	// Start the router if not already started
	p.module.routerMutex.Lock()
	if !p.module.routerStarted {
		p.module.logger.Info().Msg("Auto-starting Watermill router")
		
		go func() {
			if err := p.module.router.Run(p.module.ctx); err != nil {
				p.module.logger.Error().Err(err).Msg("Router run error")
			}
		}()
		
		// Wait for router to be fully running
		go func() {
			select {
			case <-p.module.router.Running():
				p.module.logger.Info().Msg("Watermill router is now running (auto-started)")
				p.module.routerStarted = true
			case <-p.module.ctx.Done():
				p.module.logger.Error().Msg("Context cancelled while waiting for auto-started router")
			case <-time.After(5 * time.Second):
				p.module.logger.Error().Msg("Timeout waiting for auto-started router")
			}
		}()
	}
	p.module.routerMutex.Unlock()
	
	p.module.logger.Info().
		Str("handler_id", handlerID).
		Str("topic", topic).
		Str("pubsub_id", p.id).
		Msg("Subscribed JS handler to topic")
	
	// Return handler info object
	handlerObj := p.module.vm.NewObject()
	handlerObj.Set("id", handlerID)
	handlerObj.Set("topic", topic)
	handlerObj.Set("stop", func() goja.Value {
		return p.unsubscribeHandler(handlerID)
	})
	
	return handlerObj
}

// publish publishes a message to a topic
func (p *PubSubInstance) publish(call goja.FunctionCall) goja.Value {
	if len(call.Arguments) < 2 {
		panic(p.module.vm.NewTypeError("publish requires at least 2 arguments (topic, payload)"))
	}
	
	topic := call.Arguments[0].String()
	payload := call.Arguments[1].Export()
	
	// Wait for router to be ready
	if !p.module.waitForRouter() {
		panic(p.module.vm.NewGoError(fmt.Errorf("router not ready for publishing")))
	}
	
	var metadata map[string]string
	if len(call.Arguments) > 2 && !goja.IsUndefined(call.Arguments[2]) {
		if metaVal := call.Arguments[2].Export(); metaVal != nil {
			if metaMap, ok := metaVal.(map[string]interface{}); ok {
				metadata = make(map[string]string)
				for k, v := range metaMap {
					metadata[k] = fmt.Sprintf("%v", v)
				}
			}
		}
	}
	
	if metadata == nil {
		metadata = make(map[string]string)
	}
	
	// Convert payload to bytes
	var payloadBytes []byte
	var err error
	
	switch v := payload.(type) {
	case string:
		payloadBytes = []byte(v)
	case []byte:
		payloadBytes = v
	default:
		// JSON encode other types
		payloadBytes, err = json.Marshal(v)
		if err != nil {
			panic(p.module.vm.NewGoError(fmt.Errorf("failed to marshal payload: %w", err)))
		}
		metadata["content-type"] = "application/json"
	}
	
	// Add metadata
	metadata["published_by"] = "goja-watermill"
	metadata["published_at"] = time.Now().UTC().Format(time.RFC3339)
	metadata["pubsub_id"] = p.id
	
	// Create message
	msg := message.NewMessage(uuid.New().String(), payloadBytes)
	msg.Metadata = metadata
	
	p.module.logger.Debug().
		Str("topic", topic).
		Str("message_uuid", msg.UUID).
		Str("pubsub_id", p.id).
		Int("payload_size", len(payloadBytes)).
		Msg("Publishing message")
	
	// Publish message
	if err := p.publisher.Publish(topic, msg); err != nil {
		p.module.logger.Error().
			Err(err).
			Str("topic", topic).
			Str("message_uuid", msg.UUID).
			Msg("Failed to publish message")
		panic(p.module.vm.NewGoError(fmt.Errorf("failed to publish message: %w", err)))
	}
	
	p.module.logger.Info().
		Str("topic", topic).
		Str("message_uuid", msg.UUID).
		Str("pubsub_id", p.id).
		Msg("Message published successfully")
	
	// Return message info
	msgObj := p.module.vm.NewObject()
	msgObj.Set("uuid", msg.UUID)
	msgObj.Set("topic", topic)
	msgObj.Set("publishedAt", metadata["published_at"])
	
	return msgObj
}

// unsubscribe removes a handler by its ID
func (p *PubSubInstance) unsubscribe(call goja.FunctionCall) goja.Value {
	if len(call.Arguments) < 1 {
		panic(p.module.vm.NewTypeError("unsubscribe requires 1 argument (handlerInfo or handlerID)"))
	}
	
	var handlerID string
	
	// Check if argument is a handler object or string ID
	arg := call.Arguments[0]
	if obj := arg.ToObject(p.module.vm); obj != nil {
		if idVal := obj.Get("id"); idVal != nil {
			handlerID = idVal.String()
		}
	} else {
		handlerID = arg.String()
	}
	
	if handlerID == "" {
		panic(p.module.vm.NewTypeError("invalid handler ID"))
	}
	
	return p.unsubscribeHandler(handlerID)
}

// unsubscribeHandler removes a specific handler
func (p *PubSubInstance) unsubscribeHandler(handlerID string) goja.Value {
	p.handlersMutex.Lock()
	handlerInfo, exists := p.handlers[handlerID]
	if !exists {
		p.handlersMutex.Unlock()
		p.module.logger.Warn().
			Str("handler_id", handlerID).
			Msg("Handler not found for unsubscribe")
		return p.module.vm.ToValue(false)
	}
	
	if handlerInfo.stopped {
		p.handlersMutex.Unlock()
		p.module.logger.Warn().
			Str("handler_id", handlerID).
			Msg("Handler already stopped")
		return p.module.vm.ToValue(false)
	}
	
	handlerInfo.stopped = true
	delete(p.handlers, handlerID)
	p.handlersMutex.Unlock()
	
	// Stop the handler
	handlerInfo.handler.Stop()
	
	p.module.logger.Info().
		Str("handler_id", handlerID).
		Str("topic", handlerInfo.topic).
		Msg("Handler unsubscribed successfully")
	
	return p.module.vm.ToValue(true)
}

// useMiddleware adds middleware to this PubSub instance
func (p *PubSubInstance) useMiddleware(call goja.FunctionCall) goja.Value {
	if len(call.Arguments) < 1 {
		panic(p.module.vm.NewTypeError("useMiddleware requires 1 argument (middleware function)"))
	}
	
	jsFunc, ok := goja.AssertFunction(call.Arguments[0])
	if !ok {
		panic(p.module.vm.NewTypeError("argument must be a function"))
	}
	
	middlewareName := fmt.Sprintf("middleware_%d", time.Now().UnixNano())
	
	middleware := MiddlewareFunc{
		jsFunc: jsFunc,
		name:   middlewareName,
	}
	
	p.middleware = append(p.middleware, middleware)
	
	p.module.logger.Info().
		Str("middleware_name", middlewareName).
		Str("pubsub_id", p.id).
		Msg("Added middleware to PubSub instance")
	
	return goja.Undefined()
}

// applyMiddleware applies all middleware to a handler function
func (p *PubSubInstance) applyMiddleware(handler message.NoPublishHandlerFunc) message.NoPublishHandlerFunc {
	if len(p.middleware) == 0 {
		return handler
	}
	
	// Apply middleware in reverse order (last added = outermost)
	finalHandler := handler
	for i := len(p.middleware) - 1; i >= 0; i-- {
		middleware := p.middleware[i]
		currentHandler := finalHandler
		
		finalHandler = func(msg *message.Message) error {
			var middlewareErr error
			
			p.module.executeInJS(func() {
				defer func() {
					if r := recover(); r != nil {
						middlewareErr = fmt.Errorf("middleware panic: %v", r)
						p.module.logger.Error().
							Str("middleware_name", middleware.name).
							Interface("panic", r).
							Msg("Panic in middleware")
					}
				}()
				
				// Create JS message object
				jsMsg := &JSMessage{
					UUID:     msg.UUID,
					Payload:  string(msg.Payload),
					Metadata: msg.Metadata,
					rawMsg:   msg,
					module:   p.module,
				}
				
				// Create next function
				nextCalled := false
				nextFunc := func(call goja.FunctionCall) goja.Value {
					if nextCalled {
						panic(p.module.vm.NewGoError(fmt.Errorf("next() called multiple times")))
					}
					nextCalled = true
					
					// Call the next handler
					if err := currentHandler(msg); err != nil {
						panic(p.module.vm.NewGoError(err))
					}
					
					return goja.Undefined()
				}
				
				// Call middleware
				result, err := middleware.jsFunc(goja.Undefined(), 
					jsMsg.ToJSObject(p.module.vm), 
					p.module.vm.ToValue(nextFunc))
				
				if err != nil {
					middlewareErr = fmt.Errorf("middleware error: %w", err)
					return
				}
				
				// Check if next was called
				if !nextCalled {
					middlewareErr = fmt.Errorf("middleware did not call next()")
					return
				}
				
				// Check result for errors
				if !goja.IsUndefined(result) && !goja.IsNull(result) {
					if errStr := result.String(); errStr != "" && errStr != "undefined" {
						middlewareErr = fmt.Errorf("middleware returned error: %s", errStr)
					}
				}
			})
			
			return middlewareErr
		}
	}
	
	return finalHandler
}

// stopAllHandlers stops all handlers for this PubSub instance
func (p *PubSubInstance) stopAllHandlers() {
	p.handlersMutex.Lock()
	defer p.handlersMutex.Unlock()
	
	for _, handlerInfo := range p.handlers {
		if !handlerInfo.stopped {
			handlerInfo.stopped = true
			handlerInfo.handler.Stop()
		}
	}
	
	// Clear handlers map
	p.handlers = make(map[string]*HandlerInfo)
	
	p.module.logger.Info().
		Str("pubsub_id", p.id).
		Msg("Stopped all handlers for PubSub instance")
}

