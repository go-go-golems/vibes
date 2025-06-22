package watermill

import (
	"encoding/json"
	"fmt"

	"github.com/dop251/goja"
)

// Ack acknowledges the message
func (jsMsg *JSMessage) Ack() goja.Value {
	jsMsg.rawMsg.Ack()
	jsMsg.module.logger.Debug().
		Str("message_uuid", jsMsg.UUID).
		Msg("Message acknowledged")
	return goja.Undefined()
}

// Nack negatively acknowledges the message
func (jsMsg *JSMessage) Nack() goja.Value {
	jsMsg.rawMsg.Nack()
	jsMsg.module.logger.Debug().
		Str("message_uuid", jsMsg.UUID).
		Msg("Message nacked")
	return goja.Undefined()
}

// GetPayloadAsJSON parses the payload as JSON
func (jsMsg *JSMessage) GetPayloadAsJSON() goja.Value {
	var data interface{}
	if err := json.Unmarshal([]byte(jsMsg.Payload), &data); err != nil {
		panic(jsMsg.module.vm.NewGoError(fmt.Errorf("failed to parse payload as JSON: %w", err)))
	}
	return jsMsg.module.vm.ToValue(data)
}

// SetMetadata sets a metadata value
func (jsMsg *JSMessage) SetMetadata(key, value string) {
	if jsMsg.rawMsg.Metadata == nil {
		jsMsg.rawMsg.Metadata = make(map[string]string)
	}
	jsMsg.rawMsg.Metadata[key] = value
	jsMsg.Metadata[key] = value
}

// GetMetadata gets a metadata value
func (jsMsg *JSMessage) GetMetadata(key string) string {
	if jsMsg.Metadata == nil {
		return ""
	}
	return jsMsg.Metadata[key]
}

// ToJSObject converts the JSMessage to a JavaScript object with methods
func (jsMsg *JSMessage) ToJSObject(vm *goja.Runtime) goja.Value {
	obj := vm.NewObject()
	
	// Set properties
	obj.Set("uuid", jsMsg.UUID)
	obj.Set("payload", jsMsg.Payload)
	obj.Set("metadata", jsMsg.Metadata)
	
	// Set methods
	obj.Set("ack", func() goja.Value { return jsMsg.Ack() })
	obj.Set("nack", func() goja.Value { return jsMsg.Nack() })
	obj.Set("getPayloadAsJSON", func() goja.Value { return jsMsg.GetPayloadAsJSON() })
	obj.Set("setMetadata", func(call goja.FunctionCall) goja.Value {
		if len(call.Arguments) < 2 {
			panic(vm.NewTypeError("setMetadata requires 2 arguments (key, value)"))
		}
		key := call.Arguments[0].String()
		value := call.Arguments[1].String()
		jsMsg.SetMetadata(key, value)
		return goja.Undefined()
	})
	obj.Set("getMetadata", func(call goja.FunctionCall) goja.Value {
		if len(call.Arguments) < 1 {
			panic(vm.NewTypeError("getMetadata requires 1 argument (key)"))
		}
		key := call.Arguments[0].String()
		return vm.ToValue(jsMsg.GetMetadata(key))
	})
	
	return obj
}

