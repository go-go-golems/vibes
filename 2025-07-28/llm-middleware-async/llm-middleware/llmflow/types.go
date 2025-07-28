package llmflow

import "context"

// Message is a single role/content pair for the LLM.
type Message struct {
	Role    string `json:"role"`
	Content string `json:"content"`
}

// Turn holds everything for one inference step.
// - Messages: the prompt+history to send.
// - Context: arbitrary pre-render data (artefacts, state).
// - Output: parsed results or metadata post-inference.
type Turn struct {
	Index    int                    `json:"index"`
	Messages []Message              `json:"messages"`
	Context  map[string]interface{} `json:"context"`
	Output   map[string]interface{} `json:"output"`
}

// InputHandler actually *runs* a turn. Most of the work happens
// in middleware; the terminal handler merely calls the LLM client.
type InputHandler func(ctx context.Context, turn *Turn) error

// InputMiddleware wraps one handler around another. It
// must invoke next(ctx, turn) to continue the chain.
type InputMiddleware func(next InputHandler) InputHandler

// LLMClient interface for the actual LLM API calls
type LLMClient interface {
	Infer(ctx context.Context, msgs []Message) (raw string, err error)
}

// Compose builds a single handler from N middlewares + a terminal.
// Order is left-to-right: Compose(A,B,C)(term) ⇒ A⇒B⇒C⇒term.
func Compose(mws ...InputMiddleware) func(term InputHandler) InputHandler {
	return func(term InputHandler) InputHandler {
		// Build the chain from right to left
		handler := term
		for i := len(mws) - 1; i >= 0; i-- {
			handler = mws[i](handler)
		}
		return handler
	}
}

