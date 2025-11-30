package extractor

import (
	"github.com/fact-extraction/go-extractor/pkg/types"
	"github.com/go-go-golems/geppetto/pkg/turns"
)

const systemPrompt = `You are a fact extraction assistant. Your task is to extract structured facts from documents in RDF triple format.

For each fact, extract:
- actor: The person or entity performing the action
- action: The action or relationship
- target: The person or entity receiving the action (optional)
- explicit_topic: The main topic explicitly mentioned
- implicit_topic: The underlying topic or theme
- tags: Relevant tags (e.g., "legal", "financial", "travel")
- timestamp: When the event occurred (if mentioned)
- location: Where the event occurred (if mentioned)
- actor_likely_type: Type of actor (e.g., "person", "organization")

Return ONLY a JSON object with this structure:
{
  "triples": [
    {
      "actor": "...",
      "action": "...",
      "target": "...",
      "explicit_topic": "...",
      "implicit_topic": "...",
      "tags": ["...", "..."],
      "timestamp": "...",
      "location": "...",
      "actor_likely_type": "..."
    }
  ]
}

Extract as many relevant facts as possible from the document. Focus on relationships between people, actions taken, and significant events.`

// PromptBuilder builds prompts for fact extraction
type PromptBuilder struct {
	systemPrompt string
}

// NewPromptBuilder creates a new prompt builder
func NewPromptBuilder() *PromptBuilder {
	return &PromptBuilder{
		systemPrompt: systemPrompt,
	}
}

// BuildTurn creates a Turn from a document
func (pb *PromptBuilder) BuildTurn(doc types.Document) *turns.Turn {
	turn := &turns.Turn{
		Data: map[string]any{
			"document_id": doc.ID,
		},
	}

	// Add system prompt
	turns.AppendBlock(turn, turns.NewSystemTextBlock(pb.systemPrompt))

	// Add user prompt with document content
	userPrompt := "Extract facts from the following document:\n\n" + doc.Content
	turns.AppendBlock(turn, turns.NewUserTextBlock(userPrompt))

	return turn
}
