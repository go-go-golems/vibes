package types

import "time"

// Document represents a text document to be processed
type Document struct {
	ID       string
	FilePath string
	Content  string
}

// RDFTriple represents an extracted fact in RDF format
type RDFTriple struct {
	Actor           string   `json:"actor"`
	Action          string   `json:"action"`
	Target          string   `json:"target,omitempty"`
	ExplicitTopic   string   `json:"explicit_topic,omitempty"`
	ImplicitTopic   string   `json:"implicit_topic,omitempty"`
	Tags            []string `json:"tags,omitempty"`
	Timestamp       *string  `json:"timestamp,omitempty"`
	Location        *string  `json:"location,omitempty"`
	ActorLikelyType *string  `json:"actor_likely_type,omitempty"`
}

// ExtractionResult contains the results of extracting facts from a document
type ExtractionResult struct {
	DocumentID string
	Triples    []RDFTriple
	CostUSD    float64
	TokensIn   int
	TokensOut  int
	ProcessedAt time.Time
}

// ExtractionResponse is the JSON structure returned by the LLM
type ExtractionResponse struct {
	Triples []RDFTriple `json:"triples"`
}
