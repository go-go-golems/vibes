package main

import (
	"context"
	"database/sql"
	"encoding/json"
	"fmt"
	"log"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"time"

	_ "github.com/mattn/go-sqlite3"
	"github.com/openai/openai-go"
	"github.com/openai/openai-go/option"
)

// RDFTriple represents a single relationship extracted from a document
type RDFTriple struct {
	Timestamp      string   `json:"timestamp,omitempty"`
	Actor          string   `json:"actor"`
	Action         string   `json:"action"`
	Target         string   `json:"target"`
	Location       string   `json:"location,omitempty"`
	ActorLikelyType string  `json:"actor_likely_type,omitempty"`
	Tags           []string `json:"tags"`
	ExplicitTopic  string   `json:"explicit_topic"`
	ImplicitTopic  string   `json:"implicit_topic"`
}

// DocumentAnalysis represents the complete analysis of a document
type DocumentAnalysis struct {
	DocID              string      `json:"doc_id"`
	OneSentenceSummary string      `json:"one_sentence_summary"`
	ParagraphSummary   string      `json:"paragraph_summary"`
	DateRangeEarliest  string      `json:"date_range_earliest,omitempty"`
	DateRangeLatest    string      `json:"date_range_latest,omitempty"`
	Category           string      `json:"category"`
	ContentTags        []string    `json:"content_tags"`
	RDFTriples         []RDFTriple `json:"rdf_triples"`
}

// AnalysisResult contains the full result of analyzing a document
type AnalysisResult struct {
	DocID      string
	FilePath   string
	FullText   string
	Analysis   DocumentAnalysis
	InputTokens  int
	OutputTokens int
	CostUSD    float64
	Error      string
}

const analysisPrompt = `You are analyzing a document from a legal/investigative document collection. The document ID is "%s".

IMPORTANT: You have ALL the information you need in the document text below. Do NOT attempt to read files, explore directories, or gather additional context. Analyze ONLY the text provided.

**CRITICAL IDENTIFICATION RULES:**
This document may contain communications involving Jeffrey Epstein. He may appear under these identifiers:
- Email: jeeitunes@gmail.com
- Email: e:jeeitunes@gmail.com
- Name: jee
- Name: Jeffrey Epstein
- Name: Jeffrey
- Name: Epstein

When you see ANY of these identifiers as a sender, participant, or actor, you MUST use "Jeffrey Epstein" as the actor name in your RDF triples. DO NOT use "jee", "unknown person", or any other placeholder.

Here is the document text:
` + "```" + `
%s
` + "```" + `

Your task is to analyze this document and extract structured information. Focus on:

1. **Main actors/participants** - People, organizations, entities mentioned or involved
2. **Key events and actions** - What happened, when, between whom
3. **Temporal information** - Dates, times, sequences of events
4. **Document type and content** - What kind of document is this?
5. **Key themes and topics** - What is this document about?

Return ONLY a valid JSON object with the following structure:

` + "```json" + `
{
  "one_sentence_summary": "A brief one-sentence summary including main actors, e.g., 'An email conversation between John Doe and Jane Smith regarding budget approval'",
  "paragraph_summary": "A detailed paragraph (3-5 sentences) explaining the document's content, context, significance, and key points. Include who is involved, what happened, why it matters, and any important outcomes or implications.",
  "date_range_earliest": "YYYY-MM-DD or YYYY-MM-DDTHH:MM format if dates are visible in the document, otherwise null",
  "date_range_latest": "YYYY-MM-DD or YYYY-MM-DDTHH:MM format if dates are visible in the document, otherwise null",
  "category": "One of: court_filing, email, letter, memorandum, report, transcript, financial_document, media_article, book_excerpt, photo_caption, mixed_document, public record, other",
  "content_tags": ["array", "of", "relevant", "document-level", "tags"],
  "rdf_triples": [
    {
      "timestamp": "YYYY-MM-DD or YYYY-MM-DDTHH:MM if available, otherwise omit this field",
      "actor": "PERSON NAME ONLY - Use 'Jeffrey Epstein' when you see jeeitunes@gmail.com or 'jee'",
      "action": "the action verb (e.g., 'sent email to', 'met with', 'testified before', 'paid', 'attended')",
      "target": "PERSON NAME ONLY - not organizations, movies, places (e.g., 'Donald Trump', not 'Donald Trump at party')",
      "location": "physical location if mentioned (e.g., 'Mar-a-Lago', 'New York City'), otherwise omit this field",
      "actor_likely_type": "OPTIONAL - only include if actor is unknown/unnamed/redacted AND there is sufficient evidence to infer their likely type",
      "tags": ["tags", "for", "this", "triple"],
      "explicit_topic": "short phrase describing the main theme directly evidenced",
      "implicit_topic": "short phrase describing what the interaction likely relates to"
    }
  ]
}
` + "```" + `

Guidelines for RDF triples:
- Create a sequential array capturing the key relationships and events in the document
- Include timestamps when dates/times are mentioned in the document
- **CRITICAL - Actor field**: Actor must ALWAYS be a PERSON NAME ONLY
- Use consistent naming (e.g., always "Jeffrey Epstein" not "Epstein" or "Jeffrey" or "jee")
- Actions should be descriptive verb phrases (e.g., "met with", "sent email to", "testified before")
- Focus on person-to-person AND person-to-entity relationships and interactions
- Order triples chronologically when timestamps are available, otherwise by document order

Return ONLY the JSON object, no additional text or explanation.`

func initDatabase(dbPath string) (*sql.DB, error) {
	db, err := sql.Open("sqlite3", dbPath)
	if err != nil {
		return nil, err
	}

	// Create documents table
	_, err = db.Exec(`
		CREATE TABLE IF NOT EXISTS documents (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			doc_id TEXT UNIQUE NOT NULL,
			file_path TEXT NOT NULL,
			one_sentence_summary TEXT NOT NULL,
			paragraph_summary TEXT NOT NULL,
			date_range_earliest TEXT,
			date_range_latest TEXT,
			category TEXT NOT NULL,
			content_tags TEXT NOT NULL,
			full_text TEXT,
			analysis_timestamp TEXT NOT NULL,
			input_tokens INTEGER,
			output_tokens INTEGER,
			cost_usd REAL,
			error TEXT,
			created_at DATETIME DEFAULT CURRENT_TIMESTAMP
		);
	`)
	if err != nil {
		return nil, err
	}

	// Create RDF triples table
	_, err = db.Exec(`
		CREATE TABLE IF NOT EXISTS rdf_triples (
			id INTEGER PRIMARY KEY AUTOINCREMENT,
			doc_id TEXT NOT NULL,
			timestamp TEXT,
			actor TEXT NOT NULL,
			action TEXT NOT NULL,
			target TEXT NOT NULL,
			location TEXT,
			actor_likely_type TEXT,
			triple_tags TEXT,
			explicit_topic TEXT,
			implicit_topic TEXT,
			sequence_order INTEGER NOT NULL,
			created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
			FOREIGN KEY (doc_id) REFERENCES documents(doc_id) ON DELETE CASCADE
		);
	`)
	if err != nil {
		return nil, err
	}

	// Create indexes
	_, err = db.Exec(`
		CREATE INDEX IF NOT EXISTS idx_documents_doc_id ON documents(doc_id);
		CREATE INDEX IF NOT EXISTS idx_documents_category ON documents(category);
		CREATE INDEX IF NOT EXISTS idx_rdf_triples_doc_id ON rdf_triples(doc_id);
		CREATE INDEX IF NOT EXISTS idx_rdf_triples_actor ON rdf_triples(actor);
		CREATE INDEX IF NOT EXISTS idx_rdf_triples_timestamp ON rdf_triples(timestamp);
	`)
	if err != nil {
		return nil, err
	}

	log.Printf("✓ Database initialized at: %s\n", dbPath)
	return db, nil
}

func analyzeDocument(ctx context.Context, client *openai.Client, docID, filePath, content string) AnalysisResult {
	prompt := fmt.Sprintf(analysisPrompt, docID, content)

	log.Printf("Analyzing %s...\n", docID)

	// Use gpt-4.1-mini as specified in the environment
	completion, err := client.Chat.Completions.New(ctx, openai.ChatCompletionNewParams{
		Model: openai.F("gpt-4.1-mini"),
		Messages: openai.F([]openai.ChatCompletionMessageParamUnion{
			openai.UserMessage(prompt),
		}),
		MaxTokens: openai.Int(16000),
	})

	if err != nil {
		log.Printf("  ❌ Error analyzing %s: %v\n", docID, err)
		return AnalysisResult{
			DocID:    docID,
			FilePath: filePath,
			FullText: content,
			Analysis: DocumentAnalysis{
				DocID:              docID,
				OneSentenceSummary: "Error during analysis",
				ParagraphSummary:   "An error occurred during document analysis.",
				Category:           "other",
				ContentTags:        []string{},
				RDFTriples:         []RDFTriple{},
			},
			Error: err.Error(),
		}
	}

	// Extract JSON from response
	responseText := completion.Choices[0].Message.Content
	jsonText := extractJSON(responseText)

	var analysis DocumentAnalysis
	if err := json.Unmarshal([]byte(jsonText), &analysis); err != nil {
		log.Printf("  ⚠️  JSON parse failed for %s: %v\n", docID, err)
		return AnalysisResult{
			DocID:    docID,
			FilePath: filePath,
			FullText: content,
			Analysis: DocumentAnalysis{
				DocID:              docID,
				OneSentenceSummary: "Parse error",
				ParagraphSummary:   "Failed to parse analysis result.",
				Category:           "other",
				ContentTags:        []string{},
				RDFTriples:         []RDFTriple{},
			},
			Error: fmt.Sprintf("JSON parse error: %v", err),
		}
	}

	analysis.DocID = docID

	// Calculate approximate cost (rough estimate)
	inputTokens := completion.Usage.PromptTokens
	outputTokens := completion.Usage.CompletionTokens
	costUSD := float64(inputTokens)*0.00015/1000 + float64(outputTokens)*0.0006/1000

	log.Printf("  ✓ Analyzed %s: %d triples extracted\n", docID, len(analysis.RDFTriples))

	return AnalysisResult{
		DocID:        docID,
		FilePath:     filePath,
		FullText:     content,
		Analysis:     analysis,
		InputTokens:  int(inputTokens),
		OutputTokens: int(outputTokens),
		CostUSD:      costUSD,
	}
}

func extractJSON(text string) string {
	// Try to extract JSON from markdown code blocks
	re := regexp.MustCompile("```(?:json)?\\s*([\\s\\S]*?)\\s*```")
	matches := re.FindStringSubmatch(text)
	if len(matches) > 1 {
		return matches[1]
	}
	return text
}

func saveResult(db *sql.DB, result AnalysisResult) error {
	// Marshal content_tags to JSON
	contentTagsJSON, err := json.Marshal(result.Analysis.ContentTags)
	if err != nil {
		return err
	}

	// Insert document
	_, err = db.Exec(`
		INSERT OR REPLACE INTO documents 
		(doc_id, file_path, one_sentence_summary, paragraph_summary, date_range_earliest, 
		 date_range_latest, category, content_tags, full_text, analysis_timestamp, 
		 input_tokens, output_tokens, cost_usd, error)
		VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
	`,
		result.DocID,
		result.FilePath,
		result.Analysis.OneSentenceSummary,
		result.Analysis.ParagraphSummary,
		nullString(result.Analysis.DateRangeEarliest),
		nullString(result.Analysis.DateRangeLatest),
		result.Analysis.Category,
		string(contentTagsJSON),
		result.FullText,
		time.Now().Format(time.RFC3339),
		result.InputTokens,
		result.OutputTokens,
		result.CostUSD,
		nullString(result.Error),
	)
	if err != nil {
		return err
	}

	// Insert RDF triples
	for i, triple := range result.Analysis.RDFTriples {
		tagsJSON, err := json.Marshal(triple.Tags)
		if err != nil {
			return err
		}

		_, err = db.Exec(`
			INSERT INTO rdf_triples 
			(doc_id, timestamp, actor, action, target, location, actor_likely_type, 
			 triple_tags, explicit_topic, implicit_topic, sequence_order)
			VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
		`,
			result.DocID,
			nullString(triple.Timestamp),
			triple.Actor,
			triple.Action,
			triple.Target,
			nullString(triple.Location),
			nullString(triple.ActorLikelyType),
			string(tagsJSON),
			triple.ExplicitTopic,
			triple.ImplicitTopic,
			i,
		)
		if err != nil {
			return err
		}
	}

	return nil
}

func nullString(s string) interface{} {
	if s == "" {
		return nil
	}
	return s
}

func main() {
	ctx := context.Background()

	// Initialize OpenAI client (uses OPENAI_API_KEY from environment)
	client := openai.NewClient(option.WithAPIKey(os.Getenv("OPENAI_API_KEY")))

	// Initialize database
	db, err := initDatabase("fact_extraction.db")
	if err != nil {
		log.Fatalf("Failed to initialize database: %v", err)
	}
	defer db.Close()

	// Read sample documents
	files, err := filepath.Glob("sample_data/*.txt")
	if err != nil {
		log.Fatalf("Failed to read sample data: %v", err)
	}

	log.Printf("Found %d documents to analyze\n\n", len(files))

	totalCost := 0.0
	totalTriples := 0

	for i, file := range files {
		content, err := os.ReadFile(file)
		if err != nil {
			log.Printf("Failed to read %s: %v\n", file, err)
			continue
		}

		docID := strings.TrimSuffix(filepath.Base(file), ".txt")

		result := analyzeDocument(ctx, client, docID, file, string(content))

		if err := saveResult(db, result); err != nil {
			log.Printf("Failed to save result for %s: %v\n", docID, err)
			continue
		}

		totalCost += result.CostUSD
		totalTriples += len(result.Analysis.RDFTriples)

		log.Printf("Progress: %d/%d documents analyzed\n\n", i+1, len(files))

		// Small delay to avoid rate limiting
		time.Sleep(500 * time.Millisecond)
	}

	log.Printf("\n=== Analysis Complete ===\n")
	log.Printf("Total documents: %d\n", len(files))
	log.Printf("Total triples extracted: %d\n", totalTriples)
	log.Printf("Total cost: $%.4f\n", totalCost)
}
