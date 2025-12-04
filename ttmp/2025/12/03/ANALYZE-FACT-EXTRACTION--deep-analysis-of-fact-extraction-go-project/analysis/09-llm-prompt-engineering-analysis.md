---
Title: LLM Prompt Engineering Analysis
Ticket: ANALYZE-FACT-EXTRACTION
Status: active
Topics:
    - analysis
    - go
    - fact-extraction
DocType: analysis
Intent: long-term
Owners: []
RelatedFiles:
    - Path: 2025/11/25/fact-extraction-go/extract_facts.py
      Note: Basic extraction prompt
    - Path: 2025/11/25/fact-extraction-go/extract_facts_enhanced.py
      Note: Enhanced extraction prompt with reasoning
    - Path: 2025/11/25/fact-extraction-go/extract_facts_full_enhanced.py
      Note: Prompt in Python implementation - SYSTEM_PROMPT constant
    - Path: 2025/11/25/fact-extraction-go/go-extractor/pkg/extractor/prompt.go
      Note: Prompt builder in Go extractor
    - Path: 2025/11/25/fact-extraction-go/main.go
      Note: Prompt in Go implementation - analysisPrompt constant
    - Path: 2025/11/25/fact-extraction-go/refined_deduplication.py
      Note: Deduplication prompts for entity merging
    - Path: 2025/11/25/fact-extraction-go/tag_clustering_simple.py
      Note: Tag clustering prompt
ExternalSources: []
Summary: 'Analysis of prompt engineering: prompt structure, Jeffrey Epstein variant handling, JSON extraction patterns, and prompt effectiveness'
LastUpdated: 2025-12-03T09:42:21.997486677-05:00
---



# LLM Prompt Engineering Analysis

## Research Objective

Analyze the prompt engineering techniques used in the fact extraction pipeline, including prompt structure, Jeffrey Epstein variant handling, JSON extraction patterns, and prompt effectiveness.

## Research Instructions

### Phase 1: Prompt Collection

**Files to analyze:**
- `vibes/2025/11/25/fact-extraction-go/main.go` (analysisPrompt)
- `vibes/2025/11/25/fact-extraction-go/extract_facts_full_enhanced.py` (SYSTEM_PROMPT)
- `vibes/2025/11/25/fact-extraction-go/go-extractor/pkg/extractor/prompt.go`

**Tasks:**
1. **Extract all prompts**:
   - Basic extraction prompt
   - Enhanced extraction prompt
   - Full enhanced extraction prompt
   - Go implementation prompts
   - Deduplication prompts
   - Tag clustering prompts

2. **Document prompt versions**:
   - Evolution over time
   - Differences between versions
   - What changed and why

### Phase 2: Prompt Structure Analysis

**Tasks:**
1. **Document prompt components**:
   - System instructions
   - Task description
   - Output format specification
   - Examples (if any)
   - Constraints and rules

2. **Analyze prompt patterns**:
   - How are instructions structured?
   - How is output format specified?
   - How are examples used?
   - How are constraints enforced?

### Phase 3: Special Handling Analysis

**Tasks:**
1. **Jeffrey Epstein variant handling**:
   - How are variants identified?
   - What instructions are given?
   - How effective is it?
   - Document examples

2. **JSON extraction patterns**:
   - How is JSON format specified?
   - How is JSON extracted from responses?
   - What error handling exists?
   - How reliable is extraction?

3. **Entity identification rules**:
   - How are entities identified?
   - What rules are given?
   - How are ambiguous cases handled?

### Phase 4: Prompt Effectiveness Analysis

**Tasks:**
1. **Evaluate prompt effectiveness**:
   - Does the prompt produce desired output?
   - What errors occur?
   - What edge cases fail?
   - How consistent are results?

2. **Compare prompt versions**:
   - Basic vs Enhanced vs Full
   - What improvements were made?
   - What trade-offs exist?

### Phase 5: Prompt Optimization

**Research:**
1. **Best practices**:
   - What are LLM prompt engineering best practices?
   - How do they apply here?
   - What improvements could be made?

2. **A/B testing opportunities**:
   - What variations could be tested?
   - What metrics would measure success?
   - What experiments would be valuable?

### Phase 6: Recommendations

**Deliverables:**
1. **Prompt Catalog**
2. **Prompt Structure Analysis**
3. **Effectiveness Evaluation**
4. **Optimization Recommendations**

## Key Questions to Answer

1. **How effective are the prompts?**
2. **What makes them effective?**
3. **What improvements are needed?**
4. **How do they compare to best practices?**

## Related Files

- `vibes/2025/11/25/fact-extraction-go/main.go`
- `vibes/2025/11/25/fact-extraction-go/extract_facts_full_enhanced.py`
- `vibes/2025/11/25/fact-extraction-go/go-extractor/pkg/extractor/prompt.go`

## Expected Timeline: 12-15 hours

---

## Analysis: LLM Prompt Engineering in Fact Extraction

### Introduction: The Art and Science of Prompt Design

Prompt engineering is the practice of designing inputs that guide large language models to produce desired outputs. In the fact extraction pipeline, prompts serve as the interface between human intent and LLM behavior, translating the complex task of extracting structured facts from unstructured documents into instructions that the model can follow reliably. The quality of prompt design directly impacts extraction accuracy, consistency, and the ability to handle edge cases like entity name variants, ambiguous relationships, and complex document structures.

The fact extraction project demonstrates an evolution in prompt engineering, starting with basic extraction instructions and progressing to sophisticated prompts that include reasoning chains, citations, entity descriptions, and specialized handling for domain-specific challenges like identifying Jeffrey Epstein across multiple aliases. Each prompt version reflects lessons learned from previous iterations, incorporating techniques like explicit format specification, constraint enforcement, and domain-specific rules that improve extraction quality.

### Prompt Evolution: From Basic to Enhanced

The fact extraction pipeline evolved through multiple prompt versions, each adding capabilities and addressing limitations discovered in practice. Understanding this evolution reveals the iterative process of prompt engineering and how specific challenges led to particular design decisions.

**Basic Extraction Prompt** (`main.go` lines 57-120):

The foundational prompt establishes the core extraction task and output format:

```57:120:vibes/2025/11/25/fact-extraction-go/main.go
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
```

This prompt demonstrates several key engineering techniques: explicit role definition ("You are analyzing..."), constraint enforcement ("DO NOT attempt to read files"), domain-specific rules (Jeffrey Epstein identification), structured output specification (JSON schema), and clear formatting instructions ("Return ONLY the JSON object").

**Enhanced Extraction Prompt** (`extract_facts_full_enhanced.py` lines 110-178):

The enhanced version adds provenance and description requirements:

```110:178:vibes/2025/11/25/fact-extraction-go/extract_facts_full_enhanced.py
    SYSTEM_PROMPT = """You are an expert fact extraction system. Extract structured facts (RDF triples) with complete metadata.

For EACH FACT, provide:
1. **Core Triple**: actor, action, target
2. **Reasoning**: Why you extracted this fact
3. **Citations**: Exact quotes supporting it
4. **Confidence**: Score 0.0-1.0
5. **Entity Descriptions**: Describe what/who the actor and target are
6. **Relation Description**: Describe what the action/relation means

Output JSON format:
```json
{
  "reasoning": "Overall analysis of the document...",
  "triples": [
    {
      "actor": "Entity name",
      "action": "Relationship/action",
      "target": "Entity name",
      "timestamp": "When (if mentioned)",
      "location": "Where (if mentioned)",
      "triple_tags": ["tag1", "tag2"],
      "explicit_topic": "Main topic",
      "implicit_topic": "Inferred topic",
      
      "reasoning": "Why this specific triple was extracted",
      "citations": [
        {
          "text": "Exact quote",
          "relevance": "Why this supports the fact"
        }
      ],
      "confidence": 0.95,
      
      "actor_description": "Description of who/what the actor is",
      "actor_type": "person|organization|location|event|concept|other",
      "target_description": "Description of who/what the target is",
      "target_type": "person|organization|location|event|concept|other",
      
      "relation_description": "Description of what this relation means",
      "relation_type": "action|state|attribute|membership|other"
    }
  ],
  
  "entity_descriptions": {
    "Entity Name": {
      "name": "Entity Name",
      "description": "Comprehensive description based on all mentions",
      "entity_type": "person|organization|location|event|concept|other"
    }
  },
  
  "relation_descriptions": {
    "relation name": {
      "relation": "relation name",
      "description": "What this relation means in context",
      "relation_type": "action|state|attribute|membership|other"
    }
  }
}
```

Guidelines:
- Provide detailed entity descriptions (help distinguish between entities with similar names)
- Provide clear relation descriptions (help identify synonymous relations)
- Be precise and factual
- Use exact quotes for citations
- Explain reasoning clearly
"""
```

The enhanced prompt adds requirements for reasoning chains, citations, confidence scores, and entity/relation descriptions. These additions enable provenance tracking and support downstream processes like entity deduplication, but they also increase prompt complexity and token usage.

**Go Implementation Prompt** (`go-extractor/pkg/extractor/prompt.go` lines 8-38):

The Go implementation uses a simplified prompt focused on core extraction:

```8:38:vibes/2025/11/25/fact-extraction-go/go-extractor/pkg/extractor/prompt.go
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
```

The Go prompt is more concise, omitting document-level metadata (summaries, categories) and focusing solely on triple extraction. This simplification reflects the Go implementation's goal of creating a focused extraction tool rather than a comprehensive document analysis system.

### Prompt Structure: Components and Patterns

Effective prompts combine multiple components that work together to guide the LLM's behavior. The fact extraction prompts demonstrate a consistent structure that balances clarity, specificity, and flexibility.

**Component 1: Role Definition**

Every prompt begins by establishing the LLM's role: "You are analyzing a document..." or "You are an expert fact extraction system...". This role definition sets context and expectations, helping the model understand its purpose and the domain it's operating in.

**Component 2: Task Description**

The task description explains what needs to be done: "Extract structured facts from documents in RDF triple format" or "analyze this document and extract structured information". This component provides high-level guidance about the overall objective.

**Component 3: Domain-Specific Rules**

Domain-specific rules address challenges particular to the use case. The Jeffrey Epstein identification rule is a prime example:

```61:70:vibes/2025/11/25/fact-extraction-go/main.go
**CRITICAL IDENTIFICATION RULES:**
This document may contain communications involving Jeffrey Epstein. He may appear under these identifiers:
- Email: jeeitunes@gmail.com
- Email: e:jeeitunes@gmail.com
- Name: jee
- Name: Jeffrey Epstein
- Name: Jeffrey
- Name: Epstein

When you see ANY of these identifiers as a sender, participant, or actor, you MUST use "Jeffrey Epstein" as the actor name in your RDF triples. DO NOT use "jee", "unknown person", or any other placeholder.
```

This rule addresses a real problem: documents contain emails and references where Jeffrey Epstein appears as "jee" or "jeeitunes@gmail.com", and without explicit instructions, the LLM might use these variants directly, creating inconsistent entity names. The rule uses strong language ("CRITICAL", "MUST", "DO NOT") to emphasize importance and uses examples to make the requirement concrete.

**Component 4: Output Format Specification**

The output format is specified using JSON schema examples embedded in the prompt. This approach leverages the LLM's ability to understand structured formats and provides a template that the model can follow:

```85:108:vibes/2025/11/25/fact-extraction-go/main.go
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
```

The format specification includes examples (e.g., "e.g., 'An email conversation between John Doe and Jane Smith'"), constraints (e.g., "PERSON NAME ONLY"), and format requirements (e.g., "YYYY-MM-DD or YYYY-MM-DDTHH:MM"). This combination of structure, examples, and constraints helps the LLM produce correctly formatted output.

**Component 5: Guidelines and Constraints**

Guidelines provide additional instructions that clarify edge cases and enforce consistency:

```111:120:vibes/2025/11/25/fact-extraction-go/main.go
Guidelines for RDF triples:
- Create a sequential array capturing the key relationships and events in the document
- Include timestamps when dates/times are mentioned in the document
- **CRITICAL - Actor field**: Actor must ALWAYS be a PERSON NAME ONLY
- Use consistent naming (e.g., always "Jeffrey Epstein" not "Epstein" or "Jeffrey" or "jee")
- Actions should be descriptive verb phrases (e.g., "met with", "sent email to", "testified before")
- Focus on person-to-person AND person-to-entity relationships and interactions
- Order triples chronologically when timestamps are available, otherwise by document order

Return ONLY the JSON object, no additional text or explanation.`
```

These guidelines address common failure modes: inconsistent naming, incorrect field types, missing timestamps, and extraneous output. The final instruction ("Return ONLY the JSON object") is particularly important because LLMs sometimes add explanatory text that breaks JSON parsing.

### Jeffrey Epstein Variant Handling: Domain-Specific Challenge

The Jeffrey Epstein identification rule demonstrates how domain-specific knowledge must be encoded in prompts to handle real-world complexity. Documents contain multiple ways of referring to the same person, and without explicit instructions, the LLM might treat these as different entities.

**The Problem**:

Legal documents and emails often use informal identifiers: email addresses, nicknames, abbreviations. An email from "jeeitunes@gmail.com" and a reference to "jee" both refer to Jeffrey Epstein, but without guidance, an LLM might extract these as separate entities, fragmenting the knowledge graph.

**The Solution**:

The prompt explicitly lists all known variants and mandates canonicalization:

```61:70:vibes/2025/11/25/fact-extraction-go/main.go
**CRITICAL IDENTIFICATION RULES:**
This document may contain communications involving Jeffrey Epstein. He may appear under these identifiers:
- Email: jeeitunes@gmail.com
- Email: e:jeeitunes@gmail.com
- Name: jee
- Name: Jeffrey Epstein
- Name: Jeffrey
- Name: Epstein

When you see ANY of these identifiers as a sender, participant, or actor, you MUST use "Jeffrey Epstein" as the actor name in your RDF triples. DO NOT use "jee", "unknown person", or any other placeholder.
```

**Why This Works**:

The rule uses multiple techniques: explicit enumeration (listing all variants), imperative language ("MUST use", "DO NOT use"), and negative examples (what not to do). The placement at the top of the prompt (after the role definition) ensures it's seen early, and the "CRITICAL" label signals importance. This approach leverages the LLM's pattern-matching capabilities while providing explicit guidance for edge cases.

**Limitations**:

This approach requires manual enumeration of variants—if new variants appear in documents, they won't be handled unless the prompt is updated. A more scalable approach might use entity resolution as a post-processing step, but the prompt-based approach ensures consistency during extraction.

### JSON Extraction Patterns: Handling LLM Output Variability

LLMs don't always produce clean JSON. They may wrap JSON in markdown code blocks, add explanatory text, or produce malformed JSON. The fact extraction pipeline handles these variations through robust parsing logic.

**JSON Extraction Logic** (`main.go` lines 267-275):

```267:275:vibes/2025/11/25/fact-extraction-go/main.go
func extractJSON(text string) string {
	// Try to extract JSON from markdown code blocks
	re := regexp.MustCompile("```(?:json)?\\s*([\\s\\S]*?)\\s*```")
	matches := re.FindStringSubmatch(text)
	if len(matches) > 1 {
		return matches[1]
	}
	return text
}
```

The extraction function uses a regex pattern that matches markdown code blocks (with or without a "json" language tag) and extracts the content. If no code block is found, it returns the original text, assuming it's raw JSON.

**Prompt Instructions for JSON**:

The prompts explicitly instruct the LLM to return JSON, but LLMs sometimes add markdown formatting anyway. The extraction logic handles this gracefully, but the prompt could be more explicit:

```85:85:vibes/2025/11/25/fact-extraction-go/main.go
Return ONLY a valid JSON object with the following structure:
```

A stronger instruction might be: "Return ONLY valid JSON, not wrapped in markdown code blocks, with no additional text before or after." However, the current approach balances prompt simplicity with robust parsing.

**Enhanced Extraction** (`extract_facts_enhanced.py` lines 194-199):

The enhanced version uses more sophisticated extraction:

```194:199:vibes/2025/11/25/fact-extraction-go/extract_facts_enhanced.py
        # Extract JSON
        if "```json" in content_response:
            json_text = content_response.split("```json")[1].split("```")[0].strip()
        elif "```" in content_response:
            json_text = content_response.split("```")[1].split("```")[0].strip()
        else:
            json_text = content_response
```

This approach tries multiple strategies: first looking for JSON-marked code blocks, then unmarked code blocks, then falling back to raw text. This multi-strategy approach handles the most common variations in LLM output.

**Using JSON Mode** (`extract_facts_full_enhanced.py` line 218):

The fully enhanced version uses OpenAI's JSON mode:

```218:218:vibes/2025/11/25/fact-extraction-go/extract_facts_full_enhanced.py
                response_format={"type": "json_object"}
```

JSON mode instructs the model to return valid JSON, reducing the need for extraction logic. However, JSON mode requires the prompt to explicitly request JSON object output and may have limitations with complex nested structures.

### Entity Identification Rules: Constraining Output

The prompts include explicit rules about entity identification to ensure consistent, correct extraction. These rules address common failure modes where LLMs might extract incorrect entity types or include extraneous information.

**Actor Field Constraint** (`main.go` lines 114-115):

```114:115:vibes/2025/11/25/fact-extraction-go/main.go
- **CRITICAL - Actor field**: Actor must ALWAYS be a PERSON NAME ONLY
- Use consistent naming (e.g., always "Jeffrey Epstein" not "Epstein" or "Jeffrey" or "jee")
```

This constraint prevents the LLM from including organizations, locations, or other entity types in the actor field. The "CRITICAL" label and "ALWAYS" language emphasize importance, and the example shows what consistent naming means.

**Target Field Constraint** (`main.go` line 100):

```100:100:vibes/2025/11/25/fact-extraction-go/main.go
      "target": "PERSON NAME ONLY - not organizations, movies, places (e.g., 'Donald Trump', not 'Donald Trump at party')",
```

The target constraint includes both a positive requirement ("PERSON NAME ONLY") and negative examples ("not organizations, movies, places"). The concrete example ("'Donald Trump', not 'Donald Trump at party'") shows how to handle cases where location or context might be included.

**Why These Constraints Matter**:

Without explicit constraints, LLMs might extract entities inconsistently: sometimes including locations ("Donald Trump at Mar-a-Lago"), sometimes including organizations ("Donald Trump's company"), sometimes using abbreviations ("D. Trump"). These variations make downstream processing (like entity resolution) difficult. The constraints enforce a canonical form that simplifies later processing.

### Prompt Engineering Techniques: What Makes These Prompts Effective

The fact extraction prompts employ several proven prompt engineering techniques that contribute to their effectiveness.

**Technique 1: Explicit Format Specification**

The prompts include detailed JSON schemas with examples, showing exactly what the output should look like. This leverages the LLM's ability to understand structured formats and follow templates.

**Technique 2: Constraint Enforcement**

Constraints are stated explicitly and emphatically ("CRITICAL", "MUST", "DO NOT", "ALWAYS"). This strong language helps ensure the LLM follows important rules, though it's not foolproof—LLMs can still violate constraints.

**Technique 3: Examples and Counter-Examples**

The prompts include both positive examples (what to do) and negative examples (what not to do). For example: "Use 'Jeffrey Epstein' when you see jeeitunes@gmail.com or 'jee'" (positive) and "DO NOT use 'jee', 'unknown person', or any other placeholder" (negative).

**Technique 4: Structured Information Hierarchy**

Information is organized hierarchically: role definition first, then task description, then domain rules, then format specification, then guidelines. This organization helps the LLM process information in a logical order.

**Technique 5: Repetition for Emphasis**

Important rules are repeated in multiple places. The Jeffrey Epstein identification rule appears in the "CRITICAL IDENTIFICATION RULES" section and again in the actor field specification. This repetition reinforces important constraints.

**Technique 6: Clear Separation of Concerns**

The prompts separate different types of information: document-level metadata (summaries, categories) from fact-level metadata (triples, tags). This separation helps the LLM understand what information belongs where.

### Tag Clustering Prompt: Different Use Case, Different Approach

The tag clustering prompt demonstrates how prompt engineering adapts to different tasks. Unlike extraction prompts that must handle variable document content, the clustering prompt works with a fixed set of tags.

**Tag Clustering Prompt** (`tag_clustering_simple.py` lines 60-80):

```60:80:vibes/2025/11/25/fact-extraction-go/tag_clustering_simple.py
        prompt = f"""You are analyzing tags from a legal document corpus about the Epstein case. 
Group these {len(tags)} tags into approximately {self.n_clusters} semantic clusters.

Tags: {tags_str}

For each cluster, provide:
1. A concise theme name (2-4 words)
2. The tags that belong to that cluster

Return your response as a JSON array of objects with this structure:
[
  {{
    "cluster_id": 0,
    "theme": "Legal Proceedings",
    "tags": ["deposition", "court filing", "testimony", ...]
  }},
  ...
]

Focus on creating meaningful, coherent clusters. Tags can appear in multiple clusters if relevant.
Return ONLY the JSON array, no other text."""
```

This prompt is simpler than extraction prompts because the input is structured (a list of tags) and the task is more focused (grouping rather than extraction). The prompt includes the tag count and target cluster count to guide the LLM's output, and explicitly allows tags in multiple clusters, recognizing that tags can have multiple semantic dimensions.

### Deduplication Prompts: Rich Context for Complex Decisions

The deduplication prompts demonstrate how prompts can incorporate rich context to enable complex reasoning. Unlike extraction prompts that work with single documents, deduplication prompts must reason about multiple entities or relations simultaneously.

**Entity Merge Prompt** (`refined_deduplication.py` lines 546-599):

The entity merge prompt builds a rich context for each candidate entity:

```546:599:vibes/2025/11/25/fact-extraction-go/refined_deduplication.py
    def _build_entity_merge_prompt(self, group: List[str], profiles: Dict[str, EntityProfile]) -> str:
        """Build rich prompt for entity merging"""
        prompt = """Analyze these entities and determine which refer to the same real-world entity.

Consider:
- Entity descriptions (who/what they are)
- Entity types (person, organization, etc.)
- Actions they perform (as actor)
- Actions performed on them (as target)
- Entities they co-occur with
- Reasoning chains mentioning them
- Citations referencing them

For each entity, you'll see:
- Name and description
- Entity type
- Mention count and average confidence
- Sample actions (as actor and target)
- Co-occurring entities
- Sample reasoning and citations

Analyze carefully and group entities that refer to the same real-world person or organization.

Provide your answer as JSON:
{
  "merge_groups": [
    ["Entity1", "Entity2"],
    ["Entity3"]
  ],
  "reasoning": "Brief explanation of merge decisions"
}

If entities should NOT be merged, put them in separate groups."""

        for entity_name in group:
            profile = profiles.get(entity_name)
            if not profile:
                continue
            
            prompt += f"\n---\n**{profile.name}** ({profile.entity_type})\n"
            prompt += f"Description: {profile.description}\n"
            prompt += f"Mentions: {profile.mention_count}, Avg Confidence: {profile.avg_confidence:.2f}\n"
            
            if profile.actions_as_actor:
                prompt += f"Actions as actor: {', '.join(profile.actions_as_actor[:5])}\n"
            if profile.actions_as_target:
                prompt += f"Actions as target: {', '.join(profile.actions_as_target[:5])}\n"
            if profile.co_occurring_entities:
                prompt += f"Co-occurs with: {', '.join(list(profile.co_occurring_entities)[:5])}\n"
            if profile.sample_reasoning:
                prompt += f"Sample reasoning: {profile.sample_reasoning[0][:150]}...\n"
            if profile.sample_citations:
                prompt += f"Sample citation: {profile.sample_citations[0][:150]}...\n"
        
        prompt += "\n---\n\nProvide merge groups as JSON:"
        return prompt
```

This prompt demonstrates several advanced techniques: it provides rich context (entity profiles with multiple attributes), explains what to consider (description, type, actions, co-occurrence), structures the output format clearly, and includes instructions for edge cases (entities that shouldn't be merged). The prompt dynamically builds context for each candidate group, incorporating all available metadata to enable informed decisions.

### Prompt Effectiveness: What Works and What Doesn't

The fact extraction prompts demonstrate both strengths and limitations of prompt engineering.

**What Works Well**:

1. **Explicit Format Specification**: The detailed JSON schemas produce consistent output structures
2. **Domain-Specific Rules**: The Jeffrey Epstein identification rule successfully handles variant names
3. **Constraint Enforcement**: Strong language ("CRITICAL", "MUST") helps enforce important rules
4. **Examples**: Concrete examples clarify expectations and reduce ambiguity
5. **Structured Output**: JSON format enables reliable parsing and downstream processing

**What Has Limitations**:

1. **Constraint Violations**: LLMs sometimes still violate constraints despite strong language
2. **JSON Parsing**: Even with explicit instructions, LLMs sometimes wrap JSON in markdown or add explanatory text
3. **Entity Consistency**: While rules help, some inconsistency remains (e.g., "Epstein" vs "Jeffrey Epstein")
4. **Edge Cases**: Prompts can't anticipate all edge cases, leading to occasional extraction errors
5. **Prompt Length**: Longer prompts (like the enhanced version) increase token usage and cost

**Common Failure Modes**:

1. **Format Violations**: LLMs sometimes return invalid JSON or include extra text
2. **Constraint Violations**: Actors sometimes include organizations or locations despite constraints
3. **Inconsistent Naming**: Entity names vary despite consistency instructions
4. **Missing Fields**: Optional fields are sometimes omitted when they should be included
5. **Over-Extraction**: LLMs sometimes extract facts that aren't well-supported by the document

### Prompt Optimization: Opportunities for Improvement

Several opportunities exist to improve prompt effectiveness while maintaining or reducing complexity.

**Opportunity 1: Use JSON Mode Consistently**

OpenAI's JSON mode (`response_format={"type": "json_object"}`) ensures valid JSON output, reducing the need for extraction logic. However, it requires careful prompt design to ensure the model understands the expected structure.

**Opportunity 2: Few-Shot Examples**

Adding examples of correct extractions (few-shot learning) could improve consistency. For example, showing the LLM a sample document and its correct extraction might help it understand the desired output format and quality.

**Opportunity 3: Chain-of-Thought Reasoning**

For complex extractions, prompts could request step-by-step reasoning before producing the final output. This might improve accuracy for ambiguous cases, though it increases token usage.

**Opportunity 4: Validation Instructions**

Prompts could include self-validation instructions: "Before returning your answer, verify that all actors are person names only and that all JSON is valid." This might catch some errors before output.

**Opportunity 5: Temperature and Sampling**

Lower temperature settings (0.0-0.3) produce more deterministic outputs, which is beneficial for structured extraction tasks. The fact extraction prompts use low temperatures, which is appropriate.

### Comparison: Prompt Engineering vs. Post-Processing

The fact extraction pipeline uses both prompt engineering (to guide extraction) and post-processing (to handle edge cases). Understanding the trade-offs helps determine when to use each approach.

**Prompt Engineering Advantages**:

- Handles issues at extraction time, preventing errors from propagating
- Leverages LLM's understanding of language and context
- Can incorporate domain knowledge directly
- Produces structured output that's easier to process

**Post-Processing Advantages**:

- More deterministic and testable
- Can handle cases prompts miss
- Easier to update without retraining
- Can use specialized algorithms (regex, NLP libraries)

**Hybrid Approach**:

The fact extraction pipeline uses both: prompts guide extraction and enforce basic constraints, while post-processing handles JSON extraction, validation, and error recovery. This hybrid approach balances the flexibility of prompt engineering with the reliability of deterministic processing.

### Lessons Learned: Prompt Engineering Best Practices

The fact extraction prompts provide several lessons about effective prompt engineering:

**Lesson 1: Explicit is Better Than Implicit**

Clear, explicit instructions produce better results than relying on the LLM to infer requirements. The Jeffrey Epstein identification rule works because it's explicit about variants and canonicalization.

**Lesson 2: Structure Aids Understanding**

Organized prompts with clear sections (role, task, rules, format, guidelines) are easier for LLMs to process than unstructured text. The hierarchical organization helps the model understand priorities and relationships.

**Lesson 3: Examples Reduce Ambiguity**

Concrete examples clarify abstract requirements. Showing what "PERSON NAME ONLY" means with examples ("'Donald Trump', not 'Donald Trump at party'") is more effective than just stating the constraint.

**Lesson 4: Repetition Reinforces Important Rules**

Repeating critical rules in multiple places (like the Jeffrey Epstein rule appearing in both the identification section and the actor field specification) helps ensure they're followed.

**Lesson 5: Format Specification is Essential**

Detailed JSON schemas with examples produce more consistent output than vague format descriptions. The structured format specification acts as a template the LLM can follow.

**Lesson 6: Domain Knowledge Must Be Encoded**

Domain-specific challenges (like entity name variants) require explicit encoding in prompts. The LLM can't infer domain knowledge from general instructions alone.

**Lesson 7: Robust Parsing is Necessary**

Even with explicit format instructions, LLMs sometimes produce variations (markdown wrapping, extra text). Robust parsing logic handles these variations gracefully.

### Current State and Future Directions

The fact extraction prompts successfully guide LLMs to extract structured facts from documents, with reasonable accuracy and consistency. The evolution from basic to enhanced prompts demonstrates iterative improvement based on real-world usage.

**What Works Well**:

- Basic extraction prompts produce consistent RDF triple structures
- Domain-specific rules (Jeffrey Epstein identification) handle variant names effectively
- JSON format specification enables reliable parsing
- Enhanced prompts add provenance and descriptions for downstream processing

**Areas for Enhancement**:

- More consistent entity naming (still some variation)
- Better handling of edge cases (ambiguous relationships, complex documents)
- Few-shot examples to improve quality
- Self-validation instructions to catch errors
- More sophisticated constraint enforcement

**Design Philosophy**:

The prompt engineering approach prioritizes clarity and explicitness over brevity. Longer, more detailed prompts that clearly specify requirements produce better results than concise prompts that rely on the LLM to infer intent. This philosophy recognizes that prompt engineering is a form of programming—clear specifications lead to better outcomes.
