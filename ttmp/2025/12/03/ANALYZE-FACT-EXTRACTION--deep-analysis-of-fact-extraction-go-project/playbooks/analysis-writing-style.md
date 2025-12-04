---
Title: Analysis Writing Style Guidelines
Ticket: ANALYZE-FACT-EXTRACTION
Status: active
Topics:
    - analysis
    - writing
    - guidelines
DocType: playbook
Intent: long-term
Owners: []
RelatedFiles: []
ExternalSources: []
Summary: Writing style guidelines for analysis documents - combining prose paragraphs with technical details
LastUpdated: 2025-12-03T10:00:00.000000000-05:00
---

# Analysis Writing Style Guidelines

## Overview

This playbook defines the writing style for analysis documents in the fact extraction project. The goal is to create documents that are both accessible to readers without deep project knowledge AND technically precise with implementation details.

## Core Principle

**IMPORTANT**: Write your analysis in full prose paragraphs that provide context for readers who don't know the project, BUT ALSO include comprehensive technical details like code snippets, implementation specifics, and technical analysis. Follow the style used in `03-multi-stage-entity-resolution-analysis.md` as a reference.

## Key Requirements

- **Provide context**: Explain what each concept means and why it matters before diving into technical details
- **Use narrative prose**: Write complete paragraphs, not just bullet points or technical notes
- **Include technical details**: Add code snippets with line references, implementation specifics, database schemas, algorithm details
- **Explain the "why"**: Don't just describe what was done—explain why it was done and what problem it solves
- **Connect ideas**: Show how different parts relate to each other and to the overall goal
- **Make it accessible**: Assume the reader is intelligent but doesn't have deep knowledge of the project
- **Use examples**: Include concrete examples to illustrate abstract concepts

## Document Structure

Each major section should:

1. **Start with a prose paragraph** that:
   - Explains what the section covers
   - Provides context for why it matters
   - Connects it to the overall research objective

2. **Include technical details** such as:
   - Code snippets with file paths and line references (use format: `startLine:endLine:filepath`)
   - Implementation details and algorithm explanations
   - Database schema details and query examples
   - Edge cases and examples with concrete data

**Note**: Do NOT include performance metrics, costs, timing, statistics, results from runs, or comparison metrics. Focus on explaining how things work and why they were designed that way.

## Balance

The document should have **BOTH** prose paragraphs (for readability and context) **AND** technical details (for depth and precision). 

- Do not write only prose without technical details
- Do not write only technical details without prose context

## Reference Example

See `03-multi-stage-entity-resolution-analysis.md` for examples of this balanced writing style throughout the document.

