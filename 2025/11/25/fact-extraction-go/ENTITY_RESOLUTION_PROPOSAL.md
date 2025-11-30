# Proposal for Enhancing Entity Resolution in Epstein-doc-explorer

**Date**: November 19, 2025
**Author**: Manus AI

## 1. Executive Summary

This document outlines a series of proposed enhancements to the entity resolution and deduplication pipeline of the `Epstein-doc-explorer` project. Our analysis of the current implementation and a thorough review of recent scientific literature reveal significant opportunities to improve accuracy, scalability, and semantic understanding. We recommend a phased adoption of modern techniques, including **embedding-based candidate generation**, **cross-document context aggregation**, **LLM-based batch merging**, and **graph-based disambiguation**. These improvements will transition the system from a string-based, pairwise approach to a more robust, semantic, and globally consistent entity resolution framework.

## 2. Analysis of Current Implementation

The existing pipeline leverages Claude Haiku for NER and a combination of string similarity and LLM verification for deduplication. While functional, it has several limitations that can be addressed:

| Area | Current Approach | Limitations |
| :--- | :--- | :--- |
| **Candidate Generation** | Liberal string matching (shared words, substrings) | - Prone to false negatives (e.g., "J. Epstein" vs. "Jeffrey")<br>- Does not capture semantic similarity<br>- Inefficient for large datasets |
| **Entity Context** | Single-mention context | - Ignores rich information from other documents<br>- Fails to build a holistic view of an entity |
| **Deduplication Logic** | Pairwise LLM comparison | - Ignores global consistency (A=B, B=C, but A≠C)<br>- Inefficient due to numerous LLM calls<br>- Limited to high-frequency entities (≥5 mentions) |
| **Graph Utilization** | Primarily for visualization | - Underutilizes relationship patterns for disambiguation |
| **Enrichment** | Manual / Not implemented | - Misses opportunity to add valuable external context |

## 3. Proposed Enhancements

Based on our research into recent advancements in entity resolution [1][2][3], we propose the following enhancements, organized into a logical implementation roadmap.

### Phase 1: Semantic Candidate Generation

**Objective**: Replace the current string-based blocking with a more accurate and scalable embedding-based approach.

**Methodology**:
1.  **Generate Entity Embeddings**: For each unique entity name, generate a high-quality vector embedding using a pretrained Sentence Transformer model (e.g., `all-MiniLM-L6-v2`). The input for the embedding should be a composite of the entity name and its surrounding context from the source document.
2.  **Vector Indexing**: Store these embeddings in a vector database or an in-memory index like FAISS.
3.  **Similarity Search**: For each entity, perform an Approximate Nearest Neighbor (ANN) search on the vector index to retrieve the top-k most semantically similar candidates. This forms the candidate block for deduplication.

**Benefits**:
-   **Superior Accuracy**: Captures semantic meaning beyond lexical similarity.
-   **Scalability**: Efficiently handles millions of entities.
-   **Reduces False Negatives**: Correctly groups entities like "Ghislaine" and "G. Maxwell".

### Phase 2: Cross-Document Context Aggregation

**Objective**: Build comprehensive entity profiles by aggregating all information about an entity across the entire document corpus.

**Methodology**:
1.  **Create Entity Profiles**: For each entity, create a profile that includes all known aliases, a summary of its relationships, document frequency, and temporal patterns.
2.  **Contextual Embeddings**: Generate a single, rich embedding for each entity profile, summarizing its entire context.
3.  **Refine Candidate Generation**: Use these richer embeddings for the semantic blocking described in Phase 1.

**Benefits**:
-   **Disambiguation Power**: Distinguishes between two individuals with the same name (e.g., "John Smith") based on their different relationship patterns.
-   **Richer Input for LLM**: Provides the LLM with a holistic view of each entity, leading to more accurate merge decisions.

### Phase 3: LLM-Based Batch Merging

**Objective**: Move from inefficient pairwise comparisons to a more globally consistent and efficient batch merging strategy, as proposed by recent research [2][4].

**Methodology**:
1.  **Batch Formulation**: For each candidate block from Phase 1, present all entity profiles to the LLM in a single prompt.
2.  **Multi-Merge Prompting**: Instruct the LLM to identify all groups of entities that refer to the same real-world individual and to select a canonical name for each group.
3.  **Structured Output**: Require the LLM to return a structured JSON object detailing the merge groups, which can be parsed and applied automatically.

**Benefits**:
-   **Global Consistency**: Ensures that if A=B and B=C, then A=C.
-   **Efficiency**: Drastically reduces the number of required LLM calls.
-   **Higher Accuracy**: Allows the LLM to make more informed decisions by comparing multiple candidates simultaneously.

### Phase 4: Graph-Based Disambiguation & Fact Store

**Objective**: Leverage the graph structure for entity disambiguation and create a more robust fact store.

**Methodology**:
1.  **GNN for Entity Similarity**: Implement a Graph Neural Network (GNN) to learn entity embeddings that incorporate relationship information. These embeddings can be used to further refine merge decisions [3].
2.  **Fact-Centric Storage**: Transition from storing only triples to a more structured fact store. Each fact should be a unique assertion (e.g., "Epstein met Trump on X date") with links to all supporting documents and provenance information.
3.  **Fact Deduplication**: Deduplicate facts themselves, not just entities, to create a cleaner knowledge base.

**Benefits**:
-   **Relationship-Aware ER**: Uses the rich signal from entity connections to improve accuracy.
-   **Data Quality**: A deduplicated fact store provides a more reliable foundation for analysis.
-   **Traceability**: Clear provenance for every piece of information.

## 4. Implementation Roadmap

We recommend a phased approach to implementing these enhancements:

| Phase | Timeline | Key Actions | Impact |
| :--- | :--- | :--- | :--- |
| **1** | 1-2 Weeks | - Implement embedding-based blocking<br>- Remove high-frequency entity filter | **High** (Significant improvement in candidate quality) |
| **2** | 1-2 Months | - Develop cross-document entity profiles<br>- Implement batch LLM merging | **High** (Improved accuracy and efficiency) |
| **3** | 3-6 Months | - Integrate a GNN for entity similarity<br>- Design and implement a fact store | **Medium** (Further accuracy gains and data quality) |
| **4** | Ongoing | - Build an automated enrichment pipeline<br>- Explore temporal analysis of relationships | **Medium** (Added value and deeper insights) |

## 5. Future Work

Beyond the core recommendations, further research could explore:
-   **Automated Enrichment**: An automated pipeline to enrich resolved entities with data from external knowledge bases like Wikipedia or Wikidata.
-   **Temporal Analysis**: Modeling how entity relationships and attributes evolve over time.
-   **Confidence Scoring**: Implementing a probabilistic framework to assign confidence scores to all extractions and merge decisions.

## 6. References

[1] Barlaug, N., & Gulla, J. A. (2021). Neural Networks for Entity Matching: A Survey. *ACM Transactions on Knowledge Discovery from Data, 15*(3), 1–37.

[2] Wang, T., et al. (2025). Match, Compare, or Select? An Investigation of Large Language Models for Entity Matching. *Proceedings of the 31st International Conference on Computational Linguistics*.

[3] Hu, J., et al. (2024). *When GDD meets GNN: A Knowledge-driven Neural Connection for Effective Entity Resolution in Property Graphs*. arXiv preprint arXiv:2410.04783.

[4] Jurney, R. (2025). The Rise of Semantic Entity Resolution. *Towards Data Science*.
