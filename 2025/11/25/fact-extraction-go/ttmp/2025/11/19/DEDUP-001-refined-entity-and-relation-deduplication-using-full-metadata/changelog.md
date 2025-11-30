# Changelog

## 2025-11-19

- Initial workspace created


## 2025-11-19

Implemented refined deduplication script. Features: Entity profiles (description, actions, co-occurrence, reasoning, citations), Relation profiles (description, typical actors/targets), 3-stage approach (embeddings, features, LLM). Waiting for 24-doc extraction to complete for testing.


## 2025-11-19

SUCCESS! Refined deduplication tested on 24-doc dataset. Results: 45 entity groups, 90 entities merged (from 306 total), cost $0.0064. High-quality merges including Alan Dershowitz (10 variants), Jeffrey Epstein (3 variants), Virginia Roberts Giuffre (4 variants). LLM using rich context for intelligent decisions beyond string matching.


## 2025-11-19

COMPLETE! Added relation/verb deduplication. Final results: 55 entity groups (108 entities merged), 15 relation groups (33 relations merged). Total cost $0.0089. High-quality relation merges: 'owns' (5 variants), 'pleaded guilty' (3 variants), 'invoked Fifth Amendment' (4 variants). System ready for production.

