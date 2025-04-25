# Making Work-In-Progress LLM Engineering Docs First-Class Citizens

## 🧠 What Is This Project?

This project is about **transforming our ephemeral, messy, WIP LLM engineering documents**—debug guides, architecture drafts, design notes, scratchpads—into **stable, navigable, updateable knowledge artifacts**.

We already write a ton of Markdown while building LLM features. These documents contain real insights: how the recursive planner works, how event logs stream from Redis to WebSocket to UI, what “step propagation” actually means in practice. They’re not just to “explain after the fact”—they _are_ part of how we think, debug, and design.

But right now, these docs get stale fast. They pile up in folders. Their connections to code, concepts, or other documents decay. And nobody wants to update them because it’s hard to know what’s still relevant.

**The goal of this project is to change that.**

---

## 🧩 The Problem We’re Solving

We treat code like a living organism: tested, versioned, tracked.  
But we treat engineering documents like dead fish: useful for a day, then forgotten.

Here’s the situation:

| Problem              | Symptom                                                               |
| -------------------- | --------------------------------------------------------------------- |
| Docs go stale        | Architecture diagram refers to file paths that no longer exist.       |
| Hard to browse       | “Where’s that thing about Redis events?” → nobody knows.              |
| No traceability      | Which parts of this doc relate to `GraphRunEngine` again?             |
| Rewrites are risky   | “If I change the graph code, do I need to update anything?”           |
| Live context is lost | Which logs came from which run? From what commit? In staging or prod? |

And yet, these documents _are how we work_. They scaffold new feature development. They hold mental models that aren't obvious from the code alone. They let us explain things to future teammates—or our future selves.

---

## 🌱 The Big Idea

What if every doc had a little brain?

We don’t need AI to make our documents smarter. We just need a **small, structured YAML header**—a preamble—that gives each doc:

- an identity (`id`, `title`, `status`)
- a sense of time (`created`, `updated`)
- knowledge of its surroundings (`source_files`, `related_docs`, `tags`)
- hooks into the broader system (`review_cycle`, `update_command`, `run_id`)

This tiny change turns a flat `.md` file into something we can **query**, **analyze**, and **maintain programmatically**.

It lets us build:

- 🧭 dashboards for long-term documents
- 🧪 scripts that tell you “this file changed, update your doc”
- 🔁 GitHub bots that nudge you to review stale files
- 🧠 internal knowledge graphs that understand what concepts are explained where

In short: **we turn working docs into living docs.**

---

## 🔧 What We’re Actually Doing

We’re retrofitting all our key documents—debugging guides, system diagrams, PRD-ish proposals—with structured YAML preambles.

The preamble might look like this:

```yaml
id: recursive-agent-debugging-guide
title: "Debugging Guide: Recursive Agent Architecture"
document_type: guide
longevity: long
tags: [recursive, agent, debugging]
created: 2025-04-07
updated: 2025-04-24
source_files:
  - recursive/engine.py
  - recursive/graph.py
tracked_functions:
  - GraphRunEngine.forward_one_step_not_parallel
  - AbstractNode.do_action
see_also:
  - 02-event-logging-system.md
```

This small metadata block unlocks big capabilities:

- **Docs-as-code workflows**: you can lint, grep, and validate these files automatically.
- **Search portals**: you can filter all docs by `tags`, `tracked_functions`, or `concepts`.
- **Update tooling**: when `recursive/graph.py` changes, we know which docs need review.
- **Long-term value**: files marked with `longevity: long` won’t rot silently—they’re part of our interface contract.

And we’ll define a small schema and validation script so the YAML is consistent.

---

## 🔍 Why This Matters for LLM Work

When building agent systems, debugging flows, graph engines, memory embeddings—**the doc is the product**. Code just runs the plan. The plan _lives in your head_, or your notes, or your stream-of-consciousness Markdown.

Making these docs maintainable means:

- You can trust your past thinking.
- You can onboard collaborators without repeating yourself.
- You can connect the dots between architecture, debugging, and UI behavior.
- You can build new capabilities—like a queryable doc-agent—on top of a solid foundation.

If we don’t invest in metadata and structure, our knowledge gets lost in the churn of feature branches.

---

## 🛣️ What Comes Next

Here’s the roadmap:

1. **Adopt the preamble format** across all core docs (starting with architecture/debugging guides).
2. **Write a validation CLI** that checks schema conformance and scans for stale `source_files`.
3. **Add doc-metadata parsing to our MkDocs or static site build.**
4. **Wire the doc index into a knowledge base**, so that “explain `AbstractNode.next_action_step`” can pull up relevant markdown.
5. **Tag new documents by default**, and maybe auto-inject headers from templates.
6. **Publish our format spec** so other projects can adopt it.

Eventually, our Markdown becomes a living part of the codebase—versioned, traceable, self-aware.

---

## 💡 Bonus Use Cases

This setup also supports:

- 💬 Live run logs with context-aware annotations (`env`, `run_id`, `step`)
- 🧠 RAG (Retrieval-Augmented Generation) based on `tracked_functions` or `concepts`
- 🛠 CI dashboards that show which docs haven't been updated since major changes
- 📚 Storybook-style UI documentation with embedded metadata from the source files

---

## ✍️ Final Thought

This isn’t just about YAML. It’s about respect for the thinking that goes into complex systems. It’s about building bridges between ideas and implementation.

A doc with a preamble isn’t a note.  
It’s a **module in your mind**.

Let’s build it that way.

---

## 1. Core Identity & Lifecycle

| Field                 | Example                             | Effect                                                                       |
| --------------------- | ----------------------------------- | ---------------------------------------------------------------------------- |
| `id`                  | `writehere-arch-report`             | Stable key for bidirectional links, URL slugs, GitOps pipelines.             |
| `created` / `updated` | `2025-04-07`                        | Enables “recently-updated” sort and stale-doc warnings.                      |
| `status`              | `draft` \| `active` \| `deprecated` | Dashboard filter; reviewers know whether to comment or archive.              |
| `owner`               | `@manuel`                           | Route review requests & change notifications via GitHub CODEOWNERS or Slack. |
| `audience`            | `internal-dev` \| `external-docs`   | Publish pipeline decides which docs land in the public site.                 |

---

## 2. Discovery & Classification

| Field       | Example                                      | Effect                                                                                    |
| ----------- | -------------------------------------------- | ----------------------------------------------------------------------------------------- |
| `tags`      | `[recursive-planning, event-logging, redis]` | Faceted search & tag cloud in docs portal.                                                |
| `category`  | `architecture` \| `how-to` \| `debugging`    | Groups docs in sidebar sections.                                                          |
| `longevity` | `long` \| `short`                            | CRON job flags short-lived docs for cleanup; long-lived docs get linted for broken links. |
| `keywords`  | free-text string                             | Feeds a full-text indexer for fuzzy search.                                               |

---

## 3. Code & Concept Links

| Field                                                                                       | Example                                                                         | Effect                                                             |
| ------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------- | ------------------------------------------------------------------ |
| `source_files`                                                                              |                                                                                 |
| `yaml\nsource_files:\n  - recursive/graph.py\n  - ui-react/src/components/EventTable.tsx\n` | Change-watcher (e.g., `git diff`) triggers a doc-update todo when files change. |
| `tracked_functions`                                                                         | `GraphRunEngine.forward_one_step_not_parallel`                                  | IDE plugin or mkdocs-material “hover-cards” jump straight to code. |
| `concepts`                                                                                  | `[ExecutionContext, AgentEvent]`                                                | Knowledge-graph edges between docs & glossary definitions.         |

---

## 4. Relationships to Other Docs

| Field                       | Example                                                              | Effect                                                                   |
| --------------------------- | -------------------------------------------------------------------- | ------------------------------------------------------------------------ |
| `see_also`                  | `[01-write-here-architecture-report.md, 04-event-logging-system.md]` | “Related documents” sidebar cards.                                       |
| `predecessor` / `successor` | file paths or IDs                                                    | Version lineage; migration tool shows diff between doc generations.      |
| `imports`                   | YAML anchors or remote URLs                                          | Allows composable “partials” (e.g., common admonitions or license text). |

---

## 5. Revision & Governance

| Field            | Example                                      | Effect                                                                                 |
| ---------------- | -------------------------------------------- | -------------------------------------------------------------------------------------- |
| `schema_version` | `0.2.1`                                      | Back-compat shim for doc tooling.                                                      |
| `approved_by`    | `[alice@example.com, bob@example.com]`       | Release script enforces that major PRDs have sign-off.                                 |
| `review_cycle`   | `90d`                                        | Bot opens a GitHub issue when the doc hasn’t been touched in 90 days.                  |
| `changelog`      | list of objects (`date`, `change`, `author`) | Inline history without reading git log; useful when docs live in Notion or Confluence. |

---

## 6. Automation Hooks

| Field              | Example                          | Effect                                                          |
| ------------------ | -------------------------------- | --------------------------------------------------------------- |
| `update_command`   | `python tools/regenerate_uml.py` | Docs-as-code runner executes and embeds fresh UML diagrams.     |
| `notify_on_change` | `slack://#writehere-docs`        | CI sends diff summary to channel when doc updates.              |
| `auto_sync`        | `true`                           | Indicates a doc is autogenerated; editors get a warning banner. |

---

## 7. Content Metrics & Summaries

| Field        | Example         | Effect                                                         |
| ------------ | --------------- | -------------------------------------------------------------- |
| `word_count` | `2450`          | Build fails if doc exceeds size budget for quick-start guides. |
| `abstract`   | short paragraph | Used as search-result snippet and card preview.                |
| `toc_depth`  | `3`             | Static-site generator decides how deep to nest local ToC.      |

---

## 8. Execution-Time Context (for run logs & debug guides)

| Field    | Example                      | Effect                                                                    |
| -------- | ---------------------------- | ------------------------------------------------------------------------- |
| `run_id` | UUID string                  | Links a run log to the exact agent execution snapshot.                    |
| `env`    | `dev` \| `staging` \| `prod` | Filters dashboards by environment.                                        |
| `commit` | Git SHA                      | Traceability between doc version and code revision that produced the log. |

---

## 9. Suggested Preamble Presets

### 9.1 Minimal Long-Lived Spec

```yaml
id: writehere-graph-event-system
title: "Recursive Agent Event Logging System"
document_type: spec
longevity: long
owner: "@manuel"
created: 2025-04-17
updated: 2025-04-24
status: active
tags: [event-logging, redis, websocket]
schema_version: 1.0
```

_Effect_: immediately sortable in a doc index; owner & status power a “maintenance queue”.

---

### 9.2 Code-Linked Debug Guide

```yaml
id: node-event-debug-guide
document_type: debugging
longevity: short
audience: internal-dev
source_files:
  - recursive/node/abstract.py
  - recursive/graph.py
tracked_functions:
  - AbstractNode.do_action
  - GraphRunEngine.forward_one_step_not_parallel
see_also:
  - 04-event-logging-system.md
  - 02-propagating-steps-info.md
update_command: "pytest tests/debug --record"
review_cycle: 30d
```

_Effect_: CI watches the two Python files; if they change, it runs the `update_command` to refresh screenshots and opens a doc-review ticket.

---

### 9.3 PRD/Feature Proposal Template

```yaml
id: event-storage-sqlite
title: "SQLite Event Storage Design Guide"
document_type: prd
status: proposed
stakeholders: [@manuel, @db-lead, @frontend-lead]
milestone: Q2-2025
abstract: >
  Persists all agent events to SQLite for replay and analytics; replaces ephemeral Redis stream only flow.
metrics:
  - "Cold-start recovery time < 2 s"
  - "Query 10k events under 200 ms"
dependencies:
  - recursive/utils/event_bus.py
  - websocket server redesign
risk_level: medium
```

_Effect_: Product board can slice by milestone, risk, stakeholder; automated milestone reporter pulls status.

---

## 10. Implementation Tips

1. **Place front-matter between triple-dashed lines** so both MkDocs and Jekyll understand it.
2. **Keep keys snake_case**—­easier to map to Python dataclasses or Go structs.
3. **Validate with a JSON-schema** in pre-commit to prevent typos (`yamllint` + `pykwalify`).
4. **Expose the metadata at build time** (e.g., `{{ page.meta.tags }}` in MkDocs templates) to drive navigation components.
5. **Write a tiny Python watcher** that:
   - parses YAML,
   - resolves `source_files`,
   - checks `git diff` for each,
   - raises a TODO when code changes but doc `updated` date is older.

With these fields in place your docs become _first-class, queryable objects_—not just Markdown blobs—making it painless to surface the right information, keep it fresh, and wire it directly into your LLM workflow.
