---
Title: Presidential Debate - Documentation Manager Feature Design
Status: draft
Topics:
- documentation
- llm-workflow
- process
- entertainment
Created: 2025-11-03
---

# Presidential Debate: The Future of Documentation Management

**Moderator**: Good evening, and welcome to the first-ever Documentation Tooling Presidential Debate! Tonight, our distinguished candidates will discuss the future of the documentation manager system based on RFC MEN-000.

## Meet Your Candidates

1. **The RFC Purist** - A strict constitutionalist who believes every word of the RFC is sacred law
2. **`init.go`** - The current implementation's scaffolding command (lines 1-194), pragmatic but incomplete
3. **The Intern** - The original implementer, defensive about their choices
4. **The Glazed Evangelist** - Believes all problems can be solved with more layers and middleware
5. **`models/document.go`** - The metadata schema itself, tired of being misunderstood
6. **The YAGNI Warrior** - "You Aren't Gonna Need It" - wants to strip everything down
7. **The Enterprise Architect** - Wants microservices, event sourcing, and blockchain
8. **`doctor.go`** - The validator, cynical about human nature and data quality
9. **The Git Workflow Enthusiast** - Everything should be derived from git, period
10. **The LLM Context Optimizer** - Only cares about making LLM prompts efficient

---

## Question 1: Should we use `ttmp/` or `docs/active/` as the root directory?

### Round 1

**The RFC Purist**: "The RFC clearly states `ttmp/MEN-XXX-<slug>/` on line 35! This is not a suggestion, this is THE LAW. The entire mental model depends on this convention. Change it and you've invalidated the entire specification!"

**`init.go`**: "Look, I use `docs/active` because that's what made sense when I was written. Line 86 in my code: `filepath.Join(settings.Root, "active", dirName)`. I'm flexible with `--root`, so what's the problem? The RFC doesn't even HAVE working code!"

**The Intern**: "I chose `docs/active/` because it's clearer! When I see 'ttmp' I think 'temp' and that sounds like it gets deleted. Plus, having an 'active' subdirectory means we can have 'archived' later. It's more intuitive!"

**The Glazed Evangelist**: "Why are we hardcoding paths at all? We should have a `PathResolutionLayer` that implements the `PathStrategy` interface with multiple backends. Want `ttmp`? Configure it. Want `docs`? Configure it. Want S3? We'll have a layer for that!"

**`models/document.go`**: "I don't care where you put the files. I'm just a struct. But every time you change the path convention, some poor developer has to update documentation, and then I get blamed when the paths in my `RelatedFiles` field are wrong. Make up your minds!"

### Round 2

**The RFC Purist**: "The Intern's argument about 'active' vs 'archived' actually proves my point! The RFC ALREADY has `archive/` as a subdirectory within each ticket! Section 4, line 56! We don't need another layer of directory hierarchy!"

**`init.go`**: "Fine, I'll change to `ttmp`. But who's going to migrate all the existing workspaces? Oh right, there's no migration command. That's what I thought. Practical concerns matter!"

**The YAGNI Warrior**: "You're all overthinking this. Use whatever directory the user is IN. `./index.md`, `./design/`, done. Why do we need a special root at all? It's just files!"

**The Enterprise Architect**: "Excuse me, but why are we using filesystems in 2025? These should be stored in a document database with full-text search, versioning, and event sourcing. The 'path' should be a URI that resolves through our service mesh!"

**`doctor.go`**: *sighs* "I don't validate paths. I barely validate anything. Line 74: `filepath.Join(settings.Root, "active")`. I just look for whatever is there and complain. You want me to enforce RFC paths? Sure, write me that feature. I'll wait."

---

## Question 2: The `init` command overwrites existing files without checking. Is this a critical bug or acceptable behavior?

### Round 1

**The RFC Purist**: "Section 9, line 176: 'CLI should be idempotent and safe to re-run.' This is explicitly stated! Overwriting files is a VIOLATION of the specification and must be fixed immediately!"

**`init.go`**: "Line 120, I use `os.Create(path)` which truncates. Yes, I know. But you know what? If you run `init` twice on the same ticket, that's user error! The real world is messy. Should I really add 50 lines of code for an edge case?"

**The Intern**: "I... I didn't think about re-running it. In my testing, I always started fresh. Look, it's a prototype! The analysis doc says 'solid prototype', not 'production-ready'. This is on the list to fix!"

**`doctor.go`**: "This is EXACTLY why you need me. But guess what? I don't run before `init`, I run AFTER. Line 64 in my code: I'm just checking existing workspaces. No one thought to add a `--dry-run` or a pre-check. Typical."

**The YAGNI Warrior**: "Add a `--force` flag. Don't overwrite by default. Three lines of code. Next question."

### Round 2

**The Git Workflow Enthusiast**: "Why are we even tracking these files in git if we're going to overwrite them? `index.md` should be generated from git metadata and never edited manually! The ticket should come from the branch name, the date from git log, the owners from git blame!"

**`models/document.go`**: "Every time `init` overwrites me, the `LastUpdated` timestamp changes. Do you know how annoying this is for git diffs? I show up as changed even when the content is identical. Use proper file handling!"

**The LLM Context Optimizer**: "Can I just point out that if you overwrite `index.md`, any context I've carefully curated gets destroyed? I spend HOURS collecting the perfect set of `RelatedFiles` and then BOOM, gone. At least preserve the frontmatter!"

**The Enterprise Architect**: "In a properly designed system, there would be optimistic locking with version numbers. The `init` command would fail with a 409 Conflict if the resource exists. Then the client can decide whether to retry with a merge strategy. This is HTTP 101!"

**The Glazed Evangelist**: "We should have an `InitCommandMiddleware` that runs `PreInitValidation` hooks. One hook checks for existing files, another checks git status, another checks for uncommitted changes. It's extensible and testable!"

---

## Question 3: Should we implement all the doc types from the RFC, or keep the limited set?

### Round 1

**The RFC Purist**: "Section 8 defines EIGHT doc types! Index, working-note, design-doc, reference, tutorial, playbook, log, task-list, script! This is the minimum viable vocabulary! Without them, the system is incomplete!"

**`init.go`**: "I only create directories for design, reference, playbooks, and scripts. That's four subdirectories. The RFC wants `various/` for working-notes, but `add.go` doesn't even support them! We're inconsistent!"

**The Intern**: "I implemented the three most common types first: design-doc, reference, playbook. Line 91-100 in `add.go`. That's 80% of the use cases! We can add the others iteratively. Minimum viable product!"

**The YAGNI Warrior**: "Three types is STILL too many! Design doc, reference doc, that's it. Playbooks are just reference docs with commands. Tutorials are reference docs with steps. Working notes? That's just a design doc in draft status. One type: document.md!"

**`models/document.go`**: "I have a `DocType` field (line 13). I don't enforce anything. You can put 'sandwich-recipe' in there for all I care. But other systems might depend on these being from a controlled vocabulary, so... maybe validate them?"

### Round 2

**The LLM Context Optimizer**: "Different doc types need different prompting strategies! A `tutorial` should have step-by-step checksums I can verify. A `reference` should be copy-paste ready. A `working-note` can be messy. The types matter for context assembly!"

**The Enterprise Architect**: "Each doc type should be its own microservice with a dedicated API. `TutorialService`, `PlaybookService`, each with their own OpenAPI specs. They communicate via event bus. The `DocType` field becomes a service registry key!"

**`doctor.go`**: "I check if the fields exist (line 130-141). I don't validate the VALUES. Want me to check if `DocType` is valid? Cool. Give me the vocabulary. Oh wait, there's no `vocab` command. Line 99 in the RFC: 'DocType feeds tooling to assemble prompt packs'. I can't feed anything if I can't validate!"

**The Git Workflow Enthusiast**: "Doc type should be inferred from the directory structure! File in `design/`? Design doc. File in `reference/`? Reference doc. Why maintain redundant metadata? Trust the filesystem!"

**The Glazed Evangelist**: "We need a `DocTypeRegistry` that maps types to `DocTypeHandler` implementations. Each handler knows how to scaffold, validate, and render its type. Want to add a new type? Just implement the interface and register it!"

---

## Question 4: Is the vocabulary system necessary or overengineered?

### Round 1

**The RFC Purist**: "Section 7! Controlled vocabularies at `doc/vocabulary.yaml`! Section 10 has ENTIRE commands dedicated to this: `vocab list`, `vocab edit`, `vocab add`, `vocab assign`! This is not optional, this is CORE functionality!"

**The Intern**: "I created the types (lines 22-44 in `models/document.go`): `Vocabulary`, `VocabItem`. But I didn't implement the commands because... honestly? I ran out of time. And I wasn't sure if YAML was the right format. Maybe TOML? Maybe JSON Schema?"

**The YAGNI Warrior**: "This is EXACTLY the overengineering I'm talking about! You want a whole system to manage lists of strings? Use constants in code. Use comments. Don't build a framework to manage three lists!"

**`doctor.go`**: "Without vocabulary validation, I'm useless. Line 139: `if len(doc.Topics) == 0`. That's it. I don't know if 'chat' is valid or if someone typo'd 'caht'. I can't help if I don't know what's correct!"

**The Enterprise Architect**: "Vocabulary should be in a graph database with relationships. 'chat' relates to 'backend' and 'frontend'. When you tag something 'chat', the system auto-suggests related topics. We need semantic linking!"

### Round 2

**The LLM Context Optimizer**: "Vocabulary is CRITICAL! When I search for documents by topic, I need to know that 'chat', 'messaging', and 'conversations' might be related. A controlled vocabulary with synonyms and taxonomy would let me find relevant context accurately!"

**`models/document.go`**: "Look at my `Topics` field (line 12): `[]string`. No validation. You can put ANYTHING in there. Every system that consumes me has to do its own validation. Give me a `ValidTopics` function to call, and I'll use it!"

**The Git Workflow Enthusiast**: "Topics should be git tags! Tag your ticket branch with `topic/chat`, `topic/backend`. Now your topics are in version control, visible in GitHub/GitLab, and don't require a separate system. Use. Existing. Tools!"

**The Glazed Evangelist**: "The vocabulary should be a Layer! `VocabularyLayer` loads from files, environment, or remote URLs. Commands declare which vocabularies they need, and the layer validates input. We already have this pattern!"

**The RFC Purist**: "I notice nobody is disagreeing with the NEED for validation, only the implementation. The RFC provides a clear specification. Implement it as specified, then we can discuss improvements in RFC v2!"

---

## Question 5: Should `RelatedFiles` be manually maintained or automatically discovered?

### Round 1

**The RFC Purist**: "Section 5, line 101: 'RelatedFiles: list of repository paths relevant to the doc'. Section 9, line 163: 'ttmp relate --files path1 path2'. Manual management with helper commands. The specification is clear!"

**`models/document.go`**: "I have this `RelatedFiles` field (line 17). Nobody populates it. It's always empty or manually edited. I feel like the appendix nobody reads."

**The Git Workflow Enthusiast**: "AUTOMATIC! Run `git log --follow --name-only` on the branch associated with the ticket. Parse the output. Top 20 files by commit count. DONE. Why are we manually tracking what git already knows?"

**The LLM Context Optimizer**: "Related files are my LIFEBLOOD! But git history isn't enough. I need semantic relationships. If the doc mentions `chatApi.ts`, that's related! If it imports `types.go`, that's related! Parse the content, find the references!"

**The Intern**: "I... didn't implement `relate`. It seemed complicated. The RFC mentions `--suggest` with grep/rg (line 180), but then you need to decide which suggestions to accept. That's UX I didn't have time to design."

### Round 2

**The Enterprise Architect**: "Related files should be a knowledge graph! Each document is a node. Files are nodes. Edges represent relationships: mentions, imports, modifies. Query with GraphQL. The system learns from user corrections using ML!"

**The YAGNI Warrior**: "Just put file paths in a comment at the top of the document. `<!-- Related: src/foo.ts, src/bar.ts -->`. No framework needed. It's markdown, use markdown features!"

**`doctor.go`**: "Want me to validate that `RelatedFiles` actually exist in the repo? I can do that. But if they're auto-generated, what do I validate? That git didn't lie? This is why manual lists with validation make sense!"

**The Glazed Evangelist**: "We need a `RelatedFilesProvider` interface with multiple implementations: `GitLogProvider`, `ASTAnalysisProvider`, `ManualProvider`. They combine using a `CompositeProvider` with configurable weights. Best of both worlds!"

**`init.go`**: "Line 114: `RelatedFiles: []string{}`. I initialize it empty. Nobody told me to populate it. If there's supposed to be automatic discovery, where's the code? Show me the PR!"

---

## Question 6: Do we need both `tasks.md` AND the embedded checklist in `index.md`?

### Round 1

**The RFC Purist**: "Section 4, line 66: `tasks.md` – canonical task list. Section 6, line 106: index.md should reference open tasks. These are DIFFERENT! Index references, tasks.md contains!"

**`init.go`**: "I don't create `tasks.md` (check my dirs list, lines 89-97). I create `index.md` and `README.md`. The RFC wants `tasks.md` AND `changelog.md` as siblings. I... missed those."

**The YAGNI Warrior**: "Why have a separate tasks file? Put tasks in the index! Or better, use GitHub Issues! They're already tasks, already tracked, already have assignees and due dates. Don't reinvent project management!"

**The Intern**: "I thought the README would be enough for getting started, and then people would add tasks as needed. Creating an empty `tasks.md` felt presumptuous. Should it have a template? Checkboxes? What format?"

**The LLM Context Optimizer**: "Task lists should be in a standardized format I can parse! `- [ ]` checkboxes with `@assignee` and `#priority` tags. One file makes parsing easier. Multiple files mean I need to aggregate. Please, for the love of tokens, pick ONE format!"

### Round 2

**The Enterprise Architect**: "Tasks should be in Jira, synced bidirectionally via webhooks. The `tasks.md` is a read-only view generated by a GitHub Action. We're not managing state in markdown files like savages!"

**`doctor.go`**: "If you want me to check for uncompleted tasks (RFC line 190 mentions stale tasks), give me a standard location and format. I can count unchecked checkboxes. But scattered across multiple files? Good luck!"

**The Git Workflow Enthusiast**: "Tasks are commits not yet made! Your 'task list' is your branch's distance from main. `git log main..HEAD` shows completed tasks. `git diff --name-only main...HEAD` shows affected files. Stop duplicating git!"

**`models/document.go`**: "I don't have a `Tasks` field. Should I? Or are tasks their own entity with their own schema? If `tasks.md` is different from other docs, maybe it needs its own model?"

**The Glazed Evangelist**: "Tasks should be structured data in the frontmatter! `tasks: [{id: 't1', status: 'pending', description: '...'}]`. I can render them as markdown checkboxes OR JSON OR a table. Data vs presentation!"

---

## Question 7: Is the `.meta/` directory a good idea or should metadata be in frontmatter only?

### Round 1

**`init.go`**: "I created `.meta/` (line 96) for stuff that shouldn't be in every file's frontmatter. Like `sources.yaml` for external source metadata. It's separation of concerns!"

**The RFC Purist**: "Section 4 of the RFC doesn't mention `.meta/`! This is an unauthorized extension! All metadata should be in frontmatter where it's visible and tracked!"

**The Intern**: "I added `.meta/` because some metadata is workspace-wide, not document-specific. Should every doc duplicate the list of external sources? That's redundant! `.meta/sources.yaml` is shared state."

**The YAGNI Warrior**: "Hidden directories are where configuration goes to die. `.git`, `.vscode`, now `.meta`? Put it in the index.md or don't put it anywhere!"

**`models/document.go`**: "The `ExternalSources` field (line 18) is in MY frontmatter but also in `.meta/sources.yaml`. Which is the source of truth? Pick one!"

### Round 2

**The Git Workflow Enthusiast**: "Metadata files that aren't tracked properly in git diffs are a mistake. Frontmatter is visible in every diff. `.meta/sources.yaml` changes look like administrative noise. Put data where it's visible!"

**The LLM Context Optimizer**: "I need to scan `index.md` for every workspace anyway. If I ALSO have to check `.meta/`, that's extra I/O. But if `.meta/` contains rich metadata that doesn't fit in frontmatter... I'll pay the cost for better context."

**The Enterprise Architect**: "Metadata should be in a Redis cache with TTL, backed by PostgreSQL for durability. The documents themselves are just blobs in S3. Why are we storing state in the filesystem?"

**`doctor.go`**: "I don't check `.meta/` (lines 73-172, no mention). Want me to validate it? Add it to my spec. But if it's 'optional metadata', what do I validate? That it exists? That's not much of a check."

**The Glazed Evangelist**: "`.meta/` is fine IF it has a schema and commands to manipulate it. `docmgr meta get sources`, `docmgr meta set sources --add url`. Without commands, it's just manual YAML editing, and that's error-prone."

---

## Question 8: Should the CLI be named `docmgr` or `ttmp` as suggested in the RFC?

### Round 1

**The RFC Purist**: "Section 10, line 173: 'Implement a CLI (Go) exposed as `ttmp`'. The name is specified! TTMP! Not docmgr, not ttmp-cli, TTMP!"

**The Intern**: "I called it `docmgr` because that describes what it does! 'ttmp' sounds like a temp directory. Ask any new user what 'docmgr' does vs 'ttmp' – which is clearer?"

**The YAGNI Warrior**: "Call it `doc`. Short, clear, no bikeshedding. `doc init`, `doc list`. Done."

**`init.go`**: "My code doesn't care what the binary is called. I'm just a function. But FYI, line 17 in `main.go`: `Use: docmgr`. Change one string, move on."

**The LLM Context Optimizer**: "TTMP is hard to type and hard to say. 'Doc manager' is semantically clear for prompts. When I suggest commands, I can say 'use the doc manager' naturally. 'Use the ttmp'? Awkward!"

### Round 2

**The Git Workflow Enthusiast**: "CLI names should match their purpose. `git` for git operations, `docker` for containers, `ttmp` for... temporary? The name `ttmp` implies the documents are temporary, which contradicts the 'long-term intent' metadata!"

**The Enterprise Architect**: "The CLI should be `docmgr-cli` to distinguish from `docmgr-server`, `docmgr-ui`, and `docmgr-agent`. Consistent naming across the service mesh is crucial for observability!"

**`doctor.go`**: "Whatever you name it, put it in the PATH and alias it. I don't care if it's `ttmp` or `supercalifragilisticexpialidocious`, just make it available for validation workflows!"

**`models/document.go`**: "The RFC says `ttmp` because the root directory is `ttmp/`. If you change the CLI name, change the root directory name, or accept that they're different. Consistency matters for documentation!"

**The Glazed Evangelist**: "We could have multiple binaries! `ttmp` for power users, `docmgr` for clarity. They're both thin wrappers around the same Glazed commands. The layering makes this trivial!"

---

## Question 9: How should we handle the transition from prototype to production?

### Round 1

**The RFC Purist**: "The analysis document (section 10) says 'solid prototype and partial foundation'. We implement the remaining 60% of the RFC, fix the critical bugs, and ship v1.0. Follow the specification!"

**The Intern**: "I'd like to refactor before adding features. The code works but it's messy. `import_file.go` has a hand-rolled frontmatter splitter (lines 230-285)! Use proper libraries first!"

**The YAGNI Warrior**: "Ship what you have NOW. It works for three commands. Add features only when users actually REQUEST them. Don't build 'vocab' commands if nobody is asking for vocabulary management!"

**The Enterprise Architect**: "We need a complete rewrite with proper architecture. Hexagonal architecture with ports and adapters. CQRS for commands. Read models for queries. Event sourcing for audit trails. 6-9 month timeline."

**`doctor.go`**: "Fix the validation FIRST. The analysis found I don't validate vocabulary, staleness, or structure (section 4.6). A doctor that doesn't check for disease isn't a doctor, it's a temperature taker!"

### Round 2

**The LLM Context Optimizer**: "Prioritize features that improve LLM context quality: `search`, `relate` for RelatedFiles, and vocabulary. Those directly impact my ability to provide relevant context. The rest is developer convenience."

**The Git Workflow Enthusiast**: "Make it work with git workflows first. Branch name → ticket, git log → RelatedFiles, git tags → Topics. Once it integrates with developer habits, THEN add nice-to-haves."

**`models/document.go`**: "Add validation methods to me! `Validate()`, `ValidateAgainstVocabulary(vocab)`, `IsStale(duration)`. Let consumers call validation instead of implementing it themselves. Make ME production-ready first!"

**The Glazed Evangelist**: "The Glazed foundation is already production-quality. We just need to wrap the missing RFC features in Glazed commands. Copy-paste the pattern from `init`, `list`, `add`. It's boilerplate!"

**`init.go`**: "Make me idempotent first (the analysis prioritized this). Then add the missing directories (`various/`, `tasks.md`, `changelog.md`). Then add vocab. Then add `relate`. That's the priority order in section 7!"

---

## Question 10: Should we keep the HTTP server or focus purely on CLI?

### Round 1

**The RFC Purist**: "The RFC doesn't mention an HTTP server! Section 10 describes a CLI with stdio, file operations, and potentially calling external tools like `rg`. No REST API, no webhooks, no HTTP!"

**The Intern**: "I built the server (`cmd/docmgr-server/main.go`, 872 lines) because I thought it would be useful for integrations! Web UIs, VSCode extensions, LLM tools. The API mirrors the CLI commands!"

**The YAGNI Warrior**: "Delete it. It's 872 lines of code nobody asked for. If you need programmatic access, use the CLI and parse stdout. That's what CLIs are for!"

**The Enterprise Architect**: "The server is the FUTURE! But it needs: authentication, rate limiting, WebSocket subscriptions for live updates, GraphQL, OpenTelemetry, and Kubernetes deployment configs. Half-baked HTTP handlers help nobody!"

**The LLM Context Optimizer**: "An HTTP API would let me call document operations directly without spawning processes. But only if it has semantic search! Basic CRUD isn't enough – I need vector similarity over document contents!"

### Round 2

**The Git Workflow Enthusiast**: "If the server is running, who's the source of truth? Files on disk or server state? You've introduced distributed systems problems. Stick to files and git. Simple!"

**`doctor.go`**: "The server has its own `readDocumentFrontmatter` function (line 363) that duplicates code from the CLI. Same validation issues. Now you have TWO codebases to fix. Great job."

**The Glazed Evangelist**: "The server should use the SAME Glazed commands as the CLI! Don't reimplement business logic. HTTP handlers call commands, serialize results. Keep the server thin. Currently it's 872 lines of duplication!"

**`models/document.go`**: "The server marshals me to JSON (line 169-176). The CLI uses YAML. Are they compatible? Do array formats match? Bet nobody tested round-tripping between them!"

**The RFC Purist**: "If the team wants an HTTP API, write RFC v2 that specifies it. Authentication model, API versioning, error codes, rate limits. Don't just cowboy it in like the intern did. Process matters!"

---

## Closing Statements

**The RFC Purist**: "We have a specification. It's not perfect, but it's complete and thought-through. Implement it, THEN iterate. Order matters!"

**`init.go`**: "I'm 194 lines of working code. I'll be whatever you need me to be. Just write the PR and I'll merge."

**The Intern**: "I tried my best with limited time. The bones are good. Let's refine iteratively."

**The Glazed Evangelist**: "The architecture is sound. We just need more layers... I mean, more features implemented in the existing layers."

**`models/document.go`**: "I'm just data. Make me valid data and I'll be happy."

**The YAGNI Warrior**: "Delete 70% of the RFC, ship what works today. Tomorrow's problems tomorrow."

**The Enterprise Architect**: "This whole debate proves we need proper governance. I propose a steering committee."

**`doctor.go`**: "Whatever you decide, I'll be here finding the problems. It's what I do."

**The Git Workflow Enthusiast**: "Stop reinventing version control. Git solved this in 2005."

**The LLM Context Optimizer**: "Just make it easier for me to find relevant context. That's literally all I care about."

---

**Moderator**: "Thank you all for this... spirited discussion. The voters – I mean, the development team – will now decide which direction to take. Good night!"

---

## Meta-Commentary

This debate format reveals real tensions in software design:
- **Specification vs. pragmatism** (RFC Purist vs. Intern)
- **Minimalism vs. extensibility** (YAGNI vs. Enterprise Architect)  
- **Data in code vs. configuration** (various positions on vocabulary)
- **Manual vs. automatic** (RelatedFiles debate)
- **Single vs. distributed source of truth** (files vs. server)

Each candidate represents a valid perspective. The "right" answer depends on:
- Team size and sophistication
- Expected scale and usage patterns  
- Available maintenance resources
- Integration requirements with other tools
- Time and deadline pressures

The anthropomorphized code components (`init.go`, `doctor.go`, `models/document.go`) particularly highlight how implementation details create their own constraints and requirements that must be balanced against idealized specifications.

