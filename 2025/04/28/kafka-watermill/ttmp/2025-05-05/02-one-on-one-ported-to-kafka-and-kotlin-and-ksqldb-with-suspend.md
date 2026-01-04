# Design: Porting the One-on-One V2 Bot to Kotlin + Kafka + ksqlDB (Coroutine / `suspend` Style)

## 1. Purpose & Scope

This document proposes a rewrite of the Ruby **One-on-One V2** bot as a *single* Kotlin service that leverages Apache Kafka and ksqlDB for event transport & state, but **handles all logic through suspending (`async/await`) functions** rather than a classic event-processor DAG.  The migration keeps feature-parity while simplifying operational complexity and embracing Kotlin coroutines for sequential-looking code.

Covered here:

* Kafka topic design and Avro/JSON schemas
* ksqlDB streams & tables that replace the `Run` state
* Kotlin service architecture & coroutine flow (pseudo-code)
* Scheduler replacement (cron-like) inside the same service
* Error-handling, retries and idempotency concerns

> TL;DR – The Ruby bot's state DSL and asynchronous callbacks become **awaitable repository calls** to ksqlDB, while Slack & OneOnOnePrep calls are made with suspending HTTP clients.

---

## 2. Current Ruby Bot Recap

| Concern | Ruby Implementation |
|---------|--------------------|
| **Trigger/schedule** | `OneOnOneScheduler.run` creates/updates `ScheduledEvent` DB rows with cron syntax |
| **Kick-off event** | `PrepareOneOnOneCommand` is queued and later dispatched |
| **State** | `Run` class via `Ai::Bot::StateDsl` persisted in Redis/Postgres |
| **External services** | Slack Web API (sync), `OneOnOnePrep` (async) |
| **UI** | Slack Home-tab & message blocks with interactive buttons |
| **Regeneration flow** | Button → `BlockActionEvent` → `handle_regenerate` |

---

## 3. Kafka & ksqlDB Design

### 3.1 Topics

| Topic | Direction | Purpose |
|-------|-----------|---------|
| `oneonone.preparation.commands` | IN |  New *command* requesting preparation for a manager (payload mirrors `PrepareOneOnOne` arguments).  Produced by the in-service scheduler *or* external systems. |
| `oneonone.preparation.results` | IN |  Responses from **OneOnOnePrep** micro-service containing summaries & agendas. |
| `slack.commands` | OUT | Commands for a Slack façade service (send / update messages, open modal). |
| `oneonone.userwelcomed` | OUT | (optional) internal audit events. |

### 3.2 ksqlDB Tables

1. **`oneonone_runs`** – materialised view of *active* runs indexed by `runId`.
```
CREATE TABLE oneonone_runs (
  runId STRING PRIMARY KEY,
  managerSlackId STRING,
  state STRING,              -- initializing / generating / completed
  summary MAP<STRING,STRING>,
  agenda MAP<STRING,STRING>,
  displayFor ARRAY<STRING>,   -- ids currently expanded in UI
  relatedUsers ARRAY<STRING>,
  startDate BIGINT,
  endDate BIGINT,
  createdAt BIGINT,
  updatedAt BIGINT
) WITH (KAFKA_TOPIC='oneonone_runs', VALUE_FORMAT='JSON');
```
2. **`oneonone_schedules`** – cron meta (optional if scheduler stays in service memory).

---

## 4. Kotlin Service: High-Level Modules

```
┌──────────────────────────────────────────────────────────────┐
│ OneOnOneService (Spring-Boot / Ktor)                        │
│                                                              │
│ • SchedulerCoroutine  ──► produces preparation.commands      │
│ • KafkaConsumerCoroutine                                    │
│        - listen preparation.commands (kick-off)              │
│        - listen preparation.results  (LLM replies)           │
│ • RunRepository (wraps ksqlDB REST)                          │
│ • SlackGateway  (suspend HTTP)                               │
│ • OneOnOnePrepGateway (suspend HTTP)                         │
└──────────────────────────────────────────────────────────────┘
```

All background loops are *launched* in `main` using `CoroutineScope(Dispatchers.IO)` and coordinated with `SupervisorJob()`.

---

## 5. Coroutine-Based Logic Flow

### 5.1 Kick-off (`PrepareOneOnOne`)

Instead of waiting for a *separate* Kafka event (`PrepGenerated`) we can perform **all** network / AI calls inline in the same coroutine, giving us a compact, synchronous-looking handler:

```kotlin
suspend fun runOneOnOne(managerSlackId: String, reports: List<SlackUser>, range: DateRange) = coroutineScope {
    // Map each report to a concurrently running coroutine
    reports.map { report ->
        async {
            // Step-1  : Gather follow-ups (Slack search, CRM, etc.)
            val followUps = followUpGateway.fetch(report.slackId, managerSlackId, range)

            // Step-2  : Ask OneOnOnePrep (LLM) for a summary  – suspends on HTTP
            val summary = prepGateway.generateSummary(managerSlackId, report.slackId, followUps)

            // Step-3  : Transform summary into an agenda – also LLM or local rules
            val agenda  = prepGateway.generateAgenda(summary)

            // Step-4  : Post blocks back to Slack  – suspends on HTTP
            slack.postAgendaBlocks(managerSlackId, report.slackId, agenda)

            // Optionally persist result into ksqlDB table
            runRepo.addResultIfNotExists(
                managerSlackId = managerSlackId,
                reportId       = report.slackId,
                summary        = summary,
                agenda         = agenda
            )
        }
    }.awaitAll()  // Wait for all children to finish

    // Once every async child is done, mark run completed
    runRepo.transitionToCompleted(managerSlackId)
}
```

> The **key change** is that we no longer rely on a `preparation.results` topic.  The network round-trip to `OneOnOnePrep` is awaited immediately, making the flow deterministic and readably linear.

The kick-off handler therefore becomes:

```kotlin
suspend fun handlePrepareCommand(cmd: PrepareCmd) {
    val reports = peopleService.fetchDirectReports(cmd.managerSlackId)
    if (reports.isEmpty()) return

    runRepo.openRun(cmd, reports)

    // fire-and-await
    runOneOnOne(cmd.managerSlackId, reports, DateRange(cmd.startDate, cmd.endDate))
}
```

### 5.2 UI Regeneration Flow (unchanged)

Interactive Slack callbacks arrive via HTTPS endpoint (not Kafka).  The service handles them with:

```kotlin
post("/slack/actions") { payload ->
    launch {
        val runId = payload.value // value holds runId
        val reportId = payload.actionId.removePrefix("regenerate_")
        val run = runRepo.find(runId) ?: return@launch

        runRepo.setDisplayAgenda(runId, reportId, true)

        val blocks = SlackBlocks.compose(run)
        slack.updateMessage(run.channelId, run.messageTs, blocks)

        // Kick new generation
        val _ = prepGateway.requestPrep(...)
    }
    respond 200
}
```

> **Note** – Because we keep everything in a single service the Slack HTTP route can operate directly on the same repository instance.

---

## 6. Scheduler Within the Service

The Ruby scheduler created `ScheduledEvent` rows; we instead launch a long-running coroutine:

```kotlin
fun CoroutineScope.startScheduler() = launch {
    while (isActive) {
        val now = ZonedDateTime.now(ZoneOffset.UTC)
        val users = userRepo.findManagersNeedingPrep(now)
        users.forEach { u ->
            prepareCmdProducer.send(PrepareCmd(u.slackId, defaultStart(), defaultEnd()))
        }
        delay(Duration.ofMinutes(1))
    }
}
```

The *cron* calculation (Monday 09:00 in manager's TZ) is performed in `findManagersNeedingPrep` using stored TZ information, replicating `determine_schedule`.

---

## 7. Idempotency & Error Strategy

* **Kafka keying** – `runId` used as key for both commands and results so retries overwrite.
* **Slack updates** – message `ts` persisted; `updateMessage` wrapped with exponential back-off.
* **OneOnOnePrep gateway** – outgoing HTTP retried with `RetryPolicy` (KotlinX-retry) and circuit-breaker.
* **Repo operations** – `INSERT … ON DUPLICATE KEY UPDATE` semantics via ksqlDB `INSERT INTO … KEY`.

---

## 8. Migration Steps

1. Stand-up Kafka topics & ksqlDB tables (DDL above).
2. Scaffold Kotlin project with Spring Boot 3 + kotlinx-coroutines + Kafka client.
3. Implement `RunRepository` accessing `oneonone_runs` through ksqlDB REST (`/query-stream` for reads, plain producer for writes).
4. Port scheduler logic.
5. Integrate Slack & Prep gateways.
6. Shadow-traffic mode: produce commands but keep Ruby bot active.
7. Cut-over, monitor.

---

## 9. Appendix – Data Classes

```kotlin
data class PrepareCmd(
    val managerSlackId: String,
    val directReportSlackId: String? = null,
    val startDate: LocalDate,
    val endDate: LocalDate
)

data class PrepGenerated(
    val runId: String,
    val reportSlackId: String,
    val summary: String,
    val agenda: String
)
```

---

## 10. Benefits of the Coroutine Approach

* **Linear flow** – Business logic remains easy to read; less cognitive overhead than reactive DSLs.
* **Single deployment** – Scheduler, HTTP endpoint, and Kafka I/O all live in one artifact.
* **Back-pressure aware** – coroutines suspend on I/O allowing efficient threads.
* **Testability** – functions like `handlePrepareCommand` can be unit-tested with pre-canned repositories.
* **No explicit state machines** – the `Run` table and helper repository replace the DSL-based Ruby state. 