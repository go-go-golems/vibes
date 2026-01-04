# Design: One-on-One V2 Bot – Kotlin Coroutines + **Redis Streams** & **Postgres**

> A **single** Kotlin service that consumes Redis Streams, persists state in PostgreSQL, and executes business logic with `suspend` functions.

---

## 1. Rationale

We replace Kafka/ksqlDB with technology already present in many deployments:

* **Redis Streams** (`XADD` / consumer groups) – lightweight message bus.
* **PostgreSQL** – transactional storage for session state (replaces `Run` table in ksqlDB).
* Coroutines (`async/await`) keep code sequential & testable.

This eliminates the operational cost of Kafka while retaining at‐least‐once delivery semantics via Redis **consumer-groups**.

---

## 2. Redis Streams Layout

| Stream | Purpose | Example Entry Fields |
|--------|---------|----------------------|
| `oneonone.cmd.prepare` | Manager triggers a new preparation run (scheduler or button) | `managerSlackId`, `startDate`, `endDate`, `directReportSlackId?` |
| `oneonone.cmd.regenerate` | Regenerate for a specific report | `runId`, `reportSlackId` |
| `oneonone.events.summary_ready` *(optional)* | If we externalise LLM step (not used in fully inline flow) | ... |
| `slack.commands` | Downstream commands to a Slack façade service | `type`, `payload` |

A **consumer group** (`oneonone_service`) is created for each inbound stream so that scaling horizontally is trivial.

---

## 3. Postgres Schema

```sql
-- Session state
CREATE TABLE runs (
  run_id          UUID PRIMARY KEY,
  manager_slack_id TEXT      NOT NULL,
  state            TEXT      NOT NULL,
  created_at       TIMESTAMPTZ DEFAULT now(),
  updated_at       TIMESTAMPTZ DEFAULT now()
);

CREATE TABLE run_results (
  run_id     UUID  REFERENCES runs(run_id) ON DELETE CASCADE,
  report_id  TEXT  NOT NULL,
  summary    TEXT,
  agenda     TEXT,
  PRIMARY KEY(run_id, report_id)
);
```

Add indices on `manager_slack_id` and `state` if querying frequently.

Persistence is handled via **jOOQ** or **Exposed** DSL.  Each update occurs within a coroutine-aware transaction (`suspend fun <T> tx { … }`).

---

## 4. Kotlin Service Components

```
┌─────────────────────────────────────────┐
│ RedisStreamConsumer.kt  (Lettuce)      │◄── subscribes to cmd streams
├─────────────────────────────────────────┤
│ Scheduler.kt       (coroutine ticker)  │──► XADD oneonone.cmd.prepare
├─────────────────────────────────────────┤
│ RunRepository.kt   (Postgres)          │
├─────────────────────────────────────────┤
│ SlackGateway.kt    (HTTP)              │
├─────────────────────────────────────────┤
│ OneOnOnePrepGateway.kt (HTTP/Llm)      │
└─────────────────────────────────────────┘
```

All are wired through Ktor DI or Spring Boot + Coroutines.

---

## 5. End-to-End Flow (Sequential Code)

```kotlin
suspend fun handlePrepareEntry(entry: Map<String,String>) {
    val managerId = entry["managerSlackId"]!!
    val range = DateRange(entry["startDate"].toLocalDate(), entry["endDate"].toLocalDate())

    val reports = people.fetchDirectReports(managerId)
    if (reports.isEmpty()) return

    val runId = runRepo.openRun(managerId)

    coroutineScope {
        reports.map { rep ->
            async {
                val summary = oneOnOnePrep.generateSummary(managerId, rep.slackId)
                val agenda  = oneOnOnePrep.generateAgenda(summary)

                runRepo.saveResult(runId, rep.slackId, summary, agenda)
                slack.postAgenda(managerId, rep.slackId, agenda)
            }
        }.awaitAll()
    }

    runRepo.markCompleted(runId)
}
```

### Regeneration Command

```kotlin
suspend fun handleRegenerate(entry: Map<String,String>) {
    val runId = UUID.fromString(entry["runId"]!!)
    val reportId = entry["reportSlackId"]!!

    val run = runRepo.require(runId)
    val summary = oneOnOnePrep.generateSummary(run.managerSlackId, reportId)
    val agenda  = oneOnOnePrep.generateAgenda(summary)

    runRepo.saveResult(runId, reportId, summary, agenda, upsert=true)
    slack.updateAgenda(run.managerSlackId, reportId, agenda)
}
```

---

## 6. Redis Consumer Implementation Sketch

```kotlin
class RedisStreamConsumer(
    private val client: RedisClient,
    private val prepareHandler: suspend (Map<String,String>)->Unit,
    private val regenHandler: suspend (Map<String,String>)->Unit
) {
    private val connection = client.connect()
    private val cmds = connection.async()

    fun CoroutineScope.start() = launch {
        ensureGroup("oneonone.cmd.prepare", "oneonone_service")
        ensureGroup("oneonone.cmd.regenerate", "oneonone_service")

        while(isActive) {
            val resp = cmds.xreadgroup(
                Consumer.from("oneonone_service", hostname()),
                XReadArgs.Builder.count(10).block(1_000),
                StreamOffset.lastConsumed("oneonone.cmd.prepare"),
                StreamOffset.lastConsumed("oneonone.cmd.regenerate")
            ).await()

            resp.forEach { rec ->
                when(rec.stream) {
                    "oneonone.cmd.prepare"    -> launch { prepareHandler(rec.body) }
                    "oneonone.cmd.regenerate" -> launch { regenHandler(rec.body) }
                }
                cmds.xack(rec.stream, "oneonone_service", rec.id)
            }
        }
    }
}
```

---

## 7. Scheduler

Runs every minute via `delay` and calculates Monday 09:00 in user TZ.  Upon due, it `XADD`s into `oneonone.cmd.prepare`.

```kotlin
fun CoroutineScope.startScheduler() = launch {
  while(isActive) {
    val due = userRepo.findManagersDue(nowUtc())
    due.forEach { mgr ->
       redis.xadd("oneonone.cmd.prepare", mapOf(
          "managerSlackId" to mgr.slackId,
          "startDate" to lastMonday().toString(),
          "endDate" to LocalDate.now().toString()
       ))
    }
    delay(Duration.ofMinutes(1))
  }
}
```

---

## 8. Error & Retry Strategy

* **Redis Ack after success** – messages re-delivered if consumer crashes before `XACK`.
* **Db transactions** – `SAVEPOINT` to avoid partial writes.
* **Slack / LLM** – wrap in `retry(3) { … }` and log to Sentry.
* **Idempotency** – composite `(run_id, report_id)` primary key ensures upserts.

---

## 9. Testing

* Unit tests with [TestContainers](https://www.testcontainers.org/) spinning up Redis & Postgres.
* Coroutine test scope with `runTest`.
* Integration test ensures consumer picks up XADDed entry and writes to DB.

---

## 10. Deployment Notes

* Service requires ENV vars: `REDIS_URL`, `PG_DSN`, `SLACK_TOKEN`, `PREP_API_KEY`.
* Health endpoints: `/healthz` (HTTP) checks Redis PING & `SELECT 1` on Postgres.
* Use **Flyway** for schema migrations.

---

## 11. Benefits vs Kafka Setup

| Aspect | Redis Streams + Postgres | Kafka + ksqlDB |
|--------|-------------------------|----------------|
| Footprint | Single Redis + existing Postgres | Kafka brokers + ksqlDB + ZooKeeper |
| Latency | sub-millisecond | few ms |
| Ordering | Per-stream guaranteed | Per-partition |
| Tooling | Built-in Redis CLI | Full Kafka eco-system |

Redis is sufficient for moderate traffic (< tens of thousands msg/sec) and keeps ops simple.

---

## 12. Next Steps

1. Provision Redis 6.x with AOF persistence.
2. Add Flyway migration for `runs` tables.
3. Scaffold Kotlin project (`ktor` + gradle).
4. Implement `RunRepository` using Exposed + PostgresPool.
5. Implement consumer & scheduler coroutines.
6. Dark-launch alongside Ruby bot. 