# Design: Porting the Ruby Welcome Bot to Kotlin + Kafka + ksqlDB

## 1. Purpose & Scope

This document outlines the design for migrating the functionality of the existing Ruby-based "welcome" bot to a new architecture using Kotlin, Apache Kafka, and ksqlDB. The goal is to leverage a stream-processing approach for handling events and managing any necessary state, replacing the current Ruby implementation and its state management (`Ai::Bot::StateDsl`).

The scope includes:
- Defining the event flow using Kafka topics.
- Specifying the event schemas.
- Outlining the responsibilities of the Kotlin service(s).
- Designing ksqlDB queries for event processing and state management (if required).
- Replacing the core welcome logic currently found in the Ruby `welcome` bot.

## 2. Current Ruby Implementation Overview (Based on `bot.md` and `state_dsl.md`)

The existing Ruby system uses a framework (`Ai::Bot::Bot`) with the following key components:

- **Main Bot Class (`welcome/bot.rb`)**: Inherits from `Ai::Bot::Bot`, defines service name, initializes an event filter, and likely contains event handling logic (e.g., `on_user_join`, `on_app_home_opened`). It uses middleware for processing.
- **Event Filter (`welcome/event_filter.rb`)**: Parses incoming raw events (e.g., from Slack) and transforms them into standardized internal event objects (like `Events::AppHomeOpenedEvent`, `Events::UserJoinedChannelEvent`).
- **State Management (`Ai::Bot::StateDsl`, used potentially by a `welcome/run.rb`)**: Manages the bot's state persistence, possibly tracking which users have been welcomed, interaction steps, etc., using defined states, attributes, and lookup mechanisms. State is saved with revision tracking.
- **Event/Command Dispatch**: The bot framework handles dispatching events and commands (e.g., sending a Slack message via `Services::Slack::Events::SendSlackMessageCommand`).

The Welcome bot likely listens for events indicating a user needs welcoming (e.g., joining a specific channel, opening the app home for the first time) and triggers an action, usually sending a welcome message via Slack. It might use the state (`Run` class) to prevent sending duplicate welcomes.

## 3. Proposed Kotlin + Kafka + ksqlDB Architecture

We will replace the Ruby bot logic with a system centered around Kafka streams processed by Kotlin applications and potentially ksqlDB for stateful operations.

### 3.1 Kafka Topics & Event Schemas

We need topics for incoming platform events and outgoing commands.

**Input Topics:**

1.  **`platform_slack_events` (Existing or New):**
    - Purpose: Raw events ingested from Slack (or potentially other platforms).
    - Schema (Example JSON): Varies depending on the Slack event type (`event_callback`, `interactive`, etc.). We're interested in `member_joined_channel`, `app_home_opened`, etc.
    ```json
    // Example: member_joined_channel
    {
      "token": "...",
      "team_id": "T123",
      "api_app_id": "A...",
      "event": {
        "type": "member_joined_channel",
        "user": "U456",
        "channel": "C789",
        "channel_type": "C",
        "team": "T123",
        "inviter": "UABC",
        "event_ts": "1683284892.001100"
      },
      "type": "event_callback",
      // ... other fields
    }
    ```
    ```json
    // Example: app_home_opened
    {
      "token": "...",
      "team_id": "T123",
      "api_app_id": "A...",
      "event": {
          "type": "app_home_opened",
          "user": "U456",
          "channel": "D...", // Direct message channel with the app
          "event_ts": "1683284995.001200",
          "tab": "home",
          "view": { /* ... view details ... */ }
      },
      "type": "event_callback",
      // ... other fields
    }
    ```

**Processed Event / State Topics (Managed by ksqlDB/Kotlin):**

2.  **`welcome_bot_user_channel_joins`:**
    - Purpose: A cleaned stream representing only user join events relevant to the welcome bot.
    - Schema (Example JSON):
    ```json
    {
       "eventId": "evt_abc...", // Unique ID for the event
       "eventType": "USER_JOINED_CHANNEL",
       "userId": "U456",
       "channelId": "C789",
       "teamId": "T123",
       "joinedAt": 1683284892001, // Epoch ms
       "isBot": false // Flag if the joined user is a bot
    }
    ```

3.  **`welcome_bot_app_home_opens`:**
    - Purpose: A cleaned stream representing app home open events.
    - Schema (Example JSON):
    ```json
    {
       "eventId": "evt_def...", // Unique ID for the event
       "eventType": "APP_HOME_OPENED",
       "userId": "U456",
       "teamId": "T123",
       "openedAt": 1683284995001 // Epoch ms
    }
    ```

4.  **`welcome_bot_welcomed_users` (ksqlDB Table Topic):**
    - Purpose: Stores the state of which user has been welcomed in which context (e.g., per channel or per team). This acts as the replacement for the Ruby `Run` state persistence.
    - Schema (Example JSON - Key: `userId-channelId` or `userId`):
    ```json
    // Example: Key = "U456-C789"
    {
       "userId": "U456",
       "channelId": "C789", // Nullable if welcome is per-user/team
       "teamId": "T123",
       "welcomedAt": 1683285000123, // Epoch ms
       "lastEventId": "evt_xyz..."
    }
    ```

**Output Command Topic:**

5.  **`command_slack_send_message` (Existing or New):**
    - Purpose: Commands for the Slack service to send messages.
    - Schema (Example JSON):
    ```json
    {
       "commandId": "cmd_ghi...", // Unique ID for the command
       "commandType": "SEND_SLACK_MESSAGE",
       "sourceService": "welcome-bot-kotlin",
       "target": {
         "channel": "C789", // or User ID for DM
         "threadTs": null // Optional
       },
       "payload": {
         "text": "Welcome to the channel, <@U456>!",
         "blocks": [ /* ... Slack blocks ... */ ]
       },
       "metadata": {
         "originatingEventId": "evt_abc..."
       }
    }
    ```

### 3.2 Kotlin Service(s) Responsibilities

A dedicated Kotlin service (`welcome-bot-service`) using Kafka Streams or potentially Spring Cloud Stream Kafka:

1.  **Event Ingestion & Filtering (If not done by ksqlDB):**
    - Consumes from `platform_slack_events`.
    - Filters for relevant events (`member_joined_channel`, `app_home_opened`).
    - Filters out events generated by bots (e.g., check `event.user` against known bot IDs or patterns).
    - Transforms raw Slack events into the standardized `welcome_bot_user_channel_joins` or `welcome_bot_app_home_opens` schema.
    - Produces the standardized events to their respective topics. *(Alternative: This filtering/transformation could be done first via ksqlDB).*

2.  **Welcome Logic & State Check:**
    - Consumes from `welcome_bot_user_channel_joins` and/or `welcome_bot_app_home_opens`.
    - **State Check:** For each relevant event (e.g., user join), it needs to check if the user has *already* been welcomed in that context. This involves querying the state managed by ksqlDB (the `welcome_bot_welcomed_users` table). This can be done via:
        - **Interactive Query:** The Kotlin service directly queries the ksqlDB table via REST API or a Kafka Streams `ReadOnlyKeyValueStore` mapped to the table's underlying topic.
        - **Stream-Table Join:** If using Kafka Streams, perform a KStream-KTable join between the incoming event stream and the `welcome_bot_welcomed_users` KTable.
    - **Decision:** If the user has *not* been welcomed yet:
        - Construct the welcome message (potentially personalized using event data).
        - Produce a `SEND_SLACK_MESSAGE` command to the `command_slack_send_message` topic.
        - Produce an event indicating the user *has now been welcomed* (this event will be consumed by ksqlDB to update the state table). *(Alternative: ksqlDB might update the table directly based on the join)*.

### 3.3 ksqlDB Streams & Tables

ksqlDB can handle the initial filtering/transformation and the state management.

1.  **Stream for Raw Events:**
    ```sql
    CREATE STREAM raw_slack_events (
        -- Define fields based on Slack's event structure
        type VARCHAR,
        team_id VARCHAR,
        event STRUCT<
            type VARCHAR,
            user VARCHAR,
            channel VARCHAR,
            -- ... other event fields
        >
        -- ... other top-level fields
    ) WITH (KAFKA_TOPIC='platform_slack_events', VALUE_FORMAT='JSON');
    ```

2.  **Stream for User Joins:**
    ```sql
    CREATE STREAM user_channel_joins WITH (KAFKA_TOPIC='welcome_bot_user_channel_joins', VALUE_FORMAT='JSON') AS
    SELECT
        GENERATE_UUID() AS eventId,
        'USER_JOINED_CHANNEL' AS eventType,
        event->user AS userId,
        event->channel AS channelId,
        team_id AS teamId,
        event->event_ts * 1000 AS joinedAt -- Assuming event_ts is seconds.epoch
        -- Add a check for bot users if possible, e.g. WHERE event->is_bot = false (if available)
    FROM raw_slack_events
    WHERE type = 'event_callback' AND event->type = 'member_joined_channel'
      -- Add filter for specific channels if needed: AND event->channel IN ('C123', 'C456')
      -- Add filter to exclude bot users if identifiable: AND NOT REGEXP_LIKE(event->user, '^B')
    EMIT CHANGES;
    ```
    *(Similar stream `app_home_opens_stream` for `app_home_opened` events if needed)*

3.  **Table for Welcomed Users State:**
    ```sql
    -- Key strategy depends on whether welcome is per-channel or per-user/team
    -- Option A: Per User Per Channel Welcome
    CREATE TABLE welcomed_users_state WITH (
        KAFKA_TOPIC='welcome_bot_welcomed_users',
        VALUE_FORMAT='JSON',
        KEY_FORMAT='JSON' -- Using JSON key for composite key
    ) AS
    SELECT
        -- Create a composite key
        STRUCT(userId := userId, channelId := channelId) AS userChannelKey,
        -- Aggregate: Take the latest event details for this user/channel combination
        LATEST_BY_OFFSET(userId) AS userId,
        LATEST_BY_OFFSET(channelId) AS channelId,
        LATEST_BY_OFFSET(teamId) AS teamId,
        LATEST_BY_OFFSET(joinedAt) AS welcomedAt, -- Timestamp of the event that triggered the welcome
        LATEST_BY_OFFSET(eventId) AS lastEventId
    FROM user_channel_joins -- Or potentially from a stream produced by the Kotlin app *after* sending the command
    -- Add WHERE clause if the stream contains events that shouldn't mark user as welcomed
    GROUP BY STRUCT(userId := userId, channelId := channelId) -- Group by the composite key
    EMIT CHANGES;

    -- Option B: Per User Welcome (Ignoring Channel)
    -- CREATE TABLE welcomed_users_state ... AS
    -- SELECT userId AS userIdKey, LATEST_BY_OFFSET(userId) AS userId, ...
    -- FROM ...
    -- GROUP BY userId
    -- EMIT CHANGES;
    ```
    **Note:** This table can be populated *either* directly from the filtered `user_channel_joins` stream (implying a welcome *should* happen) OR from a dedicated "user_was_welcomed" event stream produced by the Kotlin app *after* it successfully sends the welcome command. The latter is often safer to ensure state matches action taken.

### 3.4 Interaction Flow (Using ksqlDB for State)

1.  Raw Slack event (`member_joined_channel`) arrives in `platform_slack_events`.
2.  **ksqlDB:** Processes the raw event, filters/transforms it, and produces a message to `welcome_bot_user_channel_joins`.
3.  **Kotlin `welcome-bot-service`:** Consumes the message from `welcome_bot_user_channel_joins`.
4.  **Kotlin `welcome-bot-service`:** Queries the `welcomed_users_state` ksqlDB table (e.g., via interactive query `SELECT * FROM welcomed_users_state WHERE userId = 'U456' AND channelId = 'C789';`).
5.  **Kotlin `welcome-bot-service`:**
    - **If user IS found in the table:** Do nothing (already welcomed).
    - **If user IS NOT found:**
        a. Construct the Slack welcome message.
        b. Produce a command message to `command_slack_send_message`.
        c. **Crucially:** Produce a message to a topic like `welcome_bot_user_welcomed_event` (schema similar to `user_channel_joins`).
6.  **(If step 5c used): ksqlDB:** Consumes from `welcome_bot_user_welcomed_event` and updates the `welcomed_users_state` table, marking the user as welcomed.
7.  **Slack Service (Separate):** Consumes the command from `command_slack_send_message` and calls the Slack API to send the message.

### 3.5 Kotlin Service Logic (Conceptual View)

To visualize the core logic within the `welcome-bot-service`, abstracting away the Kafka Streams/Spring specifics, we can imagine it as a suspending function triggered by each relevant incoming event (like `UserChannelJoinEvent`).

```kotlin
// Represents the data class for the processed user join event
data class UserChannelJoinEvent(
    val eventId: String,
    val eventType: String, // "USER_JOINED_CHANNEL"
    val userId: String,
    val channelId: String,
    val teamId: String,
    val joinedAt: Long,
    val isBot: Boolean
)

// Represents the data class for the "user welcomed" event
data class UserWelcomedEvent(
    val eventId: String, // Often same as originating eventId or new
    val eventType: String, // "USER_WELCOMED"
    val userId: String,
    val channelId: String,
    val teamId: String,
    val welcomedAt: Long, // Timestamp when welcome was triggered
    val originatingEventId: String
)

// Represents the data class for the Slack command
data class SendSlackMessageCommand(
    val commandId: String,
    val commandType: String, // "SEND_SLACK_MESSAGE"
    val sourceService: String, // "welcome-bot-kotlin"
    val target: Target,
    val payload: Payload,
    val metadata: CommandMetadata
) {
    data class Target(val channel: String, val threadTs: String?)
    data class Payload(val text: String, val blocks: List<Any>?) // Represent blocks appropriately
    data class CommandMetadata(val originatingEventId: String)
}


// Conceptual interface for interacting with the ksqlDB state table
interface WelcomedUsersRepository {
    // Checks if a record exists for this user/channel combination
    suspend fun hasBeenWelcomed(userId: String, channelId: String): Boolean
}

// Conceptual interface for producing Kafka messages
interface KafkaEventProducer {
    suspend fun sendSlackCommand(command: SendSlackMessageCommand)
    suspend fun sendUserWelcomedEvent(event: UserWelcomedEvent)
}

// --- Conceptual Welcome Bot Processor ---

class WelcomeProcessor(
    private val welcomedUsersRepo: WelcomedUsersRepository,
    private val kafkaProducer: KafkaEventProducer
) {

    // This function represents the core logic executed for each UserChannelJoinEvent
    suspend fun processUserJoin(event: UserChannelJoinEvent) {
        // Ignore bots
        if (event.isBot) {
            // Log.debug("Ignoring join event for bot user: ${event.userId}")
            return
        }

        // 1. Check State: Has this user already been welcomed in this channel?
        val alreadyWelcomed = welcomedUsersRepo.hasBeenWelcomed(event.userId, event.channelId)

        if (!alreadyWelcomed) {
            // Log.info("User ${event.userId} needs welcoming in channel ${event.channelId}")

            // 2. Construct Welcome Command
            val welcomeText = "Welcome to the channel, <@${event.userId}>! :wave:"
            val command = SendSlackMessageCommand(
                commandId = "cmd_" + UUID.randomUUID().toString(),
                commandType = "SEND_SLACK_MESSAGE",
                sourceService = "welcome-bot-kotlin",
                target = SendSlackMessageCommand.Target(channel = event.channelId, threadTs = null),
                payload = SendSlackMessageCommand.Payload(text = welcomeText, blocks = null), // Add blocks if needed
                metadata = SendSlackMessageCommand.CommandMetadata(originatingEventId = event.eventId)
            )

            // 3. Produce Slack Command
            kafkaProducer.sendSlackCommand(command)
            // Log.info("Produced Slack command for user ${event.userId} in channel ${event.channelId}")

            // 4. Produce "User Welcomed" Event (to update the ksqlDB state table)
            val welcomedEvent = UserWelcomedEvent(
                eventId = "wlc_" + UUID.randomUUID().toString(),
                eventType = "USER_WELCOMED",
                userId = event.userId,
                channelId = event.channelId,
                teamId = event.teamId,
                welcomedAt = System.currentTimeMillis(), // Or use event timestamp if preferred
                originatingEventId = event.eventId
            )
            kafkaProducer.sendUserWelcomedEvent(welcomedEvent)
            // Log.info("Produced UserWelcomed event for user ${event.userId} in channel ${event.channelId}")

        } else {
            // Log.debug("User ${event.userId} already welcomed in channel ${event.channelId}, skipping.")
        }
        // Error handling (try-catch around repo/producer calls) would be added here
    }
}
```

This conceptual view highlights the sequence: check state -> potentially send command -> potentially update state (via event). The actual implementation using Kafka Streams/Spring would involve consumers, processors, state stores, and producers configured to achieve this flow reactively based on the Kafka topic events.

## 4. Comparison

**Pros of Kotlin + Kafka + ksqlDB:**

- **Scalability:** Leverages Kafka's distributed nature.
- **Decoupling:** Services interact via events, reducing direct dependencies compared to method calls in a monolith.
- **Persistence & Resilience:** Kafka provides durable event storage. State in ksqlDB is backed by Kafka topics.
- **Real-time Processing:** Suitable for handling event streams as they occur.
- **Standardization:** Uses common event formats and Kafka ecosystem tools.
- **State Reconstruction:** ksqlDB tables can be rebuilt from the underlying event topics if needed.

**Cons:**

- **Complexity:** Introduces more moving parts (Kafka, ksqlDB, Schema Registry, Kotlin service) compared to a single Ruby application.
- **Operational Overhead:** Requires managing and monitoring the Kafka cluster and ksqlDB.
- **Eventual Consistency:** State updates in ksqlDB are eventually consistent based on the event stream processing. Queries might briefly see stale data depending on the architecture.
- **Debugging:** Distributed tracing and debugging can be more challenging.
- **ksqlDB Limitations:** Complex state logic or joins might be cumbersome or less performant in ksqlDB compared to a dedicated database or application logic.

## 5. Next Steps

- Refine Kafka topic names and schemas.
- Decide on the exact mechanism for state checking (Interactive Query vs. Stream-Table Join) and state updating (direct from join vs. dedicated "welcomed" event).
- Implement the Kotlin `welcome-bot-service` using Kafka Streams or Spring Cloud Stream.
- Define and deploy the ksqlDB queries.
- Set up monitoring and alerting for the Kafka topics, ksqlDB queries, and Kotlin service.
- Integrate with a Schema Registry (e.g., Confluent Schema Registry) for managing event schemas.
- Plan data migration or handling for existing users if needed (ensure they aren't re-welcomed). 