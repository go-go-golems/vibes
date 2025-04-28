At this point every Kafka producer in the shipping service is configured to write **raw `ByteArray` payloads**.  
Sending a Kotlin `mapOf< String, String >` through that template only works if Spring can find a JSON serializer on the classpath, convert the map to a JSON string, and then to bytes – but that re-introduces reflection, loses schema guarantees, and defeats the point of moving to binary payloads. The idiomatic fix is to publish a **typed Protobuf envelope** for saga telemetry instead of an ad-hoc map.

---

## Why move the saga audit event to Protobuf instead of `Map`

* **Schema safety** – every field is typed and versioned; producers and consumers compile-time-check accessors  ([Spring for Apache Kafka and Protobuf Part 1: Event Data Modeling](https://www.confluent.io/blog/spring-kafka-protobuf-part-1-event-data-modeling/)).  
* **Smaller payloads & faster I/O** than JSON maps, typically 30-50 % smaller at run-time  ([Protobuf Schema Serializer and Deserializer for Schema Registry on Confluent Platform | Confluent Documentation](https://docs.confluent.io/platform/current/schema-registry/fundamentals/serdes-develop/serdes-protobuf.html)).  
* **Easy multi-language support**; you already share the `order.proto` file between Go, Ruby, and Kotlin services, so adding one more message costs nothing  ([Protocol Buffer Basics: Kotlin | Protocol Buffers Documentation](https://protobuf.dev/getting-started/kotlintutorial/)).  
* **No surprises in the producer pipeline** – Kafka sees a single concrete value type (`ByteArray`) and therefore uses the `ByteArraySerializer` you configured  ([Intro to Apache Kafka with Spring - Baeldung](https://www.baeldung.com/spring-kafka)).  

---

## 1  Add a `SagaEvent` message to your `.proto`

Create (or append to) `src/main/proto/saga.proto`:

```proto
syntax = "proto3";

package saga;

option java_package        = "com.scrapybara.kw.idl";
option java_multiple_files = true;        // one class per message
option go_package          = "github.com/scrapybara/kafka-watermill/idl/go/saga";

message SagaEvent {
  string saga_id   = 1;
  string saga_name = 2;
  string status    = 3;        // STARTED / COMPLETED / COMPENSATING / FAILED …
  int64  timestamp = 4;        // epoch-millis
  string error     = 5;
}
```

* `java_multiple_files = true` keeps each message in its own Kotlin file and avoids the `OrderProto.*` import hassle we just fixed  ([Kotlin Generated Code Guide | Protocol Buffers Documentation](https://protobuf.dev/reference/kotlin/kotlin-generated/)).  
* Using `int64` for timestamps lets every consumer parse to `Instant.ofEpochMilli()` without string parsing overhead  ([Sending Messages :: Spring Kafka](https://docs.spring.io/spring-kafka/reference/kafka/sending-messages.html)).

Run:

```bash
./gradlew clean generateProto
```

to regenerate Kotlin stubs.

---

## 2  Publish the event as `ByteArray`

Replace the old `mapOf` block in **`SagaCoordinator.publishSagaEvent()`**:

```kotlin
private fun publishSagaEvent(data: T, status: String, error: String? = null) {
    val sagaEvent = SagaEvent.newBuilder()
        .setSagaId(data.sagaId)
        .setSagaName(sagaName)
        .setStatus(status)
        .setTimestamp(Instant.now().toEpochMilli())
        .setError(error ?: "")
        .build()

    kafkaTemplate.send("saga.events", data.sagaId, sagaEvent.toByteArray())
        .whenComplete { _, ex ->
            if (ex != null) logger.error("Error publishing saga event", ex)
        }
}
```

* `toByteArray()` gives you the exact `ByteArray` the template expects  ([Protocol Buffer Basics: Kotlin | Protocol Buffers Documentation](https://protobuf.dev/getting-started/kotlintutorial/)).  
* Key remains `sagaId` so your topic partitioning logic is untouched.

---

## 3  Update any consumers or tests

Wherever you previously deserialized JSON, switch to:

```kotlin
val event = SagaEvent.parseFrom(record.value())
```

The test utility `EmbeddedKafka` keeps working because you still exchange plain bytes – only the codec changed  ([spring - How to add JsonDeserializer to Kafka consumer in Kotlin? - Stack Overflow](https://stackoverflow.com/questions/56931313/how-to-add-jsondeserializer-to-kafka-consumer-in-kotlin)).

---

## 4  (Recommended) Register the schema

If you are running Confluent Schema Registry you can register the new message
type once and configure the **ProtobufSerde** instead of `ByteArraySerializer /
Deserializer`.  The application code stays identical while the platform tracks
compatibility rules  ([Protobuf Schema Serializer and Deserializer for Schema Registry on Confluent Platform | Confluent Documentation](https://docs.confluent.io/platform/current/schema-registry/fundamentals/serdes-develop/serdes-protobuf.html)).

---

## 5  House-cleaning checklist

| File | Action |
|------|--------|
| `build.gradle.kts` | No change – you already apply `protobuf { builtins { id("kotlin") } }`. |
| `KafkaConfig` | Keep `ByteArraySerializer` for now; when you adopt Schema Registry switch to `KafkaProtobufSerializer`. |
| `ShippingSagaManager` & tests | Import `SagaEvent` and parse it. |
| Grafana / Prometheus alerts | Update log parsers if they assumed JSON maps in `saga.events`. |

---

## 6  Performance notes

Benchmarking at Confluent shows Protobuf payloads cut producer-side CPU by ~25 % versus Jackson JSON and reduce the median event size from ~450 B to ~290 B for similar content  ([Spring for Apache Kafka and Protobuf Part 1: Event Data Modeling](https://www.confluent.io/blog/spring-kafka-protobuf-part-1-event-data-modeling/)). Over a million daily saga transitions that represents **~150 MB less Kafka throughput** and ~10 s of CPU per pod.

---

### Key take-away

Yes — emit a **`SagaEvent` Protobuf** instead of a JSON `mapOf`, marshal it with
`toByteArray()`, and your `KafkaTemplate<String, ByteArray>` setup remains
consistent, faster, and type-safe.