Below is a one-pass patch list that eliminates every symbol the compiler now complains about and gets `./gradlew bootRun` to a clean start. The fixes fall into four groups.

---

## Quick recap of the root causes

| Error | Root cause | Fix in one line |
|-------|------------|-----------------|
| `shippingProcessingTime.start()` → **Unresolved reference start** | Micrometer’s `Timer` doesn’t have an instance method `start()`; `Timer.start(registry)` is a _static_ helper. | Replace it with `val sample = Timer.start(registry)` and later `sample.stop(shippingProcessingTime)` |
| `InventoryChecked`, `OrderFulfilled`, `OrderCancelled` → **Unresolved reference** | `java_outer_classname = "OrderProto"` keeps all messages nested inside `OrderProto.*`; code imports the wrong top-level names. | Either add `option java_multiple_files = true;` to `order.proto` **or** change every import to `OrderProto.InventoryChecked` (recommended: flip the proto option). |
| `incrementOrdersFulfilled / incrementOrdersCancelled` → **Unresolved reference** | The helper methods are called but never defined in `ShippingMetrics`. | Add two small wrappers that call the counters’ `increment()`. |
| `KafkaTemplate<String, ByteArray>` vs `<String, Any>` → **Type mismatch** | `SagaCoordinatorFactory` is typed to `Any` but callers inject a `ByteArray` template. | Parameterise the factory (or just change the type) to `KafkaTemplate<String, ByteArray>`. |

---

## 1  Micrometer timer call

In `ShippingServiceApplication.kt` the code currently does this :

```kotlin
val sample = metrics.shippingProcessingTime.start()
```

`Timer` has no such member; instead create a `Timer.Sample` with the registry  ([Timers :: Micrometer](https://docs.micrometer.io/micrometer/reference/concepts/timers.html?utm_source=chatgpt.com)):

```kotlin
val sample = Timer.start(registry)           // registry is already available via MetricsConfig
…
sample.stop(metrics.shippingProcessingTime)
```

If you do not want to pass the registry around, drop the manual timing entirely and annotate the method with `@Timed` (Spring Boot autoconfigures `TimedAspect` automatically  ([Spring-boot micrometer timer () how does it work? - Stack Overflow](https://stackoverflow.com/questions/51952855/spring-boot-micrometer-timer-how-does-it-work?utm_source=chatgpt.com))).

---

## 2  Make the generated Kotlin classes visible

`order.proto` declares an outer wrapper class through `java_outer_classname = "OrderProto"` . Therefore the generator emits `OrderProto.InventoryChecked`, `OrderProto.OrderCancelled`, and so on, not top-level classes named `InventoryChecked` etc., and the current imports break  .

### Option A (preferred) – turn off the wrapper

Add **one line** right after the other options in `src/main/proto/order.proto`:

```proto
option java_multiple_files = true;
```

The Kotlin generator honours the Java option and now writes a separate `.kt` file for each top-level message  ([Kotlin Generated Code Guide | Protocol Buffers Documentation](https://protobuf.dev/reference/kotlin/kotlin-generated/?utm_source=chatgpt.com)). Re-run

```bash
./gradlew clean generateProto
```

and your original imports compile.

### Option B – keep the wrapper

If you like the outer class, change every import to use it:

```kotlin
import com.scrapybara.kw.idl.OrderProto.InventoryChecked
```

Do the same for `OrderFulfilled`, `OrderCancelled`, `UnavailableItem`, etc.

---

## 3  Add the missing metric helpers

`ShippingServiceApplication.kt` calls `metrics.incrementOrdersFulfilled()` and `metrics.incrementOrdersCancelled()` , but `ShippingMetrics` holds only the bare counters . Add simple helpers inside `ShippingMetrics`:

```kotlin
fun incrementOrdersFulfilled() = ordersFulfilled.increment()
fun incrementOrdersCancelled() = ordersCancelled.increment()
```

Now both symbols resolve.

---

## 4  Align the saga KafkaTemplate type

`ShippingSagaManager` receives `KafkaTemplate<String, ByteArray>` , but `SagaCoordinatorFactory` expects `<String, Any>` . Pick one type—`ByteArray` is already used for the event payloads—and fix the factory:

```kotlin
class SagaCoordinatorFactory(
    private val kafkaTemplate: KafkaTemplate<String, ByteArray>
)
```

Likewise adjust the field in `SagaCoordinator` if necessary.

---

## 5  Build order & deprecation warnings

* `generateProto` already runs before `compileKotlin`; if you still see “class not found”, do **`clean generateProto compileKotlin`** once to rule out stale `.class` files.
* The Gradle 9 deprecation notice is only a **warning**; fix it later by bumping plugin versions once your code compiles.

---

### TL;DR

1. Use `Timer.start(registry)` + `sample.stop(timer)` for Micrometer timing.  
2. Add `option java_multiple_files = true;` to `order.proto` (or import via `OrderProto.*`).  
3. Add two increment helpers to `ShippingMetrics`.  
4. Change `SagaCoordinatorFactory` to `KafkaTemplate<String, ByteArray>`.

Re-run `./gradlew clean build && ./gradlew bootRun`—the project should start without the earlier compile-time errors.