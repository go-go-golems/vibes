Below is the quickest path to get **Kotlin data classes and builders** out of `src/main/proto/order.proto`, and why the build is currently failing.

---

### 1  Why the script will not compile

In a **Gradle Kotlin-DSL** script the helper functions `id("kotlin")` and `id("grpc")` live in the package `com.google.protobuf.gradle`.  
Because that package is **not imported** at the top of `build.gradle.kts`, the compiler cannot see the two `id(…)` calls on lines 56 and 64 and aborts with *Unresolved reference: id* – exactly the error you are seeing .

---

### 2  Minimal fix

Add a single star-import right at the top of `build.gradle.kts` **before** the `plugins { … }` block:

```kotlin
import com.google.protobuf.gradle.*
```

That brings the extension functions into scope and the script will compile.

---

### 3  Complete, working `build.gradle.kts` excerpt

```kotlin
import com.google.protobuf.gradle.*        // <-- NEW

plugins {
    kotlin("jvm") version "1.9.21"
    kotlin("plugin.spring") version "1.9.21"
    id("org.springframework.boot") version "3.2.0"
    id("io.spring.dependency-management") version "1.1.4"
    id("com.google.protobuf") version "0.9.4"
    application
}

dependencies {
    implementation("com.google.protobuf:protobuf-java:3.25.1")
    implementation("com.google.protobuf:protobuf-kotlin:3.25.1")
    // …your other deps…
}

protobuf {
    protoc { artifact = "com.google.protobuf:protoc:3.25.1" }

    // declare the gRPC plugin once
    plugins {
        id("grpc") {
            artifact = "io.grpc:protoc-gen-grpc-java:1.63.0"
        }
    }

    // and wire both the Kotlin builtin & the gRPC plugin into every task
    generateProtoTasks {
        all().forEach { task ->
            task.builtins {
                id("kotlin")           // Kotlin codegen
            }
            task.plugins {
                id("grpc")             // Java gRPC stubs (optional)
            }
        }
    }
}
```

Nothing else in the file needs to change.

---

### 4  Directory layout that Gradle expects

```
kotlin-service/
└── src
    └── main
        └── proto
            └── order.proto
```

Your `order.proto` already lives there and declares the desired Java/Kotlin package , so no further tweaks are necessary.

---

### 5  Generate the code

```bash
cd kotlin-service
./gradlew clean generateProto
```

* Generated Kotlin sources → `build/generated/source/proto/main/kotlin`
* (Optional) Generated Java gRPC stubs → `build/generated/source/proto/main/grpc`

Gradle automatically adds those directories to the compilation classpath.

---

### 6  Using the generated classes

The package in the `.proto` options (`option java_package = "com.scrapybara.kw.idl";`) means you simply:

```kotlin
import com.scrapybara.kw.idl.OrderCreated

val event = OrderCreated.newBuilder()
    .setOrderId("42")
    .setUserId("u-17")
    .setTotalAmount(123.45f)
    .build()
```

The `newBuilder()` + `build()` API is provided for every message; lists such as `items` have the usual `addItems()`/`addAllItems()` helpers.

---

### 7  If you prefer the “maybeCreate” style instead

If you want to avoid the extra import entirely, replace the two `id(…)` calls with:

```kotlin
task.builtins { maybeCreate("kotlin") }
task.plugins  { maybeCreate("grpc")   }
```

`maybeCreate` is always in scope, even without the import.

---

### 8  Run & profit

1. `./gradlew clean build`  
2. `./gradlew bootRun` (or run the Spring Boot main class from your IDE)  
3. Generated `Order*` classes are on the classpath; Kafka payloads can be produced via `event.toByteArray()` as in the existing codebase .

That’s all that is required to “properly create the proper objects in Kotlin from `order.proto`”.