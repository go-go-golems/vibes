import com.google.protobuf.gradle.* // Needed for id() calls in protobuf block

plugins {
    kotlin("jvm") version "1.9.21"
    kotlin("plugin.spring") version "1.9.21"
    id("org.springframework.boot") version "3.2.0"
    id("io.spring.dependency-management") version "1.1.4"
    id("com.google.protobuf") version "0.9.4"
    application
}

group = "com.scrapybara.kw"
version = "0.1.0"

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.springframework.boot:spring-boot-starter-web")
    implementation("org.springframework.boot:spring-boot-starter-actuator")
    implementation("org.springframework.kafka:spring-kafka")
    implementation("com.fasterxml.jackson.module:jackson-module-kotlin")
    implementation("org.jetbrains.kotlin:kotlin-reflect")
    implementation("org.jetbrains.kotlin:kotlin-stdlib-jdk8")
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.7.3")
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-reactor:1.7.3")
    implementation("io.micrometer:micrometer-core:1.12.0")
    implementation("com.google.protobuf:protobuf-java:3.25.1")
    implementation("com.google.protobuf:protobuf-kotlin:3.25.1")
    implementation("org.springframework.retry:spring-retry")
    implementation("org.aspectj:aspectjrt")
    
    testImplementation("org.springframework.boot:spring-boot-starter-test")
    testImplementation("org.springframework.kafka:spring-kafka-test")
    testImplementation(kotlin("test"))
}

tasks.test {
    useJUnitPlatform()
}

kotlin {
    jvmToolchain(17)
}

application {
    mainClass.set("com.scrapybara.kw.shipping.ShippingServiceApplicationKt")
}

// Protobuf configuration
protobuf {
    protoc {
        artifact = "com.google.protobuf:protoc:3.25.1"
    }
    generateProtoTasks {
        all().forEach { task ->
            // Configure Kotlin output
            task.builtins {
                id("kotlin") { } // Enable Kotlin generator
            }
            // Optional: Configure Java output if needed alongside Kotlin
            // task.builtins {
            //     id("java") { option("lite") } 
            // }
            // Ensure generated sources are added to the correct source sets
            // task.plugins {
            //     id("grpc") { // Assuming gRPC might be used later, keep plugin structure
            //        // artifact = "io.grpc:protoc-gen-grpc-java:1.+" // Example if using gRPC
            //     }
            // }
        }
    }
}