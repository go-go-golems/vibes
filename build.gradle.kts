// Protobuf configuration
protobuf {
    protoc {
        artifact = "com.google.protobuf:protoc:3.25.1"
    }
    generateProtoTasks {
        all().forEach { task ->
            task.builtins {
                // Generate Java code (needed by Kotlin Protobuf)
                java { option("lite") }
                // Generate Kotlin code
                kotlin { option("lite") }
            }
        }
    }
}

// Add generated sources to the Kotlin source set
sourceSets {
    main {
        java {
            srcDirs("build/generated/source/proto/main/java", "build/generated/source/proto/main/grpc")
        }
        kotlin {
            srcDirs("build/generated/source/proto/main/kotlin", "build/generated/source/proto/main/grpckt")
        }
    }
}

// Ensure compileKotlin depends on generateProto
tasks.withType<org.jetbrains.kotlin.gradle.tasks.KotlinCompile>().configureEach {
    dependsOn(tasks.withType<com.google.protobuf.gradle.GenerateProtoTask>())
} 