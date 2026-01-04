FROM gradle:8.5-jdk17 AS builder

# Set working directory
WORKDIR /app

# Copy Gradle files
COPY kotlin-service/build.gradle.kts kotlin-service/settings.gradle.kts ./

# Copy Gradle wrapper files
COPY kotlin-service/gradle ./gradle
COPY kotlin-service/gradlew ./

# Copy IDL files
COPY idl/ ./idl/

# Copy source code
COPY kotlin-service/src ./src

# Build the application
RUN gradle bootJar --no-daemon

# Final image
FROM openjdk:17-slim

# Set working directory
WORKDIR /app

# Copy JAR from builder stage
COPY --from=builder /app/build/libs/*.jar app.jar

# Copy IDL files
COPY --from=builder /app/idl/ ./idl/

# Run the application
ENTRYPOINT ["java", "-jar", "/app/app.jar"]