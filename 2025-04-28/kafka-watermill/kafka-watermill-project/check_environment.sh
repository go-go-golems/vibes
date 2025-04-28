#!/bin/bash

echo "Checking development environment..."

echo -e "\n=== Checking Docker ==="
if command -v docker &> /dev/null; then
  echo "Docker: $(docker --version)"
else
  echo "Docker: Not installed"
fi

echo -e "\n=== Checking Docker Compose ==="
if command -v docker compose &> /dev/null; then
  echo "Docker Compose: $(docker compose version)"
else
  echo "Docker Compose: Not installed"
fi

echo -e "\n=== Checking Go ==="
if command -v go &> /dev/null; then
  echo "Go: $(go version)"
else
  echo "Go: Not installed"
fi

echo -e "\n=== Checking Java/Kotlin ==="
if command -v java &> /dev/null; then
  echo "Java: $(java -version 2>&1 | head -n 1)"
  
  if command -v javac &> /dev/null; then
    echo "Java Compiler: $(javac -version)"
  else
    echo "Java Compiler: Not installed"
  fi
  
  if command -v kotlin &> /dev/null; then
    echo "Kotlin: $(kotlin -version 2>&1 | head -n 1)"
  else
    echo "Kotlin: Not installed (only JDK available)"
  fi
else
  echo "Java: Not installed"
fi

echo -e "\n=== Checking Ruby ==="
if command -v ruby &> /dev/null; then
  echo "Ruby: $(ruby -v)"
  
  if command -v bundle &> /dev/null; then
    echo "Bundler: $(bundle -v)"
  else
    echo "Bundler: Not installed"
  fi
else
  echo "Ruby: Not installed"
fi

echo -e "\n=== Checking Kafka (with Docker) ==="
if command -v docker &> /dev/null; then
  if docker images | grep -q "confluentinc/cp-kafka"; then
    echo "Kafka Docker image: Installed"
  else
    echo "Kafka Docker image: Not found (will be pulled when starting docker-compose)"
  fi
else
  echo "Kafka Docker image: Cannot check (Docker not installed)"
fi

echo -e "\n=== Project Structure ==="
echo "Project root: $(pwd)"
if [ -f "docker-compose.yml" ]; then
  echo "docker-compose.yml: Found"
else
  echo "docker-compose.yml: Not found"
fi

if [ -d "cmd" ]; then
  echo "Go microservices directory: Found"
  echo "  Services: $(ls cmd | tr '\n' ' ')"
else
  echo "Go microservices directory: Not found"
fi

if [ -d "kotlin-service" ]; then
  echo "Kotlin service directory: Found"
else
  echo "Kotlin service directory: Not found"
fi

if [ -d "ruby-service" ]; then
  echo "Ruby service directory: Found"
else
  echo "Ruby service directory: Not found"
fi

if [ -d "idl" ]; then
  echo "IDL directory: Found"
  echo "  Languages: $(ls idl | tr '\n' ' ')"
else
  echo "IDL directory: Not found"
fi

echo -e "\nEnvironment check complete."