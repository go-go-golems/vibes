FROM golang:1.18-alpine AS builder

# Set working directory
WORKDIR /app

# Install necessary packages
RUN apk add --no-cache git

# Copy go.mod and go.sum
COPY go.mod go.sum ./

# Download dependencies
RUN go mod download

# Copy source code
COPY . .

# Build the service
ARG SERVICE_NAME
RUN CGO_ENABLED=0 GOOS=linux go build -a -installsuffix cgo -o /go/bin/${SERVICE_NAME} ./cmd/${SERVICE_NAME}

# Final image
FROM alpine:latest

# Set working directory
WORKDIR /app

# Copy binary from builder stage
ARG SERVICE_NAME
COPY --from=builder /go/bin/${SERVICE_NAME} /app/${SERVICE_NAME}

# Copy IDL files
COPY idl/ /app/idl/

# Run the service
ENTRYPOINT ["/app/${SERVICE_NAME}"]