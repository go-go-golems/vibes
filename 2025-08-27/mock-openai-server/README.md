# Mock OpenAI Server

A lightweight mock implementation of the OpenAI API written in Go, designed to be compatible with the standard OpenAI Python SDK and other OpenAI-compatible clients.

## Features

- ✅ **OpenAI API Compatible**: Works seamlessly with the standard OpenAI Python SDK
- ✅ **Chat Completions**: Implements the `/v1/chat/completions` endpoint
- ✅ **Models Listing**: Provides `/v1/models` endpoint
- ✅ **Health Check**: Includes `/health` endpoint for monitoring
- ✅ **CORS Support**: Enabled for web applications
- ✅ **Error Handling**: Proper error responses matching OpenAI format
- ✅ **Mock Responses**: Intelligent mock responses based on input content

## Quick Start

### Prerequisites

- Go 1.21+ installed
- Python 3.7+ (for testing with OpenAI SDK)

### Running the Server

1. **Clone or download the project**
2. **Build and run the server:**
   ```bash
   cd mock-openai-server
   go build -o mock-openai-server .
   ./mock-openai-server serve
   ```

The server will start on `http://localhost:3117`

### Testing with Python OpenAI SDK

1. **Install the OpenAI SDK:**
   ```bash
   pip install openai
   ```

2. **Run the test script:**
   ```bash
   python3 test_mock_server.py
   ```

## API Endpoints

### Chat Completions
- **Endpoint**: `POST /v1/chat/completions`
- **Compatible with**: OpenAI Chat Completions API
- **Features**: 
  - Supports all standard parameters (`model`, `messages`, `max_tokens`, `temperature`)
  - Returns properly formatted responses with usage statistics
  - Intelligent mock responses based on message content

### Models
- **Endpoint**: `GET /v1/models`
- **Returns**: List of available mock models (`gpt-3.5-turbo`, `gpt-4`)

### Health Check
- **Endpoint**: `GET /health`
- **Returns**: Server status and timestamp

### Root
- **Endpoint**: `GET /`
- **Returns**: Server information and available endpoints

### Help
- **Endpoint**: `GET /help` lists built-in documentation topics
- **Endpoint**: `GET /help/{slug}` returns a specific help entry (markdown content with metadata)

## Usage Examples

### Python with OpenAI SDK

```python
import openai

# Configure client to use mock server
client = openai.OpenAI(
    api_key="mock-api-key",  # Any string works
    base_url="http://localhost:3117/v1"
)

# Make a chat completion request
response = client.chat.completions.create(
    model="gpt-3.5-turbo",
    messages=[
        {"role": "user", "content": "Hello, how are you?"}
    ]
)

print(response.choices[0].message.content)
```

### cURL

```bash
curl -X POST http://localhost:3117/v1/chat/completions \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer mock-api-key" \
  -d '{
    "model": "gpt-3.5-turbo",
    "messages": [
      {"role": "user", "content": "Hello!"}
    ]
  }'
```

## Mock Response Logic

The server provides intelligent mock responses based on message content:

- **Greetings** (`hello`, `hi`) → Friendly greeting response
- **Weather** queries → Mock weather response
- **Programming** questions → Coding assistance response
- **Jokes** → Returns a programming joke
- **Default** → Echoes the input with a mock response message

## Configuration

The server is configurable via YAML. By default it loads `config/bot.yaml`. Override with `MOCK_SERVER_CONFIG=/path/to/bot.yaml`.

- Customize models, streaming delay, and rule-based responses for both Chat and the Responses API.
- See `docs/CONFIGURATION.md` for the full schema and examples.
- New to the project? See `docs/GETTING_STARTED.md` for a fast setup and the built-in default configuration used when no YAML is provided.

## Error Handling

The server implements proper error handling that matches OpenAI's error format:

- **400 Bad Request**: Invalid JSON, missing required parameters
- **405 Method Not Allowed**: Wrong HTTP method
- **500 Internal Server Error**: Server errors

## CORS Support

CORS is enabled for all origins, making it suitable for web applications:

```go
w.Header().Set("Access-Control-Allow-Origin", "*")
w.Header().Set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
w.Header().Set("Access-Control-Allow-Headers", "Content-Type, Authorization")
```

## Development

### Project Structure

```
mock-openai-server/
├── main.go              # Main server implementation
├── go.mod              # Go module file
├── README.md           # This documentation
└── test_mock_server.py # Python test script
```

### Building

```bash
go build -o mock-openai-server .
```

### Testing

The included test script (`test_mock_server.py`) provides comprehensive testing:

- Basic chat completions
- Multi-turn conversations
- Error handling
- Models endpoint
- Health check endpoint

## Use Cases

- **Development**: Test applications without using real OpenAI API credits
- **CI/CD**: Run tests in environments without internet access
- **Prototyping**: Quickly prototype OpenAI-powered applications
- **Education**: Learn how to integrate with OpenAI API
- **Offline Development**: Work on AI applications without internet connectivity

## Limitations

- **No Streaming**: Streaming responses are not implemented
- **Simple Token Counting**: Uses word count instead of actual tokenization
- **Mock Responses**: Responses are generated based on simple pattern matching
- **No Authentication**: API key validation is not implemented
- **No Rate Limiting**: No rate limiting or usage tracking

## License

This project is provided as-is for educational and development purposes.

## Contributing

Feel free to extend this mock server with additional features:

- Streaming support
- More sophisticated response generation
- Additional OpenAI endpoints (embeddings, images, etc.)
- Configuration file support
- Logging and metrics
