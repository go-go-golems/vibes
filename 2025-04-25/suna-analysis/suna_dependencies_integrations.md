# SUNA Dependencies and External Integrations

## Overview

The SUNA project relies on a diverse set of dependencies and external integrations to provide its AI agent capabilities. These include Python and JavaScript libraries, third-party APIs, infrastructure services, and custom integrations. This document provides a comprehensive analysis of these dependencies and integrations.

## Core Python Dependencies

### Backend Framework

- **FastAPI** (v0.110.0): The primary web framework used for the backend API
- **Uvicorn** (v0.27.1): ASGI server implementation for running FastAPI
- **Python-Multipart** (v0.0.20): For handling multipart form data in FastAPI
- **AsyncIO** (v3.4.3): For asynchronous programming
- **Pydantic**: For data validation and settings management

### LLM Integration

- **LiteLLM** (≥v1.66.2): Unified interface for various LLM providers
- **OpenAI** (≥v1.72.0): Client library for OpenAI's API
- **Anthropic** (via LiteLLM): Accessed through LiteLLM for Claude models

### Database and Storage

- **Supabase** (≥v2.15.0): PostgreSQL database with authentication and storage
- **PyJWT** (v2.10.1): For handling JWT tokens in authentication
- **Redis** (v5.2.1): For caching, pub/sub messaging, and distributed state
- **Upstash-Redis** (v1.3.0): Serverless Redis client

### Web Search and Browsing

- **Tavily-Python** (≥v0.5.4): Client for Tavily search API
- **HTTPX**: HTTP client for making asynchronous requests
- **PyTesseract** (v0.3.13): OCR engine for extracting text from images
- **Daytona SDK** (≥v0.14.0): SDK for Daytona sandbox management

### AWS Integration

- **Boto3** (≥v1.34.0): AWS SDK for Python, used for Bedrock LLM integration

### Utilities

- **Python-Dotenv** (v1.0.1): Loading environment variables from .env files
- **Click** (v8.1.7): Command-line interface creation toolkit
- **Questionary** (v2.0.1): Interactive user prompts
- **Requests** (≥v2.31.0): HTTP library for API calls
- **Packaging** (v24.1): Utilities for version handling
- **Python-Ripgrep** (v0.0.6): Fast text search using ripgrep
- **Certifi** (v2024.2.2): CA certificate bundle for secure connections

### Testing

- **Pytest** (v8.3.3): Testing framework
- **Pytest-AsyncIO** (v0.24.0): Pytest support for asyncio

## Frontend Dependencies

### Framework and Core

- **Next.js** (v15.2.2): React framework for server-rendered applications
- **React** (v18): UI library
- **TypeScript** (v5): Typed JavaScript
- **Zustand** (v5.0.3): State management
- **SWR** (v2.2.5): React Hooks for data fetching

### UI Components

- **Tailwind CSS** (v4): Utility-first CSS framework
- **Radix UI**: Unstyled, accessible UI components
  - Multiple components including: React Accordion, Avatar, Checkbox, Dialog, Dropdown Menu, etc.
- **Lucide React** (v0.479.0): Icon set
- **Geist** (v1.2.1): UI design system
- **Sonner** (v2.0.3): Toast notifications
- **React Hook Form** (v7.55.0): Form handling
- **Zod** (v3.24.2): Schema validation

### Data Visualization and Processing

- **React Markdown** (v10.1.0): Markdown rendering
- **Remark GFM** (v4.0.0): GitHub Flavored Markdown support
- **React Syntax Highlighter** (v15.6.1): Code syntax highlighting
- **PapaParse** (v5.5.2): CSV parsing
- **CodeMirror** (v4.23.10): Code editor component
- **React PDF** (v4.3.0): PDF viewing and generation

### Analytics and Monitoring

- **Vercel Analytics** (v1.5.0): Usage analytics
- **Vercel Speed Insights** (v1.2.0): Performance monitoring

## External API Integrations

### LLM Providers

1. **Anthropic Claude**
   - API: Claude API for large language model capabilities
   - Models used: Claude-3-7-Sonnet (primary model)
   - Integration: Via LiteLLM
   - Key environment variable: `ANTHROPIC_API_KEY`

2. **OpenAI**
   - API: OpenAI API for large language model capabilities
   - Models used: GPT-4o
   - Integration: Via LiteLLM
   - Key environment variable: `OPENAI_API_KEY`

3. **AWS Bedrock**
   - API: Amazon Bedrock for LLM integration
   - Models used: Various including Claude models on Bedrock
   - Integration: Via Boto3 and LiteLLM
   - Key environment variables: `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, `AWS_REGION_NAME`

4. **OpenRouter**
   - API: Unified gateway to various LLM providers
   - Models used: Various including Groq, Mistral, and others
   - Integration: Via LiteLLM
   - Key environment variable: `OPENROUTER_API_KEY`

### Search and Web Services

1. **Tavily**
   - API: Tavily Search API for web search and content extraction
   - Features used: Web search, content extraction
   - Integration: Direct via Tavily Python client
   - Key environment variable: `TAVILY_API_KEY`
   - Implementation file: `backend/agent/tools/web_search_tool.py`

2. **RapidAPI**
   - API: RapidAPI for various data providers
   - Features used: LinkedIn, Twitter, Zillow, Amazon, Yahoo Finance data
   - Integration: Custom via HTTPX
   - Key environment variable: `RAPID_API_KEY`
   - Implementation files: `backend/agent/tools/data_providers/*.py`

### Infrastructure Services

1. **Supabase**
   - Services used: PostgreSQL database, Authentication, Storage
   - Features: User management, data persistence, file storage
   - Integration: Via Supabase Python and JavaScript clients
   - Key environment variables: `SUPABASE_URL`, `SUPABASE_SERVICE_ROLE_KEY`, `SUPABASE_ANON_KEY`
   - Implementation file: `backend/services/supabase.py`

2. **Redis**
   - Services used: Caching, Pub/Sub messaging
   - Features: Agent control, state management, distributed coordination
   - Integration: Via Redis Python client
   - Key environment variables: `REDIS_HOST`, `REDIS_PORT`, `REDIS_PASSWORD`, `REDIS_SSL`
   - Implementation file: `backend/services/redis.py`

3. **Cloudflare Pages**
   - Services used: Serverless deployment for user websites
   - Features: Static site hosting
   - Integration: Via Wrangler CLI in sandbox
   - Implementation: `backend/agent/tools/sb_deploy_tool.py`

## Sandbox Integration

The SUNA project implements a custom sandbox environment for secure execution of code and browser automation:

### Container Environment

The sandbox is built on a custom Docker container with:

- **Base Image**: Python 3.11 slim
- **Browser**: Chromium browser (via Playwright)
- **GUI**: Xvfb, x11vnc, noVNC for virtual desktop
- **Runtime**: Node.js 20.x, npm, Wrangler CLI
- **Processing Tools**: PDF processing, document processing, text processing
- **Utilities**: Various command-line tools for file manipulation

### Browser Automation

- **Playwright**: For browser automation and control
- **Custom API**: RESTful API for browser interaction
- **VNC**: For visual interaction and debugging
- **Implementation**: `backend/sandbox/docker/*` and `backend/agent/tools/sb_browser_tool.py`

## Payment and Billing Integration

The system includes billing and subscription management:

### Stripe Integration (Indirect)

- **Features**: Subscription tiers, usage tracking
- **Subscription Tiers**:
  - Free tier (10 minutes/month)
  - Base tier (300 minutes/month)
  - Extra tier (2400 minutes/month)
- **Implementation**: `backend/utils/billing.py`
- **Database Schema**: Supabase tables for billing_subscriptions

## Dependency Management

The project uses multiple dependency management approaches:

1. **Poetry**: Main dependency management for backend
   - File: `backend/pyproject.toml`

2. **pip**: Traditional Python package management
   - File: `backend/requirements.txt`

3. **npm**: JavaScript package management for frontend
   - File: `frontend/package.json`

## Security-Related Dependencies

- **SSL/TLS**: Certifi for certificate validation
- **JWT**: PyJWT for token handling
- **Sanitization**: rehype-sanitize for content sanitization
- **Isolation**: Docker containers for execution isolation

## Development Dependencies

### Backend Development

- **Pytest**: Testing framework
- **Pytest-AsyncIO**: Asynchronous testing support

### Frontend Development

- **ESLint**: JavaScript/TypeScript linting
- **TypeScript**: Type checking
- **Tailwind CSS**: CSS styling
- **Encoding**: Utilities for text encoding

## Deployment Requirements

The application requires the following environment setup for deployment:

### API Keys

- **LLM Providers**: 
  - `ANTHROPIC_API_KEY` for Anthropic Claude
  - `OPENAI_API_KEY` for OpenAI
  - AWS credentials for Bedrock

- **Search and Data**:
  - `TAVILY_API_KEY` for search capabilities
  - `RAPID_API_KEY` for data providers

### Infrastructure Credentials

- **Database**:
  - `SUPABASE_URL` and `SUPABASE_SERVICE_ROLE_KEY`

- **Caching**:
  - `REDIS_HOST`, `REDIS_PORT`, `REDIS_PASSWORD`

### Optional Environment Variables

- Additional model-specific settings
- Metrics and monitoring configuration
- Debugging and logging settings

## Dependency Analysis

### Critical Dependencies

1. **LLM Providers (Anthropic, OpenAI)**: Core to the agent's functionality
2. **Supabase**: Essential for data persistence and authentication
3. **Redis**: Required for coordination and state management
4. **Tavily**: Critical for web search capabilities

### External Service Dependencies

1. **API-Dependent**: Search, LLMs, database, caching
2. **Self-Hostable**: Redis, PostgreSQL (via Supabase)
3. **Always External**: LLM APIs, search APIs

### Versioning and Compatibility

- Most dependencies specify minimum versions with `>=`
- Some dependencies are pinned to exact versions for stability
- Frontend dependencies are generally more strictly versioned

## Integration Architecture

The system implements a modular integration architecture:

1. **Service Abstraction**: Wrapper services for external APIs
   - `services/llm.py` for LLM integration
   - `services/supabase.py` for database
   - `services/redis.py` for caching

2. **Tool Interface**: Consistent interface for agent tools
   - All tools implement the `Tool` base class
   - Standard schemas for OpenAPI and XML format

3. **Dependency Injection**: Services are initialized centrally and injected
   - `ThreadManager` receives tools during initialization
   - `ResponseProcessor` receives registry of tools

4. **Fault Tolerance**: Retry mechanisms for external services
   - Redis operations have exponential backoff retries
   - LLM API calls include retry logic with backoff

## Conclusion

The SUNA project demonstrates a sophisticated integration strategy, bringing together multiple external services and libraries to create a powerful AI agent platform. The architecture emphasizes:

1. **Flexibility**: Multiple LLM providers supported through LiteLLM
2. **Modularity**: Clear separation between components and services
3. **Resilience**: Robust error handling and retry mechanisms
4. **Security**: Sandboxed execution environment and proper authentication
5. **Scalability**: Stateless API design with distributed coordination

The dependencies and integrations reflect a modern, cloud-native application design that balances performance, security, and developer experience.