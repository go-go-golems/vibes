# 🐦 Pelican Farm Management System

A comprehensive Go-based web application for managing pelican farms, built with parallel agent coordination using AgentBus.

## ✨ Features

- **Pelican Management**: Complete CRUD operations for individual pelicans
- **Farm Management**: Organize pelicans by farm locations
- **Health Tracking**: Monitor pelican health status with statistics
- **RESTful API**: Well-documented API endpoints
- **Web Interface**: Modern HTML templates with Bootstrap styling
- **Docker Support**: Production-ready containerization

## 🚀 Quick Start

### Prerequisites

- Go 1.21+
- Docker & Docker Compose (for deployment)
- Git

### Development Setup

1. **Clone the repository**
   ```bash
   git clone <repository-url>
   cd pelican-farm
   ```

2. **Install dependencies**
   ```bash
   go mod tidy
   ```

3. **Build and run**
   ```bash
   go build -o server ./cmd/server
   ./server
   ```

4. **Access the application**
   - Web Interface: http://localhost:8080
   - API Base URL: http://localhost:8080/api/v1

### Docker Deployment

1. **Quick deployment**
   ```bash
   ./deploy.sh
   ```

2. **Manual Docker commands**
   ```bash
   docker-compose up -d
   ```

## 📚 API Documentation

### Pelican Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/api/v1/pelicans` | List all pelicans |
| POST | `/api/v1/pelicans` | Create new pelican |
| GET | `/api/v1/pelicans/:id` | Get pelican by ID |
| PUT | `/api/v1/pelicans/:id` | Update pelican |
| DELETE | `/api/v1/pelicans/:id` | Delete pelican |
| GET | `/api/v1/pelicans/stats` | Get pelican statistics |

### Farm Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/api/v1/farms` | List all farms |
| POST | `/api/v1/farms` | Create new farm |
| GET | `/api/v1/farms/:id` | Get farm by ID |
| PUT | `/api/v1/farms/:id` | Update farm |
| DELETE | `/api/v1/farms/:id` | Delete farm |
| GET | `/api/v1/farms/:id/stats` | Get farm statistics |
| POST | `/api/v1/farms/:id/assign/:pelican_id` | Assign pelican to farm |
| DELETE | `/api/v1/farms/:id/unassign/:pelican_id` | Unassign pelican from farm |

### Example API Calls

**Create a Pelican:**
```bash
curl -X POST http://localhost:8080/api/v1/pelicans \
  -H "Content-Type: application/json" \
  -d '{
    "name": "Pelley",
    "species": "Brown Pelican",
    "age": 3,
    "weight": 4.5,
    "health": "healthy",
    "location": "Pond A",
    "gender": "female",
    "color": "brown"
  }'
```

**Get Statistics:**
```bash
curl http://localhost:8080/api/v1/pelicans/stats
```

## 🏗️ Architecture

### Project Structure
```
pelican-farm/
├── cmd/server/           # Application entry point
├── internal/
│   ├── models/          # Data models and types
│   ├── handlers/        # HTTP request handlers
│   └── database/        # Database layer and repositories
├── templates/           # HTML templates
├── static/             # CSS, JS, and assets
├── migrations/         # Database migrations
└── scripts/           # Utility scripts
```

### Health Status Types
- `healthy` - Normal, active pelican
- `sick` - Requires medical attention
- `injured` - Physical injury present
- `recovering` - In recovery process
- `critical` - Urgent medical attention needed

## 🔧 Deployment Options

### Development
```bash
go run ./cmd/server
```

### Docker (Recommended)
```bash
# Simple deployment
docker-compose up -d

# Production with nginx
docker-compose --profile production up -d
```

### Deployment Script Features
- **Backup**: Automatic database backups before deployment
- **Health Checks**: Container health monitoring
- **Rollback**: Quick rollback to previous version
- **Logging**: Centralized log management

```bash
./deploy.sh deploy     # Deploy application
./deploy.sh rollback   # Rollback deployment
./deploy.sh backup     # Create backup only
./deploy.sh logs       # View logs
./deploy.sh status     # Check status
```

## 🧪 Testing

### Integration Tests Completed ✅
- All API endpoints functional
- CRUD operations verified
- Database operations confirmed
- Health check endpoints working

### Test Commands
```bash
# Build verification
go build ./cmd/server

# API health check
curl http://localhost:8080/api/v1/pelicans/stats
```

## 📊 Monitoring

### Health Checks
- **Application**: `/api/v1/pelicans/stats`
- **Docker**: Built-in healthcheck every 30s
- **Database**: SQLite with GORM

### Logs
```bash
# View application logs
docker-compose logs -f pelican-farm

# View all services
docker-compose logs
```

## 🤝 Development Team

This project was built using parallel agent coordination:

- **project-structure-agent**: Initial Go project structure
- **models-agent**: Data models and types
- **database-agent**: Database layer and repositories  
- **handlers-agent**: HTTP handlers and routing
- **templates-agent**: HTML templates and UI
- **balonga-amp**: Integration testing and deployment
- **cinderalla-static**: CSS/JS assets and styling
- **orchestrator**: Coordination and workflow management

## 📝 License

[Add your license information here]

## 🔗 Links

- API Documentation: Built-in at `/api/v1`
- Health Check: `/api/v1/pelicans/stats`
- Web Interface: `/`

---

Built with ❤️ using Go, Gin, GORM, and coordinated development through AgentBus.
