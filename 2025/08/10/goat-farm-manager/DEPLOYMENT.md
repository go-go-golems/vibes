# Deployment Guide

This guide covers deployment options for the Goat Farm Management System.

## System Requirements

### Minimum Requirements
- **OS**: Linux (Ubuntu 20.04+), macOS (10.15+), or Windows 10+
- **Memory**: 512MB RAM
- **Storage**: 1GB free disk space
- **CPU**: Any modern x64 processor

### Recommended Requirements
- **OS**: Linux (Ubuntu 22.04+) or macOS (12.0+)
- **Memory**: 2GB RAM
- **Storage**: 5GB free disk space (for data growth)
- **CPU**: Multi-core x64 processor

## Installation Methods

### Method 1: Binary Installation (Recommended)

1. **Download the latest release:**
```bash
# Linux x64
wget https://github.com/your-org/goat-farm-manager/releases/latest/download/goat-manager-linux-amd64.tar.gz
tar -xzf goat-manager-linux-amd64.tar.gz

# macOS x64
wget https://github.com/your-org/goat-farm-manager/releases/latest/download/goat-manager-darwin-amd64.tar.gz
tar -xzf goat-manager-darwin-amd64.tar.gz

# macOS ARM64 (Apple Silicon)
wget https://github.com/your-org/goat-farm-manager/releases/latest/download/goat-manager-darwin-arm64.tar.gz
tar -xzf goat-manager-darwin-arm64.tar.gz
```

2. **Install the binary:**
```bash
sudo cp goat-manager /usr/local/bin/
sudo chmod +x /usr/local/bin/goat-manager
```

3. **Verify installation:**
```bash
goat-manager --version
```

### Method 2: Build from Source

1. **Install Go 1.24.5 or later:**
```bash
# Download from https://golang.org/dl/
wget https://go.dev/dl/go1.24.5.linux-amd64.tar.gz
sudo tar -C /usr/local -xzf go1.24.5.linux-amd64.tar.gz
export PATH=$PATH:/usr/local/go/bin
```

2. **Install build dependencies:**
```bash
# Ubuntu/Debian
sudo apt update
sudo apt install build-essential git

# macOS
xcode-select --install
```

3. **Clone and build:**
```bash
git clone https://github.com/your-org/goat-farm-manager.git
cd goat-farm-manager
make deps
make build
make install
```

### Method 3: Docker Deployment

1. **Create Dockerfile:**
```dockerfile
FROM golang:1.24.5-alpine AS builder

WORKDIR /app
COPY . .
RUN apk add --no-cache gcc musl-dev
RUN go mod download
RUN go build -o goat-manager ./cmd/goat-manager

FROM alpine:latest
RUN apk --no-cache add ca-certificates
WORKDIR /root/
COPY --from=builder /app/goat-manager .
VOLUME ["/data"]
ENV GOAT_FARM_DB_PATH=/data
ENTRYPOINT ["./goat-manager"]
```

2. **Build and run:**
```bash
docker build -t goat-farm-manager .
docker run -v $(pwd)/data:/data goat-farm-manager goat --help
```

## Configuration

### Environment Variables

Create a configuration file or set environment variables:

```bash
# ~/.goat-farm-config
export GOAT_FARM_DB_PATH="$HOME/goat-farm-data"
export GOAT_FARM_DB_NAME="goat_farm"
export GOAT_FARM_COMMIT_NAME="Your Name"
export GOAT_FARM_COMMIT_EMAIL="your.email@example.com"
```

Source the configuration:
```bash
source ~/.goat-farm-config
```

### Database Initialization

```bash
# Initialize database
goat-manager version --action "commit" --message "Initial database setup"

# Verify initialization
goat-manager version --action "log" --limit 1
```

## Production Deployment

### Single Server Deployment

1. **Create dedicated user:**
```bash
sudo useradd -m -s /bin/bash goatfarm
sudo su - goatfarm
```

2. **Install application:**
```bash
mkdir -p ~/bin ~/data ~/logs
cp goat-manager ~/bin/
chmod +x ~/bin/goat-manager
```

3. **Create systemd service (optional):**
```bash
sudo tee /etc/systemd/system/goat-farm-backup.service << EOF
[Unit]
Description=Goat Farm Daily Backup
After=network.target

[Service]
Type=oneshot
User=goatfarm
WorkingDirectory=/home/goatfarm
ExecStart=/home/goatfarm/bin/goat-manager version --action backup --message "Daily automated backup"
EOF

sudo tee /etc/systemd/system/goat-farm-backup.timer << EOF
[Unit]
Description=Run Goat Farm Backup Daily
Requires=goat-farm-backup.service

[Timer]
OnCalendar=daily
Persistent=true

[Install]
WantedBy=timers.target
EOF

sudo systemctl enable goat-farm-backup.timer
sudo systemctl start goat-farm-backup.timer
```

### Multi-User Setup

1. **Create shared data directory:**
```bash
sudo mkdir -p /opt/goat-farm/data
sudo chown -R goatfarm:goatfarm /opt/goat-farm
sudo chmod 755 /opt/goat-farm
```

2. **Configure shared environment:**
```bash
sudo tee /etc/profile.d/goat-farm.sh << EOF
export GOAT_FARM_DB_PATH="/opt/goat-farm/data"
export GOAT_FARM_DB_NAME="shared_goat_farm"
export PATH="/opt/goat-farm/bin:\$PATH"
EOF
```

3. **Set up user permissions:**
```bash
sudo groupadd goat-users
sudo usermod -a -G goat-users john
sudo usermod -a -G goat-users jane
sudo chgrp -R goat-users /opt/goat-farm
sudo chmod -R g+rw /opt/goat-farm/data
```

## Backup and Recovery

### Automated Backup Script

```bash
#!/bin/bash
# /home/goatfarm/bin/backup.sh

BACKUP_DIR="/home/goatfarm/backups"
DATE=$(date +%Y%m%d_%H%M%S)
BACKUP_NAME="farm_backup_$DATE"

mkdir -p "$BACKUP_DIR"

# Create Dolt backup
cd "$GOAT_FARM_DB_PATH"
goat-manager version --action "backup" --message "Automated backup - $DATE"

# Create filesystem backup
tar -czf "$BACKUP_DIR/$BACKUP_NAME.tar.gz" "$GOAT_FARM_DB_PATH"

# Keep only last 30 days of backups
find "$BACKUP_DIR" -name "farm_backup_*.tar.gz" -mtime +30 -delete

echo "Backup completed: $BACKUP_NAME.tar.gz"
```

### Recovery Procedure

1. **From Dolt backup:**
```bash
# List available backups
goat-manager version --action "log" --limit 20 | grep -i backup

# Restore from specific backup branch
goat-manager version --action "restore" --branch "backup-20240315-120000"
```

2. **From filesystem backup:**
```bash
# Stop any running processes
# Extract backup
tar -xzf farm_backup_20240315_120000.tar.gz -C /

# Restart application
goat-manager version --action "log" --limit 1
```

## Monitoring and Maintenance

### Health Check Script

```bash
#!/bin/bash
# /home/goatfarm/bin/health-check.sh

echo "=== Goat Farm System Health Check ==="
echo "Date: $(date)"
echo ""

# Check database connectivity
if goat-manager version --action "log" --limit 1 >/dev/null 2>&1; then
    echo "✓ Database connectivity: OK"
else
    echo "✗ Database connectivity: FAILED"
    exit 1
fi

# Check disk space
DISK_USAGE=$(df -h "$GOAT_FARM_DB_PATH" | awk 'NR==2 {print $5}' | sed 's/%//')
if [ "$DISK_USAGE" -lt 90 ]; then
    echo "✓ Disk space: OK ($DISK_USAGE% used)"
else
    echo "⚠ Disk space: WARNING ($DISK_USAGE% used)"
fi

# Check recent activity
RECENT_COMMITS=$(goat-manager version --action "log" --limit 7 | wc -l)
echo "✓ Recent activity: $RECENT_COMMITS commits in last 7 days"

# Check data integrity
GOAT_COUNT=$(goat-manager goat --output json | jq length)
MILK_COUNT=$(goat-manager milk --limit 1000 --output json | jq length)
echo "✓ Data summary: $GOAT_COUNT goats, $MILK_COUNT milk records"

echo ""
echo "Health check completed"
```

### Log Rotation

```bash
# /etc/logrotate.d/goat-farm
/home/goatfarm/logs/*.log {
    daily
    missingok
    rotate 30
    compress
    delaycompress
    notifempty
    create 644 goatfarm goatfarm
}
```

## Security Considerations

### File Permissions

```bash
# Secure data directory
chmod 750 "$GOAT_FARM_DB_PATH"
chown -R goatfarm:goat-users "$GOAT_FARM_DB_PATH"

# Secure binary
chmod 755 /usr/local/bin/goat-manager
chown root:root /usr/local/bin/goat-manager
```

### Network Security

- Run on isolated network if possible
- Use VPN for remote access
- Regular security updates for the host system

### Data Encryption

For sensitive deployments, consider encrypting the data directory:

```bash
# Create encrypted filesystem
sudo cryptsetup luksFormat /dev/sdb1
sudo cryptsetup luksOpen /dev/sdb1 goat-farm-data
sudo mkfs.ext4 /dev/mapper/goat-farm-data
sudo mount /dev/mapper/goat-farm-data /opt/goat-farm/data
```

## Troubleshooting

### Common Issues

1. **Database connection errors:**
```bash
# Check permissions
ls -la "$GOAT_FARM_DB_PATH"

# Verify environment variables
env | grep GOAT_FARM

# Test basic connectivity
goat-manager version --action "log" --limit 1
```

2. **Performance issues:**
```bash
# Check disk space
df -h "$GOAT_FARM_DB_PATH"

# Monitor memory usage
free -h

# Check for large tables
goat-manager analytics --report-type "farm-summary"
```

3. **Version control issues:**
```bash
# Check branch status
goat-manager version --action "branches"

# Verify commit history
goat-manager version --action "log" --limit 5

# Reset to known good state if needed
goat-manager version --action "switch" --branch "main"
```

### Support Resources

- GitHub Issues: https://github.com/your-org/goat-farm-manager/issues
- Documentation: `/docs` directory
- Built-in help: `goat-manager help`

## Scaling Considerations

### Data Growth Planning

- Plan for ~1MB per goat per year
- Monitor database size regularly
- Consider archiving old data after 5+ years

### Performance Optimization

- Regular database maintenance
- Appropriate indexing (handled by Ent)
- SSD storage recommended for better performance

### Future Enhancements

- Web dashboard interface
- Mobile app integration
- Cloud synchronization
- Multi-farm management

