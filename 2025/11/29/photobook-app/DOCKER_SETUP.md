# Docker Compose Setup for Local Development

This setup provides everything you need to run the photobook app locally with zero external dependencies.

## What's Included

- **MySQL 8.0** - Database for storing photos and PDF jobs
- **MinIO** - S3-compatible object storage for images
- **Automatic bucket creation** - Pre-configured storage bucket

## Quick Start

### 1. Start the Services

```bash
docker-compose up -d
```

This will start:
- MySQL on `localhost:3306`
- MinIO on `localhost:9000` (API) and `localhost:9001` (Web UI)

### 2. Set Up Environment Variables

Copy the example environment file:

```bash
cp env.local.example .env
```

The default values in `env.local.example` are already configured to work with Docker Compose.

### 3. Run Database Migrations

```bash
pnpm db:push
```

This creates the necessary tables in MySQL.

### 4. Start the Development Server

```bash
pnpm dev
```

The app will be available at `http://localhost:3000`

## Accessing Services

### MinIO Web UI
- URL: http://localhost:9001
- Username: `minioadmin`
- Password: `minioadmin123`

You can browse uploaded images here!

### MySQL Database
- Host: `localhost:3306`
- Database: `photobook`
- Username: `photobook`
- Password: `photobook123`

Connect with any MySQL client (MySQL Workbench, DBeaver, etc.)

## Stopping Services

```bash
docker-compose down
```

To also remove the data volumes (⚠️ this deletes all images and database data):

```bash
docker-compose down -v
```

## Troubleshooting

### Port Already in Use

If ports 3306 or 9000 are already in use, edit `docker-compose.yml` and change the port mappings:

```yaml
ports:
  - "3307:3306"  # Use 3307 instead of 3306
```

Then update `DATABASE_URL` in `.env` accordingly.

### MinIO Bucket Not Created

If the bucket wasn't created automatically, you can create it manually:

1. Go to http://localhost:9001
2. Login with minioadmin/minioadmin123
3. Click "Buckets" → "Create Bucket"
4. Name it `photobook-images`
5. Set access policy to "Public"

### Database Connection Failed

Make sure MySQL is healthy:

```bash
docker-compose ps
```

Wait until the `photobook-mysql` service shows as "healthy".

## Authentication Note

The app requires OAuth authentication. For local development, you have two options:

1. **Skip authentication** - Modify the code to bypass auth checks (not recommended)
2. **Use Manus OAuth** - Set up OAuth credentials in `.env` (requires Manus account)
3. **Mock authentication** - I can create a simple mock auth system for local dev

Let me know if you need help with authentication!
