# Local Development Setup Guide

This photobook app uses a full-stack architecture with database, S3 storage, and authentication. Here's how to run it locally.

## Prerequisites

- Node.js 18+ and pnpm installed
- MySQL database (local or cloud)
- AWS S3 bucket (or S3-compatible storage)
- OAuth provider setup (or use Manus OAuth)

## Quick Start

### 1. Install Dependencies

```bash
pnpm install
```

### 2. Set Up Environment Variables

Create a `.env` file in the project root with the following variables:

```env
# Database Configuration
DATABASE_URL=mysql://user:password@localhost:3306/photobook

# S3 Storage Configuration
S3_ENDPOINT=https://s3.amazonaws.com
S3_REGION=us-east-1
S3_ACCESS_KEY_ID=your_access_key
S3_SECRET_ACCESS_KEY=your_secret_key
S3_BUCKET=your-bucket-name

# JWT Secret (generate a random string)
JWT_SECRET=your-super-secret-jwt-key-change-this

# OAuth Configuration (if using custom OAuth)
OAUTH_SERVER_URL=https://your-oauth-server.com
OAUTH_CLIENT_ID=your_client_id
OAUTH_CLIENT_SECRET=your_client_secret

# App Configuration
VITE_APP_TITLE=Photobook Creator
VITE_APP_LOGO=/logo.svg

# Owner Configuration (for admin access)
OWNER_OPEN_ID=your_oauth_open_id
OWNER_NAME=Your Name
```

### 3. Set Up Database

Run database migrations to create tables:

```bash
pnpm db:push
```

This will create the following tables:
- `users` - User authentication
- `photos` - Uploaded images with S3 references
- `pdfJobs` - PDF generation job queue

### 4. Start Development Server

```bash
pnpm dev
```

The app will be available at `http://localhost:3000`

## Running Without Full Backend

If you want to run the app **without** database/S3 (frontend only), you'll need to:

1. Switch back to the static template version (before we added server features)
2. Or modify the code to use local storage instead of the backend API

The current version **requires** a database and S3 storage to function properly because:
- Images are uploaded to S3 (not stored locally)
- Image metadata is stored in MySQL
- PDF generation runs as a background job
- User authentication is required

## Simplified Local Development Option

### ⭐ Recommended: Docker Compose Setup

The easiest way to run this app locally is using Docker Compose, which sets up everything automatically.

**See [DOCKER_SETUP.md](./DOCKER_SETUP.md) for the complete Docker guide.**

Quick start:
```bash
docker-compose up -d
cp env.local.example .env
pnpm db:push
pnpm dev
```

## Production Deployment

For production deployment, this app is designed to run on platforms that provide:
- MySQL database
- S3-compatible object storage
- Node.js runtime for the backend server

The Manus platform provides all of these automatically, which is why it works seamlessly there.
