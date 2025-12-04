# Use Node.js 22 as base image
FROM node:22-slim

# Install SBCL (Steel Bank Common Lisp) and other dependencies
RUN apt-get update && apt-get install -y \
    sbcl \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Copy package files
COPY package.json pnpm-lock.yaml ./

# Install pnpm
RUN npm install -g pnpm

# Install dependencies
RUN pnpm install --frozen-lockfile

# Copy application code
COPY . .

# Build the client
RUN pnpm build

# Expose port
EXPOSE 3000

# Set environment to production
ENV NODE_ENV=production

# Start the application
CMD ["pnpm", "start"]
