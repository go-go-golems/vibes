---
title: REST API Documentation
description: Comprehensive documentation for the user management REST API
tags:
    - api
    - documentation
    - rest
    - backend
    - v2.1
    - stable
category: documentation
created: 2024-08-10T09:00:00Z
modified: 2025-08-14T22:25:35.145066811-04:00
project: user-management-system
repository: https://github.com/company/user-api
branch: main
related_files:
    - user-model.md
    - authentication.md
dependencies:
    - openapi-spec.yaml
references:
    - https://restfulapi.net/
    - https://swagger.io/
status: published
priority: high
version: v2.1
author: Jane Smith
contributors:
    - John Doe
    - Alice Johnson
language: markdown
format: api-docs
template: rest-api
---

# User Management REST API

This document provides comprehensive documentation for the User Management REST API v2.1.

## Overview

The User Management API allows you to create, read, update, and delete user accounts in the system. It follows REST principles and returns JSON responses.

## Base URL

```
https://api.example.com/v2
```

## Authentication

All API requests require authentication using Bearer tokens:

```
Authorization: Bearer <your-token>
```

## Endpoints

### GET /users

Retrieve a list of all users.

**Parameters:**
- `limit` (optional): Maximum number of users to return (default: 50)
- `offset` (optional): Number of users to skip (default: 0)

**Response:**
```json
{
  "users": [
    {
      "id": 1,
      "username": "johndoe",
      "email": "john@example.com",
      "created_at": "2024-01-15T10:30:00Z"
    }
  ],
  "total": 150,
  "limit": 50,
  "offset": 0
}
```

### POST /users

Create a new user account.

**Request Body:**
```json
{
  "username": "newuser",
  "email": "newuser@example.com",
  "password": "securepassword123"
}
```

**Response:**
```json
{
  "id": 151,
  "username": "newuser",
  "email": "newuser@example.com",
  "created_at": "2024-08-14T15:30:00Z"
}
```

## Error Handling

The API uses standard HTTP status codes and returns error details in JSON format:

```json
{
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "Invalid email format",
    "details": {
      "field": "email",
      "value": "invalid-email"
    }
  }
}
```

## Rate Limiting

API requests are limited to 1000 requests per hour per API key. Rate limit information is included in response headers:

- `X-RateLimit-Limit`: Request limit per hour
- `X-RateLimit-Remaining`: Remaining requests in current window
- `X-RateLimit-Reset`: Time when the rate limit resets

## SDK Examples

### Python
```python
import requests

headers = {'Authorization': 'Bearer your-token'}
response = requests.get('https://api.example.com/v2/users', headers=headers)
users = response.json()
```

### JavaScript
```javascript
const response = await fetch('https://api.example.com/v2/users', {
  headers: {
    'Authorization': 'Bearer your-token'
  }
});
const users = await response.json();
```

## Changelog

### v2.1 (2024-08-14)
- Added rate limiting
- Improved error response format
- Added pagination support

### v2.0 (2024-07-01)
- Breaking: Changed authentication to Bearer tokens
- Added user creation endpoint
- Improved response structure

