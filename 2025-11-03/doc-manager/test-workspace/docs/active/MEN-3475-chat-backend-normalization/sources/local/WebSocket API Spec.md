# WebSocket API Specification

## Overview
This document describes the WebSocket API for real-time chat communication.

## Connection
Connect to: `wss://api.example.com/chat/ws`

## Message Format
All messages are JSON-encoded with the following structure:
```json
{
  "type": "message",
  "payload": {},
  "timestamp": "2025-10-31T12:00:00Z"
}
```

## Events
- `message`: New chat message
- `typing`: User typing indicator
- `presence`: User presence update
