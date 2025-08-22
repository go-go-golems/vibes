# API Documentation

## Base URL
```
http://localhost:8080
```

## Endpoints

### Health Check
- **GET** `/api/health`
- **Description**: Check if the API is running
- **Response**: 
  ```json
  {"status": "healthy"}
  ```

### Users

#### Get All Users
- **GET** `/api/users`
- **Description**: Retrieve all users
- **Response**:
  ```json
  [
    {
      "id": 1,
      "name": "John Doe",
      "email": "john@example.com"
    }
  ]
  ```

#### Get User by ID
- **GET** `/api/users/{id}`
- **Description**: Retrieve a specific user
- **Parameters**:
  - `id` (path): User ID
- **Response**:
  ```json
  {
    "id": 1,
    "name": "John Doe",
    "email": "john@example.com"
  }
  ```

#### Create User
- **POST** `/api/users`
- **Description**: Create a new user
- **Request Body**:
  ```json
  {
    "name": "Jane Smith",
    "email": "jane@example.com"
  }
  ```
- **Response**:
  ```json
  {
    "id": 3,
    "name": "Jane Smith",
    "email": "jane@example.com"
  }
  ```

## Error Responses

All endpoints may return the following error responses:

- **400 Bad Request**: Invalid request data
- **404 Not Found**: Resource not found
- **500 Internal Server Error**: Server error

Error response format:
```json
{
  "error": "Error message description"
}
```

