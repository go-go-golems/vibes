package main

import (
	"fmt"
	"log"
	"os"
	"strings"

	fileeditor "github.com/cline-go/file-editor"
)

func main() {
	// Create a temporary directory for testing
	tempDir, err := os.MkdirTemp("", "realworld_test")
	if err != nil {
		log.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tempDir)
	
	fmt.Printf("Running real-world file editing tests in: %s\n", tempDir)
	
	// Create file editor
	editor := fileeditor.NewFileEditor(tempDir)
	
	// Test 1: Complex Python project with multiple files
	fmt.Printf("\n=== Test 1: Complex Python Project ===\n")
	if err := testPythonProject(editor); err != nil {
		log.Printf("Python project test failed: %v", err)
	} else {
		fmt.Printf("✓ Python project test passed\n")
	}
	
	// Test 2: JavaScript/TypeScript project
	fmt.Printf("\n=== Test 2: JavaScript/TypeScript Project ===\n")
	if err := testJavaScriptProject(editor); err != nil {
		log.Printf("JavaScript project test failed: %v", err)
	} else {
		fmt.Printf("✓ JavaScript project test passed\n")
	}
	
	// Test 3: Configuration files (JSON, YAML, etc.)
	fmt.Printf("\n=== Test 3: Configuration Files ===\n")
	if err := testConfigFiles(editor); err != nil {
		log.Printf("Config files test failed: %v", err)
	} else {
		fmt.Printf("✓ Config files test passed\n")
	}
	
	// Test 4: Large file with many edits
	fmt.Printf("\n=== Test 4: Large File Multiple Edits ===\n")
	if err := testLargeFileEdits(editor); err != nil {
		log.Printf("Large file test failed: %v", err)
	} else {
		fmt.Printf("✓ Large file test passed\n")
	}
	
	// Test 5: Complex refactoring scenario
	fmt.Printf("\n=== Test 5: Complex Refactoring ===\n")
	if err := testComplexRefactoring(editor); err != nil {
		log.Printf("Complex refactoring test failed: %v", err)
	} else {
		fmt.Printf("✓ Complex refactoring test passed\n")
	}
	
	// Test 6: Error recovery and edge cases
	fmt.Printf("\n=== Test 6: Error Recovery ===\n")
	if err := testErrorRecovery(editor); err != nil {
		log.Printf("Error recovery test failed: %v", err)
	} else {
		fmt.Printf("✓ Error recovery test passed\n")
	}
	
	// Final validation
	fmt.Printf("\n=== Final Validation ===\n")
	files, err := editor.ListFiles(".", true)
	if err != nil {
		log.Printf("Failed to list final files: %v", err)
	} else {
		fmt.Printf("Total files created: %d\n", len(files))
		for _, file := range files {
			if !strings.HasSuffix(file, "/") {
				fmt.Printf("  %s\n", file)
			}
		}
	}
	
	fmt.Printf("\n✅ All real-world tests completed!\n")
	fmt.Printf("Test directory: %s\n", tempDir)
}

func testPythonProject(editor *fileeditor.FileEditor) error {
	// Create a Python web application with Flask
	
	// 1. Create main application file
	appContent := `from flask import Flask, render_template, request, jsonify
import sqlite3
import os

app = Flask(__name__)

def get_db_connection():
    conn = sqlite3.connect('database.db')
    conn.row_factory = sqlite3.Row
    return conn

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/api/users', methods=['GET'])
def get_users():
    conn = get_db_connection()
    users = conn.execute('SELECT * FROM users').fetchall()
    conn.close()
    return jsonify([dict(user) for user in users])

if __name__ == '__main__':
    app.run(debug=True)`
	
	if err := editor.WriteToFile("python_app/app.py", appContent); err != nil {
		return err
	}
	
	// 2. Add error handling to database connection
	diff1 := `------- SEARCH
def get_db_connection():
    conn = sqlite3.connect('database.db')
    conn.row_factory = sqlite3.Row
    return conn
=======
def get_db_connection():
    try:
        conn = sqlite3.connect('database.db')
        conn.row_factory = sqlite3.Row
        return conn
    except sqlite3.Error as e:
        print(f"Database connection error: {e}")
        return None
+++++++ REPLACE`
	
	if err := editor.ReplaceInFile("python_app/app.py", diff1); err != nil {
		return err
	}
	
	// 3. Update the get_users route to handle connection errors
	diff2 := `------- SEARCH
@app.route('/api/users', methods=['GET'])
def get_users():
    conn = get_db_connection()
    users = conn.execute('SELECT * FROM users').fetchall()
    conn.close()
    return jsonify([dict(user) for user in users])
=======
@app.route('/api/users', methods=['GET'])
def get_users():
    conn = get_db_connection()
    if conn is None:
        return jsonify({'error': 'Database connection failed'}), 500
    
    try:
        users = conn.execute('SELECT * FROM users').fetchall()
        return jsonify([dict(user) for user in users])
    except sqlite3.Error as e:
        return jsonify({'error': str(e)}), 500
    finally:
        conn.close()
+++++++ REPLACE`
	
	if err := editor.ReplaceInFile("python_app/app.py", diff2); err != nil {
		return err
	}
	
	// 4. Add a new route for creating users
	diff3 := `------- SEARCH
@app.route('/api/users', methods=['GET'])
def get_users():
    conn = get_db_connection()
    if conn is None:
        return jsonify({'error': 'Database connection failed'}), 500
    
    try:
        users = conn.execute('SELECT * FROM users').fetchall()
        return jsonify([dict(user) for user in users])
    except sqlite3.Error as e:
        return jsonify({'error': str(e)}), 500
    finally:
        conn.close()
=======
@app.route('/api/users', methods=['GET'])
def get_users():
    conn = get_db_connection()
    if conn is None:
        return jsonify({'error': 'Database connection failed'}), 500
    
    try:
        users = conn.execute('SELECT * FROM users').fetchall()
        return jsonify([dict(user) for user in users])
    except sqlite3.Error as e:
        return jsonify({'error': str(e)}), 500
    finally:
        conn.close()

@app.route('/api/users', methods=['POST'])
def create_user():
    data = request.get_json()
    if not data or 'name' not in data or 'email' not in data:
        return jsonify({'error': 'Name and email are required'}), 400
    
    conn = get_db_connection()
    if conn is None:
        return jsonify({'error': 'Database connection failed'}), 500
    
    try:
        conn.execute('INSERT INTO users (name, email) VALUES (?, ?)',
                    (data['name'], data['email']))
        conn.commit()
        return jsonify({'message': 'User created successfully'}), 201
    except sqlite3.Error as e:
        return jsonify({'error': str(e)}), 500
    finally:
        conn.close()
+++++++ REPLACE`
	
	if err := editor.ReplaceInFile("python_app/app.py", diff3); err != nil {
		return err
	}
	
	// 5. Create a requirements.txt file
	requirements := `Flask==2.3.3
sqlite3`
	
	if err := editor.WriteToFile("python_app/requirements.txt", requirements); err != nil {
		return err
	}
	
	// 6. Create a simple HTML template
	htmlTemplate := `<!DOCTYPE html>
<html>
<head>
    <title>Python App</title>
</head>
<body>
    <h1>Welcome to Python App</h1>
    <div id="users"></div>
    <script>
        fetch('/api/users')
            .then(response => response.json())
            .then(data => {
                document.getElementById('users').innerHTML = 
                    '<pre>' + JSON.stringify(data, null, 2) + '</pre>';
            });
    </script>
</body>
</html>`
	
	if err := editor.WriteToFile("python_app/templates/index.html", htmlTemplate); err != nil {
		return err
	}
	
	// Verify the final file content
	finalContent, err := editor.ReadFile("python_app/app.py")
	if err != nil {
		return err
	}
	
	// Check that all modifications were applied
	if !strings.Contains(finalContent, "try:") ||
		!strings.Contains(finalContent, "Database connection error") ||
		!strings.Contains(finalContent, "create_user") ||
		!strings.Contains(finalContent, "POST") {
		return fmt.Errorf("not all modifications were applied correctly")
	}
	
	return nil
}

func testJavaScriptProject(editor *fileeditor.FileEditor) error {
	// Create a React component and modify it multiple times
	
	// 1. Create initial React component
	reactComponent := `import React, { useState } from 'react';

function UserList() {
    const [users, setUsers] = useState([]);

    return (
        <div>
            <h2>User List</h2>
            <ul>
                {users.map(user => (
                    <li key={user.id}>{user.name}</li>
                ))}
            </ul>
        </div>
    );
}

export default UserList;`
	
	if err := editor.WriteToFile("js_app/src/components/UserList.js", reactComponent); err != nil {
		return err
	}
	
	// 2. Add useEffect to fetch users
	diff1 := `------- SEARCH
import React, { useState } from 'react';
=======
import React, { useState, useEffect } from 'react';
+++++++ REPLACE`
	
	if err := editor.ReplaceInFile("js_app/src/components/UserList.js", diff1); err != nil {
		return err
	}
	
	// 3. Add fetch logic
	diff2 := `------- SEARCH
function UserList() {
    const [users, setUsers] = useState([]);

    return (
=======
function UserList() {
    const [users, setUsers] = useState([]);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState(null);

    useEffect(() => {
        fetch('/api/users')
            .then(response => response.json())
            .then(data => {
                setUsers(data);
                setLoading(false);
            })
            .catch(err => {
                setError(err.message);
                setLoading(false);
            });
    }, []);

    if (loading) return <div>Loading...</div>;
    if (error) return <div>Error: {error}</div>;

    return (
+++++++ REPLACE`
	
	if err := editor.ReplaceInFile("js_app/src/components/UserList.js", diff2); err != nil {
		return err
	}
	
	// 4. Create a TypeScript version
	tsContent := `import React, { useState, useEffect } from 'react';

interface User {
    id: number;
    name: string;
    email: string;
}

interface UserListProps {
    apiEndpoint?: string;
}

const UserList: React.FC<UserListProps> = ({ apiEndpoint = '/api/users' }) => {
    const [users, setUsers] = useState<User[]>([]);
    const [loading, setLoading] = useState<boolean>(true);
    const [error, setError] = useState<string | null>(null);

    useEffect(() => {
        fetch(apiEndpoint)
            .then(response => response.json())
            .then((data: User[]) => {
                setUsers(data);
                setLoading(false);
            })
            .catch((err: Error) => {
                setError(err.message);
                setLoading(false);
            });
    }, [apiEndpoint]);

    if (loading) return <div>Loading...</div>;
    if (error) return <div>Error: {error}</div>;

    return (
        <div>
            <h2>User List</h2>
            <ul>
                {users.map(user => (
                    <li key={user.id}>
                        {user.name} ({user.email})
                    </li>
                ))}
            </ul>
        </div>
    );
};

export default UserList;`
	
	if err := editor.WriteToFile("js_app/src/components/UserList.tsx", tsContent); err != nil {
		return err
	}
	
	// 5. Create package.json
	packageJson := `{
  "name": "js-app",
  "version": "1.0.0",
  "dependencies": {
    "react": "^18.2.0",
    "react-dom": "^18.2.0"
  },
  "devDependencies": {
    "@types/react": "^18.2.0",
    "typescript": "^5.0.0"
  },
  "scripts": {
    "start": "react-scripts start",
    "build": "react-scripts build",
    "test": "react-scripts test"
  }
}`
	
	if err := editor.WriteToFile("js_app/package.json", packageJson); err != nil {
		return err
	}
	
	return nil
}

func testConfigFiles(editor *fileeditor.FileEditor) error {
	// Test various configuration file formats
	
	// 1. Create and modify JSON config
	jsonConfig := `{
  "database": {
    "host": "localhost",
    "port": 5432,
    "name": "myapp"
  },
  "server": {
    "port": 3000,
    "host": "0.0.0.0"
  }
}`
	
	if err := editor.WriteToFile("config/config.json", jsonConfig); err != nil {
		return err
	}
	
	// Modify JSON config
	diff1 := `------- SEARCH
  "server": {
    "port": 3000,
    "host": "0.0.0.0"
  }
=======
  "server": {
    "port": 8080,
    "host": "0.0.0.0",
    "ssl": true,
    "cert_path": "/etc/ssl/certs/app.crt"
  }
+++++++ REPLACE`
	
	if err := editor.ReplaceInFile("config/config.json", diff1); err != nil {
		return err
	}
	
	// 2. Create YAML config
	yamlConfig := `database:
  host: localhost
  port: 5432
  name: myapp
  username: admin
  password: secret

logging:
  level: info
  file: /var/log/app.log

features:
  - authentication
  - caching
  - monitoring`
	
	if err := editor.WriteToFile("config/config.yaml", yamlConfig); err != nil {
		return err
	}
	
	// Modify YAML config
	diff2 := `------- SEARCH
logging:
  level: info
  file: /var/log/app.log
=======
logging:
  level: debug
  file: /var/log/app.log
  max_size: 100MB
  rotate: true
+++++++ REPLACE`
	
	if err := editor.ReplaceInFile("config/config.yaml", diff2); err != nil {
		return err
	}
	
	// 3. Create Dockerfile
	dockerfile := `FROM node:18-alpine

WORKDIR /app

COPY package*.json ./
RUN npm install

COPY . .

EXPOSE 3000

CMD ["npm", "start"]`
	
	if err := editor.WriteToFile("config/Dockerfile", dockerfile); err != nil {
		return err
	}
	
	// Modify Dockerfile
	diff3 := `------- SEARCH
FROM node:18-alpine

WORKDIR /app

COPY package*.json ./
RUN npm install

COPY . .

EXPOSE 3000

CMD ["npm", "start"]
=======
FROM node:18-alpine

WORKDIR /app

# Install dependencies first for better caching
COPY package*.json ./
RUN npm ci --only=production

# Copy source code
COPY . .

# Create non-root user
RUN addgroup -g 1001 -S nodejs
RUN adduser -S nextjs -u 1001

USER nextjs

EXPOSE 8080

CMD ["npm", "start"]
+++++++ REPLACE`
	
	if err := editor.ReplaceInFile("config/Dockerfile", diff3); err != nil {
		return err
	}
	
	return nil
}

func testLargeFileEdits(editor *fileeditor.FileEditor) error {
	// Create a large file and perform many edits
	
	// Generate a large file with many functions
	var content strings.Builder
	content.WriteString("// Large JavaScript file with many functions\n\n")
	
	for i := 0; i < 100; i++ {
		content.WriteString(fmt.Sprintf(`function func%d() {
    console.log("Function %d");
    return %d;
}

`, i, i, i))
	}
	
	if err := editor.WriteToFile("large/large_file.js", content.String()); err != nil {
		return err
	}
	
	// Perform multiple edits
	for i := 0; i < 10; i++ {
		funcNum := i * 10
		diff := fmt.Sprintf(`------- SEARCH
function func%d() {
    console.log("Function %d");
    return %d;
}
=======
function func%d() {
    console.log("Modified Function %d");
    const result = %d * 2;
    return result;
}
+++++++ REPLACE`, funcNum, funcNum, funcNum, funcNum, funcNum, funcNum)
		
		if err := editor.ReplaceInFile("large/large_file.js", diff); err != nil {
			return fmt.Errorf("edit %d failed: %w", i, err)
		}
	}
	
	// Verify modifications
	finalContent, err := editor.ReadFile("large/large_file.js")
	if err != nil {
		return err
	}
	
	modifiedCount := strings.Count(finalContent, "Modified Function")
	if modifiedCount != 10 {
		return fmt.Errorf("expected 10 modifications, found %d", modifiedCount)
	}
	
	return nil
}

func testComplexRefactoring(editor *fileeditor.FileEditor) error {
	// Simulate a complex refactoring scenario
	
	// 1. Create initial class
	classContent := `class UserManager {
    constructor() {
        this.users = [];
    }

    addUser(name, email) {
        this.users.push({ name, email });
    }

    getUser(name) {
        return this.users.find(user => user.name === name);
    }

    removeUser(name) {
        this.users = this.users.filter(user => user.name !== name);
    }
}`
	
	if err := editor.WriteToFile("refactor/UserManager.js", classContent); err != nil {
		return err
	}
	
	// 2. Add ID field to users
	diff1 := `------- SEARCH
    constructor() {
        this.users = [];
    }

    addUser(name, email) {
        this.users.push({ name, email });
    }
=======
    constructor() {
        this.users = [];
        this.nextId = 1;
    }

    addUser(name, email) {
        const user = { id: this.nextId++, name, email };
        this.users.push(user);
        return user;
    }
+++++++ REPLACE`
	
	if err := editor.ReplaceInFile("refactor/UserManager.js", diff1); err != nil {
		return err
	}
	
	// 3. Update getUser to use ID
	diff2 := `------- SEARCH
    getUser(name) {
        return this.users.find(user => user.name === name);
    }
=======
    getUser(id) {
        return this.users.find(user => user.id === id);
    }

    getUserByName(name) {
        return this.users.find(user => user.name === name);
    }
+++++++ REPLACE`
	
	if err := editor.ReplaceInFile("refactor/UserManager.js", diff2); err != nil {
		return err
	}
	
	// 4. Update removeUser to use ID
	diff3 := `------- SEARCH
    removeUser(name) {
        this.users = this.users.filter(user => user.name !== name);
    }
=======
    removeUser(id) {
        this.users = this.users.filter(user => user.id !== id);
    }

    removeUserByName(name) {
        this.users = this.users.filter(user => user.name !== name);
    }
+++++++ REPLACE`
	
	if err := editor.ReplaceInFile("refactor/UserManager.js", diff3); err != nil {
		return err
	}
	
	// 5. Add validation and error handling
	diff4 := `------- SEARCH
    addUser(name, email) {
        const user = { id: this.nextId++, name, email };
        this.users.push(user);
        return user;
    }
=======
    addUser(name, email) {
        if (!name || !email) {
            throw new Error('Name and email are required');
        }
        
        if (this.getUserByName(name)) {
            throw new Error('User with this name already exists');
        }
        
        const user = { id: this.nextId++, name, email };
        this.users.push(user);
        return user;
    }
+++++++ REPLACE`
	
	if err := editor.ReplaceInFile("refactor/UserManager.js", diff4); err != nil {
		return err
	}
	
	return nil
}

func testErrorRecovery(editor *fileeditor.FileEditor) error {
	// Test error handling and recovery
	
	// 1. Create a file
	content := `function test() {
    console.log("test");
}`
	
	if err := editor.WriteToFile("error_test/test.js", content); err != nil {
		return err
	}
	
	// 2. Try to apply an invalid diff (should fail)
	invalidDiff := `------- SEARCH
nonexistent content
=======
replacement
+++++++ REPLACE`
	
	err := editor.ReplaceInFile("error_test/test.js", invalidDiff)
	if err == nil {
		return fmt.Errorf("expected error for invalid diff, but got none")
	}
	
	// 3. Verify file is unchanged
	unchangedContent, err := editor.ReadFile("error_test/test.js")
	if err != nil {
		return err
	}
	
	if unchangedContent != content {
		return fmt.Errorf("file was modified despite error")
	}
	
	// 4. Apply a valid diff
	validDiff := `------- SEARCH
function test() {
    console.log("test");
}
=======
function test() {
    console.log("Hello, World!");
}
+++++++ REPLACE`
	
	if err := editor.ReplaceInFile("error_test/test.js", validDiff); err != nil {
		return err
	}
	
	// 5. Verify the change was applied
	modifiedContent, err := editor.ReadFile("error_test/test.js")
	if err != nil {
		return err
	}
	
	if !strings.Contains(modifiedContent, "Hello, World!") {
		return fmt.Errorf("valid diff was not applied")
	}
	
	return nil
}

