// Comprehensive test file for tree-sitter goja module
// This file tests various JavaScript constructs and features

// Import statements
import { Component } from 'react';
import * as utils from './utils';

// Variable declarations
const API_URL = 'https://api.example.com';
let currentUser = null;
var globalConfig = {
    debug: true,
    timeout: 5000
};

// Function declarations
function greet(name) {
    return `Hello, ${name}!`;
}

async function fetchUserData(userId) {
    try {
        const response = await fetch(`${API_URL}/users/${userId}`);
        const data = await response.json();
        return data;
    } catch (error) {
        console.error('Failed to fetch user data:', error);
        throw error;
    }
}

function* generateNumbers(max) {
    for (let i = 0; i < max; i++) {
        yield i;
    }
}

// Arrow functions
const add = (a, b) => a + b;
const multiply = (x, y) => {
    return x * y;
};

// Class declarations
class User {
    constructor(name, email) {
        this.name = name;
        this.email = email;
        this.createdAt = new Date();
    }
    
    getName() {
        return this.name;
    }
    
    async save() {
        const response = await fetch(`${API_URL}/users`, {
            method: 'POST',
            body: JSON.stringify(this)
        });
        return response.json();
    }
    
    static findById(id) {
        return fetch(`${API_URL}/users/${id}`).then(r => r.json());
    }
}

class AdminUser extends User {
    constructor(name, email, permissions) {
        super(name, email);
        this.permissions = permissions;
    }
    
    hasPermission(permission) {
        return this.permissions.includes(permission);
    }
}

// Object literals
const config = {
    api: {
        baseUrl: API_URL,
        timeout: 10000,
        retries: 3
    },
    features: {
        darkMode: true,
        notifications: false
    }
};

// Array operations
const numbers = [1, 2, 3, 4, 5];
const doubled = numbers.map(n => n * 2);
const filtered = numbers.filter(n => n > 2);

// Destructuring
const { api, features } = config;
const [first, second, ...rest] = numbers;

// Template literals
const message = `Welcome ${currentUser?.name || 'Guest'}!
Your session expires in ${config.api.timeout}ms.`;

// Conditional statements
if (currentUser) {
    console.log('User is logged in');
} else {
    console.log('Please log in');
}

// Loops
for (const number of numbers) {
    console.log(number);
}

for (let i = 0; i < numbers.length; i++) {
    console.log(numbers[i]);
}

while (currentUser === null) {
    // Wait for user login
    break;
}

// Try-catch blocks
try {
    const result = JSON.parse('{"valid": "json"}');
    console.log(result);
} catch (error) {
    console.error('JSON parse error:', error);
} finally {
    console.log('Cleanup completed');
}

// Function calls
greet('World');
add(5, 3);
User.findById(123);

// Member expressions
console.log(config.api.baseUrl);
console.error('This is an error');
Math.max(...numbers);

// Export statements
export { User, AdminUser };
export default config;

// Comments
/* This is a multi-line comment
   that spans multiple lines */

// Single line comment

/**
 * JSDoc comment
 * @param {string} name - The user's name
 * @returns {string} A greeting message
 */
function createGreeting(name) {
    return `Hello, ${name}!`;
}

