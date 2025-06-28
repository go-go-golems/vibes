// Simple test file for tree-sitter goja module
// Compatible with Goja JavaScript engine

// Variable declarations
var API_URL = 'https://api.example.com';
var currentUser = null;
var globalConfig = {
    debug: true,
    timeout: 5000
};

// Function declarations
function greet(name) {
    return 'Hello, ' + name + '!';
}

function fetchUserData(userId) {
    console.log('Fetching user data for ID:', userId);
    return { id: userId, name: 'Test User' };
}

// Class declarations
function User(name, email) {
    this.name = name;
    this.email = email;
    this.createdAt = new Date();
}

User.prototype.getName = function() {
    return this.name;
};

User.prototype.save = function() {
    console.log('Saving user:', this.name);
    return this;
};

User.findById = function(id) {
    console.log('Finding user by ID:', id);
    return new User('Found User', 'found@example.com');
};

function AdminUser(name, email, permissions) {
    User.call(this, name, email);
    this.permissions = permissions;
}

AdminUser.prototype = Object.create(User.prototype);
AdminUser.prototype.constructor = AdminUser;

AdminUser.prototype.hasPermission = function(permission) {
    return this.permissions.indexOf(permission) !== -1;
};

// Object literals
var config = {
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
var numbers = [1, 2, 3, 4, 5];
var doubled = numbers.map(function(n) { return n * 2; });
var filtered = numbers.filter(function(n) { return n > 2; });

// Conditional statements
if (currentUser) {
    console.log('User is logged in');
} else {
    console.log('Please log in');
}

// Loops
for (var i = 0; i < numbers.length; i++) {
    console.log('Number:', numbers[i]);
}

// Try-catch blocks
try {
    var result = JSON.parse('{"valid": "json"}');
    console.log('Parsed result:', result);
} catch (error) {
    console.error('JSON parse error:', error);
}

// Function calls
greet('World');
User.findById(123);

// Member expressions
console.log('API URL:', config.api.baseUrl);
console.error('This is an error message');

// Comments
/* This is a multi-line comment
   that spans multiple lines */

// Single line comment

// More function declarations for testing
function calculateSum(a, b) {
    return a + b;
}

function processData(data) {
    if (!data) {
        return null;
    }
    
    var processed = [];
    for (var i = 0; i < data.length; i++) {
        processed.push(data[i] * 2);
    }
    return processed;
}

// More variable declarations
var isActive = true;
var counter = 0;
var items = ['apple', 'banana', 'cherry'];

// More method calls
Math.max(1, 2, 3, 4, 5);
Array.isArray(items);
String.prototype.toUpperCase.call('hello');

