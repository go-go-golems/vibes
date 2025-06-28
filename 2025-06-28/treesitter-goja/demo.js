// Demo JavaScript file for testing tree-sitter parsing
function greet(name) {
    console.log("Hello, " + name + "!");
    return "Hello, " + name + "!";
}

class Calculator {
    constructor() {
        this.result = 0;
    }
    
    add(x, y) {
        this.result = x + y;
        return this.result;
    }
    
    multiply(a, b) {
        return a * b;
    }
}

const numbers = [1, 2, 3, 4, 5];
const doubled = numbers.map(n => n * 2);

// Arrow function
const square = x => x * x;

// Async function
async function fetchData(url) {
    try {
        const response = await fetch(url);
        const data = await response.json();
        return data;
    } catch (error) {
        console.error("Error fetching data:", error);
        throw error;
    }
}

// Object literal
const config = {
    apiUrl: "https://api.example.com",
    timeout: 5000,
    retries: 3
};

// Export
export { greet, Calculator, fetchData, config };

