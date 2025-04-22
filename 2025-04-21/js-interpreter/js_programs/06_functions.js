// Functions
// Regular function declaration
function add(a, b) {
    return a + b;
}

// Function expression
const subtract = function(a, b) {
    return a - b;
};

// Arrow function
const multiply = (a, b) => a * b;

// Function with default parameters
function divide(a, b = 1) {
    return a / b;
}

// Function with rest parameters
function sum(...numbers) {
    return numbers.reduce((total, num) => total + num, 0);
}

// Immediately Invoked Function Expression (IIFE)
const result = (function(x) {
    return x * x;
})(4);

console.log("Add:", add(5, 3));
console.log("Subtract:", subtract(10, 4));
console.log("Multiply:", multiply(3, 6));
console.log("Divide:", divide(8, 2));
console.log("Divide (default param):", divide(8));
console.log("Sum:", sum(1, 2, 3, 4, 5));
console.log("IIFE result:", result);
