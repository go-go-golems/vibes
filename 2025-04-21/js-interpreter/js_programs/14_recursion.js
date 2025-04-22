// Recursion
// Factorial calculation using recursion
function factorial(n) {
    if (n <= 1) {
        return 1;
    }
    return n * factorial(n - 1);
}

console.log("Factorial of 5:", factorial(5));

// Fibonacci sequence using recursion
function fibonacci(n) {
    if (n <= 1) {
        return n;
    }
    return fibonacci(n - 1) + fibonacci(n - 2);
}

console.log("Fibonacci of 7:", fibonacci(7));

// Recursive function to calculate the sum of an array
function sumArray(arr, index = 0) {
    if (index >= arr.length) {
        return 0;
    }
    return arr[index] + sumArray(arr, index + 1);
}

console.log("Sum of array [1, 2, 3, 4, 5]:", sumArray([1, 2, 3, 4, 5]));

// Recursive function to flatten a nested array
function flattenArray(arr) {
    let result = [];
    
    for (let i = 0; i < arr.length; i++) {
        if (Array.isArray(arr[i])) {
            result = result.concat(flattenArray(arr[i]));
        } else {
            result.push(arr[i]);
        }
    }
    
    return result;
}

const nestedArray = [1, [2, [3, 4], 5], 6];
console.log("Flattened array:", flattenArray(nestedArray));
