// Error handling
try {
    // This will throw an error
    const result = 10 / 0;
    console.log("Result:", result);
    
    // This line won't execute
    console.log("This won't be printed");
} catch (error) {
    console.log("Caught an error:", error.message);
} finally {
    console.log("Finally block always executes");
}

// Try-catch with custom error
try {
    const age = 15;
    if (age < 18) {
        throw new Error("Age must be at least 18");
    }
    console.log("Access granted");
} catch (error) {
    console.log("Access denied:", error.message);
}

// Error handling with specific error types
try {
    // Trying to access property of undefined
    const user = undefined;
    const name = user.name;
} catch (error) {
    if (error instanceof TypeError) {
        console.log("Type error:", error.message);
    } else {
        console.log("Other error:", error.message);
    }
}
