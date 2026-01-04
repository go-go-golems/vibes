// Generators and iterators
// Simple generator function
function* simpleGenerator() {
    yield 1;
    yield 2;
    yield 3;
}

// Using the generator
const generator = simpleGenerator();
console.log("Generator result 1:", generator.next().value);
console.log("Generator result 2:", generator.next().value);
console.log("Generator result 3:", generator.next().value);
console.log("Generator done:", generator.next().done);

// Generator with parameters
function* parameterizedGenerator(start, end) {
    for (let i = start; i <= end; i++) {
        yield i;
    }
}

// Iterating over all values
console.log("Parameterized generator values:");
for (const value of parameterizedGenerator(5, 8)) {
    console.log(value);
}

// Infinite sequence generator with early termination
function* fibonacciGenerator() {
    let a = 0, b = 1;
    while (true) {
        yield a;
        [a, b] = [b, a + b];
    }
}

// Taking only first 6 values from infinite sequence
const fibValues = [];
const fibGen = fibonacciGenerator();
for (let i = 0; i < 6; i++) {
    fibValues.push(fibGen.next().value);
}
console.log("First 6 Fibonacci numbers:", fibValues);

// Generator delegation using yield*
function* generatorA() {
    yield 'A1';
    yield 'A2';
}

function* generatorB() {
    yield 'B1';
    yield* generatorA(); // Delegate to generatorA
    yield 'B2';
}

console.log("Generator delegation results:");
for (const value of generatorB()) {
    console.log(value);
}
