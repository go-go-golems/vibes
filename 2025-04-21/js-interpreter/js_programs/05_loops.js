// Loops
const loopCount = 5;

// For loop
let forSum = 0;
for (let i = 0; i < loopCount; i++) {
    forSum += i;
}

// While loop
let whileSum = 0;
let whileCounter = 0;
while (whileCounter < loopCount) {
    whileSum += whileCounter;
    whileCounter++;
}

// Do-while loop
let doWhileSum = 0;
let doWhileCounter = 0;
do {
    doWhileSum += doWhileCounter;
    doWhileCounter++;
} while (doWhileCounter < loopCount);

// For...of loop (iterating over array)
const numbers = [1, 2, 3, 4, 5];
let forOfSum = 0;
for (const num of numbers) {
    forOfSum += num;
}

// For...in loop (iterating over object properties)
const person = { name: "John", age: 30, job: "Developer" };
const properties = [];
for (const prop in person) {
    properties.push(prop);
}

console.log("For loop sum:", forSum);
console.log("While loop sum:", whileSum);
console.log("Do-while loop sum:", doWhileSum);
console.log("For...of loop sum:", forOfSum);
console.log("Object properties:", properties);
