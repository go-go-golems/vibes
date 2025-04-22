// Destructuring assignment
// Array destructuring
const colors = ["red", "green", "blue", "yellow", "purple"];
const [firstColor, secondColor, ...restColors] = colors;

console.log("First color:", firstColor);
console.log("Second color:", secondColor);
console.log("Rest of colors:", restColors);

// Skipping elements
const [, , thirdColor] = colors;
console.log("Third color:", thirdColor);

// Default values
const incomplete = ["one", "two"];
const [first, second, third = "default"] = incomplete;
console.log("Third with default:", third);

// Swapping variables
let a = 1;
let b = 2;
[a, b] = [b, a];
console.log("After swap:", a, b);

// Object destructuring
const person = {
    name: "Alice",
    age: 30,
    job: "Engineer",
    address: {
        city: "Boston",
        country: "USA"
    }
};

const { name, age, hobby = "Reading" } = person;
console.log("Name:", name);
console.log("Age:", age);
console.log("Hobby (default):", hobby);

// Renaming variables
const { name: fullName, job: profession } = person;
console.log("Full name:", fullName);
console.log("Profession:", profession);

// Nested destructuring
const { address: { city, country } } = person;
console.log("City:", city);
console.log("Country:", country);

// Function parameter destructuring
function printPersonInfo({ name, age, job }) {
    console.log(`${name} is ${age} years old and works as a ${job}`);
}

printPersonInfo(person);
