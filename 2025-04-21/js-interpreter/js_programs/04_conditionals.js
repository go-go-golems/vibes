// Conditional statements
const num = 15;

let result;
if (num > 10) {
    result = "Greater than 10";
} else if (num === 10) {
    result = "Equal to 10";
} else {
    result = "Less than 10";
}

const ternary = num % 2 === 0 ? "Even" : "Odd";

// Switch statement
let dayName;
const day = 3;
switch (day) {
    case 1:
        dayName = "Monday";
        break;
    case 2:
        dayName = "Tuesday";
        break;
    case 3:
        dayName = "Wednesday";
        break;
    default:
        dayName = "Other day";
}

console.log("Number:", num);
console.log("If-else result:", result);
console.log("Ternary result:", ternary);
console.log("Switch result:", dayName);
