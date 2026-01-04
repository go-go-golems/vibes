// Array operations
const arr = [1, 2, 3, 4, 5];

const mapped = arr.map(x => x * 2);
const filtered = arr.filter(x => x % 2 === 0);
const reduced = arr.reduce((acc, val) => acc + val, 0);
const found = arr.find(x => x > 3);
const every = arr.every(x => x > 0);
const some = arr.some(x => x > 4);
const joined = arr.join('-');
const reversed = [...arr].reverse();

console.log("Original array:", arr);
console.log("Mapped (x2):", mapped);
console.log("Filtered (even):", filtered);
console.log("Reduced (sum):", reduced);
console.log("Found (>3):", found);
console.log("Every (>0):", every);
console.log("Some (>4):", some);
console.log("Joined:", joined);
console.log("Reversed:", reversed);
