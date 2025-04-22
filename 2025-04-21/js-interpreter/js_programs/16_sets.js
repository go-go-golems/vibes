// Set operations
// Creating sets
const set1 = new Set([1, 2, 3, 4, 5]);
const set2 = new Set([3, 4, 5, 6, 7]);
const duplicateSet = new Set([1, 1, 2, 2, 3, 3]);

console.log("Set1:", [...set1]);
console.log("Set2:", [...set2]);
console.log("Set with duplicates:", [...duplicateSet]);

// Basic set operations
console.log("Set1 size:", set1.size);
console.log("Set1 has 3:", set1.has(3));
console.log("Set1 has 10:", set1.has(10));

// Adding and deleting elements
const modifiedSet = new Set([1, 2]);
modifiedSet.add(3);
modifiedSet.add(4);
console.log("After adding:", [...modifiedSet]);

modifiedSet.delete(2);
console.log("After deleting:", [...modifiedSet]);

// Set operations (using array methods with spread operator)
// Union
const union = new Set([...set1, ...set2]);
console.log("Union:", [...union]);

// Intersection
const intersection = new Set([...set1].filter(x => set2.has(x)));
console.log("Intersection:", [...intersection]);

// Difference (set1 - set2)
const difference = new Set([...set1].filter(x => !set2.has(x)));
console.log("Difference (set1 - set2):", [...difference]);

// Symmetric difference
const symmetricDifference = new Set(
    [...set1].filter(x => !set2.has(x)).concat([...set2].filter(x => !set1.has(x)))
);
console.log("Symmetric difference:", [...symmetricDifference]);
