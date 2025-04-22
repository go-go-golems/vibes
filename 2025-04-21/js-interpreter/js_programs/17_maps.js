// Map operations
// Creating Maps
const map = new Map();
map.set('key1', 'value1');
map.set('key2', 'value2');
map.set(3, 'value3');        // Numeric key
map.set({id: 1}, 'value4');  // Object as key
map.set(function() {}, 'value5'); // Function as key

// Creating a Map from an array of key-value pairs
const mapFromArray = new Map([
    ['a', 1],
    ['b', 2],
    ['c', 3]
]);

console.log("Map size:", map.size);
console.log("Map has 'key1':", map.has('key1'));
console.log("Value for 'key2':", map.get('key2'));
console.log("Value for non-existent key:", map.get('nonexistent'));

// Iterating over a Map
console.log("Map keys:");
for (const key of map.keys()) {
    console.log(key);
}

console.log("Map values:");
for (const value of map.values()) {
    console.log(value);
}

console.log("Map entries:");
for (const [key, value] of map.entries()) {
    console.log(key, "->", value);
}

// Modifying a Map
map.set('key1', 'updated value'); // Update existing key
console.log("Updated value:", map.get('key1'));

map.delete('key2');
console.log("After deletion, has 'key2':", map.has('key2'));

// Converting Map to array
const keysArray = Array.from(map.keys());
const valuesArray = Array.from(map.values());
const entriesArray = Array.from(map.entries());

console.log("Keys array:", keysArray);
console.log("Values array:", valuesArray);
console.log("Entries array:", entriesArray);
