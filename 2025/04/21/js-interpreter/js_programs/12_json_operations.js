// JSON operations
const person = {
    name: "Alice",
    age: 28,
    isEmployed: true,
    skills: ["JavaScript", "HTML", "CSS"],
    address: {
        city: "San Francisco",
        country: "USA"
    }
};

// Converting object to JSON string
const jsonString = JSON.stringify(person);
console.log("JSON string:", jsonString);

// Pretty printing with indentation
const prettyJson = JSON.stringify(person, null, 2);
console.log("Pretty JSON:");
console.log(prettyJson);

// Converting JSON string back to object
const parsedPerson = JSON.parse(jsonString);
console.log("Parsed object name:", parsedPerson.name);
console.log("Parsed object skills:", parsedPerson.skills);

// Using replacer function to filter properties
const filteredJson = JSON.stringify(person, (key, value) => {
    // Skip the 'age' property
    if (key === 'age') return undefined;
    return value;
});
console.log("Filtered JSON:", filteredJson);

// Using reviver function when parsing
const revivedJson = JSON.parse(jsonString, (key, value) => {
    // Convert skill array to uppercase
    if (key === 'skills' && Array.isArray(value)) {
        return value.map(skill => skill.toUpperCase());
    }
    return value;
});
console.log("Revived skills:", revivedJson.skills);
