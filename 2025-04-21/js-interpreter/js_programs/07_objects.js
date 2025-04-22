// Objects and properties
const person = {
    firstName: "John",
    lastName: "Doe",
    age: 30,
    address: {
        street: "123 Main St",
        city: "Anytown",
        country: "USA"
    },
    fullName: function() {
        return this.firstName + " " + this.lastName;
    }
};

// Accessing properties
const firstName = person.firstName;
const city = person["address"]["city"];

// Adding new properties
person.email = "john.doe@example.com";

// Deleting properties
delete person.age;

// Object methods
const fullName = person.fullName();

// Object.keys, values, entries
const keys = Object.keys(person);
const values = Object.values(person.address);
const entries = Object.entries(person.address);

console.log("Person object:", person);
console.log("First name:", firstName);
console.log("City:", city);
console.log("Full name:", fullName);
console.log("Object keys:", keys);
console.log("Address values:", values);
console.log("Address entries:", entries);
