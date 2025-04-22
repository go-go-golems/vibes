// Prototype and inheritance
// Parent constructor function
function Animal(name) {
    this.name = name;
}

// Adding method to the prototype
Animal.prototype.speak = function() {
    return `${this.name} makes a noise.`;
};

// Child constructor function
function Dog(name, breed) {
    // Call parent constructor
    Animal.call(this, name);
    this.breed = breed;
}

// Set up inheritance
Dog.prototype = Object.create(Animal.prototype);
Dog.prototype.constructor = Dog;

// Override the speak method
Dog.prototype.speak = function() {
    return `${this.name} barks.`;
};

// Add a new method to Dog prototype
Dog.prototype.getBreed = function() {
    return this.breed;
};

// Creating instances
const animal = new Animal("Generic Animal");
const dog = new Dog("Rex", "German Shepherd");

console.log("Animal name:", animal.name);
console.log("Animal speaks:", animal.speak());
console.log("Dog name:", dog.name);
console.log("Dog breed:", dog.getBreed());
console.log("Dog speaks:", dog.speak());

// Checking inheritance
console.log("dog instanceof Dog:", dog instanceof Dog);
console.log("dog instanceof Animal:", dog instanceof Animal);

// Creating object with Object.create
const personProto = {
    greet: function() {
        return `Hello, my name is ${this.name}`;
    }
};

const person = Object.create(personProto);
person.name = "John";
console.log("Person greeting:", person.greet());
