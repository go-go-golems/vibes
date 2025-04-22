// Regular expressions
const text = "The quick brown fox jumps over the lazy dog. The dog was not amused.";

// Creating RegExp objects
const regex1 = /fox/;
const regex2 = new RegExp("dog", "g");

// Testing for matches
const containsFox = regex1.test(text);
const containsDog = regex2.test(text);

// Finding matches
const foxMatch = text.match(/fox/);
const dogMatches = text.match(/dog/g);

// Replacing with regex
const replaced = text.replace(/dog/g, "cat");

// Using capture groups
const pattern = /(\w+)\s(\w+)/;
const matches = "John Doe".match(pattern);
const firstName = matches ? matches[1] : "";
const lastName = matches ? matches[2] : "";

// Using special characters
const digits = "abc123def456".match(/\d+/g);
const words = text.match(/\b\w{5,}\b/g); // Words with 5+ characters

// Using regex with split
const parts = text.split(/\s+/);

console.log("Contains 'fox':", containsFox);
console.log("Contains 'dog':", containsDog);
console.log("Fox match:", foxMatch);
console.log("Dog matches:", dogMatches);
console.log("Replaced text:", replaced);
console.log("First name:", firstName);
console.log("Last name:", lastName);
console.log("Digits:", digits);
console.log("Long words:", words);
console.log("Split parts count:", parts.length);
