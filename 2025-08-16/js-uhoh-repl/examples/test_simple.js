// Very simple test form without validation
console.log("Creating very simple test form...");

const result = createUI({
    name: "Simple Test",
    theme: "Default",
    groups: [{
        name: "Basic",
        fields: [{
            type: "input",
            key: "name",
            title: "Your Name",
            value: "Test User"
        }]
    }]
});

console.log("Result:", result);

