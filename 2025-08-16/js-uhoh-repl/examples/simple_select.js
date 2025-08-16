// Simple Select Form Example
console.log("Creating simple select form...");

const result = createUI({
    name: "Product Selection",
    theme: "Charm",
    groups: [{
        name: "Choose Product",
        fields: [{
            type: "select",
            key: "product",
            title: "Select a Product",
            options: [
                { label: "Basic Widget", value: "basic" },
                { label: "Premium Widget", value: "premium" },
                { label: "Deluxe Widget", value: "deluxe" }
            ]
        }, {
            type: "confirm",
            key: "newsletter",
            title: "Subscribe to newsletter?",
            value: false
        }]
    }]
});

console.log("Result:", result);

