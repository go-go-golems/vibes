// Product Order Form Example
function createProductOrderForm() {
    const formDef = {
        name: "Product Order",
        theme: "Charm",
        groups: [
            {
                name: "Product Selection",
                fields: [
                    {
                        type: "select",
                        key: "product",
                        title: "Choose a Product",
                        options: [
                            { label: "Basic Widget", value: "basic" },
                            { label: "Premium Widget", value: "premium" },
                            { label: "Deluxe Widget", value: "deluxe" }
                        ],
                        attributes: {
                            filterable: true
                        }
                    },
                    {
                        type: "input",
                        key: "quantity",
                        title: "Quantity",
                        value: "1",
                        validation: [
                            {
                                condition: "parseInt(value) <= 0",
                                error: "Please enter a positive number"
                            }
                        ]
                    }
                ]
            },
            {
                name: "Additional Options",
                fields: [
                    {
                        type: "multiselect",
                        key: "addons",
                        title: "Select Add-ons",
                        options: [
                            { label: "Extended Warranty", value: "warranty" },
                            { label: "Gift Wrapping", value: "giftwrap" },
                            { label: "Express Shipping", value: "express" }
                        ],
                        attributes: {
                            limit: 2
                        }
                    },
                    {
                        type: "confirm",
                        key: "terms",
                        title: "Do you accept the terms and conditions?",
                        attributes: {
                            affirmative: "I Accept",
                            negative: "I Do Not Accept"
                        }
                    }
                ]
            }
        ]
    };
    
    return createUI(formDef);
}

// Test the function
console.log("Creating product order form...");
const orderResult = createProductOrderForm();
console.log("Order Result:", orderResult);

