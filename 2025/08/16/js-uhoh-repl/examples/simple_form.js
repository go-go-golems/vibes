// Simple Contact Form Example
function createContactForm() {
    const formDef = {
        name: "Contact Form",
        theme: "Default",
        groups: [
            {
                name: "Contact Information",
                fields: [
                    {
                        type: "input",
                        key: "name",
                        title: "Your Name",
                        value: "John Doe"
                    },
                    {
                        type: "input",
                        key: "email",
                        title: "Email Address",
                        validation: [
                            {
                                condition: "!value.includes('@')",
                                error: "Please enter a valid email address"
                            }
                        ]
                    },
                    {
                        type: "text",
                        key: "message",
                        title: "Your Message",
                        attributes: {
                            char_limit: 500,
                            lines: 5
                        }
                    }
                ]
            }
        ]
    };
    
    return createUI(formDef);
}

// Test the function
console.log("Creating simple contact form...");
const result = createContactForm();
console.log("Result:", result);

