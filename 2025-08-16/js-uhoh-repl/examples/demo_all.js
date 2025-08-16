// Comprehensive Demo Script
console.log("=== JavaScript + Uhoh REPL Demo ===");
console.log("This script demonstrates all the available functionality.\n");

// Helper function to create a separator
function separator(title) {
    console.log("=" + "=".repeat(title.length + 2) + "=");
    console.log("| " + title + " |");
    console.log("=" + "=".repeat(title.length + 2) + "=");
}

// Demo 1: Simple Input Form
separator("Demo 1: Simple Input Form");
function createSimpleForm() {
    return createUI({
        name: "Simple Demo",
        theme: "Default",
        groups: [{
            name: "Basic Info",
            fields: [
                {
                    type: "input",
                    key: "username",
                    title: "Username",
                    value: "demo_user"
                },
                {
                    type: "confirm",
                    key: "agree",
                    title: "I agree to the terms",
                    value: true
                }
            ]
        }]
    });
}

const demo1 = createSimpleForm();
console.log("Simple form result:", demo1);
console.log("");

// Demo 2: Complex Selection Form
separator("Demo 2: Complex Selection Form");
function createComplexForm() {
    return createUI({
        name: "Complex Demo",
        theme: "Charm",
        groups: [
            {
                name: "Preferences",
                fields: [
                    {
                        type: "select",
                        key: "theme",
                        title: "Choose Theme",
                        options: [
                            { label: "Light", value: "light" },
                            { label: "Dark", value: "dark" },
                            { label: "Auto", value: "auto" }
                        ],
                        value: "dark"
                    },
                    {
                        type: "multiselect",
                        key: "features",
                        title: "Select Features",
                        options: [
                            { label: "Notifications", value: "notifications" },
                            { label: "Auto-save", value: "autosave" },
                            { label: "Sync", value: "sync" }
                        ]
                    }
                ]
            }
        ]
    });
}

const demo2 = createComplexForm();
console.log("Complex form result:", demo2);
console.log("");

// Demo 3: Text and Note Form
separator("Demo 3: Text and Note Form");
function createTextForm() {
    return createUI({
        name: "Text Demo",
        theme: "Dracula",
        groups: [{
            name: "Content",
            fields: [
                {
                    type: "note",
                    title: "Instructions",
                    description: "Please fill out the form below with your information."
                },
                {
                    type: "text",
                    key: "description",
                    title: "Description",
                    attributes: {
                        lines: 3,
                        placeholder: "Enter your description here..."
                    }
                }
            ]
        }]
    });
}

const demo3 = createTextForm();
console.log("Text form result:", demo3);
console.log("");

// Demo 4: Validation Example
separator("Demo 4: Form with Validation");
function createValidationForm() {
    return createUI({
        name: "Validation Demo",
        theme: "Base16",
        groups: [{
            name: "Validated Fields",
            fields: [
                {
                    type: "input",
                    key: "email",
                    title: "Email Address",
                    validation: [
                        {
                            condition: "!value.includes('@')",
                            error: "Must contain @ symbol"
                        }
                    ]
                },
                {
                    type: "input",
                    key: "age",
                    title: "Age",
                    validation: [
                        {
                            condition: "parseInt(value) < 18",
                            error: "Must be 18 or older"
                        }
                    ]
                }
            ]
        }]
    });
}

const demo4 = createValidationForm();
console.log("Validation form result:", demo4);
console.log("");

// Demo 5: All Field Types
separator("Demo 5: All Field Types Showcase");
function createShowcaseForm() {
    return createUI({
        name: "Field Types Showcase",
        theme: "Catppuccin",
        groups: [
            {
                name: "Input Fields",
                fields: [
                    {
                        type: "input",
                        key: "text_input",
                        title: "Text Input",
                        attributes: {
                            placeholder: "Type something..."
                        }
                    },
                    {
                        type: "text",
                        key: "textarea",
                        title: "Text Area",
                        attributes: {
                            lines: 2
                        }
                    }
                ]
            },
            {
                name: "Selection Fields",
                fields: [
                    {
                        type: "select",
                        key: "single_select",
                        title: "Single Select",
                        options: [
                            { label: "Option 1", value: "opt1" },
                            { label: "Option 2", value: "opt2" }
                        ]
                    },
                    {
                        type: "multiselect",
                        key: "multi_select",
                        title: "Multi Select",
                        options: [
                            { label: "Choice A", value: "a" },
                            { label: "Choice B", value: "b" },
                            { label: "Choice C", value: "c" }
                        ]
                    }
                ]
            },
            {
                name: "Other Fields",
                fields: [
                    {
                        type: "confirm",
                        key: "confirmation",
                        title: "Confirm Action",
                        value: false
                    },
                    {
                        type: "note",
                        title: "Information",
                        description: "This is an informational note field."
                    }
                ]
            }
        ]
    });
}

const demo5 = createShowcaseForm();
console.log("Showcase form result:", demo5);
console.log("");

separator("Demo Complete");
console.log("All demos have been executed successfully!");
console.log("You can now load individual example files using:");
console.log("  /load examples/simple_form.js");
console.log("  /load examples/product_order.js");
console.log("  /load examples/file_upload.js");
console.log("  /load examples/survey_form.js");

