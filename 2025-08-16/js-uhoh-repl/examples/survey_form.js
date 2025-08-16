// Survey Form Example
function createSurveyForm() {
    const formDef = {
        name: "Customer Satisfaction Survey",
        theme: "Catppuccin",
        groups: [
            {
                name: "Personal Information",
                fields: [
                    {
                        type: "input",
                        key: "customer_id",
                        title: "Customer ID",
                        attributes: {
                            placeholder: "Enter your customer ID"
                        }
                    },
                    {
                        type: "select",
                        key: "age_group",
                        title: "Age Group",
                        options: [
                            { label: "18-25", value: "18-25" },
                            { label: "26-35", value: "26-35" },
                            { label: "36-45", value: "36-45" },
                            { label: "46-55", value: "46-55" },
                            { label: "56+", value: "56+" }
                        ]
                    }
                ]
            },
            {
                name: "Service Evaluation",
                fields: [
                    {
                        type: "select",
                        key: "satisfaction",
                        title: "Overall Satisfaction",
                        options: [
                            { label: "Very Satisfied", value: 5 },
                            { label: "Satisfied", value: 4 },
                            { label: "Neutral", value: 3 },
                            { label: "Dissatisfied", value: 2 },
                            { label: "Very Dissatisfied", value: 1 }
                        ]
                    },
                    {
                        type: "multiselect",
                        key: "improvement_areas",
                        title: "Areas for Improvement",
                        options: [
                            { label: "Customer Service", value: "customer_service" },
                            { label: "Product Quality", value: "product_quality" },
                            { label: "Delivery Speed", value: "delivery_speed" },
                            { label: "Website Experience", value: "website" },
                            { label: "Pricing", value: "pricing" },
                            { label: "Communication", value: "communication" }
                        ],
                        attributes: {
                            limit: 3
                        }
                    },
                    {
                        type: "text",
                        key: "comments",
                        title: "Additional Comments",
                        attributes: {
                            lines: 4,
                            placeholder: "Please share any additional feedback..."
                        }
                    }
                ]
            },
            {
                name: "Follow-up",
                fields: [
                    {
                        type: "confirm",
                        key: "follow_up",
                        title: "Would you like us to follow up on your feedback?",
                        value: false
                    },
                    {
                        type: "confirm",
                        key: "newsletter",
                        title: "Subscribe to our newsletter for updates?",
                        value: false
                    }
                ]
            }
        ]
    };
    
    return createUI(formDef);
}

// Test the function
console.log("Creating customer satisfaction survey...");
const surveyResult = createSurveyForm();
console.log("Survey Result:", surveyResult);

