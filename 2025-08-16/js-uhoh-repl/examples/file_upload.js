// File Upload Form Example
function createFileUploadForm() {
    const formDef = {
        name: "File Upload",
        theme: "Dracula",
        groups: [
            {
                name: "File Selection",
                fields: [
                    {
                        type: "filepicker",
                        key: "upload_file",
                        title: "Select a file to upload",
                        attributes: {
                            current_directory: "/home/ubuntu",
                            allowed_types: [".pdf", ".doc", ".docx", ".txt", ".js"],
                            show_hidden: false,
                            show_size: true,
                            file_allowed: true,
                            dir_allowed: false
                        }
                    },
                    {
                        type: "note",
                        title: "Upload Instructions",
                        description: "Please select a document file to upload. Supported formats: PDF, Word documents, text files, and JavaScript files.",
                        attributes: {
                            show_next_button: true,
                            next_label: "Proceed to Upload"
                        }
                    }
                ]
            },
            {
                name: "Upload Options",
                fields: [
                    {
                        type: "select",
                        key: "visibility",
                        title: "File Visibility",
                        options: [
                            { label: "Private", value: "private" },
                            { label: "Public", value: "public" },
                            { label: "Shared with Team", value: "team" }
                        ],
                        value: "private"
                    },
                    {
                        type: "confirm",
                        key: "notify",
                        title: "Send notification when upload completes?",
                        value: true
                    }
                ]
            }
        ]
    };
    
    return createUI(formDef);
}

// Test the function
console.log("Creating file upload form...");
const uploadResult = createFileUploadForm();
console.log("Upload Result:", uploadResult);

