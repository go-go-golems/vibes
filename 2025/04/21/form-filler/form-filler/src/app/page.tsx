"use client";
import { SignupForm } from "@/components/SignupForm";
import { AssistantSidebar } from "@/components/AssistantSidebar";
import { Form } from "@/components/ui/form";
import { useAssistantForm } from "@assistant-ui/react-hook-form";
import { useAssistantInstructions } from "@assistant-ui/react";
import { setFormField, submitForm } from "@/lib/api";

const SetFormFieldTool = () => {
  return (
    <p className="text-center font-mono text-sm font-bold text-blue-500">
      set_form_field(...)
    </p>
  );
};

const SubmitFormTool = () => {
  return (
    <p className="text-center font-mono text-sm font-bold text-blue-500">
      submit_form(...)
    </p>
  );
};

export default function Home() {
  useAssistantInstructions("Help users fill out the form by asking for their information one by one. Be friendly and conversational.");
  
  const form = useAssistantForm({
    defaultValues: {
      firstName: "",
      lastName: "",
      email: "",
      city: "",
      projectIdea: "",
    },
    assistant: {
      tools: {
        set_form_field: {
          render: SetFormFieldTool,
          parameters: {
            type: "object",
            properties: {
              field: {
                type: "string",
                enum: ["firstName", "lastName", "email", "city", "projectIdea"],
              },
              value: {
                type: "string",
              },
            },
            required: ["field", "value"],
          },
          exec: async ({ field, value }) => {
            try {
              const response = await setFormField(field, value);
              if (response.success) {
                // Update the form field
                form.setValue(field, value);
                return `Set ${field} to "${value}"`;
              } else {
                return `Failed to set ${field}: ${response.message}`;
              }
            } catch (error) {
              console.error("Error setting form field:", error);
              return "Error setting form field. Please try again.";
            }
          }
        },
        submit_form: {
          render: SubmitFormTool,
          parameters: {
            type: "object",
            properties: {},
          },
          exec: async () => {
            try {
              const formData = form.getValues();
              const response = await submitForm(formData);
              if (response.success) {
                return "Form submitted successfully!";
              } else {
                return `Failed to submit form: ${response.message}`;
              }
            } catch (error) {
              console.error("Error submitting form:", error);
              return "Error submitting form. Please try again.";
            }
          }
        },
      },
    },
  });

  return (
    <AssistantSidebar>
      <div className="h-full overflow-y-auto">
        <main className="container py-8">
          <h1 className="mb-2 text-2xl font-semibold">
            Form Filling Co-Pilot
          </h1>
          <p className="mb-6">
            I'm here to help you fill out this form. You can chat with me to provide your information.
          </p>
          <Form {...form}>
            <SignupForm />
          </Form>
        </main>
      </div>
    </AssistantSidebar>
  );
}
