"use client";
import { AssistantRuntimeProvider } from "@assistant-ui/react";
import { useChatRuntime } from "@assistant-ui/react-ai-sdk";

export function MyRuntimeProvider({
  children,
}: Readonly<{ children: React.ReactNode }>) {
  // Create a mock runtime that doesn't actually call an API
  const runtime = useChatRuntime({
    api: "/api/chat", // This won't be used in mock mode
    mock: true, // Enable mock mode
    mockMessages: [
      {
        role: "assistant",
        content: "Hello! I'm here to help you fill out the form. What's your name?",
      },
    ],
  });

  return (
    <AssistantRuntimeProvider runtime={runtime}>
      {children}
    </AssistantRuntimeProvider>
  );
}
