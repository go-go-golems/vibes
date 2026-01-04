"use client";
import { ThreadPrimitive } from "@assistant-ui/react";
import { FC, useState, useEffect } from "react";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "./ui/tabs";
import { sendChatMessage, ChatMessage } from "@/lib/api";
import { Input } from "./ui/input";
import { Button } from "./ui/button";

export const AssistantSidebar: FC<{ children: React.ReactNode }> = ({
  children,
}) => {
  const [messages, setMessages] = useState<ChatMessage[]>([
    { role: "assistant", content: "Hello! I'm here to help you fill out the form. What's your name?" }
  ]);
  const [inputValue, setInputValue] = useState("");
  const [isLoading, setIsLoading] = useState(false);

  const handleSendMessage = async () => {
    if (!inputValue.trim() || isLoading) return;
    
    // Add user message to UI immediately
    const userMessage: ChatMessage = { role: "user", content: inputValue };
    setMessages(prev => [...prev, userMessage]);
    setInputValue("");
    setIsLoading(true);
    
    try {
      // Send to backend
      const response = await sendChatMessage(inputValue);
      if (response.success && response.data) {
        // Update with full history from backend
        setMessages(response.data.history);
      }
    } catch (error) {
      console.error("Error sending message:", error);
      // Add error message
      setMessages(prev => [...prev, { 
        role: "assistant", 
        content: "Sorry, I encountered an error. Please try again." 
      }]);
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <Tabs defaultValue="form" className="mx-auto flex h-full max-w-[1200px] flex-col px-4 pt-4">
      <TabsList className="grid w-full grid-cols-2">
        <TabsTrigger value="form">Form</TabsTrigger>
        <TabsTrigger value="chat">Chat</TabsTrigger>
      </TabsList>
      <TabsContent value="form" className="flex-1 overflow-auto">
        {children}
      </TabsContent>
      <TabsContent value="chat" className="flex-1 overflow-hidden">
        <div className="h-full overflow-hidden rounded-md border p-4 flex flex-col">
          <div className="mb-4 text-center">
            <h3 className="text-lg font-medium">Chat Assistant</h3>
            <p>I can help you fill out the form. Just tell me what information you want to provide.</p>
          </div>
          <div className="space-y-4 flex-1 overflow-y-auto mb-4">
            {messages.map((msg, index) => (
              <div 
                key={index} 
                className={`rounded-lg p-3 ${
                  msg.role === "assistant" ? "bg-gray-100" : "bg-blue-100 text-right"
                }`}
              >
                <p className={`text-sm ${
                  msg.role === "assistant" ? "text-gray-800" : "text-blue-800"
                }`}>
                  {msg.content}
                </p>
              </div>
            ))}
            {isLoading && (
              <div className="rounded-lg bg-gray-100 p-3">
                <p className="text-sm text-gray-800">Thinking...</p>
              </div>
            )}
          </div>
          <div className="flex items-center gap-2">
            <Input 
              type="text" 
              placeholder="Type your message..." 
              className="flex-1 rounded-md border border-gray-300 p-2 text-sm"
              value={inputValue}
              onChange={(e) => setInputValue(e.target.value)}
              onKeyDown={(e) => e.key === "Enter" && handleSendMessage()}
              disabled={isLoading}
            />
            <Button 
              className="rounded-md bg-blue-500 px-3 py-2 text-sm text-white"
              onClick={handleSendMessage}
              disabled={isLoading}
            >
              Send
            </Button>
          </div>
        </div>
      </TabsContent>
    </Tabs>
  );
};
