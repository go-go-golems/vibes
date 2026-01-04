import React, { useState, useEffect, useRef } from 'react';

// Types for our chat application
interface Message {
  id: string;
  role: 'user' | 'assistant';
  content: string;
  isStreaming?: boolean;
}

interface Session {
  id: string;
  title: string;
  messages: Message[];
}

const App: React.FC = () => {
  const [sessions, setSessions] = useState<Session[]>([
    {
      id: '1',
      title: 'Welcome Session',
      messages: [
        {
          id: '1',
          role: 'assistant',
          content: 'Hello! I am your coding assistant. Ask me anything about programming!',
        }
      ]
    }
  ]);
  const [activeSessionId, setActiveSessionId] = useState<string>('1');
  const [inputMessage, setInputMessage] = useState<string>('');
  const messagesEndRef = useRef<HTMLDivElement>(null);

  // Get the active session
  const activeSession = sessions.find(s => s.id === activeSessionId) || sessions[0];

  // Scroll to bottom of messages
  const scrollToBottom = () => {
    messagesEndRef.current?.scrollIntoView({ behavior: 'smooth' });
  };

  useEffect(() => {
    scrollToBottom();
  }, [activeSession.messages]);

  // Function to simulate streaming response
  const simulateStreamingResponse = async (message: string): Promise<string> => {
    // Determine response based on message content
    let response = '';
    
    // Check if message contains code-related keywords
    const codeKeywords = ['code', 'example', 'javascript', 'python', 'go', 'typescript', 'react'];
    const hasCodeKeyword = codeKeywords.some(keyword => 
      message.toLowerCase().includes(keyword)
    );
    
    // Code examples for different languages
    const codeExamples: Record<string, string> = {
      javascript: 'console.log("Hello, world!");',
      python: 'print("Hello, world!")',
      go: `package main

import "fmt"

func main() {
    fmt.Println("Hello, world!")
}`,
      typescript: 'const greeting: string = "Hello, world!";\nconsole.log(greeting);',
      react: `import React from "react";

const App = () => {
    return <div>Hello, world!</div>;
};

export default App;`
    };
    
    // Prepare response based on message content
    if (hasCodeKeyword) {
      // Determine which language to show
      let language = 'javascript'; // default
      for (const lang of Object.keys(codeExamples)) {
        if (message.toLowerCase().includes(lang)) {
          language = lang;
          break;
        }
      }
      
      response = `Here's a ${language} code example:\n\`\`\`${language}\n${codeExamples[language]}\n\`\`\`\n\nIs there anything specific about this code you'd like me to explain?`;
    } else if (message.toLowerCase().includes('hello') || message.toLowerCase().includes('hi')) {
      response = "Hello! How can I help you with coding today?";
    } else {
      response = "I'm your coding assistant. I can provide code examples and explanations. What programming language are you interested in?";
    }
    
    return response;
  };

  // Handle sending a new message
  const handleSendMessage = async () => {
    if (!inputMessage.trim()) return;
    
    // Create a new user message
    const userMessage: Message = {
      id: Date.now().toString(),
      role: 'user',
      content: inputMessage,
    };
    
    // Add user message to session
    const updatedSessions = [...sessions];
    const sessionIndex = updatedSessions.findIndex(s => s.id === activeSessionId);
    
    if (sessionIndex !== -1) {
      updatedSessions[sessionIndex] = {
        ...updatedSessions[sessionIndex],
        messages: [...updatedSessions[sessionIndex].messages, userMessage]
      };
      setSessions(updatedSessions);
    }
    
    // Clear input field
    setInputMessage('');
    
    // Create placeholder for assistant response
    const assistantMessageId = (Date.now() + 1).toString();
    const assistantMessage: Message = {
      id: assistantMessageId,
      role: 'assistant',
      content: '',
      isStreaming: true,
    };
    
    // Add placeholder message
    const sessionsWithPlaceholder = [...updatedSessions];
    sessionsWithPlaceholder[sessionIndex] = {
      ...sessionsWithPlaceholder[sessionIndex],
      messages: [...sessionsWithPlaceholder[sessionIndex].messages, assistantMessage]
    };
    setSessions(sessionsWithPlaceholder);
    
    // Get the full response text
    const responseText = await simulateStreamingResponse(inputMessage);
    
    // Simulate streaming by adding one character at a time
    let currentResponse = '';
    for (let i = 0; i < responseText.length; i++) {
      await new Promise(resolve => setTimeout(resolve, 20)); // Delay between characters
      currentResponse += responseText[i];
      
      // Update the assistant message with the current response
      const updatedSessionsWithResponse = [...sessions];
      const sessionIdx = updatedSessionsWithResponse.findIndex(s => s.id === activeSessionId);
      
      if (sessionIdx !== -1) {
        const messages = [...updatedSessionsWithResponse[sessionIdx].messages];
        const messageIdx = messages.findIndex(m => m.id === assistantMessageId);
        
        if (messageIdx !== -1) {
          messages[messageIdx] = {
            ...messages[messageIdx],
            content: currentResponse,
          };
          
          updatedSessionsWithResponse[sessionIdx] = {
            ...updatedSessionsWithResponse[sessionIdx],
            messages
          };
          
          setSessions(updatedSessionsWithResponse);
        }
      }
    }
    
    // Mark streaming as complete
    const finalSessions = [...sessions];
    const finalSessionIdx = finalSessions.findIndex(s => s.id === activeSessionId);
    
    if (finalSessionIdx !== -1) {
      const messages = [...finalSessions[finalSessionIdx].messages];
      const messageIdx = messages.findIndex(m => m.id === assistantMessageId);
      
      if (messageIdx !== -1) {
        messages[messageIdx] = {
          ...messages[messageIdx],
          isStreaming: false,
        };
        
        finalSessions[finalSessionIdx] = {
          ...finalSessions[finalSessionIdx],
          messages
        };
        
        setSessions(finalSessions);
      }
    }
  };

  // Create a new session
  const handleNewSession = () => {
    const newSession: Session = {
      id: Date.now().toString(),
      title: 'New Chat',
      messages: [
        {
          id: Date.now().toString(),
          role: 'assistant',
          content: 'How can I help you with coding today?',
        }
      ]
    };
    
    setSessions([...sessions, newSession]);
    setActiveSessionId(newSession.id);
  };

  return (
    <div className="chat-container">
      {/* Sidebar */}
      <div className="sidebar">
        <button 
          onClick={handleNewSession}
          className="new-chat-btn"
        >
          <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor">
            <path fillRule="evenodd" d="M10 3a1 1 0 00-1 1v5H4a1 1 0 100 2h5v5a1 1 0 102 0v-5h5a1 1 0 100-2h-5V4a1 1 0 00-1-1z" clipRule="evenodd" />
          </svg>
          New Chat
        </button>
        <div className="session-list">
          <div className="session-group-heading">Sessions</div>
          {sessions.map(session => (
            <button
              key={session.id}
              onClick={() => setActiveSessionId(session.id)}
              className={`session-item ${session.id === activeSessionId ? 'active' : ''}`}
            >
              {session.title}
            </button>
          ))}
        </div>
      </div>
      
      {/* Main chat area */}
      <div className="main-content">
        {/* Header */}
        <div className="chat-header">
          <h2>{activeSession.title}</h2>
        </div>
        
        {/* Messages */}
        <div className="messages-container">
          {activeSession.messages.map(message => (
            <div 
              key={message.id} 
              className={`message ${message.role}`}
            >
              <div className="message-content">
                {message.content.split('\n').map((line, i) => {
                  // Check if this is a code block
                  if (line.startsWith('```')) {
                    const language = line.slice(3).trim();
                    const codeBlockEnd = message.content.indexOf('```', message.content.indexOf(line) + line.length);
                    
                    if (codeBlockEnd !== -1) {
                      const code = message.content.slice(
                        message.content.indexOf(line) + line.length, 
                        codeBlockEnd
                      ).trim();
                      
                      return (
                        <div key={i} className="code-block">
                          <div className="code-block-header">{language}</div>
                          <pre className="code-block-content">{code}</pre>
                        </div>
                      );
                    }
                  }
                  
                  // Regular text line
                  return line === '' ? <br key={i} /> : <div key={i}>{line}</div>;
                })}
                {message.isStreaming && (
                  <span className="typing-indicator"></span>
                )}
              </div>
            </div>
          ))}
          <div ref={messagesEndRef} />
        </div>
        
        {/* Input area */}
        <div className="input-container">
          <div className="input-wrapper">
            <input
              type="text"
              value={inputMessage}
              onChange={(e) => setInputMessage(e.target.value)}
              onKeyPress={(e) => e.key === 'Enter' && handleSendMessage()}
              placeholder="Type your message..."
              className="message-input"
            />
            <button
              onClick={handleSendMessage}
              className="send-button"
            >
              <svg xmlns="http://www.w3.org/2000/svg" className="h-5 w-5" viewBox="0 0 20 20" fill="currentColor">
                <path d="M10.894 2.553a1 1 0 00-1.788 0l-7 14a1 1 0 001.169 1.409l5-1.429A1 1 0 009 15.571V11a1 1 0 112 0v4.571a1 1 0 00.725.962l5 1.428a1 1 0 001.17-1.408l-7-14z" />
              </svg>
            </button>
          </div>
        </div>
      </div>
    </div>
  );
};

export default App;
