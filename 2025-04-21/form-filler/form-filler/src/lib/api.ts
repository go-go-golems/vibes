// API client for the form filler backend

const API_BASE_URL = 'http://localhost:8080/api';

export interface FormData {
  firstName: string;
  lastName: string;
  email: string;
  city: string;
  projectIdea: string;
}

export interface ChatMessage {
  role: 'user' | 'assistant';
  content: string;
}

export interface ApiResponse<T> {
  success: boolean;
  message: string;
  data?: T;
}

// Submit form data
export async function submitForm(formData: FormData): Promise<ApiResponse<FormData>> {
  const response = await fetch(`${API_BASE_URL}/submit`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify(formData),
  });
  
  return response.json();
}

// Get all form submissions
export async function getSubmissions(): Promise<ApiResponse<FormData[]>> {
  const response = await fetch(`${API_BASE_URL}/submissions`);
  return response.json();
}

// Send a chat message
export async function sendChatMessage(content: string): Promise<ApiResponse<{reply: ChatMessage, history: ChatMessage[]}>> {
  const response = await fetch(`${API_BASE_URL}/chat`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({
      role: 'user',
      content,
    }),
  });
  
  return response.json();
}

// Update a form field
export async function setFormField(field: string, value: string): Promise<ApiResponse<FormData>> {
  const response = await fetch(`${API_BASE_URL}/set-field`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({
      field,
      value,
    }),
  });
  
  return response.json();
}

// Get current form data
export async function getCurrentForm(): Promise<ApiResponse<FormData>> {
  const response = await fetch(`${API_BASE_URL}/form`);
  return response.json();
}

// Health check
export async function healthCheck(): Promise<ApiResponse<void>> {
  const response = await fetch(`${API_BASE_URL}/health`);
  return response.json();
}
