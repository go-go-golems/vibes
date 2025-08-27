#!/usr/bin/env python3
"""
Demonstration script for the mock OpenAI Responses API.
Shows key features including basic responses, conversation state,
tools, and streaming.
"""

import json
import requests
import time

# Configuration
BASE_URL = "http://localhost:8080"
API_KEY = "mock-api-key"

def make_request(endpoint, method="POST", data=None, stream=False):
    """Make API request with proper headers"""
    headers = {
        "Authorization": f"Bearer {API_KEY}",
        "Content-Type": "application/json"
    }
    
    if method == "POST":
        response = requests.post(f"{BASE_URL}{endpoint}", json=data, headers=headers, stream=stream)
    else:
        response = requests.get(f"{BASE_URL}{endpoint}", headers=headers)
    
    return response

def demo_basic_response():
    """Demonstrate basic response creation"""
    print("🔹 Basic Response Creation")
    print("-" * 40)
    
    payload = {
        "model": "gpt-4o",
        "input": "Hello! Can you tell me about the Responses API?",
        "instructions": "You are a helpful AI assistant."
    }
    
    response = make_request("/v1/responses", data=payload)
    data = response.json()
    
    print(f"Response ID: {data['id']}")
    print(f"Model: {data['model']}")
    print(f"Response: {data['output'][0]['content'][0]['text']}")
    print()
    
    return data

def demo_conversation_continuation(previous_response_id):
    """Demonstrate conversation continuation"""
    print("🔹 Conversation Continuation")
    print("-" * 40)
    
    payload = {
        "model": "gpt-4o",
        "input": "Can you tell me a joke?",
        "previous_response_id": previous_response_id
    }
    
    response = make_request("/v1/responses", data=payload)
    data = response.json()
    
    print(f"Previous Response ID: {previous_response_id}")
    print(f"New Response ID: {data['id']}")
    print(f"Response: {data['output'][0]['content'][0]['text']}")
    print()
    
    return data

def demo_conversation_forking(original_response_id):
    """Demonstrate conversation forking"""
    print("🔹 Conversation Forking")
    print("-" * 40)
    
    payload = {
        "model": "gpt-4o",
        "input": "Actually, tell me about the weather instead",
        "previous_response_id": original_response_id
    }
    
    response = make_request("/v1/responses", data=payload)
    data = response.json()
    
    print(f"Forked from Response ID: {original_response_id}")
    print(f"New Branch Response ID: {data['id']}")
    print(f"Response: {data['output'][0]['content'][0]['text']}")
    print()
    
    return data

def demo_web_search_tool():
    """Demonstrate web search tool"""
    print("🔹 Web Search Tool")
    print("-" * 40)
    
    payload = {
        "model": "gpt-4o",
        "input": "What are the latest developments in AI?",
        "tools": [{"type": "web_search"}]
    }
    
    response = make_request("/v1/responses", data=payload)
    data = response.json()
    
    print(f"Response ID: {data['id']}")
    print("Output structure:")
    for i, output in enumerate(data['output']):
        print(f"  {i+1}. Type: {output['type']}")
        if output['type'] == 'web_search_call':
            print(f"     Status: {output['status']}")
        elif output['type'] == 'message':
            content = output['content'][0]
            print(f"     Text: {content['text'][:100]}...")
            print(f"     Citations: {len(content.get('annotations', []))}")
    print()

def demo_file_search_tool():
    """Demonstrate file search tool"""
    print("🔹 File Search Tool")
    print("-" * 40)
    
    payload = {
        "model": "gpt-4o",
        "input": "Find information about API documentation",
        "tools": [{"type": "file_search"}]
    }
    
    response = make_request("/v1/responses", data=payload)
    data = response.json()
    
    print(f"Response ID: {data['id']}")
    print("Output structure:")
    for i, output in enumerate(data['output']):
        print(f"  {i+1}. Type: {output['type']}")
        if output['type'] == 'file_search_call':
            print(f"     Status: {output['status']}")
        elif output['type'] == 'message':
            content = output['content'][0]
            print(f"     Text: {content['text'][:100]}...")
            print(f"     File citations: {len(content.get('annotations', []))}")
    print()

def demo_streaming_response():
    """Demonstrate streaming response"""
    print("🔹 Streaming Response")
    print("-" * 40)
    
    payload = {
        "model": "gpt-4o",
        "input": "Explain how streaming works in APIs",
        "stream": True
    }
    
    print("Streaming response (real-time):")
    print("> ", end="", flush=True)
    
    response = make_request("/v1/responses", data=payload, stream=True)
    
    full_content = ""
    for line in response.iter_lines():
        if line:
            line_str = line.decode('utf-8')
            if line_str.startswith('data: '):
                data_str = line_str[6:]
                if data_str.strip() == '[DONE]':
                    break
                try:
                    chunk_data = json.loads(data_str)
                    if chunk_data.get("type") == "response.output_text.delta":
                        delta = chunk_data.get("delta", "")
                        print(delta, end="", flush=True)
                        full_content += delta
                except json.JSONDecodeError:
                    pass
    
    print("\n")
    print(f"Total content length: {len(full_content)} characters")
    print()

def demo_response_retrieval(response_id):
    """Demonstrate response retrieval"""
    print("🔹 Response Retrieval")
    print("-" * 40)
    
    response = make_request(f"/v1/responses/{response_id}", method="GET")
    data = response.json()
    
    print(f"Retrieved Response ID: {data['id']}")
    print(f"Created: {data['created']}")
    print(f"Model: {data['model']}")
    print(f"Output count: {len(data['output'])}")
    print()

def demo_multimodal_input():
    """Demonstrate multimodal input"""
    print("🔹 Multimodal Input")
    print("-" * 40)
    
    payload = {
        "model": "gpt-4o",
        "input": [
            {
                "role": "user",
                "content": "Analyze this product image"
            },
            {
                "role": "user",
                "content": [
                    {
                        "type": "input_image",
                        "image_url": "https://example.com/product.jpg"
                    }
                ]
            }
        ]
    }
    
    response = make_request("/v1/responses", data=payload)
    data = response.json()
    
    print(f"Response ID: {data['id']}")
    print(f"Processed multimodal input successfully")
    print(f"Response: {data['output'][0]['content'][0]['text'][:100]}...")
    print()

def main():
    """Run all demonstrations"""
    print("🚀 Mock OpenAI Responses API Demonstration")
    print("=" * 60)
    print()
    
    # Demo 1: Basic response
    initial_response = demo_basic_response()
    
    # Demo 2: Conversation continuation
    continued_response = demo_conversation_continuation(initial_response['id'])
    
    # Demo 3: Conversation forking
    demo_conversation_forking(initial_response['id'])
    
    # Demo 4: Web search tool
    demo_web_search_tool()
    
    # Demo 5: File search tool
    demo_file_search_tool()
    
    # Demo 6: Streaming response
    demo_streaming_response()
    
    # Demo 7: Response retrieval
    demo_response_retrieval(initial_response['id'])
    
    # Demo 8: Multimodal input
    demo_multimodal_input()
    
    print("=" * 60)
    print("✅ All demonstrations completed successfully!")
    print("🎉 The mock Responses API is fully functional!")

if __name__ == "__main__":
    main()

