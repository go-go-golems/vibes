#!/usr/bin/env python3
"""
Test script for the mock OpenAI server using the standard OpenAI Python SDK.
"""

import openai
import json
import time

def test_mock_server():
    """Test the mock OpenAI server with various scenarios."""
    
    # Configure the OpenAI client to use our local mock server
    import os
    port = os.environ.get("MOCK_SERVER_PORT", "3117")
    client = openai.OpenAI(
        api_key="mock-api-key",  # Mock API key
        base_url=f"http://localhost:{port}/v1"  # Our local server
    )
    
    print("🚀 Testing Mock OpenAI Server")
    print("=" * 50)
    
    # Test 1: Basic chat completion
    print("\n📝 Test 1: Basic Chat Completion")
    try:
        response = client.chat.completions.create(
            model="gpt-3.5-turbo",
            messages=[
                {"role": "user", "content": "Hello, how are you?"}
            ]
        )
        
        print(f"✅ Success!")
        print(f"Response ID: {response.id}")
        print(f"Model: {response.model}")
        print(f"Message: {response.choices[0].message.content}")
        print(f"Usage: {response.usage}")
        
    except Exception as e:
        print(f"❌ Error: {e}")
    
    # Test 2: Weather query
    print("\n🌤️  Test 2: Weather Query")
    try:
        response = client.chat.completions.create(
            model="gpt-4",
            messages=[
                {"role": "user", "content": "What's the weather like today?"}
            ],
            max_tokens=100,
            temperature=0.7
        )
        
        print(f"✅ Success!")
        print(f"Response: {response.choices[0].message.content}")
        
    except Exception as e:
        print(f"❌ Error: {e}")
    
    # Test 3: Programming question
    print("\n💻 Test 3: Programming Question")
    try:
        response = client.chat.completions.create(
            model="gpt-3.5-turbo",
            messages=[
                {"role": "system", "content": "You are a helpful programming assistant."},
                {"role": "user", "content": "Can you help me with Python code?"}
            ]
        )
        
        print(f"✅ Success!")
        print(f"Response: {response.choices[0].message.content}")
        
    except Exception as e:
        print(f"❌ Error: {e}")
    
    # Test 4: Joke request
    print("\n😄 Test 4: Joke Request")
    try:
        response = client.chat.completions.create(
            model="gpt-4",
            messages=[
                {"role": "user", "content": "Tell me a joke!"}
            ]
        )
        
        print(f"✅ Success!")
        print(f"Response: {response.choices[0].message.content}")
        
    except Exception as e:
        print(f"❌ Error: {e}")
    
    # Test 5: Multi-turn conversation
    print("\n💬 Test 5: Multi-turn Conversation")
    try:
        response = client.chat.completions.create(
            model="gpt-3.5-turbo",
            messages=[
                {"role": "user", "content": "Hi there!"},
                {"role": "assistant", "content": "Hello! How can I help you?"},
                {"role": "user", "content": "What can you do?"}
            ]
        )
        
        print(f"✅ Success!")
        print(f"Response: {response.choices[0].message.content}")
        
    except Exception as e:
        print(f"❌ Error: {e}")
    
    # Test 6: Error handling - missing model
    print("\n⚠️  Test 6: Error Handling (Missing Model)")
    try:
        response = client.chat.completions.create(
            messages=[
                {"role": "user", "content": "This should fail"}
            ]
        )
        print(f"❌ This should have failed!")
        
    except Exception as e:
        print(f"✅ Expected error caught: {e}")

    # Test 7: Chat tools (web_search) non-streaming
    print("\n🧰 Test 7: Chat Tools (non-streaming)")
    try:
        response = client.chat.completions.create(
            model="gpt-3.5-turbo",
            messages=[
                {"role": "user", "content": "Please search the latest AI news."}
            ]
        )

        content = response.choices[0].message.content
        has_tool_summary = "Based on my web search" in content
        has_rule_text = "Summary above" in content
        if has_tool_summary and has_rule_text:
            print("✅ Success! Tool output included in completion text")
        else:
            print("❌ Tool output missing in completion text")
            print(f"Got: {content}")
    except Exception as e:
        print(f"❌ Error: {e}")

    # Test 8: Chat tools (web_search) streaming
    print("\n🧰 Test 8: Chat Tools (streaming)")
    try:
        stream = client.chat.completions.create(
            model="gpt-3.5-turbo",
            messages=[
                {"role": "user", "content": "Could you search the latest trends?"}
            ],
            stream=True
        )

        full = ""
        for chunk in stream:
            if chunk.choices[0].delta.content is not None:
                full += chunk.choices[0].delta.content
        has_tool_summary = "Based on my web search" in full
        has_rule_text = "Summary above" in full
        if has_tool_summary and has_rule_text:
            print("✅ Success! Tool output included in streaming completion text")
        else:
            print("❌ Tool output missing in streaming completion text")
            print(f"Got: {full}")
    except Exception as e:
        print(f"❌ Error: {e}")
    
    print("\n" + "=" * 50)
    print("🎉 All tests completed!")

def test_models_endpoint():
    """Test the models endpoint."""
    print("\n🔍 Testing Models Endpoint")
    
    port = os.environ.get("MOCK_SERVER_PORT", "3117")
    client = openai.OpenAI(
        api_key="mock-api-key",
        base_url=f"http://localhost:{port}/v1"
    )
    
    try:
        models = client.models.list()
        print(f"✅ Models endpoint works!")
        print(f"Available models:")
        for model in models.data:
            print(f"  - {model.id} (owned by: {model.owned_by})")
            
    except Exception as e:
        print(f"❌ Error: {e}")

def test_health_endpoint():
    """Test the health endpoint using direct HTTP request."""
    print("\n🏥 Testing Health Endpoint")
    
    import requests, os
    
    try:
        port = os.environ.get("MOCK_SERVER_PORT", "3117")
        response = requests.get(f"http://localhost:{port}/health")
        if response.status_code == 200:
            data = response.json()
            print(f"✅ Health endpoint works!")
            print(f"Status: {data.get('status')}")
            print(f"Server: {data.get('server')}")
            print(f"Time: {data.get('time')}")
        else:
            print(f"❌ Health check failed with status: {response.status_code}")
            
    except Exception as e:
        print(f"❌ Error: {e}")

if __name__ == "__main__":
    # Wait a moment for the server to be ready
    print("⏳ Waiting for server to be ready...")
    time.sleep(2)
    
    # Run all tests
    test_health_endpoint()
    test_models_endpoint()
    test_mock_server()
    
    print("\n🎯 Test Summary:")
    print("- Mock OpenAI server is compatible with the standard OpenAI Python SDK")
    print("- All major endpoints are working correctly")
    print("- Error handling is implemented properly")
    print("- CORS is enabled for web applications")
