#!/usr/bin/env python3
"""
Streaming test client for the mock OpenAI server.
Demonstrates real-time token streaming with visual output.
"""

import openai
import time
import sys
from datetime import datetime

def print_with_timestamp(message, color_code="0"):
    """Print message with timestamp and optional color."""
    timestamp = datetime.now().strftime("%H:%M:%S.%f")[:-3]
    print(f"\033[{color_code}m[{timestamp}] {message}\033[0m")

def test_streaming_basic():
    """Test basic streaming functionality."""
    print_with_timestamp("🚀 Starting Basic Streaming Test", "92")  # Green
    print("=" * 60)
    
    client = openai.OpenAI(
        api_key="mock-api-key",
        base_url="http://localhost:8080/v1"
    )
    
    print_with_timestamp("📤 Sending request with stream=True", "94")  # Blue
    
    try:
        stream = client.chat.completions.create(
            model="gpt-3.5-turbo",
            messages=[
                {"role": "user", "content": "Hello! Tell me about streaming."}
            ],
            stream=True
        )
        
        print_with_timestamp("📥 Starting to receive streaming response:", "93")  # Yellow
        print("\n🤖 Assistant: ", end="", flush=True)
        
        full_response = ""
        chunk_count = 0
        
        for chunk in stream:
            chunk_count += 1
            if chunk.choices[0].delta.content is not None:
                content = chunk.choices[0].delta.content
                print(content, end="", flush=True)
                full_response += content
                
                # Add a small delay to make streaming more visible
                time.sleep(0.05)
        
        print("\n")
        print_with_timestamp(f"✅ Streaming completed! Received {chunk_count} chunks", "92")
        print_with_timestamp(f"📝 Full response: {full_response}", "90")  # Gray
        
    except Exception as e:
        print_with_timestamp(f"❌ Error: {e}", "91")  # Red

def test_streaming_different_prompts():
    """Test streaming with different types of prompts."""
    print_with_timestamp("🎯 Testing Different Prompt Types", "92")
    print("=" * 60)
    
    client = openai.OpenAI(
        api_key="mock-api-key",
        base_url="http://localhost:8080/v1"
    )
    
    test_prompts = [
        "Tell me a joke!",
        "What's the weather like?",
        "Help me with programming",
        "Explain streaming technology"
    ]
    
    for i, prompt in enumerate(test_prompts, 1):
        print_with_timestamp(f"📤 Test {i}/4: '{prompt}'", "94")
        
        try:
            stream = client.chat.completions.create(
                model="gpt-4",
                messages=[{"role": "user", "content": prompt}],
                stream=True
            )
            
            print(f"🤖 Response: ", end="", flush=True)
            
            for chunk in stream:
                if chunk.choices[0].delta.content is not None:
                    print(chunk.choices[0].delta.content, end="", flush=True)
                    time.sleep(0.03)  # Slightly faster for multiple tests
            
            print("\n")
            
        except Exception as e:
            print_with_timestamp(f"❌ Error: {e}", "91")
        
        if i < len(test_prompts):
            print_with_timestamp("⏳ Waiting before next test...", "90")
            time.sleep(1)

def test_streaming_vs_non_streaming():
    """Compare streaming vs non-streaming responses."""
    print_with_timestamp("⚖️  Comparing Streaming vs Non-Streaming", "92")
    print("=" * 60)
    
    client = openai.OpenAI(
        api_key="mock-api-key",
        base_url="http://localhost:8080/v1"
    )
    
    prompt = "Explain the difference between streaming and non-streaming responses."
    
    # Test non-streaming first
    print_with_timestamp("📤 Testing NON-STREAMING response:", "94")
    start_time = time.time()
    
    try:
        response = client.chat.completions.create(
            model="gpt-3.5-turbo",
            messages=[{"role": "user", "content": prompt}],
            stream=False
        )
        
        end_time = time.time()
        print(f"🤖 Response: {response.choices[0].message.content}")
        print_with_timestamp(f"⏱️  Non-streaming took: {end_time - start_time:.2f} seconds", "93")
        
    except Exception as e:
        print_with_timestamp(f"❌ Error: {e}", "91")
    
    print("\n" + "-" * 40 + "\n")
    
    # Test streaming
    print_with_timestamp("📤 Testing STREAMING response:", "94")
    start_time = time.time()
    
    try:
        stream = client.chat.completions.create(
            model="gpt-3.5-turbo",
            messages=[{"role": "user", "content": prompt}],
            stream=True
        )
        
        print("🤖 Response: ", end="", flush=True)
        first_token_time = None
        
        for chunk in stream:
            if chunk.choices[0].delta.content is not None:
                if first_token_time is None:
                    first_token_time = time.time()
                print(chunk.choices[0].delta.content, end="", flush=True)
                time.sleep(0.05)
        
        end_time = time.time()
        print("\n")
        
        if first_token_time:
            print_with_timestamp(f"⚡ Time to first token: {first_token_time - start_time:.2f} seconds", "93")
        print_with_timestamp(f"⏱️  Total streaming time: {end_time - start_time:.2f} seconds", "93")
        
    except Exception as e:
        print_with_timestamp(f"❌ Error: {e}", "91")

def test_streaming_with_system_message():
    """Test streaming with system message."""
    print_with_timestamp("🎭 Testing Streaming with System Message", "92")
    print("=" * 60)
    
    client = openai.OpenAI(
        api_key="mock-api-key",
        base_url="http://localhost:8080/v1"
    )
    
    try:
        stream = client.chat.completions.create(
            model="gpt-4",
            messages=[
                {"role": "system", "content": "You are a helpful assistant that explains things clearly."},
                {"role": "user", "content": "What is streaming and why is it useful?"}
            ],
            stream=True,
            temperature=0.7
        )
        
        print("🤖 Assistant: ", end="", flush=True)
        
        for chunk in stream:
            if chunk.choices[0].delta.content is not None:
                print(chunk.choices[0].delta.content, end="", flush=True)
                time.sleep(0.04)
        
        print("\n")
        print_with_timestamp("✅ System message + streaming test completed", "92")
        
    except Exception as e:
        print_with_timestamp(f"❌ Error: {e}", "91")

def main():
    """Run all streaming tests."""
    print("\n" + "🌊" * 20)
    print_with_timestamp("MOCK OPENAI STREAMING TEST SUITE", "95")  # Magenta
    print("🌊" * 20 + "\n")
    
    # Wait for server to be ready
    print_with_timestamp("⏳ Waiting for server to be ready...", "90")
    time.sleep(2)
    
    # Run tests
    test_streaming_basic()
    print("\n")
    
    test_streaming_different_prompts()
    print("\n")
    
    test_streaming_vs_non_streaming()
    print("\n")
    
    test_streaming_with_system_message()
    print("\n")
    
    print_with_timestamp("🎉 All streaming tests completed!", "92")
    print_with_timestamp("💡 The mock server successfully demonstrates:", "93")
    print("   • Real-time token streaming")
    print("   • OpenAI SDK compatibility")
    print("   • Server-Sent Events (SSE) format")
    print("   • Proper chunk formatting")
    print("   • Visual streaming demonstration")

if __name__ == "__main__":
    main()

