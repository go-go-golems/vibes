#!/usr/bin/env python3
"""
Very slow streaming demonstration for clear tmux capture.
"""

import openai
import time
import sys

def main():
    print("=== SLOW Streaming Demo ===")
    print("Each token will appear with 1 second delay")
    print("Perfect for visual demonstration!")
    print("")
    
    client = openai.OpenAI(
        api_key="mock-api-key",
        base_url="http://localhost:8080/v1"
    )
    
    print("Request: 'Tell me a joke!'")
    print("Response: ", end="", flush=True)
    
    try:
        stream = client.chat.completions.create(
            model="gpt-3.5-turbo",
            messages=[
                {"role": "user", "content": "Tell me a joke!"}
            ],
            stream=True
        )
        
        for chunk in stream:
            if chunk.choices[0].delta.content is not None:
                print(chunk.choices[0].delta.content, end="", flush=True)
                time.sleep(1.0)  # Very slow for clear capture
        
        print("\n\n=== Streaming Demo Complete ===")
        
    except Exception as e:
        print(f"Error: {e}")

if __name__ == "__main__":
    main()

