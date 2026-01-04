#!/usr/bin/env python3
"""
Simple streaming demonstration for tmux capture.
"""

import openai
import time
import sys

def main():
    print("=== Mock OpenAI Streaming Demo ===")
    print("Connecting to local server...")
    
    client = openai.OpenAI(
        api_key="mock-api-key",
        base_url="http://localhost:8080/v1"
    )
    
    print("Sending streaming request...")
    print("Assistant: ", end="", flush=True)
    
    try:
        stream = client.chat.completions.create(
            model="gpt-3.5-turbo",
            messages=[
                {"role": "user", "content": "Explain how streaming works in AI applications"}
            ],
            stream=True
        )
        
        for chunk in stream:
            if chunk.choices[0].delta.content is not None:
                print(chunk.choices[0].delta.content, end="", flush=True)
                time.sleep(0.2)  # Slower for better visual capture
        
        print("\n\nStreaming completed successfully!")
        
    except Exception as e:
        print(f"Error: {e}")

if __name__ == "__main__":
    main()

