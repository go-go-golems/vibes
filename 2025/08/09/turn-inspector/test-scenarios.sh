#!/bin/bash

# Test scenarios for turn-inspector CLI
# This script creates diverse conversation turns to demonstrate functionality

set -e

# Use an isolated temporary database for deterministic IDs
export TURN_INSPECTOR_DB="$(mktemp -u).db"

CLI="./turn-inspector"

echo "=== Turn Inspector Test Scenarios ==="
echo

# Clean up any existing data
echo "1. Initializing fresh database at $TURN_INSPECTOR_DB"
echo

echo "1b. Creating a run for scenarios..."
$CLI create run --name "Test Scenarios" >/dev/null
RUN_ID=1
echo "Using RUN_ID=$RUN_ID"
echo

# Scenario 1: Simple user-assistant conversation
echo "2. Creating simple user-assistant conversation..."
$CLI create turn --run-id $RUN_ID --blocks '[
  {
    "order": 0,
    "kind": "user",
    "role": "user",
    "payload": {"text": "Hello, how are you today?"}
  },
  {
    "order": 1,
    "kind": "llm_text",
    "role": "assistant",
    "payload": {"text": "Hello! I am doing well, thank you for asking. How can I help you today?"}
  }
]'
echo

# Scenario 2: Tool-calling conversation
echo "3. Creating tool-calling conversation..."
$CLI create turn --run-id $RUN_ID --blocks '[
  {
    "order": 0,
    "kind": "user",
    "role": "user",
    "payload": {"text": "What is the weather like in New York?"}
  },
  {
    "order": 1,
    "kind": "tool_call",
    "role": "assistant",
    "payload": {
      "tool": "get_weather",
      "args": {"city": "New York", "units": "fahrenheit"}
    },
    "metadata": [
      {"source": "runtime", "key": "latency_ms", "value": "245"},
      {"source": "runtime", "key": "api_version", "value": "v2.1"}
    ]
  },
  {
    "order": 2,
    "kind": "tool_use",
    "role": "tool",
    "payload": {
      "result": "Current weather in New York: 72°F, partly cloudy with light winds"
    }
  },
  {
    "order": 3,
    "kind": "llm_text",
    "role": "assistant",
    "payload": {"text": "The current weather in New York is 72°F and partly cloudy with light winds. It is a pleasant day!"}
  }
]'
echo

# Scenario 3: Multi-tool complex conversation
echo "4. Creating complex multi-tool conversation..."
$CLI create turn --run-id $RUN_ID --blocks '[
  {
    "order": 0,
    "kind": "user",
    "role": "user",
    "payload": {"text": "I need to book a flight from San Francisco to Tokyo and check the weather there"}
  },
  {
    "order": 1,
    "kind": "tool_call",
    "role": "assistant",
    "payload": {
      "tool": "search_flights",
      "args": {"from": "SFO", "to": "NRT", "date": "2024-03-15"}
    }
  },
  {
    "order": 2,
    "kind": "tool_use",
    "role": "tool",
    "payload": {
      "result": "Found 3 flights: UA 838 ($1,200), ANA 106 ($1,150), JAL 002 ($1,300)"
    }
  },
  {
    "order": 3,
    "kind": "tool_call",
    "role": "assistant",
    "payload": {
      "tool": "get_weather",
      "args": {"city": "Tokyo", "units": "celsius"}
    }
  },
  {
    "order": 4,
    "kind": "tool_use",
    "role": "tool",
    "payload": {
      "result": "Tokyo weather: 18°C, overcast with occasional rain"
    }
  },
  {
    "order": 5,
    "kind": "llm_text",
    "role": "assistant",
    "payload": {"text": "I found several flight options for you from San Francisco to Tokyo on March 15th. The best value is ANA 106 at $1,150. The weather in Tokyo will be 18°C and overcast with occasional rain, so pack accordingly!"}
  }
]'
echo

# Scenario 4: Error handling conversation
echo "5. Creating error handling conversation..."
$CLI create turn --run-id $RUN_ID --blocks '[
  {
    "order": 0,
    "kind": "user",
    "role": "user",
    "payload": {"text": "Calculate the square root of -1"}
  },
  {
    "order": 1,
    "kind": "tool_call",
    "role": "assistant",
    "payload": {
      "tool": "calculator",
      "args": {"operation": "sqrt", "value": -1}
    }
  },
  {
    "order": 2,
    "kind": "system",
    "role": "system",
    "payload": {
      "error": "Invalid operation: cannot calculate square root of negative number",
      "error_code": "MATH_ERROR_001"
    },
    "metadata": [
      {"source": "error", "key": "type", "value": "mathematical"},
      {"source": "error", "key": "severity", "value": "low"}
    ]
  },
  {
    "order": 3,
    "kind": "llm_text",
    "role": "assistant",
    "payload": {"text": "I cannot calculate the square root of -1 using real numbers. However, in complex numbers, the square root of -1 is represented as i (the imaginary unit)."}
  }
]'
echo

# Scenario 5: Long conversation with system messages
echo "6. Creating long conversation with system messages..."
$CLI create turn --run-id $RUN_ID --blocks '[
  {
    "order": 0,
    "kind": "system",
    "role": "system",
    "payload": {"text": "User session started. Context: Customer support for premium account."}
  },
  {
    "order": 1,
    "kind": "user",
    "role": "user",
    "payload": {"text": "I am having trouble accessing my premium features"}
  },
  {
    "order": 2,
    "kind": "llm_text",
    "role": "assistant",
    "payload": {"text": "I understand you are having trouble with your premium features. Let me check your account status."}
  },
  {
    "order": 3,
    "kind": "tool_call",
    "role": "assistant",
    "payload": {
      "tool": "check_account_status",
      "args": {"user_id": "premium_user_123"}
    }
  },
  {
    "order": 4,
    "kind": "tool_use",
    "role": "tool",
    "payload": {
      "result": "Account status: Active Premium, expires 2024-12-31, all features enabled"
    }
  },
  {
    "order": 5,
    "kind": "llm_text",
    "role": "assistant",
    "payload": {"text": "Your premium account is active and all features should be available. Could you tell me which specific feature you are trying to access?"}
  },
  {
    "order": 6,
    "kind": "user",
    "role": "user",
    "payload": {"text": "I cannot access the advanced analytics dashboard"}
  },
  {
    "order": 7,
    "kind": "system",
    "role": "system",
    "payload": {"text": "Escalating to technical support team due to feature access issue."}
  }
]'
echo

# Scenario 6: Code assistance conversation
echo "7. Creating code assistance conversation..."
$CLI create turn --run-id $RUN_ID --blocks '[
  {
    "order": 0,
    "kind": "user",
    "role": "user",
    "payload": {"text": "Help me write a Python function to reverse a string"}
  },
  {
    "order": 1,
    "kind": "llm_text",
    "role": "assistant",
    "payload": {
      "text": "I will help you write a Python function to reverse a string. Here is a simple implementation:",
      "code": "def reverse_string(s):\n    return s[::-1]\n\n# Example usage\nresult = reverse_string(\"hello\")\nprint(result)  # Output: \"olleh\""
    }
  },
  {
    "order": 2,
    "kind": "user",
    "role": "user",
    "payload": {"text": "Can you show me an alternative method?"}
  },
  {
    "order": 3,
    "kind": "llm_text",
    "role": "assistant",
    "payload": {
      "text": "Certainly! Here is an alternative method using a loop:",
      "code": "def reverse_string_loop(s):\n    reversed_str = \"\"\n    for char in s:\n        reversed_str = char + reversed_str\n    return reversed_str"
    }
  }
]'
echo

echo "=== Test data creation completed! ==="
echo "Created 6 diverse conversation scenarios:"
echo "1. Simple user-assistant conversation"
echo "2. Tool-calling conversation (weather)"
echo "3. Complex multi-tool conversation (travel)"
echo "4. Error handling conversation"
echo "5. Long customer support conversation"
echo "6. Code assistance conversation"
echo

