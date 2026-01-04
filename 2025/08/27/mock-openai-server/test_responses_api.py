#!/usr/bin/env python3
"""
Comprehensive test suite for the mock OpenAI Responses API.
Tests all major features including basic responses, streaming, tools, 
conversation state, and forking.
"""

import json
import requests
import time
import sys
from typing import Dict, Any, List

# Configuration
import os
BASE_URL = f"http://localhost:{os.environ.get('MOCK_SERVER_PORT','3117')}"
API_KEY = "mock-api-key"

class ResponsesAPITester:
    def __init__(self, base_url: str = BASE_URL):
        self.base_url = base_url
        self.session = requests.Session()
        self.session.headers.update({
            "Authorization": f"Bearer {API_KEY}",
            "Content-Type": "application/json"
        })
        self.test_results = []
        
    def log_test(self, test_name: str, success: bool, details: str = ""):
        """Log test results"""
        status = "✅ PASS" if success else "❌ FAIL"
        print(f"{status} {test_name}")
        if details:
            print(f"    {details}")
        self.test_results.append({
            "test": test_name,
            "success": success,
            "details": details
        })
        
    def test_health_check(self):
        """Test server health and API availability"""
        try:
            response = self.session.get(f"{self.base_url}/health")
            data = response.json()
            
            success = (
                response.status_code == 200 and
                data.get("status") == "healthy" and
                data.get("apis", {}).get("responses") == "available"
            )
            
            self.log_test(
                "Health Check", 
                success,
                f"Status: {data.get('status')}, Responses API: {data.get('apis', {}).get('responses')}"
            )
            return success
        except Exception as e:
            self.log_test("Health Check", False, f"Error: {str(e)}")
            return False
            
    def test_basic_response_creation(self):
        """Test basic response creation"""
        try:
            payload = {
                "model": "gpt-4o",
                "input": "Hello, how are you?",
                "instructions": "You are a helpful assistant."
            }
            
            response = self.session.post(f"{self.base_url}/v1/responses", json=payload)
            data = response.json()
            
            success = (
                response.status_code == 200 and
                data.get("object") == "response" and
                "id" in data and
                len(data.get("output", [])) > 0 and
                data["output"][0].get("type") == "message"
            )
            
            response_text = ""
            if success and data["output"][0].get("content"):
                response_text = data["output"][0]["content"][0].get("text", "")
            
            self.log_test(
                "Basic Response Creation",
                success,
                f"Response ID: {data.get('id')}, Text: {response_text[:50]}..."
            )
            
            return data if success else None
        except Exception as e:
            self.log_test("Basic Response Creation", False, f"Error: {str(e)}")
            return None
            
    def test_response_retrieval(self, response_id: str):
        """Test response retrieval by ID"""
        try:
            response = self.session.get(f"{self.base_url}/v1/responses/{response_id}")
            data = response.json()
            
            success = (
                response.status_code == 200 and
                data.get("id") == response_id and
                data.get("object") == "response"
            )
            
            self.log_test(
                "Response Retrieval",
                success,
                f"Retrieved response ID: {data.get('id')}"
            )
            return success
        except Exception as e:
            self.log_test("Response Retrieval", False, f"Error: {str(e)}")
            return False
            
    def test_conversation_continuation(self, previous_response_id: str):
        """Test conversation continuation"""
        try:
            payload = {
                "model": "gpt-4o",
                "input": "Tell me a joke",
                "previous_response_id": previous_response_id
            }
            
            response = self.session.post(f"{self.base_url}/v1/responses", json=payload)
            data = response.json()
            
            success = (
                response.status_code == 200 and
                data.get("object") == "response" and
                "id" in data and
                data["id"] != previous_response_id
            )
            
            response_text = ""
            if success and data["output"][0].get("content"):
                response_text = data["output"][0]["content"][0].get("text", "")
            
            self.log_test(
                "Conversation Continuation",
                success,
                f"New response ID: {data.get('id')}, Contains joke: {'joke' in response_text.lower()}"
            )
            
            return data if success else None
        except Exception as e:
            self.log_test("Conversation Continuation", False, f"Error: {str(e)}")
            return None
            
    def test_conversation_forking(self, original_response_id: str):
        """Test conversation forking"""
        try:
            payload = {
                "model": "gpt-4o",
                "input": "Actually, tell me about the weather instead",
                "previous_response_id": original_response_id
            }
            
            response = self.session.post(f"{self.base_url}/v1/responses", json=payload)
            data = response.json()
            
            success = (
                response.status_code == 200 and
                data.get("object") == "response" and
                "id" in data
            )
            
            response_text = ""
            if success and data["output"][0].get("content"):
                response_text = data["output"][0]["content"][0].get("text", "")
            
            self.log_test(
                "Conversation Forking",
                success,
                f"Forked response ID: {data.get('id')}, About weather: {'weather' in response_text.lower()}"
            )
            
            return data if success else None
        except Exception as e:
            self.log_test("Conversation Forking", False, f"Error: {str(e)}")
            return None
            
    def test_web_search_tool(self):
        """Test web search tool integration"""
        try:
            payload = {
                "model": "gpt-4o",
                "input": "What's the latest news about AI?",
                "tools": [{"type": "web_search"}]
            }
            
            response = self.session.post(f"{self.base_url}/v1/responses", json=payload)
            data = response.json()
            
            # Check for web search call and message response
            has_web_search = any(
                output.get("type") == "web_search_call" 
                for output in data.get("output", [])
            )
            
            has_message = any(
                output.get("type") == "message" and 
                output.get("content", [{}])[0].get("annotations")
                for output in data.get("output", [])
            )
            
            success = (
                response.status_code == 200 and
                has_web_search and
                has_message
            )
            
            self.log_test(
                "Web Search Tool",
                success,
                f"Web search call: {has_web_search}, Citations: {has_message}"
            )
            
            return success
        except Exception as e:
            self.log_test("Web Search Tool", False, f"Error: {str(e)}")
            return False
            
    def test_file_search_tool(self):
        """Test file search tool integration"""
        try:
            payload = {
                "model": "gpt-4o",
                "input": "Find information about API specifications",
                "tools": [{"type": "file_search"}]
            }
            
            response = self.session.post(f"{self.base_url}/v1/responses", json=payload)
            data = response.json()
            
            # Check for file search call and message response
            has_file_search = any(
                output.get("type") == "file_search_call" 
                for output in data.get("output", [])
            )
            
            has_message = any(
                output.get("type") == "message"
                for output in data.get("output", [])
            )
            
            success = (
                response.status_code == 200 and
                has_file_search and
                has_message
            )
            
            self.log_test(
                "File Search Tool",
                success,
                f"File search call: {has_file_search}, Message response: {has_message}"
            )
            
            return success
        except Exception as e:
            self.log_test("File Search Tool", False, f"Error: {str(e)}")
            return False
            
    def test_multimodal_input(self):
        """Test multimodal input (text + image references)"""
        try:
            payload = {
                "model": "gpt-4o",
                "input": [
                    {
                        "role": "user",
                        "content": "Analyze this image"
                    },
                    {
                        "role": "user", 
                        "content": [
                            {
                                "type": "input_image",
                                "image_url": "https://example.com/image.jpg"
                            }
                        ]
                    }
                ]
            }
            
            response = self.session.post(f"{self.base_url}/v1/responses", json=payload)
            data = response.json()
            
            success = (
                response.status_code == 200 and
                data.get("object") == "response" and
                len(data.get("output", [])) > 0
            )
            
            self.log_test(
                "Multimodal Input",
                success,
                f"Processed multimodal input successfully"
            )
            
            return success
        except Exception as e:
            self.log_test("Multimodal Input", False, f"Error: {str(e)}")
            return False
            
    def test_streaming_response(self):
        """Test streaming response"""
        try:
            payload = {
                "model": "gpt-4o",
                "input": "Tell me about streaming APIs",
                "stream": True
            }
            
            response = self.session.post(
                f"{self.base_url}/v1/responses", 
                json=payload,
                stream=True
            )
            
            success = response.status_code == 200
            chunks_received = 0
            content_received = ""
            
            if success:
                for line in response.iter_lines():
                    if line:
                        line_str = line.decode('utf-8')
                        if line_str.startswith('data: '):
                            data_str = line_str[6:]  # Remove 'data: ' prefix
                            if data_str.strip() == '[DONE]':
                                break
                            try:
                                chunk_data = json.loads(data_str)
                                if chunk_data.get("type") == "response.output_text.delta":
                                    content_received += chunk_data.get("delta", "")
                                    chunks_received += 1
                            except json.JSONDecodeError:
                                pass
                                
            success = success and chunks_received > 0 and len(content_received) > 0
            
            self.log_test(
                "Streaming Response",
                success,
                f"Chunks: {chunks_received}, Content length: {len(content_received)}"
            )
            
            return success
        except Exception as e:
            self.log_test("Streaming Response", False, f"Error: {str(e)}")
            return False
            
    def test_responses_list(self):
        """Test listing responses"""
        try:
            response = self.session.get(f"{self.base_url}/v1/responses")
            data = response.json()
            
            success = (
                response.status_code == 200 and
                data.get("object") == "list" and
                "data" in data
            )
            
            response_count = len(data.get("data", []))
            
            self.log_test(
                "Responses List",
                success,
                f"Listed {response_count} responses"
            )
            
            return success
        except Exception as e:
            self.log_test("Responses List", False, f"Error: {str(e)}")
            return False
            
    def test_error_handling(self):
        """Test error handling for invalid requests"""
        try:
            # Test invalid JSON
            response = self.session.post(
                f"{self.base_url}/v1/responses",
                data="invalid json"
            )
            
            invalid_json_handled = response.status_code == 400
            
            # Test missing required fields
            response = self.session.post(
                f"{self.base_url}/v1/responses",
                json={}
            )
            
            missing_fields_handled = response.status_code in [400, 422]
            
            # Test non-existent response retrieval
            response = self.session.get(f"{self.base_url}/v1/responses/nonexistent")
            
            not_found_handled = response.status_code == 404
            
            success = invalid_json_handled and missing_fields_handled and not_found_handled
            
            self.log_test(
                "Error Handling",
                success,
                f"Invalid JSON: {invalid_json_handled}, Missing fields: {missing_fields_handled}, Not found: {not_found_handled}"
            )
            
            return success
        except Exception as e:
            self.log_test("Error Handling", False, f"Error: {str(e)}")
            return False
            
    def run_all_tests(self):
        """Run all tests in sequence"""
        print("🧪 Starting Comprehensive Responses API Test Suite")
        print("=" * 60)
        
        # Test 1: Health check
        if not self.test_health_check():
            print("❌ Server not healthy, aborting tests")
            return False
            
        # Test 2: Basic response creation
        initial_response = self.test_basic_response_creation()
        if not initial_response:
            print("❌ Basic response creation failed, aborting tests")
            return False
            
        response_id = initial_response["id"]
        
        # Test 3: Response retrieval
        self.test_response_retrieval(response_id)
        
        # Test 4: Conversation continuation
        continued_response = self.test_conversation_continuation(response_id)
        
        # Test 5: Conversation forking
        self.test_conversation_forking(response_id)
        
        # Test 6: Web search tool
        self.test_web_search_tool()
        
        # Test 7: File search tool
        self.test_file_search_tool()
        
        # Test 8: Multimodal input
        self.test_multimodal_input()
        
        # Test 9: Streaming response
        self.test_streaming_response()
        
        # Test 10: Responses list
        self.test_responses_list()
        
        # Test 11: Error handling
        self.test_error_handling()
        
        # Summary
        print("\n" + "=" * 60)
        print("📊 Test Results Summary")
        print("=" * 60)
        
        passed = sum(1 for result in self.test_results if result["success"])
        total = len(self.test_results)
        
        print(f"Total Tests: {total}")
        print(f"Passed: {passed}")
        print(f"Failed: {total - passed}")
        print(f"Success Rate: {(passed/total)*100:.1f}%")
        
        if passed == total:
            print("\n🎉 All tests passed! The Responses API is working perfectly.")
        else:
            print(f"\n⚠️  {total - passed} test(s) failed. Check the details above.")
            
        return passed == total

def main():
    """Main test runner"""
    print("🚀 Mock OpenAI Responses API Test Suite")
    print("Testing comprehensive functionality...")
    print()
    
    tester = ResponsesAPITester()
    success = tester.run_all_tests()
    
    sys.exit(0 if success else 1)

if __name__ == "__main__":
    main()
