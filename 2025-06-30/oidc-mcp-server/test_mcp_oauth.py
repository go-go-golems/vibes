#!/usr/bin/env python3
"""
Test MCP Client with OAuth Integration

This script demonstrates how an MCP client can use OAuth2 with PKCE
to authenticate with an MCP server that requires OAuth authentication.
"""

import asyncio
import base64
import hashlib
import secrets
import urllib.parse
from typing import Dict, Any
import httpx
import json


class MCPOAuthClient:
    def __init__(self, server_base_url: str = "http://localhost:8080"):
        self.server_base_url = server_base_url
        self.client_id = None
        self.access_token = None
        self.code_verifier = None
        self.code_challenge = None
        
    def generate_pkce_pair(self):
        """Generate PKCE code verifier and challenge"""
        self.code_verifier = base64.urlsafe_b64encode(
            secrets.token_bytes(32)
        ).decode('utf-8').rstrip('=')
        
        self.code_challenge = base64.urlsafe_b64encode(
            hashlib.sha256(self.code_verifier.encode('utf-8')).digest()
        ).decode('utf-8').rstrip('=')
        
    async def register_client(self) -> Dict[str, Any]:
        """Register as a dynamic OAuth client"""
        registration_data = {
            "redirect_uris": ["http://localhost:3000/callback"],
            "client_name": "Test MCP Client",
            "grant_types": ["authorization_code"],
            "response_types": ["code"],
            "token_endpoint_auth_method": "none"  # Public client
        }
        
        async with httpx.AsyncClient() as client:
            response = await client.post(
                f"{self.server_base_url}/register",
                json=registration_data,
                headers={"Content-Type": "application/json"}
            )
            
            if response.status_code == 201:
                client_info = response.json()
                self.client_id = client_info["client_id"]
                print(f"✅ Successfully registered client: {self.client_id}")
                return client_info
            else:
                raise Exception(f"Failed to register client: {response.status_code} - {response.text}")
    
    def get_authorization_url(self) -> str:
        """Generate the authorization URL for the OAuth flow"""
        if not self.client_id:
            raise Exception("Client not registered yet")
            
        self.generate_pkce_pair()
        
        params = {
            "response_type": "code",
            "client_id": self.client_id,
            "redirect_uri": "http://localhost:3000/callback",
            "code_challenge": self.code_challenge,
            "code_challenge_method": "S256",
            "state": "test_state_12345678"
        }
        
        query_string = urllib.parse.urlencode(params)
        return f"{self.server_base_url}/authorize?{query_string}"
    
    async def simulate_user_login(self, auth_url: str) -> str:
        """Simulate user login and extract authorization code"""
        print(f"🔐 Simulating user login at: {auth_url}")
        
        # First, get the login form
        async with httpx.AsyncClient() as client:
            response = await client.get(auth_url)
            if response.status_code != 200:
                raise Exception(f"Failed to get login form: {response.status_code}")
            
            # Now submit the login form with credentials
            login_data = {
                "username": "wesen",
                "password": "secret"
            }
            
            response = await client.post(
                auth_url,
                data=login_data,
                headers={"Content-Type": "application/x-www-form-urlencoded"},
                follow_redirects=False
            )
            
            if response.status_code == 303:
                # Extract authorization code from redirect location
                location = response.headers.get("Location")
                if location:
                    parsed_url = urllib.parse.urlparse(location)
                    query_params = urllib.parse.parse_qs(parsed_url.query)
                    if "code" in query_params:
                        auth_code = query_params["code"][0]
                        print(f"✅ Got authorization code: {auth_code[:20]}...")
                        return auth_code
                        
            raise Exception(f"Failed to get authorization code: {response.status_code} - {response.text}")
    
    async def exchange_code_for_token(self, auth_code: str) -> Dict[str, Any]:
        """Exchange authorization code for access token"""
        token_data = {
            "grant_type": "authorization_code",
            "code": auth_code,
            "redirect_uri": "http://localhost:3000/callback",
            "client_id": self.client_id,
            "code_verifier": self.code_verifier
        }
        
        async with httpx.AsyncClient() as client:
            response = await client.post(
                f"{self.server_base_url}/token",
                data=token_data,
                headers={"Content-Type": "application/x-www-form-urlencoded"}
            )
            
            if response.status_code == 200:
                token_info = response.json()
                self.access_token = token_info["access_token"]
                print(f"✅ Got access token: {self.access_token[:20]}...")
                return token_info
            else:
                raise Exception(f"Failed to exchange code for token: {response.status_code} - {response.text}")
    
    async def access_protected_resource(self, endpoint: str = "/v1/contexts") -> Dict[str, Any]:
        """Access a protected MCP resource using the access token"""
        if not self.access_token:
            raise Exception("No access token available")
            
        headers = {
            "Authorization": f"Bearer {self.access_token}",
            "Content-Type": "application/json"
        }
        
        async with httpx.AsyncClient() as client:
            response = await client.get(
                f"{self.server_base_url}{endpoint}",
                headers=headers
            )
            
            if response.status_code == 200:
                resource_data = response.json()
                print(f"✅ Successfully accessed protected resource: {endpoint}")
                return resource_data
            else:
                raise Exception(f"Failed to access protected resource: {response.status_code} - {response.text}")
    
    async def run_oauth_flow(self):
        """Run the complete OAuth flow"""
        print("🚀 Starting OAuth flow for MCP client...")
        
        try:
            # Step 1: Register client
            print("\n📝 Step 1: Registering OAuth client...")
            client_info = await self.register_client()
            
            # Step 2: Get authorization URL
            print("\n🔗 Step 2: Generating authorization URL...")
            auth_url = self.get_authorization_url()
            print(f"Authorization URL: {auth_url}")
            
            # Step 3: Simulate user login
            print("\n👤 Step 3: Simulating user login...")
            auth_code = await self.simulate_user_login(auth_url)
            
            # Step 4: Exchange code for token
            print("\n🔑 Step 4: Exchanging code for access token...")
            token_info = await self.exchange_code_for_token(auth_code)
            
            # Step 5: Access protected resource
            print("\n🛡️  Step 5: Accessing protected MCP resource...")
            resource_data = await self.access_protected_resource()
            
            print("\n✅ OAuth flow completed successfully!")
            print(f"Resource data: {json.dumps(resource_data, indent=2)}")
            
            return True
            
        except Exception as e:
            print(f"\n❌ OAuth flow failed: {e}")
            return False


async def main():
    """Main function to test the OAuth integration"""
    print("=" * 60)
    print("MCP Client OAuth Integration Test")
    print("=" * 60)
    
    # Check if server is running
    try:
        async with httpx.AsyncClient() as client:
            response = await client.get("http://localhost:8080/health")
            if response.status_code == 200:
                print("✅ OIDC MCP Server is running")
            else:
                print("❌ OIDC MCP Server is not responding correctly")
                return
    except Exception as e:
        print(f"❌ Cannot connect to OIDC MCP Server: {e}")
        print("Make sure the server is running on http://localhost:8080")
        return
    
    # Run the OAuth flow
    oauth_client = MCPOAuthClient()
    success = await oauth_client.run_oauth_flow()
    
    if success:
        print("\n🎉 MCP OAuth integration test completed successfully!")
        print("The MCP client can now authenticate with the OAuth server and access protected resources.")
    else:
        print("\n💥 MCP OAuth integration test failed!")


if __name__ == "__main__":
    asyncio.run(main())

