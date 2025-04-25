1	import os
2	import time
3	import base64
4	import aiohttp
5	import asyncio
6	import logging
7	from typing import Optional, Dict, Any, Union
8	from PIL import Image
9	
10	from agentpress.tool import Tool, ToolResult, openapi_schema, xml_schema
11	from sandbox.sandbox import SandboxToolsBase, Sandbox
12	
13	KEYBOARD_KEYS = [
14	    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
15	    'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
16	    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
17	    'enter', 'esc', 'backspace', 'tab', 'space', 'delete',
18	    'ctrl', 'alt', 'shift', 'win',
19	    'up', 'down', 'left', 'right',
20	    'f1', 'f2', 'f3', 'f4', 'f5', 'f6', 'f7', 'f8', 'f9', 'f10', 'f11', 'f12',
21	    'ctrl+c', 'ctrl+v', 'ctrl+x', 'ctrl+z', 'ctrl+a', 'ctrl+s',
22	    'alt+tab', 'alt+f4', 'ctrl+alt+delete'
23	]
24	
25	class ComputerUseTool(SandboxToolsBase):
26	    """Computer automation tool for controlling the sandbox browser and GUI."""
27	    
28	    def __init__(self, sandbox: Sandbox):
29	        """Initialize automation tool with sandbox connection."""
30	        super().__init__(sandbox)
31	        self.session = None
32	        self.mouse_x = 0  # Track current mouse position
33	        self.mouse_y = 0
34	        # Get automation service URL using port 8000
35	        self.api_base_url = self.sandbox.get_preview_link(8000)
36	        logging.info(f"Initialized Computer Use Tool with API URL: {self.api_base_url}")
37	    
38	    async def _get_session(self) -> aiohttp.ClientSession:
39	        """Get or create aiohttp session for API requests."""
40	        if self.session is None or self.session.closed:
41	            self.session = aiohttp.ClientSession()
42	        return self.session
43	    
44	    async def _api_request(self, method: str, endpoint: str, data: Optional[Dict] = None) -> Dict:
45	        """Send request to automation service API."""
46	        try:
47	            session = await self._get_session()
48	            url = f"{self.api_base_url}/api{endpoint}"
49	            
50	            logging.debug(f"API request: {method} {url} {data}")
51	            
52	            if method.upper() == "GET":
53	                async with session.get(url) as response:
54	                    result = await response.json()
55	            else:  # POST
56	                async with session.post(url, json=data) as response:
57	                    result = await response.json()
58	            
59	            logging.debug(f"API response: {result}")
60	            return result
61	            
62	        except Exception as e:
63	            logging.error(f"API request failed: {str(e)}")
64	            return {"success": False, "error": str(e)}
65	    
66	    async def cleanup(self):
67	        """Clean up resources."""
68	        if self.session and not self.session.closed:
69	            await self.session.close()
70	            self.session = None
71	    
72	    @openapi_schema({
73	        "type": "function",
74	        "function": {
75	            "name": "move_to",
76	            "description": "Move cursor to specified position",
77	            "parameters": {
78	                "type": "object",
79	                "properties": {
80	                    "x": {
81	                        "type": "number",
82	                        "description": "X coordinate"
83	                    },
84	                    "y": {
85	                        "type": "number",
86	                        "description": "Y coordinate"
87	                    }
88	                },
89	                "required": ["x", "y"]
90	            }
91	        }
92	    })
93	    @xml_schema(
94	        tag_name="move-to",
95	        mappings=[
96	            {"param_name": "x", "node_type": "attribute", "path": "."},
97	            {"param_name": "y", "node_type": "attribute", "path": "."}
98	        ],
99	        example='''
100	        <move-to x="100" y="200">