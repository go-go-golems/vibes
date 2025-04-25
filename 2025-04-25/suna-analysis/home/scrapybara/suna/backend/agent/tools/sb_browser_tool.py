1	
2	    @openapi_schema({
3	        "type": "function",
4	        "function": {
5	            "name": "browser_click_element",
6	            "description": "Click on an element by index",
7	            "parameters": {
8	                "type": "object",
9	                "properties": {
10	                    "index": {
11	                        "type": "integer",
12	                        "description": "The index of the element to click"
13	                    }
14	                },
15	                "required": ["index"]
16	            }
17	        }
18	    })
19	    @xml_schema(
20	        tag_name="browser-click-element",
21	        mappings=[
22	            {"param_name": "index", "node_type": "content", "path": "."}
23	        ],
24	        example='''
25	        <browser-click-element>
26	        2
27	        </browser-click-element>
28	        '''
29	    )
30	    async def browser_click_element(self, index: int) -> ToolResult:
31	        """Click on an element by index
32	        
33	        Args:
34	            index (int): The index of the element to click
35	            
36	        Returns:
37	            dict: Result of the execution
38	        """
39	        print(f"\033[95mClicking element with index: {index}\033[0m")
40	        return await self._execute_browser_action("click_element", {"index": index})
41	
42	    @openapi_schema({
43	        "type": "function",
44	        "function": {
45	            "name": "browser_input_text",
46	            "description": "Input text into an element",
47	            "parameters": {
48	                "type": "object",
49	                "properties": {
50	                    "index": {
51	                        "type": "integer",
52	                        "description": "The index of the element to input text into"
53	                    },
54	                    "text": {
55	                        "type": "string",
56	                        "description": "The text to input"
57	                    }
58	                },
59	                "required": ["index", "text"]
60	            }
61	        }
62	    })
63	    @xml_schema(
64	        tag_name="browser-input-text",
65	        mappings=[
66	            {"param_name": "index", "node_type": "attribute", "path": "."},
67	            {"param_name": "text", "node_type": "content", "path": "."}
68	        ],
69	        example='''
70	        <browser-input-text index="2">
71	        Hello, world!
72	        </browser-input-text>
73	        '''
74	    )
75	    async def browser_input_text(self, index: int, text: str) -> ToolResult:
76	        """Input text into an element
77	        
78	        Args:
79	            index (int): The index of the element to input text into
80	            text (str): The text to input
81	            
82	        Returns:
83	            dict: Result of the execution
84	        """
85	        print(f"\033[95mInputting text into element {index}: {text}\033[0m")
86	        return await self._execute_browser_action("input_text", {"index": index, "text": text})
87	
88	    @openapi_schema({
89	        "type": "function",
90	        "function": {
91	            "name": "browser_send_keys",
92	            "description": "Send keyboard keys such as Enter, Escape, or keyboard shortcuts",
93	            "parameters": {
94	                "type": "object",
95	                "properties": {
96	                    "keys": {
97	                        "type": "string",
98	                        "description": "The keys to send (e.g., 'Enter', 'Escape', 'Control+a')"
99	                    }
100	                },
101	                "required": ["keys"]