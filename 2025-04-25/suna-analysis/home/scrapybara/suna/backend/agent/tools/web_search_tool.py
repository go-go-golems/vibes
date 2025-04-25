1	from tavily import AsyncTavilyClient
2	import httpx
3	from typing import List, Optional
4	from datetime import datetime
5	import os
6	from dotenv import load_dotenv
7	from agentpress.tool import Tool, ToolResult, openapi_schema, xml_schema
8	import json
9	
10	# TODO: add subpages, etc... in filters as sometimes its necessary 
11	
12	class WebSearchTool(Tool):
13	    """Tool for performing web searches using the Exa API."""
14	
15	    def __init__(self, api_key: str = None):
16	        super().__init__()
17	        # Load environment variables
18	        load_dotenv()
19	        # Use the provided API key or get it from environment variables
20	        self.api_key = api_key or os.getenv("TAVILY_API_KEY")
21	        if not self.api_key:
22	            raise ValueError("TAVILY_API_KEY not found in environment variables")
23	
24	        # Tavily asynchronous search client
25	        self.tavily_client = AsyncTavilyClient(api_key=self.api_key)
26	
27	    @openapi_schema({
28	        "type": "function",
29	        "function": {
30	            "name": "web_search",
31	            "description": "Search the web for up-to-date information on a specific topic using the Exa API. This tool allows you to gather real-time information from the internet to answer user queries, research topics, validate facts, and find recent developments. Results include titles, URLs, summaries, and publication dates. Use this tool for discovering relevant web pages before potentially crawling them for complete content.",
32	            "parameters": {
33	                "type": "object",
34	                "properties": {
35	                    "query": {
36	                        "type": "string",
37	                        "description": "The search query to find relevant web pages. Be specific and include key terms to improve search accuracy. For best results, use natural language questions or keyword combinations that precisely describe what you're looking for."
38	                    },
39	                    "summary": {
40	                        "type": "boolean",
41	                        "description": "Whether to include a summary of each search result. Summaries provide key context about each page without requiring full content extraction. Set to true to get concise descriptions of each result.",
42	                        "default": True
43	                    },
44	                    "num_results": {
45	                        "type": "integer",
46	                        "description": "The number of search results to return. Increase for more comprehensive research or decrease for focused, high-relevance results.",
47	                        "default": 20
48	                    }
49	                },
50	                "required": ["query"]