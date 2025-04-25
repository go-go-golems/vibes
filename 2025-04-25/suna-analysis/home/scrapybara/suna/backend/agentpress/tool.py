1	    and result handling capabilities.
2	    
3	    Attributes:
4	        _schemas (Dict[str, List[ToolSchema]]): Registered schemas for tool methods
5	        
6	    Methods:
7	        get_schemas: Get all registered tool schemas
8	        success_response: Create a successful result
9	        fail_response: Create a failed result
10	    """
11	    
12	    def __init__(self):
13	        """Initialize tool with empty schema registry."""
14	        self._schemas: Dict[str, List[ToolSchema]] = {}
15	        logger.debug(f"Initializing tool class: {self.__class__.__name__}")
16	        self._register_schemas()
17	
18	    def _register_schemas(self):
19	        """Register schemas from all decorated methods."""
20	        for name, method in inspect.getmembers(self, predicate=inspect.ismethod):
21	            if hasattr(method, 'tool_schemas'):
22	                self._schemas[name] = method.tool_schemas
23	                logger.debug(f"Registered schemas for method '{name}' in {self.__class__.__name__}")
24	
25	    def get_schemas(self) -> Dict[str, List[ToolSchema]]:
26	        """Get all registered tool schemas.
27	        
28	        Returns:
29	            Dict mapping method names to their schema definitions
30	        """
31	        return self._schemas
32	
33	    def success_response(self, data: Union[Dict[str, Any], str]) -> ToolResult:
34	        """Create a successful tool result.
35	        
36	        Args:
37	            data: Result data (dictionary or string)
38	            
39	        Returns:
40	            ToolResult with success=True and formatted output
41	        """
42	        if isinstance(data, str):
43	            text = data
44	        else:
45	            text = json.dumps(data, indent=2)
46	        logger.debug(f"Created success response for {self.__class__.__name__}")
47	        return ToolResult(success=True, output=text)
48	
49	    def fail_response(self, msg: str) -> ToolResult:
50	        """Create a failed tool result.
51	        
52	        Args:
53	            msg: Error message describing the failure
54	            
55	        Returns:
56	            ToolResult with success=False and error message
57	        """
58	        logger.debug(f"Tool {self.__class__.__name__} returned failed result: {msg}")
59	        return ToolResult(success=False, output=msg)
60	
61	def _add_schema(func, schema: ToolSchema):
62	    """Helper to add schema to a function."""
63	    if not hasattr(func, 'tool_schemas'):
64	        func.tool_schemas = []
65	    func.tool_schemas.append(schema)
66	    logger.debug(f"Added {schema.schema_type.value} schema to function {func.__name__}")
67	    return func
68	
69	def openapi_schema(schema: Dict[str, Any]):
70	    """Decorator for OpenAPI schema tools."""
71	    def decorator(func):
72	        logger.debug(f"Applying OpenAPI schema to function {func.__name__}")
73	        return _add_schema(func, ToolSchema(
74	            schema_type=SchemaType.OPENAPI,
75	            schema=schema
76	        ))
77	    return decorator
78	
79	def xml_schema(
80	    tag_name: str,
81	    mappings: List[Dict[str, Any]] = None,
82	    example: str = None
83	):
84	    """
85	    Decorator for XML schema tools with improved node mapping.
86	    
87	    Args:
88	        tag_name: Name of the root XML tag
89	        mappings: List of mapping definitions, each containing:
90	            - param_name: Name of the function parameter
91	            - node_type: "element", "attribute", or "content" 
92	            - path: Path to the node (default "." for root)
93	            - required: Whether the parameter is required (default True)
94	        example: Optional example showing how to use the XML tag
95	    
96	    Example:
97	        @xml_schema(
98	            tag_name="str-replace",
99	            mappings=[
100	                {"param_name": "file_path", "node_type": "attribute", "path": "."},
101	                {"param_name": "old_str", "node_type": "element", "path": "old_str"},