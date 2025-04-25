1	import os
2	
3	from daytona_sdk import Daytona, DaytonaConfig, CreateSandboxParams, Sandbox, SessionExecuteRequest
4	from daytona_api_client.models.workspace_state import WorkspaceState
5	from dotenv import load_dotenv
6	
7	from agentpress.tool import Tool
8	from utils.logger import logger
9	from utils.files_utils import clean_path
10	
11	load_dotenv()
12	
13	logger.debug("Initializing Daytona sandbox configuration")
14	config = DaytonaConfig(
15	    api_key=os.getenv("DAYTONA_API_KEY"),
16	    server_url=os.getenv("DAYTONA_SERVER_URL"),
17	    target=os.getenv("DAYTONA_TARGET")
18	)
19	
20	if config.api_key:
21	    logger.debug("Daytona API key configured successfully")
22	else:
23	    logger.warning("No Daytona API key found in environment variables")
24	
25	if config.server_url:
26	    logger.debug(f"Daytona server URL set to: {config.server_url}")
27	else:
28	    logger.warning("No Daytona server URL found in environment variables")
29	
30	if config.target:
31	    logger.debug(f"Daytona target set to: {config.target}")
32	else:
33	    logger.warning("No Daytona target found in environment variables")
34	
35	daytona = Daytona(config)
36	logger.debug("Daytona client initialized")
37	
38	async def get_or_start_sandbox(sandbox_id: str):
39	    """Retrieve a sandbox by ID, check its state, and start it if needed."""
40	    
41	    logger.info(f"Getting or starting sandbox with ID: {sandbox_id}")
42	    
43	    try:
44	        sandbox = daytona.get_current_sandbox(sandbox_id)
45	        
46	        # Check if sandbox needs to be started
47	        if sandbox.instance.state == WorkspaceState.ARCHIVED or sandbox.instance.state == WorkspaceState.STOPPED:
48	            logger.info(f"Sandbox is in {sandbox.instance.state} state. Starting...")
49	            try:
50	                daytona.start(sandbox)
51	                # Wait a moment for the sandbox to initialize
52	                # sleep(5)
53	                # Refresh sandbox state after starting
54	                sandbox = daytona.get_current_sandbox(sandbox_id)
55	                
56	                # Start supervisord in a session when restarting
57	                start_supervisord_session(sandbox)
58	            except Exception as e:
59	                logger.error(f"Error starting sandbox: {e}")
60	                raise e
61	        
62	        logger.info(f"Sandbox {sandbox_id} is ready")
63	        return sandbox
64	        
65	    except Exception as e:
66	        logger.error(f"Error retrieving or starting sandbox: {str(e)}")
67	        raise e
68	
69	def start_supervisord_session(sandbox: Sandbox):
70	    """Start supervisord in a session."""
71	    session_id = "supervisord-session"
72	    try:
73	        logger.info(f"Creating session {session_id} for supervisord")
74	        sandbox.process.create_session(session_id)
75	        
76	        # Execute supervisord command
77	        sandbox.process.execute_session_command(session_id, SessionExecuteRequest(
78	            command="exec /usr/bin/supervisord -n -c /etc/supervisor/conf.d/supervisord.conf",
79	            var_async=True
80	        ))
81	        logger.info(f"Supervisord started in session {session_id}")
82	    except Exception as e:
83	        logger.error(f"Error starting supervisord session: {str(e)}")
84	        raise e
85	
86	def create_sandbox(password: str):
87	    """Create a new sandbox with all required services configured and running."""
88	    
89	    logger.info("Creating new Daytona sandbox environment")
90	    logger.debug("Configuring sandbox with browser-use image and environment variables")
91	        
92	    sandbox = daytona.create(CreateSandboxParams(
93	        image="adamcohenhillel/kortix-suna:0.0.20",
94	        public=True,
95	        env_vars={
96	            "CHROME_PERSISTENT_SESSION": "true",
97	            "RESOLUTION": "1024x768x24",
98	            "RESOLUTION_WIDTH": "1024",
99	            "RESOLUTION_HEIGHT": "768",
100	            "VNC_PASSWORD": password,