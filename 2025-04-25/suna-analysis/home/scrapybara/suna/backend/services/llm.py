1	"""
2	LLM API interface for making calls to various language models.
3	
4	This module provides a unified interface for making API calls to different LLM providers
5	(OpenAI, Anthropic, Groq, etc.) using LiteLLM. It includes support for:
6	- Streaming responses
7	- Tool calls and function calling
8	- Retry logic with exponential backoff
9	- Model-specific configurations
10	- Comprehensive error handling and logging
11	"""
12	
13	from typing import Union, Dict, Any, Optional, AsyncGenerator, List
14	import os
15	import json
16	import asyncio
17	from openai import OpenAIError
18	import litellm
19	from utils.logger import logger
20	from datetime import datetime
21	import traceback
22	
23	# litellm.set_verbose=True
24	litellm.modify_params=True
25	
26	# Constants
27	MAX_RETRIES = 3
28	RATE_LIMIT_DELAY = 30
29	RETRY_DELAY = 5
30	
31	class LLMError(Exception):
32	    """Base exception for LLM-related errors."""
33	    pass
34	
35	class LLMRetryError(LLMError):
36	    """Exception raised when retries are exhausted."""
37	    pass
38	
39	def setup_api_keys() -> None:
40	    """Set up API keys from environment variables."""
41	    providers = ['OPENAI', 'ANTHROPIC', 'GROQ', 'OPENROUTER']
42	    for provider in providers:
43	        key = os.environ.get(f'{provider}_API_KEY')
44	        if key:
45	            logger.debug(f"API key set for provider: {provider}")
46	        else:
47	            logger.warning(f"No API key found for provider: {provider}")
48	    
49	    # Set up OpenRouter API base if not already set
50	    if os.environ.get('OPENROUTER_API_KEY') and not os.environ.get('OPENROUTER_API_BASE'):