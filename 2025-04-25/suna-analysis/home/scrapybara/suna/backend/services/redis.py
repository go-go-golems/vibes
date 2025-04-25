1	import redis.asyncio as redis
2	import os
3	from dotenv import load_dotenv
4	import asyncio
5	import certifi
6	import ssl
7	from utils.logger import logger
8	import random
9	from functools import wraps
10	
11	# Redis client
12	client = None
13	REDIS_KEY_TTL = 3600 * 24  # 24 hour TTL as safety mechanism
14	_initialized = False
15	_init_lock = asyncio.Lock()
16	
17	# Retry configuration
18	MAX_RETRIES = 5
19	BASE_RETRY_DELAY = 0.5  # Start with 500ms delay
20	MAX_RETRY_DELAY = 10.0  # Maximum delay of 10 seconds
21	RETRY_JITTER = 0.1  # Add 10% random jitter to retry delay
22	
23	async def with_retry(func, *args, **kwargs):
24	    """Execute a Redis operation with exponential backoff retry."""
25	    retries = 0
26	    last_exception = None
27	    func_name = getattr(func, "__name__", str(func))
28	    
29	    while retries < MAX_RETRIES:
30	        try:
31	            return await func(*args, **kwargs)
32	        except (redis.ConnectionError, redis.TimeoutError, ConnectionResetError) as e:
33	            retries += 1
34	            last_exception = e
35	            
36	            if retries >= MAX_RETRIES:
37	                logger.error(f"Redis operation {func_name} failed after {MAX_RETRIES} retries: {str(e)}")
38	                raise
39	            
40	            # Calculate backoff with jitter
41	            delay = min(BASE_RETRY_DELAY * (2 ** (retries - 1)), MAX_RETRY_DELAY)
42	            jitter = delay * RETRY_JITTER * random.uniform(-1, 1)
43	            wait_time = delay + jitter
44	            
45	            logger.warning(f"Redis {func_name} error (attempt {retries}/{MAX_RETRIES}): {str(e)}. Retrying in {wait_time:.2f}s")
46	            await asyncio.sleep(wait_time)
47	            
48	            # Try to reconnect if needed
49	            if client and hasattr(client, 'connection_pool'):
50	                try: