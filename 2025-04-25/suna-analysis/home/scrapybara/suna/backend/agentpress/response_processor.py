1	        llm_model: str,
2	        config: ProcessorConfig = ProcessorConfig(),
3	    ) -> AsyncGenerator[Dict[str, Any], None]:
4	        """Process a streaming LLM response, handling tool calls and execution.
5	        
6	        Args:
7	            llm_response: Streaming response from the LLM
8	            thread_id: ID of the conversation thread
9	            prompt_messages: List of messages sent to the LLM (the prompt)
10	            llm_model: The name of the LLM model used
11	            config: Configuration for parsing and execution
12	            
13	        Yields:
14	            Complete message objects matching the DB schema, except for content chunks.
15	        """
16	        accumulated_content = ""
17	        tool_calls_buffer = {}
18	        current_xml_content = ""
19	        xml_chunks_buffer = []
20	        pending_tool_executions = []
21	        yielded_tool_indices = set() # Stores indices of tools whose *status* has been yielded
22	        tool_index = 0
23	        xml_tool_call_count = 0
24	        finish_reason = None
25	        last_assistant_message_object = None # Store the final saved assistant message object
26	        tool_result_message_objects = {} # tool_index -> full saved message object
27	
28	        logger.info(f"Streaming Config: XML={config.xml_tool_calling}, Native={config.native_tool_calling}, "
29	                   f"Execute on stream={config.execute_on_stream}, Strategy={config.tool_execution_strategy}")
30	
31	        thread_run_id = str(uuid.uuid4())
32	
33	        try:
34	            # --- Save and Yield Start Events ---
35	            start_content = {"status_type": "thread_run_start", "thread_run_id": thread_run_id}
36	            start_msg_obj = await self.add_message(
37	                thread_id=thread_id, type="status", content=start_content, 
38	                is_llm_message=False, metadata={"thread_run_id": thread_run_id}
39	            )
40	            if start_msg_obj: yield start_msg_obj
41	
42	            assist_start_content = {"status_type": "assistant_response_start"}
43	            assist_start_msg_obj = await self.add_message(
44	                thread_id=thread_id, type="status", content=assist_start_content, 
45	                is_llm_message=False, metadata={"thread_run_id": thread_run_id}
46	            )
47	            if assist_start_msg_obj: yield assist_start_msg_obj
48	            # --- End Start Events ---
49	
50	            async for chunk in llm_response:
51	                if hasattr(chunk, 'choices') and chunk.choices and hasattr(chunk.choices[0], 'finish_reason') and chunk.choices[0].finish_reason:
52	                    finish_reason = chunk.choices[0].finish_reason
53	                    logger.debug(f"Detected finish_reason: {finish_reason}")
54	
55	                if hasattr(chunk, 'choices') and chunk.choices:
56	                    delta = chunk.choices[0].delta if hasattr(chunk.choices[0], 'delta') else None
57	                    
58	                    # Check for and log Anthropic thinking content
59	                    if delta and hasattr(delta, 'reasoning_content') and delta.reasoning_content:
60	                        logger.info(f"[THINKING]: {delta.reasoning_content}")
61	                        # Append reasoning to main content to be saved in the final message
62	                        accumulated_content += delta.reasoning_content
63	
64	                    # Process content chunk
65	                    if delta and hasattr(delta, 'content') and delta.content:
66	                        chunk_content = delta.content
67	                        accumulated_content += chunk_content
68	                        current_xml_content += chunk_content
69	
70	                        if not (config.max_xml_tool_calls > 0 and xml_tool_call_count >= config.max_xml_tool_calls):
71	                            # Yield ONLY content chunk (don't save)
72	                            now_chunk = datetime.now(timezone.utc).isoformat()
73	                            yield {
74	                                "message_id": None, "thread_id": thread_id, "type": "assistant",
75	                                "is_llm_message": True,
76	                                "content": json.dumps({"role": "assistant", "content": chunk_content}),
77	                                "metadata": json.dumps({"stream_status": "chunk", "thread_run_id": thread_run_id}),
78	                                "created_at": now_chunk, "updated_at": now_chunk
79	                            }
80	                        else:
81	                            logger.info("XML tool call limit reached - not yielding more content chunks")
82	
83	                        # --- Process XML Tool Calls (if enabled and limit not reached) ---
84	                        if config.xml_tool_calling and not (config.max_xml_tool_calls > 0 and xml_tool_call_count >= config.max_xml_tool_calls):
85	                            xml_chunks = self._extract_xml_chunks(current_xml_content)
86	                            for xml_chunk in xml_chunks:
87	                                current_xml_content = current_xml_content.replace(xml_chunk, "", 1)
88	                                xml_chunks_buffer.append(xml_chunk)
89	                                result = self._parse_xml_tool_call(xml_chunk)
90	                                if result:
91	                                    tool_call, parsing_details = result
92	                                    xml_tool_call_count += 1
93	                                    current_assistant_id = last_assistant_message_object['message_id'] if last_assistant_message_object else None
94	                                    context = self._create_tool_context(
95	                                        tool_call, tool_index, current_assistant_id, parsing_details
96	                                    )
97	
98	                                    if config.execute_tools and config.execute_on_stream:
99	                                        # Save and Yield tool_started status
100	                                        started_msg_obj = await self._yield_and_save_tool_started(context, thread_id, thread_run_id)
101	                                        if started_msg_obj: yield started_msg_obj