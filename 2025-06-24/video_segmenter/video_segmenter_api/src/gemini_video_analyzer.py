import google.generativeai as genai
import os
import logging
from typing import List, Dict, Tuple, Optional
import json
import time
from PIL import Image
import tempfile
import mimetypes
from datetime import datetime

logger = logging.getLogger(__name__)

class GeminiVideoAnalyzer:
    """
    Advanced Gemini 2.5 API integration for comprehensive video analysis
    targeting developer audiences with native video understanding, audio transcription,
    and technical content analysis - using only Gemini APIs
    """
    
    def __init__(self, api_key: str, debug_dir: str = "/tmp/gemini_debug"):
        """
        Initialize Gemini API client for comprehensive video analysis
        
        Args:
            api_key: Gemini API key
            debug_dir: Directory to store debug information and intermediate results
        """
        logger.info("Initializing GeminiVideoAnalyzer with Gemini 2.5")
        genai.configure(api_key=api_key)
        
        # Use Gemini 2.5 Flash for comprehensive video analysis
        self.model = genai.GenerativeModel('gemini-2.5-flash')
        
        # Debug storage setup
        self.debug_dir = debug_dir
        os.makedirs(debug_dir, exist_ok=True)
        self.debug_data = {
            'session_id': datetime.now().strftime('%Y%m%d_%H%M%S'),
            'api_calls': [],
            'intermediate_results': {},
            'timing_data': {},
            'errors': []
        }
        
        # Technical content factors for developer audiences
        self.technical_factors = {
            "code_elements": ["syntax_highlighting", "code_snippets", "terminal_output", "IDE_screenshots", "documentation"],
            "technical_concepts": ["algorithms", "architecture", "debugging", "performance", "security", "best_practices"],
            "developer_engagement": ["problem_solving", "tutorials", "code_reviews", "technical_discussions", "tools_demos"],
            "educational_value": ["step_by_step", "clear_explanations", "practical_examples", "real_world_applications"],
            "platform_optimization": ["searchability", "shareability", "technical_accuracy", "code_readability"]
        }
        
        logger.info(f"GeminiVideoAnalyzer initialized successfully with Gemini 2.5, debug session: {self.debug_data['session_id']}")
    
    def _save_debug_data(self, step_name: str, data: Dict) -> None:
        """Save intermediate debug data to file"""
        try:
            debug_file = os.path.join(self.debug_dir, f"{self.debug_data['session_id']}_{step_name}.json")
            with open(debug_file, 'w') as f:
                json.dump(data, f, indent=2, default=str)
            logger.debug(f"Debug data saved: {debug_file}")
        except Exception as e:
            logger.error(f"Failed to save debug data for {step_name}: {e}")
    
    def _record_api_call(self, call_type: str, prompt_preview: str, response_preview: str, 
                        timing: float, success: bool, error: str = None) -> None:
        """Record details of each API call for debugging"""
        call_record = {
            'timestamp': datetime.now().isoformat(),
            'call_type': call_type,
            'prompt_preview': prompt_preview[:500] + "..." if len(prompt_preview) > 500 else prompt_preview,
            'response_preview': response_preview[:500] + "..." if len(response_preview) > 500 else response_preview,
            'timing_seconds': timing,
            'success': success,
            'error': error
        }
        self.debug_data['api_calls'].append(call_record)
        logger.info(f"API call recorded: {call_type}, success: {success}, timing: {timing:.2f}s")
    
    def upload_video_to_gemini(self, video_path: str) -> Dict:
        """
        Upload video to Gemini using Files API for comprehensive analysis
        
        Args:
            video_path: Path to the video file
            
        Returns:
            Dictionary with upload result and file URI
        """
        step_name = "video_upload"
        start_time = time.time()
        logger.info(f"Uploading video to Gemini: {video_path}")
        
        try:
            # Get file size and MIME type
            file_size = os.path.getsize(video_path)
            mime_type, _ = mimetypes.guess_type(video_path)
            
            if not mime_type or not mime_type.startswith('video/'):
                mime_type = 'video/mp4'  # Default fallback
            
            upload_info = {
                'video_path': video_path,
                'file_size': file_size,
                'mime_type': mime_type,
                'upload_start_time': datetime.now().isoformat()
            }
            
            logger.info(f"Video file size: {file_size} bytes, MIME type: {mime_type}")
            
            # Upload video file to Gemini
            uploaded_file = genai.upload_file(
                path=video_path,
                mime_type=mime_type
            )
            
            upload_info['upload_response'] = {
                'uri': uploaded_file.uri,
                'name': uploaded_file.name,
                'initial_state': uploaded_file.state.name
            }
            
            logger.info(f"Video uploaded successfully. URI: {uploaded_file.uri}")
            
            # Wait for the file to be processed
            logger.info("Waiting for video processing to complete...")
            processing_states = []
            
            while uploaded_file.state.name == "PROCESSING":
                processing_states.append({
                    'timestamp': datetime.now().isoformat(),
                    'state': uploaded_file.state.name
                })
                time.sleep(2)
                uploaded_file = genai.get_file(uploaded_file.name)
            
            upload_info['processing_states'] = processing_states
            upload_info['final_state'] = uploaded_file.state.name
            upload_info['processing_duration'] = time.time() - start_time
            
            if uploaded_file.state.name == "FAILED":
                logger.error("Video processing failed")
                upload_info['error'] = 'Video processing failed'
                self._save_debug_data(f"{step_name}_failed", upload_info)
                
                return {
                    'success': False,
                    'error': 'Video processing failed',
                    'state': uploaded_file.state.name,
                    'debug_info': upload_info
                }
            
            logger.info("Video processing completed successfully")
            
            result = {
                'success': True,
                'file_uri': uploaded_file.uri,
                'file_name': uploaded_file.name,
                'mime_type': uploaded_file.mime_type,
                'size_bytes': uploaded_file.size_bytes,
                'state': uploaded_file.state.name,
                'debug_info': upload_info
            }
            
            # Record API call for debugging
            timing = time.time() - start_time
            self._record_api_call(
                call_type="file_upload",
                prompt_preview=f"Upload {video_path}",
                response_preview=f"URI: {uploaded_file.uri}, State: {uploaded_file.state.name}",
                timing=timing,
                success=True
            )
            
            # Save intermediate result
            self.debug_data['intermediate_results'][step_name] = result
            self._save_debug_data(step_name, result)
            
            return result
            
        except Exception as e:
            timing = time.time() - start_time
            error_msg = str(e)
            logger.error(f"Error uploading video to Gemini: {error_msg}", exc_info=True)
            
            # Record failed API call
            self._record_api_call(
                call_type="file_upload",
                prompt_preview=f"Upload {video_path}",
                response_preview="",
                timing=timing,
                success=False,
                error=error_msg
            )
            
            # Save error info
            error_info = {
                'error': error_msg,
                'video_path': video_path,
                'timing': timing
            }
            self.debug_data['errors'].append(error_info)
            self._save_debug_data(f"{step_name}_error", error_info)
            
            return {
                'success': False,
                'error': error_msg,
                'debug_info': error_info
            }
    
    def extract_comprehensive_video_analysis(self, file_uri: str, analysis_focus: str = "technical_developer") -> Dict:
        """
        Extract comprehensive video analysis including transcription and visual analysis using Gemini 2.5
        
        Args:
            file_uri: Gemini file URI for the uploaded video
            analysis_focus: Focus of analysis (technical_developer, general, etc.)
            
        Returns:
            Dictionary with comprehensive video analysis
        """
        step_name = "comprehensive_analysis"
        start_time = time.time()
        logger.info(f"Starting comprehensive video analysis for: {file_uri}")
        
        try:
            # Comprehensive analysis prompt for technical content
            analysis_prompt = f"""
            You are an expert technical content analyst specializing in developer-focused video content analysis.
            
            Analyze this video comprehensively with focus on: {analysis_focus}
            
            Provide a detailed analysis covering:
            
            1. AUDIO TRANSCRIPTION & ANALYSIS:
            - Generate a complete transcript with timestamps for significant moments
            - Identify technical keywords, programming languages, frameworks mentioned
            - Extract key technical concepts and educational points
            - Note any code explanations, debugging processes, or technical discussions
            
            2. VISUAL CONTENT ANALYSIS:
            - Describe visual elements throughout the video (sample every 10-15 seconds)
            - Identify code snippets, IDE interfaces, terminal windows, documentation
            - Note technical diagrams, architecture charts, or development tools shown
            - Analyze the quality and clarity of visual technical content
            
            3. TECHNICAL EDUCATIONAL VALUE:
            - Assess the learning progression and educational structure
            - Identify step-by-step tutorials, problem-solving approaches
            - Rate the complexity level (beginner/intermediate/advanced)
            - Evaluate practical applicability for developers
            
            4. DEVELOPER ENGAGEMENT FACTORS:
            - Identify moments that would engage developer audiences
            - Note best practices demonstrations, debugging sessions
            - Highlight novel techniques or interesting technical insights
            - Assess shareability within developer communities
            
            5. SHORTS OPTIMIZATION OPPORTUNITIES:
            - Identify 30-60 second segments with high technical value
            - Suggest compelling technical hooks for short-form content
            - Recommend platform-specific optimizations for developer audiences
            - Propose technical call-to-actions and follow-up content
            
            Provide your analysis as a comprehensive JSON object with the following structure:
            
            {{
                "transcript_analysis": {{
                    "full_transcript": "complete transcript with timestamps",
                    "technical_keywords": ["keyword1", "keyword2"],
                    "programming_languages": ["lang1", "lang2"],
                    "frameworks_tools": ["tool1", "tool2"],
                    "key_concepts": [{{
                        "concept": "concept name",
                        "timestamp": "MM:SS",
                        "explanation_quality": 1-10,
                        "complexity_level": "beginner/intermediate/advanced"
                    }}],
                    "transcript_length": "total duration",
                    "word_count": "approximate word count"
                }},
                "visual_analysis": {{
                    "visual_timeline": [{{
                        "timestamp": "MM:SS",
                        "description": "what's shown",
                        "technical_elements": ["element1", "element2"],
                        "code_visible": true/false,
                        "ide_interface": true/false,
                        "documentation": true/false,
                        "technical_quality": 1-10
                    }}],
                    "overall_visual_quality": 1-10,
                    "code_readability": 1-10,
                    "technical_clarity": 1-10
                }},
                "educational_assessment": {{
                    "learning_structure": "description of educational flow",
                    "complexity_progression": "how complexity builds",
                    "practical_examples": 1-10,
                    "step_by_step_quality": 1-10,
                    "target_skill_level": "junior/mid/senior/mixed",
                    "educational_value": 1-10
                }},
                "developer_engagement": {{
                    "engagement_moments": [{{
                        "timestamp": "MM:SS",
                        "description": "what makes this engaging",
                        "engagement_type": "problem_solving/insight/technique/debugging",
                        "appeal_score": 1-10
                    }}],
                    "code_quality_demo": 1-10,
                    "problem_solving_approach": 1-10,
                    "technical_insights": 1-10,
                    "shareability_score": 1-10
                }},
                "shorts_opportunities": [{{
                    "start_timestamp": "MM:SS",
                    "end_timestamp": "MM:SS",
                    "duration": "duration in seconds",
                    "technical_hook": "compelling technical angle",
                    "key_learning": "main takeaway",
                    "platform_fit": {{
                        "youtube_shorts": 1-10,
                        "linkedin": 1-10,
                        "twitter": 1-10,
                        "github": 1-10
                    }},
                    "optimization_suggestions": ["suggestion1", "suggestion2"],
                    "call_to_action": "specific developer CTA"
                }}],
                "overall_assessment": {{
                    "technical_value": 1-10,
                    "educational_impact": 1-10,
                    "developer_appeal": 1-10,
                    "content_uniqueness": 1-10,
                    "production_quality": 1-10
                }},
                "recommendations": {{
                    "target_platforms": ["platform1", "platform2"],
                    "content_improvements": ["improvement1", "improvement2"],
                    "series_potential": "episodic content opportunities",
                    "follow_up_content": ["idea1", "idea2"]
                }}
            }}
            """
            
            logger.info("Sending video to Gemini 2.5 for comprehensive analysis")
            
            # Store request details for debugging
            request_info = {
                'file_uri': file_uri,
                'analysis_focus': analysis_focus,
                'prompt_length': len(analysis_prompt),
                'request_timestamp': datetime.now().isoformat()
            }
            
            # Create the request with file URI
            api_start_time = time.time()
            response = self.model.generate_content([
                {
                    "file_data": {
                        "file_uri": file_uri
                    }
                },
                analysis_prompt
            ])
            api_timing = time.time() - api_start_time
            
            # Store raw response for debugging
            raw_response = response.text
            request_info['raw_response'] = raw_response
            request_info['api_timing'] = api_timing
            request_info['response_length'] = len(raw_response)
            
            # Parse the response
            try:
                response_text = raw_response.strip()
                if response_text.startswith('```json'):
                    response_text = response_text[7:-3]
                elif response_text.startswith('```'):
                    response_text = response_text[3:-3]
                
                analysis = json.loads(response_text)
                analysis['success'] = True
                analysis['analysis_type'] = 'comprehensive_video_analysis'
                analysis['model_used'] = 'gemini-2.5-flash'
                analysis['file_uri'] = file_uri
                analysis['debug_info'] = request_info
                
                # Record successful API call
                total_timing = time.time() - start_time
                self._record_api_call(
                    call_type="comprehensive_analysis",
                    prompt_preview=analysis_prompt[:200],
                    response_preview=raw_response[:200],
                    timing=total_timing,
                    success=True
                )
                
                # Save intermediate result
                self.debug_data['intermediate_results'][step_name] = analysis
                self._save_debug_data(step_name, analysis)
                
                logger.info("Comprehensive video analysis completed successfully")
                
            except json.JSONDecodeError as e:
                logger.error(f"JSON parsing failed: {e}")
                
                # Record parsing failure
                error_msg = f"JSON parsing failed: {e}"
                total_timing = time.time() - start_time
                self._record_api_call(
                    call_type="comprehensive_analysis",
                    prompt_preview=analysis_prompt[:200],
                    response_preview=raw_response[:200],
                    timing=total_timing,
                    success=False,
                    error=error_msg
                )
                
                # Fallback analysis with raw response
                analysis = {
                    'success': False,
                    'raw_response': raw_response,
                    'analysis_type': 'comprehensive_video_analysis',
                    'model_used': 'gemini-2.5-flash',
                    'file_uri': file_uri,
                    'error': 'JSON parsing failed - response may be in different format',
                    'debug_info': request_info
                }
                
                # Save error details
                self.debug_data['errors'].append({
                    'step': step_name,
                    'error': error_msg,
                    'raw_response': raw_response[:1000]  # Truncated for storage
                })
                self._save_debug_data(f"{step_name}_parse_error", analysis)
            
            return analysis
            
        except Exception as e:
            error_msg = str(e)
            total_timing = time.time() - start_time
            logger.error(f"Error in comprehensive video analysis: {error_msg}", exc_info=True)
            
            # Record failed API call
            self._record_api_call(
                call_type="comprehensive_analysis",
                prompt_preview=analysis_prompt[:200] if 'analysis_prompt' in locals() else "N/A",
                response_preview="",
                timing=total_timing,
                success=False,
                error=error_msg
            )
            
            # Save error info
            error_info = {
                'error': error_msg,
                'file_uri': file_uri,
                'analysis_focus': analysis_focus,
                'timing': total_timing
            }
            self.debug_data['errors'].append(error_info)
            self._save_debug_data(f"{step_name}_error", error_info)
            
            return {
                'success': False,
                'error': error_msg,
                'analysis_type': 'comprehensive_video_analysis',
                'model_used': 'gemini-2.5-flash',
                'file_uri': file_uri,
                'debug_info': error_info
            }
    
    def analyze_video_segment(self, file_uri: str, start_time: str, end_time: str, segment_focus: str = "technical_deep_dive") -> Dict:
        """
        Analyze a specific video segment with deep technical focus
        
        Args:
            file_uri: Gemini file URI for the uploaded video
            start_time: Start timestamp (MM:SS format)
            end_time: End timestamp (MM:SS format)
            segment_focus: Focus of segment analysis
            
        Returns:
            Dictionary with detailed segment analysis
        """
        logger.info(f"Analyzing video segment {start_time} to {end_time}")
        try:
            segment_prompt = f"""
            Analyze this specific video segment from {start_time} to {end_time} with deep technical focus.
            
            Focus on: {segment_focus}
            
            Provide detailed analysis:
            
            1. SEGMENT CONTENT ANALYSIS:
            - Transcribe all spoken content in this segment
            - Identify every technical element shown or discussed
            - Note any code, commands, or technical demonstrations
            - Describe the learning objective of this segment
            
            2. TECHNICAL DEPTH ASSESSMENT:
            - Rate the technical complexity and depth
            - Identify the primary technical skill being taught
            - Note any best practices or common pitfalls mentioned
            - Assess the practical value for developers
            
            3. SHORT-FORM OPTIMIZATION:
            - How could this segment work as a standalone short?
            - What technical hook would make it compelling?
            - What setup/context would be needed for a short?
            - What call-to-action would be most effective?
            
            Provide response as JSON:
            {{
                "segment_info": {{
                    "start_time": "{start_time}",
                    "end_time": "{end_time}",
                    "duration": "calculated duration",
                    "segment_transcript": "complete transcript for this segment",
                    "primary_topic": "main technical topic"
                }},
                "technical_analysis": {{
                    "complexity_level": "beginner/intermediate/advanced",
                    "technical_skills": ["skill1", "skill2"],
                    "code_elements": ["element1", "element2"],
                    "tools_technologies": ["tool1", "tool2"],
                    "learning_objective": "what developers will learn",
                    "practical_value": 1-10
                }},
                "content_structure": {{
                    "has_problem_statement": true/false,
                    "demonstrates_solution": true/false,
                    "shows_results": true/false,
                    "provides_explanation": true/false,
                    "includes_best_practices": true/false
                }},
                "shorts_potential": {{
                    "standalone_viability": 1-10,
                    "technical_hook": "compelling opening",
                    "context_needed": "background required",
                    "optimal_duration": "recommended short length",
                    "platform_recommendations": ["platform1", "platform2"],
                    "call_to_action": "specific developer CTA"
                }},
                "optimization_suggestions": {{
                    "title_suggestions": ["title1", "title2"],
                    "thumbnail_focus": "what to highlight in thumbnail",
                    "caption_ideas": ["caption1", "caption2"],
                    "hashtag_suggestions": ["#hashtag1", "#hashtag2"]
                }}
            }}
            """
            
            response = self.model.generate_content([
                {
                    "file_data": {
                        "file_uri": file_uri
                    }
                },
                segment_prompt
            ])
            
            # Parse response
            try:
                response_text = response.text.strip()
                if response_text.startswith('```json'):
                    response_text = response_text[7:-3]
                elif response_text.startswith('```'):
                    response_text = response_text[3:-3]
                
                analysis = json.loads(response_text)
                analysis['success'] = True
                analysis['analysis_type'] = 'segment_analysis'
                analysis['model_used'] = 'gemini-2.5-flash'
                
                logger.info(f"Segment analysis completed for {start_time}-{end_time}")
                
            except json.JSONDecodeError as e:
                logger.error(f"JSON parsing failed for segment analysis: {e}")
                analysis = {
                    'success': False,
                    'raw_response': response.text,
                    'analysis_type': 'segment_analysis',
                    'model_used': 'gemini-2.5-flash',
                    'error': 'JSON parsing failed'
                }
            
            return analysis
            
        except Exception as e:
            logger.error(f"Error in segment analysis: {str(e)}", exc_info=True)
            return {
                'success': False,
                'error': str(e),
                'analysis_type': 'segment_analysis',
                'model_used': 'gemini-2.5-flash'
            }
    
    def create_developer_shorts_strategy(self, comprehensive_analysis: Dict, video_metadata: Dict) -> Dict:
        """
        Create strategic plan for developer-focused shorts based on comprehensive analysis
        
        Args:
            comprehensive_analysis: Full video analysis from Gemini
            video_metadata: Video file metadata
            
        Returns:
            Strategic plan for developer shorts creation
        """
        logger.info("Creating developer shorts strategy from comprehensive analysis")
        try:
            strategy_prompt = f"""
            Based on this comprehensive video analysis, create a strategic plan for developer-focused short-form content.
            
            COMPREHENSIVE ANALYSIS:
            {json.dumps(comprehensive_analysis, indent=2)}
            
            VIDEO METADATA:
            {json.dumps(video_metadata, indent=2)}
            
            Create a strategic plan focusing on:
            
            1. CONTENT PRIORITIZATION:
            - Rank the identified shorts opportunities by technical value
            - Identify the most educational and engaging segments
            - Consider developer community trends and interests
            - Balance technical depth with accessibility
            
            2. PLATFORM STRATEGY:
            - Optimize content for developer-focused platforms
            - Consider LinkedIn tech, YouTube Shorts, Twitter dev community
            - Adapt technical content for each platform's audience
            - Plan cross-platform content distribution
            
            3. SERIES DEVELOPMENT:
            - Identify opportunities for episodic technical content
            - Plan logical progression of technical concepts
            - Create hooks for follow-up content and engagement
            - Build thought leadership in specific technical areas
            
            4. ENGAGEMENT OPTIMIZATION:
            - Design developer-specific calls-to-action
            - Plan community engagement and discussion starters
            - Integrate with developer resources and documentation
            - Create pathways for deeper technical learning
            
            Provide comprehensive strategy as JSON:
            
            {{
                "executive_summary": {{
                    "primary_technical_themes": ["theme1", "theme2"],
                    "target_developer_segments": ["junior", "mid", "senior"],
                    "key_value_propositions": ["value1", "value2"],
                    "expected_impact": "description of expected outcomes"
                }},
                "prioritized_shorts": [{{
                    "priority_rank": 1,
                    "segment_timeframe": "MM:SS - MM:SS",
                    "technical_topic": "specific topic",
                    "target_audience": "developer segment",
                    "educational_value": 1-10,
                    "viral_potential": 1-10,
                    "production_effort": "low/medium/high",
                    "platform_recommendations": ["platform1", "platform2"],
                    "success_metrics": ["metric1", "metric2"]
                }}],
                "platform_strategy": {{
                    "youtube_shorts": {{
                        "content_approach": "strategy for YouTube",
                        "optimization_tactics": ["tactic1", "tactic2"],
                        "success_metrics": ["metric1", "metric2"]
                    }},
                    "linkedin": {{
                        "content_approach": "professional network strategy",
                        "optimization_tactics": ["tactic1", "tactic2"],
                        "success_metrics": ["metric1", "metric2"]
                    }},
                    "twitter": {{
                        "content_approach": "developer community strategy",
                        "optimization_tactics": ["tactic1", "tactic2"],
                        "success_metrics": ["metric1", "metric2"]
                    }}
                }},
                "series_opportunities": {{
                    "technical_series": [{{
                        "series_name": "series title",
                        "episode_count": "estimated episodes",
                        "learning_progression": "skill building approach",
                        "target_outcomes": ["outcome1", "outcome2"]
                    }}],
                    "content_calendar": "suggested posting schedule",
                    "cross_promotion": "strategies for series promotion"
                }},
                "engagement_strategy": {{
                    "community_building": ["strategy1", "strategy2"],
                    "developer_resources": ["resource1", "resource2"],
                    "collaboration_opportunities": ["collab1", "collab2"],
                    "thought_leadership": "positioning strategy"
                }},
                "success_framework": {{
                    "kpi_targets": {{
                        "views": "realistic view targets",
                        "engagement": "engagement rate goals",
                        "shares": "developer sharing behavior",
                        "conversions": "learning outcome metrics"
                    }},
                    "optimization_triggers": ["when to adjust strategy"],
                    "scaling_opportunities": ["growth strategies"]
                }}
            }}
            """
            
            response = self.model.generate_content(strategy_prompt)
            
            # Parse strategic plan
            try:
                response_text = response.text.strip()
                if response_text.startswith('```json'):
                    response_text = response_text[7:-3]
                elif response_text.startswith('```'):
                    response_text = response_text[3:-3]
                
                strategy = json.loads(response_text)
                strategy['success'] = True
                strategy['analysis_type'] = 'developer_shorts_strategy'
                strategy['model_used'] = 'gemini-2.5-flash'
                
                logger.info("Developer shorts strategy created successfully")
                
            except json.JSONDecodeError as e:
                logger.error(f"JSON parsing failed for strategy: {e}")
                strategy = {
                    'success': False,
                    'raw_response': response.text,
                    'analysis_type': 'developer_shorts_strategy',
                    'model_used': 'gemini-2.5-flash',
                    'error': 'JSON parsing failed'
                }
            
            return strategy
            
        except Exception as e:
            logger.error(f"Error in developer shorts strategy creation: {str(e)}", exc_info=True)
            return {
                'success': False,
                'error': str(e),
                'analysis_type': 'developer_shorts_strategy',
                'model_used': 'gemini-2.5-flash'
            }
    
    def get_debug_data(self) -> Dict:
        """
        Get all debug data collected during the session
        
        Returns:
            Dictionary with comprehensive debug information
        """
        # Save final debug summary
        debug_summary = {
            'session_summary': {
                'session_id': self.debug_data['session_id'],
                'total_api_calls': len(self.debug_data['api_calls']),
                'successful_calls': sum(1 for call in self.debug_data['api_calls'] if call['success']),
                'failed_calls': sum(1 for call in self.debug_data['api_calls'] if not call['success']),
                'total_errors': len(self.debug_data['errors']),
                'intermediate_results_count': len(self.debug_data['intermediate_results']),
                'total_session_time': sum(call['timing_seconds'] for call in self.debug_data['api_calls'])
            },
            'detailed_debug_data': self.debug_data
        }
        
        self._save_debug_data("session_summary", debug_summary)
        return debug_summary
    
    def cleanup_uploaded_file(self, file_name: str) -> bool:
        """
        Clean up uploaded file from Gemini
        
        Args:
            file_name: Name of the uploaded file to delete
            
        Returns:
            True if successful, False otherwise
        """
        try:
            genai.delete_file(file_name)
            logger.info(f"Cleaned up uploaded file: {file_name}")
            return True
        except Exception as e:
            logger.error(f"Error cleaning up file {file_name}: {str(e)}")
            return False
