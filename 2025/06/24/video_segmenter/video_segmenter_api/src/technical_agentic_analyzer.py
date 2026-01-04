import google.generativeai as genai
import os
import logging
import base64
from typing import List, Dict, Tuple, Optional
import json
import time
from PIL import Image
import io
import speech_recognition as sr
import tempfile
import subprocess

logger = logging.getLogger(__name__)

class TechnicalAgenticAnalyzer:
    """
    Advanced agentic Gemini API integration optimized for technical content analysis
    targeting developer audiences with code analysis, technical concept extraction,
    and developer-focused content optimization
    """
    
    def __init__(self, api_key: str):
        """
        Initialize Gemini API client for technical content analysis
        
        Args:
            api_key: Gemini API key
        """
        logger.info("Initializing TechnicalAgenticAnalyzer")
        genai.configure(api_key=api_key)
        
        # Use Gemini Flash for most operations (fast and cost-effective)
        self.flash_model = genai.GenerativeModel('gemini-1.5-flash')
        
        # Use Gemini Pro for complex analysis and planning
        self.pro_model = genai.GenerativeModel('gemini-1.5-pro')
        
        # Technical content factors for developer audiences
        self.technical_factors = {
            "code_elements": ["syntax_highlighting", "code_snippets", "terminal_output", "IDE_screenshots", "documentation"],
            "technical_concepts": ["algorithms", "architecture", "debugging", "performance", "security", "best_practices"],
            "developer_engagement": ["problem_solving", "tutorials", "code_reviews", "technical_discussions", "tools_demos"],
            "educational_value": ["step_by_step", "clear_explanations", "practical_examples", "real_world_applications"],
            "platform_optimization": ["searchability", "shareability", "technical_accuracy", "code_readability"]
        }
        
        # Initialize speech recognition for audio transcription
        self.recognizer = sr.Recognizer()
        logger.info("TechnicalAgenticAnalyzer initialized successfully")
    
    def extract_audio_transcript(self, video_path: str) -> Dict:
        """
        Extract audio transcript from video using speech recognition
        
        Args:
            video_path: Path to the video file
            
        Returns:
            Dictionary with transcript and metadata
        """
        logger.info(f"Extracting audio transcript from: {video_path}")
        try:
            # Extract audio from video using ffmpeg
            with tempfile.NamedTemporaryFile(suffix=".wav", delete=False) as temp_audio:
                audio_path = temp_audio.name
            
            # Use ffmpeg to extract audio
            cmd = [
                'ffmpeg', '-i', video_path, 
                '-ac', '1', '-ar', '16000', 
                '-y', audio_path
            ]
            
            logger.info(f"Running ffmpeg command: {' '.join(cmd)}")
            result = subprocess.run(cmd, capture_output=True, text=True)
            
            if result.returncode != 0:
                logger.error(f"ffmpeg failed: {result.stderr}")
                return {'success': False, 'error': 'Audio extraction failed'}
            
            # Transcribe audio
            logger.info("Transcribing audio content")
            with sr.AudioFile(audio_path) as source:
                audio = self.recognizer.record(source)
            
            try:
                transcript = self.recognizer.recognize_google(audio)
                logger.info(f"Transcript extracted successfully. Length: {len(transcript)} characters")
                
                # Clean up temp file
                os.unlink(audio_path)
                
                return {
                    'success': True,
                    'transcript': transcript,
                    'length': len(transcript),
                    'word_count': len(transcript.split())
                }
                
            except sr.UnknownValueError:
                logger.warning("Could not understand audio")
                os.unlink(audio_path)
                return {'success': False, 'error': 'Could not understand audio'}
            except sr.RequestError as e:
                logger.error(f"Speech recognition service error: {e}")
                os.unlink(audio_path)
                return {'success': False, 'error': f'Speech recognition failed: {e}'}
                
        except Exception as e:
            logger.error(f"Error in audio transcript extraction: {str(e)}", exc_info=True)
            return {'success': False, 'error': str(e)}
    
    def analyze_technical_frame(self, image_path: str, context: Dict = None, transcript: str = None) -> Dict:
        """
        Analyze frame for technical content targeting developer audiences
        
        Args:
            image_path: Path to the frame image
            context: Additional context about the video/segment
            transcript: Audio transcript for this frame segment
            
        Returns:
            Dictionary with detailed technical analysis
        """
        logger.info(f"Analyzing technical frame: {image_path}")
        try:
            image = Image.open(image_path)
            
            # Technical analysis prompt focused on developers
            technical_prompt = f"""
            You are an expert technical content analyst specializing in developer-focused video content.
            
            CONTEXT: {json.dumps(context) if context else "No additional context provided"}
            TRANSCRIPT: {transcript if transcript else "No audio transcript available"}
            
            Analyze this video frame for technical content targeting software developers:
            
            STEP 1 - TECHNICAL CONTENT IDENTIFICATION:
            Identify technical elements visible in the frame:
            - Code snippets, syntax highlighting, programming languages
            - IDE/editor interfaces, terminal windows, command line
            - Architecture diagrams, flowcharts, technical documentation
            - Development tools, debugging interfaces, profiling tools
            - Technical concepts being demonstrated or explained
            - Database schemas, API documentation, configuration files
            
            STEP 2 - EDUCATIONAL VALUE ASSESSMENT:
            Evaluate the educational potential for developers:
            - Clarity of technical explanations or demonstrations
            - Complexity level (beginner/intermediate/advanced)
            - Practical applicability to real-world development
            - Step-by-step learning progression
            - Problem-solving approach demonstrated
            
            STEP 3 - DEVELOPER ENGAGEMENT FACTORS:
            Analyze factors that would engage developer audiences:
            - Code quality and best practices shown
            - Novel techniques or approaches demonstrated
            - Common pain points being addressed
            - Tools or technologies being showcased
            - Debugging or troubleshooting content
            - Performance optimization techniques
            
            STEP 4 - TECHNICAL ACCURACY ASSESSMENT:
            Evaluate technical correctness and quality:
            - Code syntax and logic accuracy
            - Best practices adherence
            - Security considerations
            - Performance implications
            - Industry standard compliance
            
            STEP 5 - CONTENT OPTIMIZATION FOR DEVELOPERS:
            Recommend optimizations for developer audiences:
            - Code readability improvements
            - Additional technical context needed
            - Better visual presentation of technical concepts
            - Improved explanation of complex topics
            - Integration with popular developer platforms
            
            OUTPUT FORMAT (JSON):
            {{
                "technical_content": {{
                    "programming_languages": ["language1", "language2"],
                    "technologies_shown": ["tech1", "tech2"],
                    "code_elements": ["snippets", "configs", "documentation"],
                    "development_tools": ["tool1", "tool2"],
                    "technical_concepts": ["concept1", "concept2"],
                    "complexity_level": "beginner/intermediate/advanced"
                }},
                "educational_assessment": {{
                    "learning_value": 1-10,
                    "clarity_score": 1-10,
                    "practical_applicability": 1-10,
                    "step_by_step_quality": 1-10,
                    "problem_solving_approach": 1-10
                }},
                "developer_engagement": {{
                    "code_quality_demo": 1-10,
                    "novel_techniques": 1-10,
                    "pain_point_relevance": 1-10,
                    "tool_showcase_value": 1-10,
                    "debugging_content": 1-10,
                    "performance_focus": 1-10
                }},
                "technical_accuracy": {{
                    "syntax_correctness": 1-10,
                    "best_practices": 1-10,
                    "security_awareness": 1-10,
                    "performance_considerations": 1-10,
                    "industry_standards": 1-10
                }},
                "optimization_recommendations": {{
                    "code_readability": ["improvement1", "improvement2"],
                    "technical_context": ["context1", "context2"],
                    "visual_improvements": ["visual1", "visual2"],
                    "explanation_enhancements": ["enhancement1", "enhancement2"]
                }},
                "developer_appeal": {{
                    "overall_technical_value": 1-10,
                    "shareability_among_devs": 1-10,
                    "tutorial_potential": 1-10,
                    "discussion_generator": 1-10
                }},
                "platform_recommendations": {{
                    "github": {{
                        "fit_score": 1-10,
                        "optimization_notes": "specific advice"
                    }},
                    "dev_to": {{
                        "fit_score": 1-10,
                        "optimization_notes": "specific advice"
                    }},
                    "youtube_tech": {{
                        "fit_score": 1-10,
                        "optimization_notes": "specific advice"
                    }},
                    "linkedin_tech": {{
                        "fit_score": 1-10,
                        "optimization_notes": "specific advice"
                    }},
                    "twitter_dev": {{
                        "fit_score": 1-10,
                        "optimization_notes": "specific advice"
                    }}
                }},
                "content_categorization": {{
                    "primary_category": "tutorial/demo/review/discussion/debugging",
                    "technical_topics": ["topic1", "topic2"],
                    "skill_level_target": "junior/mid/senior/architect",
                    "content_type": "educational/promotional/entertainment/news"
                }},
                "shorts_potential": {{
                    "key_moments": [{{
                        "timestamp": "relative_position",
                        "description": "what makes this moment valuable",
                        "technical_highlight": "specific technical point",
                        "shorts_viability": 1-10
                    }}],
                    "best_short_format": "before_after/step_by_step/quick_tip/problem_solution",
                    "technical_hook": "compelling technical angle for short content"
                }}
            }}
            """
            
            logger.info("Sending frame to Gemini for technical analysis")
            response = self.flash_model.generate_content([technical_prompt, image])
            
            # Parse JSON response
            try:
                response_text = response.text.strip()
                if response_text.startswith('```json'):
                    response_text = response_text[7:-3]
                elif response_text.startswith('```'):
                    response_text = response_text[3:-3]
                
                analysis = json.loads(response_text)
                analysis['success'] = True
                analysis['analysis_type'] = 'technical_frame_analysis'
                analysis['model_used'] = 'flash'
                
                logger.info("Technical frame analysis completed successfully")
                
            except json.JSONDecodeError as e:
                logger.error(f"JSON parsing failed: {e}")
                # Fallback analysis
                analysis = {
                    'success': False,
                    'raw_response': response.text,
                    'analysis_type': 'technical_frame_analysis',
                    'model_used': 'flash',
                    'error': 'JSON parsing failed'
                }
            
            return analysis
            
        except Exception as e:
            logger.error(f"Error in technical frame analysis: {str(e)}", exc_info=True)
            return {
                'success': False,
                'error': str(e),
                'analysis_type': 'technical_frame_analysis',
                'model_used': 'flash'
            }
    
    def create_developer_shorts_strategy(self, frame_analyses: List[Dict], transcript: str, video_metadata: Dict) -> Dict:
        """
        Create strategic plan for technical shorts targeting developers
        
        Args:
            frame_analyses: All frame analyses
            transcript: Full video transcript
            video_metadata: Video information and metadata
            
        Returns:
            Strategic plan for developer-focused shorts creation
        """
        logger.info("Creating developer shorts strategy")
        try:
            # Analyze transcript for technical content
            transcript_analysis = self.analyze_transcript_for_technical_content(transcript)
            
            strategy_prompt = f"""
            You are a senior developer advocate and technical content strategist creating 
            a comprehensive shorts strategy for technical content targeting software developers.
            
            VIDEO METADATA:
            {json.dumps(video_metadata, indent=2)}
            
            TRANSCRIPT ANALYSIS:
            {json.dumps(transcript_analysis, indent=2)}
            
            FRAME ANALYSES SUMMARY:
            {json.dumps([a for a in frame_analyses if a.get('success')], indent=2)}
            
            Create a strategic plan for developer-focused shorts:
            
            STRATEGIC ANALYSIS:
            
            1. TECHNICAL CONTENT AUDIT:
            - What are the key technical concepts demonstrated?
            - Which programming languages, tools, or frameworks are featured?
            - What problem-solving approaches are shown?
            - Which segments have the highest educational value?
            
            2. DEVELOPER AUDIENCE TARGETING:
            - What skill levels are most appropriate for this content?
            - Which developer communities would find this most valuable?
            - What pain points or interests does this address?
            - How does this fit current developer trends and needs?
            
            3. SHORTS OPTIMIZATION STRATEGY:
            - Which segments are best suited for 60-second technical shorts?
            - How can complex technical concepts be condensed effectively?
            - What visual techniques will maximize code readability?
            - Which platforms are optimal for technical developer content?
            
            4. TECHNICAL ACCURACY AND BEST PRACTICES:
            - How can technical accuracy be maintained in short format?
            - What additional context or resources should be provided?
            - How can best practices be highlighted effectively?
            - What disclaimers or clarifications are needed?
            
            Provide comprehensive strategy as JSON:
            
            {{
                "executive_summary": {{
                    "primary_technical_focus": "main technical theme",
                    "target_developer_personas": ["persona1", "persona2"],
                    "key_value_propositions": ["value1", "value2"],
                    "expected_engagement_drivers": ["driver1", "driver2"]
                }},
                "shorts_opportunities": [{{
                    "segment_description": "what this short would cover",
                    "technical_hook": "compelling technical angle",
                    "duration_estimate": "30s/45s/60s",
                    "complexity_level": "beginner/intermediate/advanced",
                    "key_technical_points": ["point1", "point2"],
                    "visual_requirements": ["requirement1", "requirement2"],
                    "platform_optimization": {{
                        "youtube_shorts": "optimization strategy",
                        "linkedin": "professional network approach",
                        "twitter": "developer community engagement",
                        "tiktok_tech": "if applicable to platform"
                    }},
                    "call_to_action": "specific developer-focused CTA",
                    "follow_up_content": "related content opportunities"
                }}],
                "technical_accuracy_guidelines": {{
                    "fact_checking_requirements": ["requirement1", "requirement2"],
                    "code_review_needs": ["need1", "need2"],
                    "documentation_links": ["link_type1", "link_type2"],
                    "disclaimer_requirements": ["disclaimer1", "disclaimer2"]
                }},
                "platform_strategy": {{
                    "primary_platforms": [{{
                        "platform": "platform_name",
                        "content_adaptations": ["adaptation1", "adaptation2"],
                        "engagement_tactics": ["tactic1", "tactic2"],
                        "technical_considerations": ["consideration1", "consideration2"]
                    }}],
                    "content_distribution": {{
                        "posting_schedule": "optimal timing for developers",
                        "cross_platform_synergies": ["synergy1", "synergy2"],
                        "community_engagement": ["strategy1", "strategy2"]
                    }}
                }},
                "success_metrics": {{
                    "engagement_targets": {{
                        "views": "realistic target",
                        "shares": "developer sharing behavior",
                        "comments": "technical discussion generation",
                        "saves": "reference value for developers"
                    }},
                    "educational_impact": {{
                        "learning_outcomes": ["outcome1", "outcome2"],
                        "skill_development": ["skill1", "skill2"],
                        "community_value": "how this benefits dev community"
                    }}
                }},
                "production_requirements": {{
                    "editing_focus": ["focus1", "focus2"],
                    "technical_review_process": "quality assurance steps",
                    "resource_needs": ["need1", "need2"],
                    "timeline_recommendations": "production schedule"
                }},
                "long_term_strategy": {{
                    "series_potential": "episodic content opportunities",
                    "community_building": "how to build developer following",
                    "expertise_positioning": "thought leadership opportunities",
                    "monetization_approaches": ["approach1", "approach2"]
                }}
            }}
            """
            
            logger.info("Sending strategy request to Gemini Pro")
            response = self.pro_model.generate_content(strategy_prompt)
            
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
                strategy['model_used'] = 'pro'
                
                logger.info("Developer shorts strategy created successfully")
                
            except json.JSONDecodeError as e:
                logger.error(f"JSON parsing failed for strategy: {e}")
                strategy = {
                    'success': False,
                    'raw_response': response.text,
                    'analysis_type': 'developer_shorts_strategy',
                    'model_used': 'pro',
                    'error': 'JSON parsing failed'
                }
            
            return strategy
            
        except Exception as e:
            logger.error(f"Error in developer shorts strategy creation: {str(e)}", exc_info=True)
            return {
                'success': False,
                'error': str(e),
                'analysis_type': 'developer_shorts_strategy',
                'model_used': 'pro'
            }
    
    def analyze_transcript_for_technical_content(self, transcript: str) -> Dict:
        """
        Analyze audio transcript for technical content and concepts
        
        Args:
            transcript: Full video transcript
            
        Returns:
            Analysis of technical content in transcript
        """
        logger.info("Analyzing transcript for technical content")
        try:
            if not transcript or len(transcript.strip()) == 0:
                return {'success': False, 'error': 'No transcript provided'}
            
            transcript_prompt = f"""
            Analyze this video transcript for technical content targeting software developers:
            
            TRANSCRIPT:
            {transcript}
            
            Extract and analyze:
            
            1. TECHNICAL CONCEPTS MENTIONED:
            - Programming languages, frameworks, tools
            - Technical methodologies and practices
            - Software engineering concepts
            - Development processes and workflows
            
            2. EDUCATIONAL CONTENT STRUCTURE:
            - How are concepts explained?
            - What learning progression is followed?
            - Which parts are most valuable for developers?
            - What skill levels are addressed?
            
            3. PRACTICAL APPLICATIONS:
            - Real-world examples or use cases
            - Problem-solving approaches demonstrated
            - Code examples or technical implementations
            - Best practices or recommendations shared
            
            Provide analysis as JSON:
            {{
                "technical_keywords": ["keyword1", "keyword2"],
                "programming_languages": ["lang1", "lang2"],
                "frameworks_tools": ["tool1", "tool2"],
                "concepts_explained": [{{
                    "concept": "concept name",
                    "explanation_quality": 1-10,
                    "complexity_level": "beginner/intermediate/advanced",
                    "practical_examples": "yes/no"
                }}],
                "educational_structure": {{
                    "learning_progression": "description",
                    "explanation_clarity": 1-10,
                    "practical_focus": 1-10,
                    "engagement_level": 1-10
                }},
                "developer_value": {{
                    "skill_development": ["skill1", "skill2"],
                    "practical_applicability": 1-10,
                    "industry_relevance": 1-10,
                    "unique_insights": ["insight1", "insight2"]
                }},
                "content_segments": [{{
                    "start_indicator": "approximate start phrase",
                    "topic": "segment topic",
                    "technical_depth": 1-10,
                    "shorts_potential": 1-10,
                    "key_takeaway": "main learning point"
                }}]
            }}
            """
            
            response = self.flash_model.generate_content(transcript_prompt)
            
            try:
                response_text = response.text.strip()
                if response_text.startswith('```json'):
                    response_text = response_text[7:-3]
                elif response_text.startswith('```'):
                    response_text = response_text[3:-3]
                
                analysis = json.loads(response_text)
                analysis['success'] = True
                analysis['transcript_length'] = len(transcript)
                analysis['word_count'] = len(transcript.split())
                
                logger.info("Transcript analysis completed successfully")
                
            except json.JSONDecodeError as e:
                logger.error(f"JSON parsing failed for transcript: {e}")
                analysis = {
                    'success': False,
                    'raw_response': response.text,
                    'error': 'JSON parsing failed',
                    'transcript_length': len(transcript),
                    'word_count': len(transcript.split())
                }
            
            return analysis
            
        except Exception as e:
            logger.error(f"Error in transcript analysis: {str(e)}", exc_info=True)
            return {
                'success': False,
                'error': str(e),
                'transcript_length': len(transcript) if transcript else 0
            }
