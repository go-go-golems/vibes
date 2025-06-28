import google.generativeai as genai
import os
import base64
from typing import List, Dict, Tuple, Optional
import json
import time
from PIL import Image
import io

class AgenticGeminiAnalyzer:
    """
    Advanced agentic Gemini API integration with chain-of-thought reasoning,
    self-reflection, and sophisticated viral content analysis
    """
    
    def __init__(self, api_key: str):
        """
        Initialize Gemini API client with advanced models
        
        Args:
            api_key: Google Gemini API key
        """
        genai.configure(api_key=api_key)
        
        # Use Gemini Flash for most operations (fast and cost-effective)
        self.flash_model = genai.GenerativeModel('gemini-1.5-flash')
        
        # Use Gemini Pro for complex analysis and planning
        self.pro_model = genai.GenerativeModel('gemini-1.5-pro')
        
        # Viral content factors based on research
        self.viral_factors = {
            "emotional_triggers": ["surprise", "humor", "inspiration", "controversy", "nostalgia", "fear", "anger", "joy"],
            "engagement_signals": ["clear_hook", "visual_appeal", "relatability", "shareability", "comment_bait"],
            "technical_quality": ["good_lighting", "clear_audio", "stable_footage", "proper_framing"],
            "content_structure": ["strong_opening", "clear_narrative", "satisfying_conclusion", "call_to_action"],
            "platform_optimization": ["vertical_format", "captions", "trending_sounds", "hashtag_potential"]
        }
    
    def analyze_frame_with_cot(self, image_path: str, context: Dict = None) -> Dict:
        """
        Analyze frame using chain-of-thought reasoning
        
        Args:
            image_path: Path to the frame image
            context: Additional context about the video/segment
            
        Returns:
            Dictionary with detailed analysis
        """
        try:
            image = Image.open(image_path)
            
            # Chain-of-thought prompt with step-by-step reasoning
            cot_prompt = f"""
            You are an expert video content analyst specializing in social media viral content detection.
            
            CONTEXT: {json.dumps(context) if context else "No additional context provided"}
            
            Analyze this video frame using the following chain-of-thought process:
            
            STEP 1 - INITIAL OBSERVATION:
            First, describe what you see in the frame in detail. Look at:
            - Visual composition and framing
            - People, objects, and their positioning
            - Colors, lighting, and visual quality
            - Any text or graphics visible
            - Overall scene setting and mood
            
            STEP 2 - EMOTIONAL ANALYSIS:
            Analyze the emotional content:
            - What emotions are being expressed by people in the frame?
            - What emotional response might this evoke in viewers?
            - Rate emotional intensity (1-10)
            - Identify primary emotional triggers: {', '.join(self.viral_factors['emotional_triggers'])}
            
            STEP 3 - ENGAGEMENT POTENTIAL:
            Evaluate engagement factors:
            - Visual appeal and "scroll-stopping" power
            - Relatability to target audiences
            - Potential for comments/discussions
            - Shareability factors
            - Hook strength for opening a video
            
            STEP 4 - TECHNICAL ASSESSMENT:
            Assess technical quality:
            - Image clarity and resolution
            - Lighting quality
            - Composition and framing
            - Professional vs amateur feel
            - Mobile-friendly viewing
            
            STEP 5 - VIRAL POTENTIAL REASONING:
            Based on steps 1-4, reason through:
            - Why this frame would/wouldn't perform well on social media
            - Which platforms it's best suited for (TikTok, Instagram, YouTube Shorts, Twitter)
            - What type of audience would engage with this content
            - Specific viral characteristics present
            
            STEP 6 - SELF-REFLECTION:
            Critically evaluate your analysis:
            - Are there any biases in your assessment?
            - What might you have missed?
            - How confident are you in each rating (1-10)?
            - What additional context would improve this analysis?
            
            OUTPUT FORMAT:
            Provide your response as a JSON object with this structure:
            {{
                "step1_observation": {{
                    "visual_description": "detailed description",
                    "key_elements": ["list", "of", "elements"],
                    "scene_type": "indoor/outdoor/studio/etc",
                    "people_count": number,
                    "text_detected": "any visible text"
                }},
                "step2_emotional": {{
                    "primary_emotions": ["list", "of", "emotions"],
                    "emotional_intensity": 1-10,
                    "viewer_response": "predicted emotional response",
                    "emotional_triggers": ["applicable", "triggers"]
                }},
                "step3_engagement": {{
                    "visual_appeal": 1-10,
                    "scroll_stopping_power": 1-10,
                    "relatability": 1-10,
                    "shareability": 1-10,
                    "hook_strength": 1-10,
                    "comment_potential": 1-10
                }},
                "step4_technical": {{
                    "image_quality": 1-10,
                    "lighting_quality": 1-10,
                    "composition": 1-10,
                    "mobile_friendly": 1-10,
                    "professional_level": 1-10
                }},
                "step5_viral_reasoning": {{
                    "viral_potential": 1-10,
                    "best_platforms": ["platform1", "platform2"],
                    "target_audience": "description",
                    "viral_characteristics": ["list", "of", "characteristics"],
                    "performance_prediction": "detailed reasoning"
                }},
                "step6_reflection": {{
                    "confidence_level": 1-10,
                    "potential_biases": ["list", "of", "biases"],
                    "missing_context": ["what", "would", "help"],
                    "analysis_limitations": "description"
                }},
                "final_scores": {{
                    "overall_viral_potential": 1-10,
                    "social_media_appeal": 1-10,
                    "engagement_prediction": 1-10
                }},
                "actionable_insights": {{
                    "suggested_caption": "engaging caption",
                    "hashtag_suggestions": ["#hashtag1", "#hashtag2"],
                    "optimization_tips": ["tip1", "tip2"],
                    "best_use_case": "how to use this frame"
                }}
            }}
            """
            
            response = self.flash_model.generate_content([cot_prompt, image])
            
            # Parse JSON response
            try:
                response_text = response.text.strip()
                if response_text.startswith('```json'):
                    response_text = response_text[7:-3]
                elif response_text.startswith('```'):
                    response_text = response_text[3:-3]
                
                analysis = json.loads(response_text)
                analysis['success'] = True
                analysis['analysis_type'] = 'chain_of_thought'
                analysis['model_used'] = 'flash'
                
            except json.JSONDecodeError:
                # Fallback analysis
                analysis = {
                    'success': False,
                    'raw_response': response.text,
                    'analysis_type': 'chain_of_thought',
                    'model_used': 'flash',
                    'error': 'JSON parsing failed'
                }
            
            return analysis
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e),
                'analysis_type': 'chain_of_thought',
                'model_used': 'flash'
            }
    
    def analyze_sequence_with_reasoning(self, frame_paths: List[str], segment_info: Dict = None) -> Dict:
        """
        Analyze frame sequence with multi-step reasoning
        
        Args:
            frame_paths: List of frame image paths
            segment_info: Information about the video segment
            
        Returns:
            Dictionary with comprehensive sequence analysis
        """
        try:
            # Load images (limit to 4 for token efficiency)
            images = []
            for path in frame_paths[:4]:
                images.append(Image.open(path))
            
            reasoning_prompt = f"""
            You are an expert video content strategist analyzing a sequence of frames for social media optimization.
            
            SEGMENT INFO: {json.dumps(segment_info) if segment_info else "No segment information provided"}
            
            Perform a multi-step analysis of this frame sequence:
            
            STEP 1 - SEQUENCE FLOW ANALYSIS:
            - How do the frames connect visually and narratively?
            - Is there a clear progression or story arc?
            - Are there any jarring transitions or inconsistencies?
            - Rate sequence coherence (1-10)
            
            STEP 2 - CONTENT CATEGORIZATION:
            Identify the content type and structure:
            - Content category (tutorial, entertainment, educational, promotional, etc.)
            - Narrative structure (problem-solution, before-after, step-by-step, etc.)
            - Pacing and rhythm
            - Key moments or highlights
            
            STEP 3 - AUDIENCE ENGAGEMENT PREDICTION:
            - Which demographics would find this most engaging?
            - What keeps viewers watching through the sequence?
            - Where might viewers drop off?
            - Predicted watch time completion rate
            
            STEP 4 - PLATFORM OPTIMIZATION ANALYSIS:
            For each major platform, analyze fit:
            - TikTok: Trend potential, music sync opportunities, hashtag compatibility
            - Instagram Reels: Visual aesthetics, story potential, brand safety
            - YouTube Shorts: Educational value, searchability, retention factors
            - Twitter/X: News value, discussion potential, shareability
            
            STEP 5 - VIRAL MECHANICS IDENTIFICATION:
            Identify specific viral mechanics present:
            - Hook effectiveness (first 3 seconds)
            - Emotional journey throughout sequence
            - Surprise or twist elements
            - Call-to-action or engagement drivers
            - Memetic potential
            
            STEP 6 - STRATEGIC RECOMMENDATIONS:
            Based on analysis, provide strategic advice:
            - Best platform for this content
            - Optimal posting time and strategy
            - Content modifications for better performance
            - Series or follow-up content opportunities
            
            STEP 7 - COMPETITIVE ANALYSIS:
            - How does this compare to trending content in similar categories?
            - What unique value does this sequence provide?
            - Competitive advantages and disadvantages
            
            OUTPUT as JSON:
            {{
                "sequence_analysis": {{
                    "coherence_score": 1-10,
                    "narrative_flow": "description",
                    "transition_quality": 1-10,
                    "pacing_assessment": "fast/medium/slow with reasoning"
                }},
                "content_classification": {{
                    "primary_category": "category",
                    "secondary_categories": ["list"],
                    "narrative_structure": "structure type",
                    "key_moments": [{{
                        "timestamp": "relative position",
                        "description": "what happens",
                        "importance": 1-10
                    }}]
                }},
                "audience_prediction": {{
                    "primary_demographics": ["demo1", "demo2"],
                    "engagement_drivers": ["driver1", "driver2"],
                    "drop_off_risks": ["risk1", "risk2"],
                    "completion_rate_prediction": 1-100
                }},
                "platform_optimization": {{
                    "tiktok": {{
                        "fit_score": 1-10,
                        "optimization_notes": "specific advice",
                        "trend_potential": 1-10
                    }},
                    "instagram": {{
                        "fit_score": 1-10,
                        "optimization_notes": "specific advice",
                        "aesthetic_appeal": 1-10
                    }},
                    "youtube_shorts": {{
                        "fit_score": 1-10,
                        "optimization_notes": "specific advice",
                        "educational_value": 1-10
                    }},
                    "twitter": {{
                        "fit_score": 1-10,
                        "optimization_notes": "specific advice",
                        "discussion_potential": 1-10
                    }}
                }},
                "viral_mechanics": {{
                    "hook_effectiveness": 1-10,
                    "emotional_journey": "description",
                    "surprise_elements": ["element1", "element2"],
                    "engagement_drivers": ["driver1", "driver2"],
                    "memetic_potential": 1-10
                }},
                "strategic_recommendations": {{
                    "best_platform": "platform name",
                    "posting_strategy": "detailed strategy",
                    "content_modifications": ["mod1", "mod2"],
                    "series_potential": "description",
                    "follow_up_opportunities": ["opp1", "opp2"]
                }},
                "competitive_analysis": {{
                    "uniqueness_score": 1-10,
                    "competitive_advantages": ["adv1", "adv2"],
                    "market_positioning": "description",
                    "differentiation_factors": ["factor1", "factor2"]
                }},
                "overall_assessment": {{
                    "viral_potential": 1-10,
                    "commercial_viability": 1-10,
                    "content_quality": 1-10,
                    "strategic_value": 1-10
                }}
            }}
            """
            
            content = [reasoning_prompt] + images
            response = self.pro_model.generate_content(content)
            
            # Parse response
            try:
                response_text = response.text.strip()
                if response_text.startswith('```json'):
                    response_text = response_text[7:-3]
                elif response_text.startswith('```'):
                    response_text = response_text[3:-3]
                
                analysis = json.loads(response_text)
                analysis['success'] = True
                analysis['analysis_type'] = 'multi_step_reasoning'
                analysis['model_used'] = 'pro'
                analysis['frames_analyzed'] = len(frame_paths)
                
            except json.JSONDecodeError:
                analysis = {
                    'success': False,
                    'raw_response': response.text,
                    'analysis_type': 'multi_step_reasoning',
                    'model_used': 'pro',
                    'frames_analyzed': len(frame_paths),
                    'error': 'JSON parsing failed'
                }
            
            return analysis
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e),
                'analysis_type': 'multi_step_reasoning',
                'model_used': 'pro',
                'frames_analyzed': len(frame_paths)
            }
    
    def create_strategic_video_plan(self, all_analyses: List[Dict], video_metadata: Dict) -> Dict:
        """
        Create comprehensive strategic plan using Gemini Pro with advanced reasoning
        
        Args:
            all_analyses: All frame and sequence analyses
            video_metadata: Video information and metadata
            
        Returns:
            Strategic plan for content optimization
        """
        try:
            # Prepare comprehensive data summary
            analysis_summary = {
                'total_segments': len(all_analyses),
                'high_potential_segments': [],
                'content_themes': [],
                'technical_quality_avg': 0,
                'engagement_potential_avg': 0
            }
            
            for i, analysis in enumerate(all_analyses):
                if analysis.get('success') and 'overall_assessment' in analysis:
                    viral_score = analysis['overall_assessment'].get('viral_potential', 0)
                    if viral_score >= 7:
                        analysis_summary['high_potential_segments'].append({
                            'index': i,
                            'viral_score': viral_score,
                            'best_platform': analysis.get('strategic_recommendations', {}).get('best_platform', 'unknown')
                        })
            
            strategic_prompt = f"""
            You are a senior social media strategist and content director tasked with creating a comprehensive 
            content strategy for a video with the following characteristics:
            
            VIDEO METADATA:
            {json.dumps(video_metadata, indent=2)}
            
            ANALYSIS SUMMARY:
            {json.dumps(analysis_summary, indent=2)}
            
            DETAILED ANALYSES:
            {json.dumps(all_analyses[:3], indent=2)}  # Include first 3 for context
            
            Create a strategic plan using this reasoning process:
            
            STRATEGIC REASONING PHASE 1 - CONTENT AUDIT:
            - What are the strongest content elements across all segments?
            - Which segments have the highest commercial and viral potential?
            - What are the recurring themes and messaging opportunities?
            - How does the content align with current social media trends?
            
            STRATEGIC REASONING PHASE 2 - AUDIENCE STRATEGY:
            - Who is the primary target audience for this content?
            - What are their content consumption patterns and preferences?
            - Which platforms do they use most actively?
            - What type of content do they engage with and share?
            
            STRATEGIC REASONING PHASE 3 - COMPETITIVE POSITIONING:
            - How does this content differentiate from competitors?
            - What unique value propositions can be highlighted?
            - Which content gaps in the market can this fill?
            - What trending topics or formats can this leverage?
            
            STRATEGIC REASONING PHASE 4 - PLATFORM STRATEGY:
            - Which platforms offer the best ROI for this content type?
            - How should content be adapted for each platform's algorithm?
            - What posting schedule and frequency would be optimal?
            - Which platform-specific features should be utilized?
            
            STRATEGIC REASONING PHASE 5 - CONTENT OPTIMIZATION:
            - How can each segment be enhanced for maximum impact?
            - What editing techniques would improve engagement?
            - Which segments should be combined or split?
            - What additional content elements are needed?
            
            STRATEGIC REASONING PHASE 6 - PERFORMANCE PREDICTION:
            - What are realistic performance expectations for each segment?
            - Which metrics should be tracked for success measurement?
            - What are potential risks and mitigation strategies?
            - How can performance be optimized post-launch?
            
            STRATEGIC REASONING PHASE 7 - LONG-TERM STRATEGY:
            - How does this content fit into a broader content strategy?
            - What follow-up content opportunities exist?
            - How can this content be repurposed across different formats?
            - What learnings can be applied to future content creation?
            
            Provide your strategic plan as a comprehensive JSON response:
            
            {{
                "executive_summary": {{
                    "key_opportunities": ["opportunity1", "opportunity2"],
                    "primary_recommendation": "main strategic direction",
                    "expected_outcomes": ["outcome1", "outcome2"],
                    "investment_priority": "high/medium/low"
                }},
                "content_strategy": {{
                    "priority_segments": [{{
                        "segment_index": number,
                        "priority_level": "high/medium/low",
                        "rationale": "why this segment is prioritized",
                        "optimization_plan": "specific improvements needed"
                    }}],
                    "content_themes": ["theme1", "theme2"],
                    "messaging_strategy": "core messaging approach",
                    "brand_positioning": "how to position the content"
                }},
                "platform_strategy": {{
                    "primary_platform": {{
                        "platform": "platform name",
                        "rationale": "why this platform is primary",
                        "content_adaptations": ["adaptation1", "adaptation2"],
                        "posting_schedule": "optimal timing strategy"
                    }},
                    "secondary_platforms": [{{
                        "platform": "platform name",
                        "content_format": "how to adapt content",
                        "expected_performance": "performance prediction"
                    }}],
                    "cross_platform_synergies": ["synergy1", "synergy2"]
                }},
                "optimization_roadmap": {{
                    "immediate_actions": [{{
                        "action": "specific action",
                        "timeline": "when to complete",
                        "expected_impact": "predicted result"
                    }}],
                    "short_term_goals": ["goal1", "goal2"],
                    "long_term_objectives": ["objective1", "objective2"]
                }},
                "performance_framework": {{
                    "success_metrics": [{{
                        "metric": "metric name",
                        "target": "specific target",
                        "measurement_method": "how to measure"
                    }}],
                    "risk_assessment": [{{
                        "risk": "potential risk",
                        "probability": "high/medium/low",
                        "mitigation": "how to mitigate"
                    }}],
                    "optimization_triggers": ["when to optimize"]
                }},
                "resource_requirements": {{
                    "editing_needs": ["need1", "need2"],
                    "additional_content": ["content1", "content2"],
                    "team_requirements": ["requirement1", "requirement2"],
                    "budget_considerations": ["consideration1", "consideration2"]
                }},
                "future_opportunities": {{
                    "content_series_potential": "description",
                    "repurposing_opportunities": ["opp1", "opp2"],
                    "collaboration_possibilities": ["collab1", "collab2"],
                    "monetization_strategies": ["strategy1", "strategy2"]
                }}
            }}
            """
            
            response = self.pro_model.generate_content(strategic_prompt)
            
            # Parse strategic plan
            try:
                response_text = response.text.strip()
                if response_text.startswith('```json'):
                    response_text = response_text[7:-3]
                elif response_text.startswith('```'):
                    response_text = response_text[3:-3]
                
                plan = json.loads(response_text)
                plan['success'] = True
                plan['analysis_type'] = 'strategic_planning'
                plan['model_used'] = 'pro'
                plan['segments_analyzed'] = len(all_analyses)
                
            except json.JSONDecodeError:
                plan = {
                    'success': False,
                    'raw_response': response.text,
                    'analysis_type': 'strategic_planning',
                    'model_used': 'pro',
                    'segments_analyzed': len(all_analyses),
                    'error': 'JSON parsing failed'
                }
            
            return plan
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e),
                'analysis_type': 'strategic_planning',
                'model_used': 'pro',
                'segments_analyzed': len(all_analyses)
            }
    
    def self_evaluate_analysis(self, analysis_results: Dict) -> Dict:
        """
        Self-evaluate the quality and reliability of analysis results
        
        Args:
            analysis_results: Results from previous analysis
            
        Returns:
            Self-evaluation and confidence metrics
        """
        try:
            evaluation_prompt = f"""
            You are a quality assurance expert reviewing AI-generated content analysis.
            
            ANALYSIS TO EVALUATE:
            {json.dumps(analysis_results, indent=2)}
            
            Perform a critical self-evaluation:
            
            1. ACCURACY ASSESSMENT:
            - How accurate do the scores and ratings appear?
            - Are there any obvious inconsistencies or contradictions?
            - Do the recommendations align with the analysis?
            
            2. COMPLETENESS CHECK:
            - What important aspects might have been missed?
            - Are there gaps in the analysis?
            - What additional context would improve accuracy?
            
            3. BIAS DETECTION:
            - What potential biases might be present in this analysis?
            - Are there cultural or demographic assumptions?
            - How might different perspectives change the conclusions?
            
            4. CONFIDENCE CALIBRATION:
            - How confident should we be in each major conclusion?
            - Which aspects of the analysis are most/least reliable?
            - What would increase confidence in the results?
            
            Provide evaluation as JSON:
            {{
                "overall_quality": 1-10,
                "accuracy_confidence": 1-10,
                "completeness_score": 1-10,
                "bias_risk_level": "low/medium/high",
                "reliability_assessment": {{
                    "most_reliable_aspects": ["aspect1", "aspect2"],
                    "least_reliable_aspects": ["aspect1", "aspect2"],
                    "confidence_by_category": {{
                        "technical_analysis": 1-10,
                        "engagement_prediction": 1-10,
                        "viral_potential": 1-10,
                        "strategic_recommendations": 1-10
                    }}
                }},
                "improvement_suggestions": ["suggestion1", "suggestion2"],
                "validation_needs": ["what needs human validation"],
                "risk_factors": ["potential risks in following recommendations"]
            }}
            """
            
            response = self.flash_model.generate_content(evaluation_prompt)
            
            try:
                response_text = response.text.strip()
                if response_text.startswith('```json'):
                    response_text = response_text[7:-3]
                elif response_text.startswith('```'):
                    response_text = response_text[3:-3]
                
                evaluation = json.loads(response_text)
                evaluation['success'] = True
                evaluation['evaluation_type'] = 'self_assessment'
                
            except json.JSONDecodeError:
                evaluation = {
                    'success': False,
                    'raw_response': response.text,
                    'evaluation_type': 'self_assessment',
                    'error': 'JSON parsing failed'
                }
            
            return evaluation
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e),
                'evaluation_type': 'self_assessment'
            }

