import google.generativeai as genai
import os
import base64
from typing import List, Dict, Tuple, Optional
import json
import time
from PIL import Image
import io

class GeminiVideoAnalyzer:
    """
    Gemini API integration for video analysis and content understanding
    """
    
    def __init__(self, api_key: str):
        """
        Initialize Gemini API client
        
        Args:
            api_key: Google Gemini API key
        """
        genai.configure(api_key=api_key)
        
        # Use Gemini Flash for most operations (fast and cost-effective)
        self.flash_model = genai.GenerativeModel('gemini-1.5-flash')
        
        # Use Gemini Pro for complex analysis and planning
        self.pro_model = genai.GenerativeModel('gemini-1.5-pro')
        
    def encode_image(self, image_path: str) -> str:
        """
        Encode image to base64 for API
        
        Args:
            image_path: Path to image file
            
        Returns:
            Base64 encoded image string
        """
        with open(image_path, "rb") as image_file:
            return base64.b64encode(image_file.read()).decode('utf-8')
    
    def analyze_frame_content(self, image_path: str, use_flash: bool = True) -> Dict:
        """
        Analyze a single frame for content and visual interest
        
        Args:
            image_path: Path to the frame image
            use_flash: Whether to use Gemini Flash (faster) or Pro (more detailed)
            
        Returns:
            Dictionary with analysis results
        """
        try:
            # Load and prepare image
            image = Image.open(image_path)
            
            model = self.flash_model if use_flash else self.pro_model
            
            prompt = """
            Analyze this video frame and provide a JSON response with the following information:
            
            {
                "visual_interest_score": <1-10 rating of how visually interesting this frame is>,
                "content_description": "<brief description of what's happening in the frame>",
                "key_elements": ["<list of key visual elements>"],
                "text_detected": "<any text visible in the frame>",
                "people_count": <number of people visible>,
                "scene_type": "<indoor/outdoor/studio/etc>",
                "emotional_tone": "<happy/serious/energetic/calm/etc>",
                "social_media_potential": <1-10 rating for social media appeal>,
                "suggested_caption": "<short engaging caption for social media>"
            }
            
            Focus on identifying frames that would work well for social media clips - look for expressive faces, interesting visuals, clear text, or dynamic moments.
            """
            
            response = model.generate_content([prompt, image])
            
            # Try to parse JSON response
            try:
                # Extract JSON from response text
                response_text = response.text.strip()
                if response_text.startswith('```json'):
                    response_text = response_text[7:-3]
                elif response_text.startswith('```'):
                    response_text = response_text[3:-3]
                
                analysis = json.loads(response_text)
                analysis['success'] = True
                analysis['model_used'] = 'flash' if use_flash else 'pro'
                
            except json.JSONDecodeError:
                # Fallback if JSON parsing fails
                analysis = {
                    'success': False,
                    'raw_response': response.text,
                    'visual_interest_score': 5,
                    'content_description': 'Analysis failed - JSON parsing error',
                    'social_media_potential': 5,
                    'model_used': 'flash' if use_flash else 'pro'
                }
            
            return analysis
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e),
                'visual_interest_score': 1,
                'content_description': f'Analysis failed: {str(e)}',
                'social_media_potential': 1,
                'model_used': 'flash' if use_flash else 'pro'
            }
    
    def analyze_frame_sequence(self, frame_paths: List[str], use_flash: bool = True) -> Dict:
        """
        Analyze a sequence of frames to understand content flow
        
        Args:
            frame_paths: List of paths to frame images in sequence
            use_flash: Whether to use Gemini Flash or Pro
            
        Returns:
            Dictionary with sequence analysis
        """
        try:
            model = self.flash_model if use_flash else self.pro_model
            
            # Load images
            images = []
            for path in frame_paths[:5]:  # Limit to 5 frames to avoid token limits
                images.append(Image.open(path))
            
            prompt = """
            Analyze this sequence of video frames and provide a JSON response:
            
            {
                "sequence_coherence": <1-10 rating of how well frames flow together>,
                "topic_consistency": <1-10 rating of topic consistency across frames>,
                "narrative_summary": "<brief summary of what happens in this sequence>",
                "key_moments": ["<list of important moments or transitions>"],
                "discussion_topics": ["<list of topics being discussed if applicable>"],
                "segment_type": "<conversation/presentation/action/transition/etc>",
                "engagement_level": <1-10 rating of how engaging this segment is>,
                "best_frame_index": <0-based index of most interesting frame>,
                "clip_potential": <1-10 rating for creating a social media clip>,
                "suggested_clip_title": "<engaging title for this segment>"
            }
            
            Focus on identifying coherent discussion segments that would make good standalone clips.
            """
            
            content = [prompt] + images
            response = model.generate_content(content)
            
            # Parse JSON response
            try:
                response_text = response.text.strip()
                if response_text.startswith('```json'):
                    response_text = response_text[7:-3]
                elif response_text.startswith('```'):
                    response_text = response_text[3:-3]
                
                analysis = json.loads(response_text)
                analysis['success'] = True
                analysis['frame_count'] = len(frame_paths)
                analysis['model_used'] = 'flash' if use_flash else 'pro'
                
            except json.JSONDecodeError:
                analysis = {
                    'success': False,
                    'raw_response': response.text,
                    'sequence_coherence': 5,
                    'clip_potential': 5,
                    'frame_count': len(frame_paths),
                    'model_used': 'flash' if use_flash else 'pro'
                }
            
            return analysis
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e),
                'sequence_coherence': 1,
                'clip_potential': 1,
                'frame_count': len(frame_paths),
                'model_used': 'flash' if use_flash else 'pro'
            }
    
    def create_video_plan(self, segment_analyses: List[Dict], video_info: Dict) -> Dict:
        """
        Create a comprehensive plan for video editing using Gemini Pro
        
        Args:
            segment_analyses: List of segment analysis results
            video_info: Video metadata
            
        Returns:
            Dictionary with video editing plan
        """
        try:
            # Prepare summary of all segments
            segments_summary = []
            for i, analysis in enumerate(segment_analyses):
                if analysis.get('success', False):
                    segments_summary.append({
                        'segment_index': i,
                        'clip_potential': analysis.get('clip_potential', 0),
                        'engagement_level': analysis.get('engagement_level', 0),
                        'narrative_summary': analysis.get('narrative_summary', ''),
                        'suggested_clip_title': analysis.get('suggested_clip_title', ''),
                        'segment_type': analysis.get('segment_type', 'unknown')
                    })
            
            prompt = f"""
            Based on the analysis of {len(segments_summary)} video segments from a {video_info.get('duration_formatted', 'unknown')} video, create a comprehensive video editing plan.
            
            Segment Analysis Summary:
            {json.dumps(segments_summary, indent=2)}
            
            Provide a JSON response with:
            
            {{
                "top_clips": [
                    {{
                        "segment_index": <index>,
                        "priority": <1-10>,
                        "clip_title": "<engaging title>",
                        "target_duration": "<suggested duration in seconds>",
                        "social_platform": "<best platform: tiktok/instagram/youtube_shorts/twitter>",
                        "editing_notes": "<specific editing suggestions>"
                    }}
                ],
                "content_themes": ["<main themes found in the video>"],
                "overall_engagement": <1-10 rating of overall video engagement>,
                "recommended_posting_strategy": "<strategy for releasing clips>",
                "hashtag_suggestions": ["<relevant hashtags>"],
                "target_audience": "<description of target audience>",
                "editing_priority_order": [<list of segment indices in priority order>]
            }}
            
            Focus on creating clips that will perform well on social media platforms. Consider factors like:
            - Attention-grabbing openings
            - Clear value or entertainment
            - Appropriate length for each platform
            - Trending topics or formats
            """
            
            response = self.pro_model.generate_content(prompt)
            
            # Parse JSON response
            try:
                response_text = response.text.strip()
                if response_text.startswith('```json'):
                    response_text = response_text[7:-3]
                elif response_text.startswith('```'):
                    response_text = response_text[3:-3]
                
                plan = json.loads(response_text)
                plan['success'] = True
                plan['total_segments_analyzed'] = len(segment_analyses)
                plan['model_used'] = 'pro'
                
            except json.JSONDecodeError:
                plan = {
                    'success': False,
                    'raw_response': response.text,
                    'total_segments_analyzed': len(segment_analyses),
                    'model_used': 'pro'
                }
            
            return plan
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e),
                'total_segments_analyzed': len(segment_analyses),
                'model_used': 'pro'
            }
    
    def find_best_frames(self, frame_analyses: List[Dict], top_n: int = 5) -> List[Dict]:
        """
        Find the best frames for social media based on analysis scores
        
        Args:
            frame_analyses: List of frame analysis results
            top_n: Number of top frames to return
            
        Returns:
            List of best frames with their analysis data
        """
        # Sort frames by combined score of visual interest and social media potential
        scored_frames = []
        
        for i, analysis in enumerate(frame_analyses):
            if analysis.get('success', False):
                visual_score = analysis.get('visual_interest_score', 0)
                social_score = analysis.get('social_media_potential', 0)
                combined_score = (visual_score + social_score) / 2
                
                scored_frames.append({
                    'frame_index': i,
                    'combined_score': combined_score,
                    'analysis': analysis
                })
        
        # Sort by combined score and return top N
        scored_frames.sort(key=lambda x: x['combined_score'], reverse=True)
        return scored_frames[:top_n]

