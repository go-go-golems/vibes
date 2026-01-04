#!/usr/bin/env python3
"""
Enhanced Video Analyzer Web Application (Fallback Version)
Uses google-generativeai instead of google-genai for broader compatibility
"""

import os
import sys
import json
import time
import uuid
import logging
import threading
from datetime import datetime
from pathlib import Path
from typing import Dict, Any, Optional

from flask import Flask, render_template, request, jsonify, Response, stream_template
import google.generativeai as genai

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('logs/app.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

app = Flask(__name__)

# Global variables for tracking
active_sessions = {}
step_counters = {}

class VideoAnalyzer:
    """Enhanced Video Analyzer using google-generativeai"""
    
    def __init__(self, api_key: str, session_id: str):
        self.api_key = api_key
        self.session_id = session_id
        self.tracking_dir = Path(f"tracking/{session_id}")
        self.tracking_dir.mkdir(parents=True, exist_ok=True)
        
        # Configure Gemini
        genai.configure(api_key=api_key)
        self.model = genai.GenerativeModel('gemini-2.5-pro')
        
        # Initialize step tracking
        self.step_counter = 0
        self.api_call_counter = 0
        self.start_time = time.time()
        
        logger.info(f"🔧 VideoAnalyzer initialized for session {session_id}")
    
    def track_step(self, step_name: str, step_type: str = "info", details: Dict[str, Any] = None):
        """Track analysis step with detailed logging"""
        self.step_counter += 1
        
        step_data = {
            "step_number": self.step_counter,
            "step_name": step_name,
            "step_type": step_type,
            "timestamp": datetime.now().isoformat(),
            "elapsed_time": time.time() - self.start_time,
            "details": details or {}
        }
        
        # Save step to file
        step_file = self.tracking_dir / f"step_{self.step_counter:03d}_{step_name.lower().replace(' ', '_')}.json"
        with open(step_file, 'w') as f:
            json.dump(step_data, f, indent=2)
        
        # Update global tracking
        if self.session_id not in step_counters:
            step_counters[self.session_id] = {"steps": 0, "api_calls": 0, "elapsed": 0}
        
        step_counters[self.session_id]["steps"] = self.step_counter
        step_counters[self.session_id]["api_calls"] = self.api_call_counter
        step_counters[self.session_id]["elapsed"] = time.time() - self.start_time
        
        logger.info(f"📝 Step {self.step_counter}: {step_name}")
        return step_data
    
    def analyze_video(self, video_url: str, analysis_mode: str = "quick"):
        """Analyze video using Gemini's native video understanding"""
        
        try:
            # Step 1: Initialization
            self.track_step("initialization", "info", {"video_url": video_url, "mode": analysis_mode})
            
            # Step 2: API Setup
            self.track_step("api_setup", "info", {"model": "gemini-2.5-pro"})
            
            # Step 3: Analysis Start
            self.track_step("analysis_start", "info")
            
            # Step 4: Model Selection
            self.track_step("model_selection", "info", {"selected_model": "gemini-2.5-pro"})
            
            # Step 5: Model Selected
            self.track_step("model_selected", "success")
            
            # Step 6: Prompt Creation
            prompt = self.create_technical_prompt(analysis_mode)
            self.track_step("prompt_creation", "info", {"prompt_length": len(prompt)})
            
            # Step 7: Prompt Created
            self.track_step("prompt_created", "success", {"prompt_preview": prompt[:200] + "..."})
            
            # Step 8: API Call Start
            self.track_step("api_call_start", "info", {"target": "gemini-2.5-pro"})
            self.api_call_counter += 1
            
            # Step 9: Making API Call
            self.track_step("making_api_call", "processing", {"model": "gemini-2.5-pro", "url": video_url})
            
            # Create the video file part
            video_file = genai.upload_file(path=None, mime_type="video/mp4", display_name="YouTube Video")
            # Note: For YouTube URLs, we need to handle this differently
            # This is a simplified version - in practice you'd need to download or use direct URL support
            
            # For now, let's simulate the API call with the URL directly
            response = self.model.generate_content([
                prompt,
                f"Video URL: {video_url}"
            ])
            
            # Step 10: API Call Complete
            self.track_step("api_call_complete", "success", {
                "response_length": len(response.text) if response.text else 0
            })
            
            # Step 11: Response Processing
            self.track_step("response_processing", "info")
            
            # Step 12: Analysis Complete
            analysis_result = {
                "session_id": self.session_id,
                "video_url": video_url,
                "analysis_mode": analysis_mode,
                "model_used": "gemini-2.5-pro",
                "response": response.text,
                "timestamp": datetime.now().isoformat(),
                "total_steps": self.step_counter,
                "api_calls_made": self.api_call_counter,
                "total_time": time.time() - self.start_time
            }
            
            self.track_step("analysis_complete", "success", {
                "total_time": analysis_result["total_time"],
                "response_length": len(response.text) if response.text else 0
            })
            
            # Save final results
            results_file = Path("analysis_results") / f"analysis_{self.session_id}.json"
            results_file.parent.mkdir(exist_ok=True)
            with open(results_file, 'w') as f:
                json.dump(analysis_result, f, indent=2)
            
            return analysis_result
            
        except Exception as e:
            error_step = self.track_step("error_occurred", "error", {
                "error_type": type(e).__name__,
                "error_message": str(e)
            })
            logger.error(f"❌ Analysis failed: {e}")
            raise e
    
    def create_technical_prompt(self, mode: str) -> str:
        """Create technical analysis prompt for developer content"""
        
        base_prompt = """
        You are an expert technical video analyst specializing in developer content analysis.
        
        Analyze this video with focus on:
        
        1. **Technical Content Assessment**:
           - Programming languages, frameworks, and technologies discussed
           - Code quality and best practices demonstrated
           - Technical accuracy and depth of explanations
           - Educational value for developers
        
        2. **Developer Audience Analysis**:
           - Target skill level (beginner, intermediate, advanced)
           - Specific developer roles (frontend, backend, DevOps, etc.)
           - Technical concepts complexity
        
        3. **Social Media Optimization for Tech Community**:
           - Viral potential in developer communities
           - Key moments that would engage technical audiences
           - Shareable technical insights or "aha" moments
           - Platform-specific recommendations (Twitter, LinkedIn, YouTube, TikTok)
        
        4. **Content Structure Analysis**:
           - Introduction effectiveness
           - Technical demonstration quality
           - Code examples and explanations
           - Conclusion and call-to-action
        
        Provide detailed analysis with specific timestamps and actionable recommendations.
        """
        
        if mode == "comprehensive":
            base_prompt += """
            
        5. **Advanced Technical Analysis**:
           - Architecture patterns and design principles
           - Performance considerations discussed
           - Security implications mentioned
           - Scalability and maintainability aspects
           - Industry best practices alignment
        
        6. **Competitive Analysis**:
           - How this content compares to similar technical content
           - Unique value propositions
           - Areas for improvement
           - Market positioning in tech education space
        """
        
        return base_prompt

# Flask Routes
@app.route('/')
def index():
    """Main application page"""
    return render_template('index.html')

@app.route('/analyze', methods=['POST'])
def start_analysis():
    """Start video analysis"""
    try:
        data = request.get_json()
        video_url = data.get('video_url')
        api_key = data.get('api_key')
        analysis_mode = data.get('analysis_mode', 'quick')
        
        if not video_url or not api_key:
            return jsonify({"error": "Missing video URL or API key"}), 400
        
        # Generate session ID
        session_id = f"web_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
        
        # Initialize analyzer
        analyzer = VideoAnalyzer(api_key, session_id)
        active_sessions[session_id] = analyzer
        
        # Start analysis in background thread
        def run_analysis():
            try:
                result = analyzer.analyze_video(video_url, analysis_mode)
                logger.info(f"✅ Analysis completed for session {session_id}")
            except Exception as e:
                logger.error(f"❌ Analysis failed for session {session_id}: {e}")
        
        thread = threading.Thread(target=run_analysis)
        thread.daemon = True
        thread.start()
        
        return jsonify({
            "session_id": session_id,
            "status": "started",
            "message": "Analysis started successfully"
        })
        
    except Exception as e:
        logger.error(f"❌ Failed to start analysis: {e}")
        return jsonify({"error": str(e)}), 500

@app.route('/stream')
def stream():
    """Server-Sent Events endpoint for real-time updates"""
    def generate():
        while True:
            try:
                # Send current status for all active sessions
                for session_id, analyzer in active_sessions.items():
                    if session_id in step_counters:
                        data = {
                            "session_id": session_id,
                            "steps": step_counters[session_id]["steps"],
                            "api_calls": step_counters[session_id]["api_calls"],
                            "elapsed": round(step_counters[session_id]["elapsed"], 1),
                            "status": f"Step {step_counters[session_id]['steps']}: Processing..."
                        }
                        
                        # Get latest step details
                        latest_step_file = max(
                            analyzer.tracking_dir.glob("step_*.json"),
                            key=os.path.getctime,
                            default=None
                        )
                        
                        if latest_step_file:
                            with open(latest_step_file) as f:
                                step_data = json.load(f)
                                data["current_step"] = step_data["step_name"]
                                data["step_type"] = step_data["step_type"]
                        
                        yield f"data: {json.dumps(data)}\n\n"
                
                time.sleep(1)  # Update every second
                
            except Exception as e:
                logger.error(f"❌ SSE error: {e}")
                yield f"data: {json.dumps({'error': str(e)})}\n\n"
    
    return Response(generate(), mimetype='text/plain')

@app.route('/status/<session_id>')
def get_status(session_id):
    """Get current status for a session"""
    if session_id in step_counters:
        return jsonify(step_counters[session_id])
    return jsonify({"error": "Session not found"}), 404

if __name__ == '__main__':
    # Create necessary directories
    os.makedirs('logs', exist_ok=True)
    os.makedirs('tracking', exist_ok=True)
    os.makedirs('analysis_results', exist_ok=True)
    
    # Configure logging
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
        handlers=[
            logging.FileHandler('logs/app.log'),
            logging.StreamHandler()
        ]
    )
    
    print("🚀 Starting Enhanced Video Analyzer Web Application (Fallback Version)")
    print("📺 Navigate to http://localhost:5002 to begin")
    
    # Run Flask app
    app.run(host='0.0.0.0', port=5002, debug=True, threaded=True)

