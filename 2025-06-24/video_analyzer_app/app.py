#!/usr/bin/env python3
"""
Enhanced Video Analyzer Web Application
Flask app with real-time step tracking using Server-Sent Events
"""

import os
import sys
import json
import logging
import traceback
import time
import threading
import queue
from datetime import datetime
from typing import Dict, List, Any, Optional
from flask import Flask, render_template, request, jsonify, Response, stream_template
from werkzeug.serving import WSGIRequestHandler
import hashlib

# Import the correct SDK
import google.genai as genai
from google.genai.types import Content, Part, FileData, VideoMetadata

# Global event queue for SSE
event_queue = queue.Queue()

class WebStepTracker:
    """Enhanced step tracker for web application with SSE support"""
    
    def __init__(self, session_id: str):
        self.session_id = session_id
        self.steps = []
        self.api_calls = []
        self.start_time = datetime.now()
        
        # Create tracking directory
        self.tracking_dir = f"tracking/{session_id}"
        os.makedirs(self.tracking_dir, exist_ok=True)
        
    def log_step(self, step_type: str, description: str, data: Any = None):
        """Log an intermediate step and broadcast via SSE"""
        step = {
            "step_id": len(self.steps) + 1,
            "timestamp": datetime.now().isoformat(),
            "step_type": step_type,
            "description": description,
            "data": data,
            "elapsed_time": (datetime.now() - self.start_time).total_seconds()
        }
        self.steps.append(step)
        
        # Save step to file
        step_file = f"{self.tracking_dir}/step_{step['step_id']:03d}_{step_type}.json"
        with open(step_file, 'w', encoding='utf-8') as f:
            json.dump(step, f, indent=2, ensure_ascii=False, default=str)
        
        # Broadcast via SSE
        sse_data = {
            "type": "step",
            "session_id": self.session_id,
            "step": step
        }
        event_queue.put(sse_data)
        
        print(f"📝 STEP {step['step_id']}: {description}")
        
    def log_api_call(self, model: str, prompt: str, response: str, metadata: Dict = None):
        """Log an API call with full details and broadcast via SSE"""
        call_id = len(self.api_calls) + 1
        api_call = {
            "call_id": call_id,
            "timestamp": datetime.now().isoformat(),
            "model": model,
            "prompt": prompt,
            "prompt_hash": hashlib.md5(prompt.encode()).hexdigest(),
            "response": response,
            "response_length": len(response),
            "metadata": metadata or {},
            "elapsed_time": (datetime.now() - self.start_time).total_seconds()
        }
        self.api_calls.append(api_call)
        
        # Save API call to file
        call_file = f"{self.tracking_dir}/api_call_{call_id:03d}_{model.replace('-', '_')}.json"
        with open(call_file, 'w', encoding='utf-8') as f:
            json.dump(api_call, f, indent=2, ensure_ascii=False, default=str)
        
        # Broadcast via SSE (without full response to avoid overwhelming the UI)
        sse_data = {
            "type": "api_call",
            "session_id": self.session_id,
            "api_call": {
                "call_id": call_id,
                "model": model,
                "response_length": len(response),
                "timestamp": api_call["timestamp"],
                "elapsed_time": api_call["elapsed_time"]
            }
        }
        event_queue.put(sse_data)
        
        print(f"🔥 API CALL {call_id}: {model} -> {len(response)} chars")
        
    def save_final_summary(self):
        """Save complete tracking summary and broadcast completion"""
        summary = {
            "session_id": self.session_id,
            "start_time": self.start_time.isoformat(),
            "end_time": datetime.now().isoformat(),
            "total_duration": (datetime.now() - self.start_time).total_seconds(),
            "total_steps": len(self.steps),
            "total_api_calls": len(self.api_calls),
            "steps": self.steps,
            "api_calls": self.api_calls
        }
        
        summary_file = f"{self.tracking_dir}/complete_session_summary.json"
        with open(summary_file, 'w', encoding='utf-8') as f:
            json.dump(summary, f, indent=2, ensure_ascii=False, default=str)
        
        # Broadcast completion via SSE
        sse_data = {
            "type": "completion",
            "session_id": self.session_id,
            "summary": {
                "total_duration": summary["total_duration"],
                "total_steps": summary["total_steps"],
                "total_api_calls": summary["total_api_calls"],
                "summary_file": summary_file
            }
        }
        event_queue.put(sse_data)
        
        return summary_file

class WebVideoAnalyzer:
    """Enhanced video analyzer for web application"""
    
    def __init__(self, api_key: str):
        self.api_key = api_key
        self.client = genai.Client(api_key=api_key)
        
    def analyze_video_async(self, youtube_url: str, mode: str, session_id: str):
        """Analyze video asynchronously with web step tracking"""
        
        tracker = WebStepTracker(session_id)
        
        try:
            tracker.log_step("initialization", "Starting Enhanced Video Analysis")
            tracker.log_step("api_setup", "Configuring Gemini API Client")
            tracker.log_step("analysis_start", f"Starting video analysis", {
                "video_url": youtube_url,
                "mode": mode
            })
            
            # Choose model
            tracker.log_step("model_selection", f"Selecting model for {mode} mode")
            if mode == "quick":
                model_name = "gemini-2.5-flash"
            else:
                model_name = "gemini-2.5-pro"
            tracker.log_step("model_selected", f"Selected model: {model_name}")
            
            # Create prompt
            tracker.log_step("prompt_creation", "Creating technical analysis prompt")
            technical_prompt = self._create_technical_prompt(mode)
            tracker.log_step("prompt_created", "Technical prompt created", {
                "prompt_length": len(technical_prompt)
            })
            
            # Make API call
            tracker.log_step("api_call_start", f"Making API call to {model_name}")
            
            start_time = time.time()
            response = self.client.models.generate_content(
                model=model_name,
                contents=Content(
                    parts=[
                        Part(file_data=FileData(file_uri=youtube_url)),
                        Part(text=technical_prompt)
                    ]
                ),
                config={"temperature": 0.2}
            )
            api_duration = time.time() - start_time
            
            tracker.log_step("api_call_complete", f"API call completed in {api_duration:.2f}s")
            
            # Process response
            response_text = response.text if hasattr(response, 'text') else str(response)
            
            # Log API call
            tracker.log_api_call(
                model=model_name,
                prompt=technical_prompt,
                response=response_text,
                metadata={
                    "video_url": youtube_url,
                    "mode": mode,
                    "duration": api_duration
                }
            )
            
            # Parse response
            tracker.log_step("response_parsing", "Parsing and structuring API response")
            
            analysis_result = {
                "video_url": youtube_url,
                "analysis_mode": mode,
                "model_used": model_name,
                "analysis_time": datetime.now().isoformat(),
                "raw_analysis": response_text,
                "response_length": len(response_text),
                "session_id": session_id
            }
            
            # Save results
            tracker.log_step("results_saving", "Saving analysis results")
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            result_file = f"analysis_results/web_analysis_{timestamp}.json"
            
            with open(result_file, 'w', encoding='utf-8') as f:
                json.dump(analysis_result, f, indent=2, ensure_ascii=False, default=str)
            
            tracker.log_step("results_saved", f"Analysis results saved to {result_file}")
            
            # Complete tracking
            summary_file = tracker.save_final_summary()
            tracker.log_step("analysis_complete", "Analysis completed successfully")
            
            # Add tracking info to result
            analysis_result["tracking"] = {
                "session_id": session_id,
                "total_steps": len(tracker.steps),
                "total_api_calls": len(tracker.api_calls),
                "summary_file": summary_file,
                "tracking_dir": tracker.tracking_dir
            }
            
            # Broadcast final result via SSE
            sse_data = {
                "type": "result",
                "session_id": session_id,
                "result": analysis_result
            }
            event_queue.put(sse_data)
            
        except Exception as e:
            tracker.log_step("analysis_error", f"Analysis failed: {str(e)}")
            
            # Broadcast error via SSE
            sse_data = {
                "type": "error",
                "session_id": session_id,
                "error": str(e)
            }
            event_queue.put(sse_data)
    
    def _create_technical_prompt(self, mode: str) -> str:
        """Create comprehensive technical analysis prompt"""
        
        if mode == "quick":
            return """
            Analyze this technical video for developer audiences. Provide:
            
            1. **Technical Overview** (2-3 sentences describing what this video teaches)
            2. **Key Technologies** (list main frameworks/languages/tools mentioned)
            3. **Target Audience** (beginner/intermediate/advanced developers)
            4. **Main Learning Points** (3-5 bullet points of key takeaways)
            5. **Code Quality Assessment** (if code is shown, evaluate it)
            6. **Social Media Clips** (suggest 2-3 short segments with timestamps for viral potential)
            
            Focus on technical accuracy and developer value. Be specific about what technologies and concepts are covered.
            """
        else:
            return """
            Perform a comprehensive technical analysis of this video for developer audiences:
            
            ## TECHNICAL CONTENT ANALYSIS
            1. **Technologies & Frameworks**: Identify ALL mentioned technologies, frameworks, libraries, and tools
            2. **Code Quality Assessment**: Evaluate any code shown for best practices, patterns, and potential issues
            3. **Technical Accuracy**: Assess the correctness of technical explanations and implementations
            4. **Complexity Level**: Rate difficulty and identify prerequisites
            5. **Architecture Patterns**: Identify any architectural patterns, design patterns, or methodologies discussed
            
            ## EDUCATIONAL VALUE
            6. **Learning Objectives**: What specific skills/knowledge developers will gain
            7. **Practical Applications**: Real-world use cases and applications
            8. **Follow-up Resources**: What viewers should study next
            
            ## CONTENT STRUCTURE
            9. **Key Timestamps**: Identify important moments with MM:SS format and what happens at each
            10. **Chapter Breakdown**: Logical sections and their focus areas
            11. **Demonstration Quality**: How well concepts are explained and shown
            
            ## DEVELOPER AUDIENCE TARGETING
            12. **Audience Segmentation**: Who would benefit most (frontend/backend/fullstack/DevOps/etc.)
            13. **Skill Level Requirements**: Prerequisites and assumed knowledge
            14. **Career Relevance**: How this content helps professional development
            
            ## SOCIAL MEDIA OPTIMIZATION
            15. **Viral Potential**: Rate engagement potential for developer communities (1-10)
            16. **Clip Recommendations**: Suggest 5-7 short segments (30-60 seconds) with exact timestamps
            17. **Platform Strategy**: Optimize for Twitter, LinkedIn, TikTok, YouTube Shorts
            
            ## TECHNICAL CRITIQUE
            18. **Strengths**: What the video does well technically
            19. **Improvements**: Areas that could be enhanced
            20. **Missing Context**: Important information not covered
            
            Provide detailed, actionable insights focused on technical accuracy and developer value.
            """

# Initialize Flask app
app = Flask(__name__)
app.config['SECRET_KEY'] = 'your-secret-key-here'

# Global analyzer instance
analyzer = None

@app.route('/')
def index():
    """Main page with video analysis form"""
    return render_template('index.html')

@app.route('/analyze', methods=['POST'])
def analyze_video():
    """Start video analysis"""
    global analyzer
    
    try:
        data = request.get_json()
        youtube_url = data.get('youtube_url')
        api_key = data.get('api_key')
        mode = data.get('mode', 'comprehensive')
        
        if not youtube_url or not api_key:
            return jsonify({"error": "Missing required fields"}), 400
        
        # Initialize analyzer
        analyzer = WebVideoAnalyzer(api_key)
        
        # Generate session ID
        session_id = f"web_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
        
        # Start analysis in background thread
        thread = threading.Thread(
            target=analyzer.analyze_video_async,
            args=(youtube_url, mode, session_id)
        )
        thread.daemon = True
        thread.start()
        
        return jsonify({
            "status": "started",
            "session_id": session_id,
            "message": "Analysis started successfully"
        })
        
    except Exception as e:
        return jsonify({"error": str(e)}), 500

@app.route('/events')
def events():
    """Server-Sent Events endpoint for real-time updates"""
    
    def event_stream():
        while True:
            try:
                # Get event from queue (blocking with timeout)
                event_data = event_queue.get(timeout=30)
                
                # Format as SSE
                yield f"data: {json.dumps(event_data)}\n\n"
                
            except queue.Empty:
                # Send heartbeat to keep connection alive
                yield f"data: {json.dumps({'type': 'heartbeat', 'timestamp': datetime.now().isoformat()})}\n\n"
            except Exception as e:
                yield f"data: {json.dumps({'type': 'error', 'error': str(e)})}\n\n"
                break
    
    return Response(
        event_stream(),
        mimetype='text/event-stream',
        headers={
            'Cache-Control': 'no-cache',
            'Connection': 'keep-alive',
            'Access-Control-Allow-Origin': '*'
        }
    )

@app.route('/results/<session_id>')
def get_results(session_id):
    """Get analysis results for a session"""
    try:
        summary_file = f"tracking/{session_id}/complete_session_summary.json"
        
        if os.path.exists(summary_file):
            with open(summary_file, 'r', encoding='utf-8') as f:
                summary = json.load(f)
            return jsonify(summary)
        else:
            return jsonify({"error": "Results not found"}), 404
            
    except Exception as e:
        return jsonify({"error": str(e)}), 500

@app.route('/health')
def health():
    """Health check endpoint"""
    return jsonify({
        "status": "healthy",
        "timestamp": datetime.now().isoformat(),
        "version": "1.0.0"
    })

if __name__ == '__main__':
    # Ensure directories exist
    os.makedirs('logs', exist_ok=True)
    os.makedirs('tracking', exist_ok=True)
    os.makedirs('analysis_results', exist_ok=True)
    
    # Configure logging
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
        handlers=[
            logging.FileHandler('logs/web_app.log'),
            logging.StreamHandler()
        ]
    )
    
    print("🚀 Starting Enhanced Video Analyzer Web Application")
    print("📺 Navigate to http://localhost:5001 to begin")
    
    # Run Flask app
    app.run(host='0.0.0.0', port=5001, debug=True, threaded=True)

