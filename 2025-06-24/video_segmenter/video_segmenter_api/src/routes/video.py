import os
import sys
import json
import tempfile
import shutil
import logging
from flask import Blueprint, request, jsonify, current_app, send_file
from werkzeug.utils import secure_filename
import uuid
from datetime import datetime

# Add parent directory to path to import our modules
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(__file__))))

from video_processor import VideoProcessor
from gemini_video_analyzer import GeminiVideoAnalyzer

video_bp = Blueprint('video', __name__)
logger = logging.getLogger(__name__)

# Default API key for testing (replace with environment variable in production)
DEFAULT_GEMINI_API_KEY = "AIzaSyC4ShT-r48DUVDC95pBlKX3m4aIoJVruI4"

# Configuration
UPLOAD_FOLDER = '/tmp/video_uploads'
FRAMES_FOLDER = '/tmp/video_frames'
OUTPUT_FOLDER = '/tmp/video_output'
ALLOWED_EXTENSIONS = {'mp4', 'avi', 'mov', 'mkv', 'webm'}

# Ensure directories exist
for folder in [UPLOAD_FOLDER, FRAMES_FOLDER, OUTPUT_FOLDER]:
    os.makedirs(folder, exist_ok=True)

def allowed_file(filename):
    return '.' in filename and filename.rsplit('.', 1)[1].lower() in ALLOWED_EXTENSIONS

@video_bp.route('/upload', methods=['POST'])
def upload_video():
    """Upload a video file for processing"""
    logger.info("Video upload request received")
    try:
        if 'video' not in request.files:
            logger.warning("No video file provided in request")
            return jsonify({'error': 'No video file provided'}), 400
        
        file = request.files['video']
        if file.filename == '':
            logger.warning("No file selected in upload request")
            return jsonify({'error': 'No file selected'}), 400
        
        if not allowed_file(file.filename):
            logger.warning(f"Invalid file type uploaded: {file.filename}")
            return jsonify({'error': 'Invalid file type. Allowed: mp4, avi, mov, mkv, webm'}), 400
        
        # Generate unique filename
        file_id = str(uuid.uuid4())
        filename = secure_filename(file.filename)
        file_extension = filename.rsplit('.', 1)[1].lower()
        unique_filename = f"{file_id}.{file_extension}"
        
        logger.info(f"Processing video upload: {filename} -> {unique_filename}")
        
        # Save file
        file_path = os.path.join(UPLOAD_FOLDER, unique_filename)
        file.save(file_path)
        logger.info(f"Video saved to: {file_path}")
        
        # Get video info
        logger.info("Extracting video information")
        processor = VideoProcessor(file_path)
        video_info = processor.get_video_info()
        logger.info(f"Video info extracted: {video_info}")
        
        return jsonify({
            'success': True,
            'file_id': file_id,
            'filename': filename,
            'video_info': video_info
        })
        
    except Exception as e:
        logger.error(f"Error in video upload: {str(e)}", exc_info=True)
        return jsonify({'error': str(e)}), 500

@video_bp.route('/analyze', methods=['POST'])
def analyze_video():
    """Analyze video with AI for technical developer content optimization"""
    logger.info("Video analysis request received")
    try:
        data = request.get_json()
        file_id = data.get('file_id')
        api_key = data.get('api_key', DEFAULT_GEMINI_API_KEY)  # Use default if not provided
        mode = data.get('mode', 'technical')  # 'technical' focused analysis
        analysis_type = data.get('analysis_type', 'developer_shorts')  # 'developer_shorts' or 'general'
        
        if not file_id:
            logger.warning("No file_id provided in analysis request")
            return jsonify({'error': 'file_id is required'}), 400
        
        logger.info(f"Analyzing video: {file_id}, mode: {mode}, type: {analysis_type}")
        
        # Find video file
        video_files = [f for f in os.listdir(UPLOAD_FOLDER) if f.startswith(file_id)]
        if not video_files:
            logger.error(f"Video file not found for file_id: {file_id}")
            return jsonify({'error': 'Video file not found'}), 404
        
        video_path = os.path.join(UPLOAD_FOLDER, video_files[0])
        logger.info(f"Found video file: {video_path}")
        
        # Initialize processors  
        logger.info("Initializing video processor and Gemini analyzer")
        processor = VideoProcessor(video_path)
        gemini_analyzer = GeminiVideoAnalyzer(api_key)
        
        # Upload video to Gemini for comprehensive analysis
        logger.info("Uploading video to Gemini for native processing")
        upload_result = gemini_analyzer.upload_video_to_gemini(video_path)
        
        if not upload_result.get('success'):
            logger.error(f"Failed to upload video to Gemini: {upload_result.get('error')}")
            return jsonify({'error': f"Video upload failed: {upload_result.get('error')}"}), 500
        
        file_uri = upload_result['file_uri']
        file_name = upload_result['file_name']
        logger.info(f"Video uploaded successfully to Gemini: {file_uri}")
        
        # Get video info
        video_info = processor.get_video_info()
        
        # Perform comprehensive Gemini video analysis
        logger.info("Starting comprehensive Gemini video analysis")
        comprehensive_analysis = gemini_analyzer.extract_comprehensive_video_analysis(
            file_uri, 
            analysis_focus="technical_developer"
        )
        
        if not comprehensive_analysis.get('success'):
            logger.error(f"Comprehensive analysis failed: {comprehensive_analysis.get('error')}")
            # Clean up uploaded file
            gemini_analyzer.cleanup_uploaded_file(file_name)
            return jsonify({'error': f"Video analysis failed: {comprehensive_analysis.get('error')}"}), 500
        
        logger.info("Comprehensive analysis completed successfully")
        
        # Create developer shorts strategy based on comprehensive analysis
        logger.info("Creating developer shorts strategy")
        shorts_strategy = gemini_analyzer.create_developer_shorts_strategy(
            comprehensive_analysis, 
            {**video_info, **upload_result}
        )
        
        # Analyze top segments in detail if shorts opportunities were found
        segment_analyses = []
        if comprehensive_analysis.get('shorts_opportunities'):
            logger.info("Analyzing top segments in detail")
            top_segments = comprehensive_analysis['shorts_opportunities'][:3]  # Top 3 segments
            
            for i, segment in enumerate(top_segments):
                logger.info(f"Analyzing segment {i+1}/{len(top_segments)}: {segment.get('start_timestamp')} - {segment.get('end_timestamp')}")
                segment_analysis = gemini_analyzer.analyze_video_segment(
                    file_uri,
                    segment['start_timestamp'],
                    segment['end_timestamp'],
                    "technical_deep_dive"
                )
                segment_analyses.append(segment_analysis)
        
        # Get comprehensive debug data
        logger.info("Collecting debug data from analysis session")
        debug_data = gemini_analyzer.get_debug_data()
        
        # Prepare comprehensive analysis response
        logger.info("Preparing analysis response")
        result = {
            'success': True,
            'file_id': file_id,
            'mode': mode,
            'analysis_type': analysis_type,
            'video_info': video_info,
            'gemini_upload': upload_result,
            'comprehensive_analysis': comprehensive_analysis,
            'developer_shorts_strategy': shorts_strategy,
            'detailed_segment_analyses': segment_analyses,
            'debug_data': debug_data,
            'analysis_summary': {
                'comprehensive_analysis_success': comprehensive_analysis.get('success', False),
                'transcript_available': 'transcript_analysis' in comprehensive_analysis,
                'visual_timeline_analyzed': 'visual_analysis' in comprehensive_analysis,
                'shorts_opportunities_found': len(comprehensive_analysis.get('shorts_opportunities', [])),
                'segments_analyzed_in_detail': len(segment_analyses),
                'has_shorts_strategy': shorts_strategy is not None and shorts_strategy.get('success', False),
                'technical_focus': True,
                'gemini_native_processing': True,
                'debug_session_id': debug_data['session_summary']['session_id'],
                'total_api_calls': debug_data['session_summary']['total_api_calls'],
                'successful_api_calls': debug_data['session_summary']['successful_calls'],
                'total_processing_time': debug_data['session_summary']['total_session_time']
            },
            'optimization_recommendations': {
                'platform_focus': 'developer_platforms',
                'content_type': 'technical_education',
                'target_audience': 'software_developers',
                'ai_processing': 'gemini_2.5_native'
            }
        }
        
        # Save results
        logger.info("Saving analysis results")
        output_file = os.path.join(OUTPUT_FOLDER, f"{file_id}_gemini_analysis.json")
        with open(output_file, 'w') as f:
            json.dump(result, f, indent=2)
        
        # Clean up uploaded file from Gemini (optional - comment out to keep for reuse)
        # gemini_analyzer.cleanup_uploaded_file(file_name)
        
        logger.info(f"Gemini video analysis completed successfully for {file_id}")
        return jsonify(result)
        
    except Exception as e:
        logger.error(f"Error in video analysis: {str(e)}", exc_info=True)
        # Clean up uploaded file in case of error
        if 'file_name' in locals():
            try:
                gemini_analyzer.cleanup_uploaded_file(file_name)
            except:
                pass
        return jsonify({'error': str(e)}), 500

@video_bp.route('/debug/<session_id>', methods=['GET'])
def get_debug_info(session_id):
    """Get debug information for a specific analysis session"""
    logger.info(f"Debug info requested for session: {session_id}")
    try:
        debug_dir = "/tmp/gemini_debug"
        
        # Find all debug files for this session
        debug_files = []
        if os.path.exists(debug_dir):
            for filename in os.listdir(debug_dir):
                if filename.startswith(session_id):
                    debug_files.append(filename)
        
        if not debug_files:
            return jsonify({
                'error': f'No debug data found for session {session_id}',
                'available_sessions': [f.split('_')[0] + '_' + f.split('_')[1] 
                                     for f in os.listdir(debug_dir) 
                                     if '_' in f][:10]  # Show up to 10 recent sessions
            }), 404
        
        # Read all debug files for this session
        debug_data = {}
        for filename in debug_files:
            file_path = os.path.join(debug_dir, filename)
            try:
                with open(file_path, 'r') as f:
                    step_name = filename.replace(f"{session_id}_", "").replace(".json", "")
                    debug_data[step_name] = json.load(f)
            except Exception as e:
                logger.error(f"Error reading debug file {filename}: {e}")
                debug_data[filename] = {'error': f'Failed to read file: {e}'}
        
        return jsonify({
            'session_id': session_id,
            'debug_files_found': len(debug_files),
            'debug_data': debug_data
        })
        
    except Exception as e:
        logger.error(f"Error retrieving debug info: {str(e)}", exc_info=True)
        return jsonify({'error': str(e)}), 500

@video_bp.route('/debug/list', methods=['GET'])
def list_debug_sessions():
    """List all available debug sessions"""
    logger.info("Listing all debug sessions")
    try:
        debug_dir = "/tmp/gemini_debug"
        
        if not os.path.exists(debug_dir):
            return jsonify({
                'available_sessions': [],
                'message': 'No debug directory found'
            })
        
        # Extract unique session IDs
        sessions = set()
        for filename in os.listdir(debug_dir):
            if '_' in filename and filename.endswith('.json'):
                # Extract session ID (first two parts: YYYYMMDD_HHMMSS)
                parts = filename.split('_')
                if len(parts) >= 2:
                    session_id = f"{parts[0]}_{parts[1]}"
                    sessions.add(session_id)
        
        session_list = sorted(list(sessions), reverse=True)  # Most recent first
        
        return jsonify({
            'available_sessions': session_list[:20],  # Limit to 20 most recent
            'total_sessions': len(session_list)
        })
        
    except Exception as e:
        logger.error(f"Error listing debug sessions: {str(e)}", exc_info=True)
        return jsonify({'error': str(e)}), 500

@video_bp.route('/segments', methods=['POST'])
def create_segments():
    """Create video segments based on analysis results"""
    try:
        data = request.get_json()
        file_id = data.get('file_id')
        segment_length = data.get('segment_length', 30.0)
        
        if not file_id:
            return jsonify({'error': 'file_id is required'}), 400
        
        # Find video file
        video_files = [f for f in os.listdir(UPLOAD_FOLDER) if f.startswith(file_id)]
        if not video_files:
            return jsonify({'error': 'Video file not found'}), 404
        
        video_path = os.path.join(UPLOAD_FOLDER, video_files[0])
        
        # Process video
        processor = VideoProcessor(video_path)
        video_info = processor.get_video_info()
        segments = processor.create_basic_segments(segment_length=segment_length)
        
        # Extract keyframes for each segment
        keyframes_dir = os.path.join(FRAMES_FOLDER, f"{file_id}_keyframes")
        os.makedirs(keyframes_dir, exist_ok=True)
        
        segment_frames = processor.extract_keyframes_from_segments(segments, keyframes_dir)
        
        return jsonify({
            'success': True,
            'file_id': file_id,
            'video_info': video_info,
            'segments': [
                {
                    'index': i,
                    'start_time': start,
                    'end_time': end,
                    'duration': end - start
                }
                for i, (start, end) in enumerate(segments)
            ],
            'segment_frames': {
                name: [os.path.basename(path) for path in paths]
                for name, paths in segment_frames.items()
            }
        })
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@video_bp.route('/frames/<file_id>/<filename>')
def get_frame(file_id, filename):
    """Serve extracted frame images"""
    try:
        # Check multiple possible directories
        possible_dirs = [
            os.path.join(FRAMES_FOLDER, file_id),
            os.path.join(FRAMES_FOLDER, f"{file_id}_keyframes"),
            FRAMES_FOLDER
        ]
        
        for frames_dir in possible_dirs:
            file_path = os.path.join(frames_dir, filename)
            if os.path.exists(file_path):
                return send_file(file_path, mimetype='image/jpeg')
        
        return jsonify({'error': 'Frame not found'}), 404
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@video_bp.route('/results/<file_id>')
def get_results(file_id):
    """Get analysis results for a video"""
    try:
        result_file = os.path.join(OUTPUT_FOLDER, f"{file_id}_analysis.json")
        
        if not os.path.exists(result_file):
            return jsonify({'error': 'Results not found'}), 404
        
        with open(result_file, 'r') as f:
            results = json.load(f)
        
        return jsonify(results)
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@video_bp.route('/status')
def status():
    """Get API status"""
    return jsonify({
        'status': 'online',
        'timestamp': datetime.now().isoformat(),
        'upload_folder': UPLOAD_FOLDER,
        'frames_folder': FRAMES_FOLDER,
        'output_folder': OUTPUT_FOLDER
    })

