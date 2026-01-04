#!/usr/bin/env python3

import sys
import os
sys.path.append('/home/ubuntu/video_segmenter')

from gemini_analyzer import GeminiVideoAnalyzer
from video_processor import VideoProcessor
import json

def test_gemini_analyzer():
    """Test the Gemini analyzer with sample frames"""
    
    # Use the provided API key
    api_key = "AIzaSyBuqIGUdU3LNLC2XP1I6wiXpC7Us9_cA7Q"
    
    print("Initializing Gemini analyzer...")
    analyzer = GeminiVideoAnalyzer(api_key)
    
    # Check if we have extracted frames
    frame_dir = "/home/ubuntu/video_segmenter/frames/interval_frames"
    if not os.path.exists(frame_dir):
        print("No frames found. Running video processor first...")
        
        # Process video to get frames
        video_path = "/home/ubuntu/video_segmenter/videos/sample_test.mp4"
        processor = VideoProcessor(video_path)
        frame_paths = processor.extract_frames_at_intervals(interval_seconds=10.0, output_dir=frame_dir)
    else:
        frame_paths = [os.path.join(frame_dir, f) for f in os.listdir(frame_dir) if f.endswith('.jpg')]
        frame_paths.sort()
    
    if not frame_paths:
        print("Error: No frame images found!")
        return
    
    print(f"Found {len(frame_paths)} frames to analyze")
    
    # Test single frame analysis
    print("\n=== Testing Single Frame Analysis ===")
    if frame_paths:
        test_frame = frame_paths[0]
        print(f"Analyzing frame: {test_frame}")
        
        try:
            analysis = analyzer.analyze_frame_content(test_frame, use_flash=True)
            print("Frame analysis result:")
            print(json.dumps(analysis, indent=2))
        except Exception as e:
            print(f"Error analyzing frame: {e}")
    
    # Test sequence analysis
    print("\n=== Testing Frame Sequence Analysis ===")
    if len(frame_paths) >= 3:
        test_sequence = frame_paths[:3]
        print(f"Analyzing sequence of {len(test_sequence)} frames")
        
        try:
            sequence_analysis = analyzer.analyze_frame_sequence(test_sequence, use_flash=True)
            print("Sequence analysis result:")
            print(json.dumps(sequence_analysis, indent=2))
        except Exception as e:
            print(f"Error analyzing sequence: {e}")
    
    # Test finding best frames
    print("\n=== Testing Best Frame Selection ===")
    try:
        # Analyze multiple frames
        frame_analyses = []
        for i, frame_path in enumerate(frame_paths[:3]):  # Limit to 3 for testing
            print(f"Analyzing frame {i+1}/{min(3, len(frame_paths))}...")
            analysis = analyzer.analyze_frame_content(frame_path, use_flash=True)
            frame_analyses.append(analysis)
        
        # Find best frames
        best_frames = analyzer.find_best_frames(frame_analyses, top_n=2)
        print("Best frames:")
        for frame_info in best_frames:
            print(f"  Frame {frame_info['frame_index']}: Score {frame_info['combined_score']:.1f}")
            print(f"    Description: {frame_info['analysis'].get('content_description', 'N/A')}")
    
    except Exception as e:
        print(f"Error in best frame selection: {e}")
    
    print("\n=== Gemini Integration Test Complete ===")

if __name__ == "__main__":
    test_gemini_analyzer()

