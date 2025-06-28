#!/usr/bin/env python3

import sys
import os
sys.path.append('/home/ubuntu/video_segmenter')

from video_processor import VideoProcessor
import json

def test_video_processor():
    """Test the video processor with sample video"""
    
    video_path = "/home/ubuntu/video_segmenter/videos/sample_test.mp4"
    
    if not os.path.exists(video_path):
        print(f"Error: Video file not found at {video_path}")
        return
    
    print("Initializing video processor...")
    processor = VideoProcessor(video_path)
    
    # Get video info
    print("\n=== Video Information ===")
    video_info = processor.get_video_info()
    print(json.dumps(video_info, indent=2))
    
    # Create basic segments
    print("\n=== Creating Basic Segments ===")
    segments = processor.create_basic_segments(segment_length=15.0)
    print(f"Created {len(segments)} segments:")
    for i, (start, end) in enumerate(segments):
        print(f"  Segment {i+1}: {start:.1f}s - {end:.1f}s ({end-start:.1f}s duration)")
    
    # Extract frames at intervals
    print("\n=== Extracting Frames at Intervals ===")
    frame_paths = processor.extract_frames_at_intervals(interval_seconds=10.0, output_dir="frames/interval_frames")
    print(f"Extracted {len(frame_paths)} frames:")
    for path in frame_paths[:5]:  # Show first 5
        print(f"  {path}")
    if len(frame_paths) > 5:
        print(f"  ... and {len(frame_paths) - 5} more")
    
    # Detect scene changes
    print("\n=== Detecting Scene Changes ===")
    scene_changes = processor.detect_scene_changes(threshold=0.7)
    print(f"Detected {len(scene_changes)} scene changes:")
    for timestamp in scene_changes[:10]:  # Show first 10
        print(f"  Scene change at {timestamp:.2f}s")
    if len(scene_changes) > 10:
        print(f"  ... and {len(scene_changes) - 10} more")
    
    # Extract keyframes from segments
    print("\n=== Extracting Keyframes from Segments ===")
    segment_frames = processor.extract_keyframes_from_segments(segments[:3], output_dir="frames/segment_frames")
    for segment_name, frames in segment_frames.items():
        print(f"  {segment_name}: {len(frames)} frames")
        for frame_path in frames:
            print(f"    {frame_path}")
    
    print("\n=== Test Complete ===")
    print("Check the 'frames' directory for extracted images.")

if __name__ == "__main__":
    test_video_processor()

