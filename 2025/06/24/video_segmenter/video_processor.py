import cv2
import os
import numpy as np
from typing import List, Tuple, Dict
import json
from datetime import timedelta

class VideoProcessor:
    """
    Video processor for extracting frames and analyzing video content
    """
    
    def __init__(self, video_path: str):
        self.video_path = video_path
        self.cap = cv2.VideoCapture(video_path)
        self.fps = self.cap.get(cv2.CAP_PROP_FPS)
        self.total_frames = int(self.cap.get(cv2.CAP_PROP_FRAME_COUNT))
        self.duration = self.total_frames / self.fps
        
    def __del__(self):
        if hasattr(self, 'cap'):
            self.cap.release()
    
    def extract_frames_at_intervals(self, interval_seconds: float = 5.0, output_dir: str = "frames") -> List[str]:
        """
        Extract frames at regular intervals
        
        Args:
            interval_seconds: Time interval between frame extractions
            output_dir: Directory to save extracted frames
            
        Returns:
            List of paths to extracted frame files
        """
        os.makedirs(output_dir, exist_ok=True)
        frame_paths = []
        
        interval_frames = int(interval_seconds * self.fps)
        
        for frame_num in range(0, self.total_frames, interval_frames):
            self.cap.set(cv2.CAP_PROP_POS_FRAMES, frame_num)
            ret, frame = self.cap.read()
            
            if ret:
                timestamp = frame_num / self.fps
                frame_filename = f"frame_{timestamp:.2f}s.jpg"
                frame_path = os.path.join(output_dir, frame_filename)
                
                cv2.imwrite(frame_path, frame)
                frame_paths.append(frame_path)
                
        return frame_paths
    
    def detect_scene_changes(self, threshold: float = 0.3) -> List[float]:
        """
        Detect scene changes using histogram comparison
        
        Args:
            threshold: Threshold for scene change detection
            
        Returns:
            List of timestamps where scene changes occur
        """
        scene_changes = []
        prev_hist = None
        
        frame_step = int(self.fps)  # Check every second
        
        for frame_num in range(0, self.total_frames, frame_step):
            self.cap.set(cv2.CAP_PROP_POS_FRAMES, frame_num)
            ret, frame = self.cap.read()
            
            if ret:
                # Convert to HSV for better histogram comparison
                hsv = cv2.cvtColor(frame, cv2.COLOR_BGR2HSV)
                hist = cv2.calcHist([hsv], [0, 1, 2], None, [50, 60, 60], [0, 180, 0, 256, 0, 256])
                
                if prev_hist is not None:
                    # Calculate correlation coefficient
                    correlation = cv2.compareHist(hist, prev_hist, cv2.HISTCMP_CORREL)
                    
                    if correlation < threshold:
                        timestamp = frame_num / self.fps
                        scene_changes.append(timestamp)
                
                prev_hist = hist
                
        return scene_changes
    
    def extract_keyframes_from_segments(self, segments: List[Tuple[float, float]], output_dir: str = "frames") -> Dict[str, List[str]]:
        """
        Extract key frames from video segments
        
        Args:
            segments: List of (start_time, end_time) tuples in seconds
            output_dir: Directory to save extracted frames
            
        Returns:
            Dictionary mapping segment names to lists of frame paths
        """
        os.makedirs(output_dir, exist_ok=True)
        segment_frames = {}
        
        for i, (start_time, end_time) in enumerate(segments):
            segment_name = f"segment_{i+1}_{start_time:.1f}s-{end_time:.1f}s"
            segment_frames[segment_name] = []
            
            # Extract frames at key points in the segment
            segment_duration = end_time - start_time
            
            # Extract frames at beginning, middle, and end of segment
            key_times = [start_time, start_time + segment_duration/2, end_time - 1]
            
            for key_time in key_times:
                if key_time >= 0 and key_time < self.duration:
                    frame_num = int(key_time * self.fps)
                    self.cap.set(cv2.CAP_PROP_POS_FRAMES, frame_num)
                    ret, frame = self.cap.read()
                    
                    if ret:
                        frame_filename = f"{segment_name}_frame_{key_time:.2f}s.jpg"
                        frame_path = os.path.join(output_dir, frame_filename)
                        
                        cv2.imwrite(frame_path, frame)
                        segment_frames[segment_name].append(frame_path)
        
        return segment_frames
    
    def get_video_info(self) -> Dict:
        """
        Get basic video information
        
        Returns:
            Dictionary with video metadata
        """
        return {
            "path": self.video_path,
            "fps": self.fps,
            "total_frames": self.total_frames,
            "duration_seconds": self.duration,
            "duration_formatted": str(timedelta(seconds=int(self.duration))),
            "resolution": (
                int(self.cap.get(cv2.CAP_PROP_FRAME_WIDTH)),
                int(self.cap.get(cv2.CAP_PROP_FRAME_HEIGHT))
            )
        }
    
    def create_basic_segments(self, segment_length: float = 30.0) -> List[Tuple[float, float]]:
        """
        Create basic time-based segments
        
        Args:
            segment_length: Length of each segment in seconds
            
        Returns:
            List of (start_time, end_time) tuples
        """
        segments = []
        current_time = 0.0
        
        while current_time < self.duration:
            end_time = min(current_time + segment_length, self.duration)
            segments.append((current_time, end_time))
            current_time = end_time
            
        return segments

