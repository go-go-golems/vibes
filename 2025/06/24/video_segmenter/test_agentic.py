#!/usr/bin/env python3

import sys
import os
import argparse
sys.path.append('/home/ubuntu/video_segmenter')

from agentic_analyzer import AgenticGeminiAnalyzer
from video_processor import VideoProcessor
import json
import time

def test_agentic_analyzer(mode='quick', api_key="AIzaSyBuqIGUdU3LNLC2XP1I6wiXpC7Us9_cA7Q"):
    """
    Test the agentic Gemini analyzer with different modes
    
    Args:
        mode: 'quick' for small segments, 'full' for complete analysis
        api_key: Gemini API key
    """
    
    print(f"=== AGENTIC VIDEO ANALYZER TEST ({mode.upper()} MODE) ===")
    print("Initializing advanced agentic analyzer...")
    
    analyzer = AgenticGeminiAnalyzer(api_key)
    
    # Choose video based on mode
    if mode == 'quick':
        video_path = "/home/ubuntu/video_segmenter/videos/sample_short.mp4"
        segment_length = 5.0  # 5-second segments for quick testing
        max_segments = 2      # Analyze only 2 segments
        frame_interval = 2.5  # Extract frames every 2.5 seconds
        print(f"Using short video for quick testing: {video_path}")
    else:
        video_path = "/home/ubuntu/video_segmenter/videos/sample_test.mp4"
        segment_length = 15.0  # 15-second segments for full analysis
        max_segments = None    # Analyze all segments
        frame_interval = 5.0   # Extract frames every 5 seconds
        print(f"Using full video for complete analysis: {video_path}")
    
    if not os.path.exists(video_path):
        print(f"Error: Video file not found at {video_path}")
        return
    
    # Initialize video processor
    print("Processing video...")
    processor = VideoProcessor(video_path)
    video_info = processor.get_video_info()
    
    print(f"Video Info: {video_info['duration_formatted']} duration, {video_info['resolution']} resolution")
    
    # Create segments
    segments = processor.create_basic_segments(segment_length=segment_length)
    if max_segments:
        segments = segments[:max_segments]
    
    print(f"Created {len(segments)} segments for analysis")
    
    # Extract frames for each segment
    print("Extracting frames...")
    all_frame_paths = processor.extract_frames_at_intervals(
        interval_seconds=frame_interval, 
        output_dir=f"frames/{mode}_test"
    )
    
    print(f"Extracted {len(all_frame_paths)} frames")
    
    # PHASE 1: Individual Frame Analysis with Chain-of-Thought
    print("\n=== PHASE 1: CHAIN-OF-THOUGHT FRAME ANALYSIS ===")
    frame_analyses = []
    
    for i, frame_path in enumerate(all_frame_paths[:3 if mode == 'quick' else 6]):
        print(f"Analyzing frame {i+1} with chain-of-thought reasoning...")
        
        context = {
            "frame_number": i+1,
            "total_frames": len(all_frame_paths),
            "video_duration": video_info['duration_seconds'],
            "analysis_mode": mode
        }
        
        try:
            analysis = analyzer.analyze_frame_with_cot(frame_path, context)
            frame_analyses.append(analysis)
            
            if analysis.get('success'):
                print(f"  ✓ Frame {i+1} analyzed successfully")
                if 'final_scores' in analysis:
                    scores = analysis['final_scores']
                    print(f"    Viral Potential: {scores.get('overall_viral_potential', 'N/A')}/10")
                    print(f"    Social Media Appeal: {scores.get('social_media_appeal', 'N/A')}/10")
                else:
                    print(f"    Analysis completed but scores not available")
            else:
                print(f"  ✗ Frame {i+1} analysis failed: {analysis.get('error', 'Unknown error')}")
                
        except Exception as e:
            print(f"  ✗ Frame {i+1} analysis error: {e}")
            frame_analyses.append({'success': False, 'error': str(e)})
        
        # Small delay to avoid rate limiting
        time.sleep(1)
    
    # PHASE 2: Sequence Analysis with Multi-Step Reasoning
    print("\n=== PHASE 2: MULTI-STEP SEQUENCE REASONING ===")
    sequence_analyses = []
    
    # Group frames into sequences for analysis
    frames_per_sequence = 3 if mode == 'quick' else 4
    
    for i in range(0, len(all_frame_paths), frames_per_sequence):
        sequence_frames = all_frame_paths[i:i+frames_per_sequence]
        if len(sequence_frames) < 2:  # Skip if too few frames
            continue
            
        sequence_num = (i // frames_per_sequence) + 1
        print(f"Analyzing sequence {sequence_num} ({len(sequence_frames)} frames)...")
        
        segment_info = {
            "sequence_number": sequence_num,
            "frame_count": len(sequence_frames),
            "start_time": i * frame_interval,
            "end_time": (i + len(sequence_frames)) * frame_interval,
            "video_context": video_info
        }
        
        try:
            analysis = analyzer.analyze_sequence_with_reasoning(sequence_frames, segment_info)
            sequence_analyses.append(analysis)
            
            if analysis.get('success'):
                print(f"  ✓ Sequence {sequence_num} analyzed successfully")
                if 'overall_assessment' in analysis:
                    assessment = analysis['overall_assessment']
                    print(f"    Viral Potential: {assessment.get('viral_potential', 'N/A')}/10")
                    print(f"    Content Quality: {assessment.get('content_quality', 'N/A')}/10")
                    
                if 'strategic_recommendations' in analysis:
                    best_platform = analysis['strategic_recommendations'].get('best_platform', 'N/A')
                    print(f"    Best Platform: {best_platform}")
            else:
                print(f"  ✗ Sequence {sequence_num} analysis failed: {analysis.get('error', 'Unknown error')}")
                
        except Exception as e:
            print(f"  ✗ Sequence {sequence_num} analysis error: {e}")
            sequence_analyses.append({'success': False, 'error': str(e)})
        
        # Delay to avoid rate limiting
        time.sleep(2)
        
        # Limit sequences in quick mode
        if mode == 'quick' and sequence_num >= 2:
            break
    
    # PHASE 3: Strategic Planning (only if we have successful analyses)
    successful_analyses = [a for a in sequence_analyses if a.get('success')]
    
    if successful_analyses:
        print("\n=== PHASE 3: STRATEGIC PLANNING WITH ADVANCED REASONING ===")
        print("Creating comprehensive strategic plan...")
        
        try:
            strategic_plan = analyzer.create_strategic_video_plan(successful_analyses, video_info)
            
            if strategic_plan.get('success'):
                print("  ✓ Strategic plan created successfully")
                
                if 'executive_summary' in strategic_plan:
                    summary = strategic_plan['executive_summary']
                    print(f"    Primary Recommendation: {summary.get('primary_recommendation', 'N/A')}")
                    print(f"    Investment Priority: {summary.get('investment_priority', 'N/A')}")
                
                if 'platform_strategy' in strategic_plan:
                    platform = strategic_plan['platform_strategy'].get('primary_platform', {})
                    print(f"    Best Platform: {platform.get('platform', 'N/A')}")
                    
            else:
                print(f"  ✗ Strategic planning failed: {strategic_plan.get('error', 'Unknown error')}")
                
        except Exception as e:
            print(f"  ✗ Strategic planning error: {e}")
            strategic_plan = {'success': False, 'error': str(e)}
        
        # PHASE 4: Self-Evaluation
        print("\n=== PHASE 4: SELF-EVALUATION AND QUALITY ASSURANCE ===")
        print("Performing self-evaluation of analysis quality...")
        
        try:
            # Evaluate the strategic plan
            evaluation = analyzer.self_evaluate_analysis(strategic_plan)
            
            if evaluation.get('success'):
                print("  ✓ Self-evaluation completed")
                print(f"    Overall Quality: {evaluation.get('overall_quality', 'N/A')}/10")
                print(f"    Accuracy Confidence: {evaluation.get('accuracy_confidence', 'N/A')}/10")
                print(f"    Bias Risk Level: {evaluation.get('bias_risk_level', 'N/A')}")
                
                if 'improvement_suggestions' in evaluation:
                    suggestions = evaluation['improvement_suggestions'][:2]  # Show first 2
                    print(f"    Key Improvements: {', '.join(suggestions)}")
                    
            else:
                print(f"  ✗ Self-evaluation failed: {evaluation.get('error', 'Unknown error')}")
                
        except Exception as e:
            print(f"  ✗ Self-evaluation error: {e}")
    
    else:
        print("\n⚠️  No successful sequence analyses - skipping strategic planning")
    
    # SUMMARY
    print(f"\n=== ANALYSIS SUMMARY ({mode.upper()} MODE) ===")
    print(f"Frames analyzed: {len(frame_analyses)}")
    print(f"Sequences analyzed: {len(sequence_analyses)}")
    print(f"Successful frame analyses: {sum(1 for a in frame_analyses if a.get('success'))}")
    print(f"Successful sequence analyses: {len(successful_analyses)}")
    
    if mode == 'quick':
        print("\n💡 Quick test completed! Run with --mode=full for comprehensive analysis.")
    else:
        print("\n🎯 Full analysis completed! Check output files for detailed results.")
    
    # Save results
    results = {
        'mode': mode,
        'video_info': video_info,
        'frame_analyses': frame_analyses,
        'sequence_analyses': sequence_analyses,
        'strategic_plan': strategic_plan if 'strategic_plan' in locals() else None,
        'self_evaluation': evaluation if 'evaluation' in locals() else None
    }
    
    output_file = f"output/agentic_analysis_{mode}.json"
    os.makedirs("output", exist_ok=True)
    
    with open(output_file, 'w') as f:
        json.dump(results, f, indent=2)
    
    print(f"Results saved to: {output_file}")

def main():
    parser = argparse.ArgumentParser(description='Test Agentic Video Analyzer')
    parser.add_argument('--mode', choices=['quick', 'full'], default='quick',
                       help='Analysis mode: quick for testing, full for complete analysis')
    parser.add_argument('--api-key', default="AIzaSyBuqIGUdU3LNLC2XP1I6wiXpC7Us9_cA7Q",
                       help='Gemini API key')
    
    args = parser.parse_args()
    
    test_agentic_analyzer(mode=args.mode, api_key=args.api_key)

if __name__ == "__main__":
    main()

