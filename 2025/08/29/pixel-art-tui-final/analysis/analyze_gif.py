#!/usr/bin/env python3

import os
from PIL import Image, ImageSequence
import json

def analyze_gif(gif_path, max_size=32):
    """Analyze an animated GIF and extract frame data."""
    print(f"\nAnalyzing animated GIF: {os.path.basename(gif_path)}")
    
    img = Image.open(gif_path)
    
    # Get basic info
    width, height = img.size
    print(f"Original size: {width}x{height}")
    print(f"Format: {img.format}")
    print(f"Mode: {img.mode}")
    
    # Check if it's animated
    try:
        img.seek(1)
        is_animated = True
        img.seek(0)
        frame_count = img.n_frames
        print(f"Animated: Yes, {frame_count} frames")
    except:
        is_animated = False
        frame_count = 1
        print("Animated: No")
    
    frames = []
    
    for frame_num, frame in enumerate(ImageSequence.Iterator(img)):
        print(f"Processing frame {frame_num + 1}/{frame_count}")
        
        # Convert to RGB
        if frame.mode != 'RGB':
            frame = frame.convert('RGB')
        
        # Resize if too large, maintaining aspect ratio
        if width > max_size or height > max_size:
            ratio = min(max_size/width, max_size/height)
            new_width = int(width * ratio)
            new_height = int(height * ratio)
            frame = frame.resize((new_width, new_height), Image.NEAREST)
        
        frame_width, frame_height = frame.size
        
        # Extract pixels and quantize colors
        pixels = []
        color_map = {}
        color_index = 0
        
        for y in range(frame_height):
            row = []
            for x in range(frame_width):
                r, g, b = frame.getpixel((x, y))
                # Quantize to reduce color variations
                r = (r // 32) * 32
                g = (g // 32) * 32  
                b = (b // 32) * 32
                color = (r, g, b)
                
                if color not in color_map:
                    color_map[color] = color_index
                    color_index += 1
                
                row.append(color_map[color])
            pixels.append(row)
        
        # Create palette for this frame
        palette = [''] * len(color_map)
        for color, index in color_map.items():
            palette[index] = f"#{color[0]:02x}{color[1]:02x}{color[2]:02x}"
        
        frames.append({
            'frame_number': frame_num,
            'width': frame_width,
            'height': frame_height,
            'pixels': pixels,
            'palette': palette,
            'color_count': len(palette)
        })
        
        print(f"  Frame {frame_num + 1}: {frame_width}x{frame_height}, {len(palette)} colors")
    
    return {
        'filename': os.path.basename(gif_path),
        'original_width': width,
        'original_height': height,
        'is_animated': is_animated,
        'frame_count': frame_count,
        'frames': frames
    }

def main():
    gif_file = '/home/ubuntu/blockbob.gif'
    
    if os.path.exists(gif_file):
        data = analyze_gif(gif_file)
        
        # Save analysis to JSON
        with open('/home/ubuntu/gif_analysis.json', 'w') as f:
            json.dump(data, f, indent=2)
        
        print(f"\nGIF analysis complete. Data saved to gif_analysis.json")
        print(f"Frames: {data['frame_count']}")
        print(f"Animated: {data['is_animated']}")
    else:
        print(f"File not found: {gif_file}")

if __name__ == "__main__":
    main()

