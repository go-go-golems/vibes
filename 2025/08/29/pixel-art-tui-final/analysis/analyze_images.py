#!/usr/bin/env python3

import os
from PIL import Image
import json

def analyze_image(image_path):
    """Analyze a pixel image and extract its data."""
    print(f"\nAnalyzing: {image_path}")
    
    # Open the image
    img = Image.open(image_path)
    
    # Get basic info
    width, height = img.size
    mode = img.mode
    
    print(f"Dimensions: {width}x{height}")
    print(f"Mode: {mode}")
    
    # Convert to RGB if needed
    if mode != 'RGB':
        img = img.convert('RGB')
    
    # Extract pixel data
    pixels = []
    unique_colors = set()
    
    for y in range(height):
        row = []
        for x in range(width):
            r, g, b = img.getpixel((x, y))
            row.append((r, g, b))
            unique_colors.add((r, g, b))
        pixels.append(row)
    
    print(f"Unique colors: {len(unique_colors)}")
    print("Color palette:")
    for i, color in enumerate(sorted(unique_colors)):
        print(f"  {i+1}: RGB{color} (#{color[0]:02x}{color[1]:02x}{color[2]:02x})")
    
    return {
        'filename': os.path.basename(image_path),
        'width': width,
        'height': height,
        'mode': mode,
        'pixels': pixels,
        'unique_colors': list(unique_colors),
        'color_count': len(unique_colors)
    }

def main():
    image_files = [
        '/home/ubuntu/upload/pasted_file_4jP02c_image.png',
        '/home/ubuntu/upload/pasted_file_mHt5aq_image.png', 
        '/home/ubuntu/upload/pasted_file_zdUAFD_image.png',
        '/home/ubuntu/upload/pasted_file_n1aHDK_image.png'
    ]
    
    all_data = []
    
    for image_file in image_files:
        if os.path.exists(image_file):
            data = analyze_image(image_file)
            all_data.append(data)
        else:
            print(f"File not found: {image_file}")
    
    # Save analysis to JSON
    with open('/home/ubuntu/image_analysis.json', 'w') as f:
        json.dump(all_data, f, indent=2)
    
    print(f"\nAnalysis complete. Data saved to image_analysis.json")
    print(f"Total images analyzed: {len(all_data)}")

if __name__ == "__main__":
    main()

