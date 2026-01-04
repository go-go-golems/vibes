#!/usr/bin/env python3

import os
from PIL import Image
import json

def simplify_image(image_path, max_size=32):
    """Extract and simplify pixel art to manageable size."""
    print(f"\nProcessing: {os.path.basename(image_path)}")
    
    img = Image.open(image_path)
    width, height = img.size
    print(f"Original size: {width}x{height}")
    
    # Convert to RGB
    if img.mode != 'RGB':
        img = img.convert('RGB')
    
    # Resize if too large, maintaining aspect ratio
    if width > max_size or height > max_size:
        ratio = min(max_size/width, max_size/height)
        new_width = int(width * ratio)
        new_height = int(height * ratio)
        img = img.resize((new_width, new_height), Image.NEAREST)
        print(f"Resized to: {new_width}x{new_height}")
    
    width, height = img.size
    
    # Extract pixels and quantize colors
    pixels = []
    color_map = {}
    color_index = 0
    
    for y in range(height):
        row = []
        for x in range(width):
            r, g, b = img.getpixel((x, y))
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
    
    # Create palette
    palette = [''] * len(color_map)
    for color, index in color_map.items():
        palette[index] = f"#{color[0]:02x}{color[1]:02x}{color[2]:02x}"
    
    print(f"Colors: {len(palette)}")
    print("Palette:", palette[:10], "..." if len(palette) > 10 else "")
    
    return {
        'filename': os.path.basename(image_path),
        'width': width,
        'height': height,
        'pixels': pixels,
        'palette': palette,
        'color_count': len(palette)
    }

def main():
    image_files = [
        '/home/ubuntu/upload/pasted_file_4jP02c_image.png',
        '/home/ubuntu/upload/pasted_file_mHt5aq_image.png', 
        '/home/ubuntu/upload/pasted_file_zdUAFD_image.png',
        '/home/ubuntu/upload/pasted_file_n1aHDK_image.png'
    ]
    
    pixel_data = []
    
    for image_file in image_files:
        if os.path.exists(image_file):
            data = simplify_image(image_file)
            pixel_data.append(data)
        else:
            print(f"File not found: {image_file}")
    
    # Save simplified data
    with open('/home/ubuntu/pixel_data.json', 'w') as f:
        json.dump(pixel_data, f, indent=2)
    
    print(f"\nPixel data saved to pixel_data.json")
    print(f"Total images processed: {len(pixel_data)}")

if __name__ == "__main__":
    main()

