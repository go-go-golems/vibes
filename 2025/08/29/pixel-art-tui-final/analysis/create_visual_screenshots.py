#!/usr/bin/env python3

import os
from PIL import Image, ImageDraw, ImageFont

def create_terminal_screenshot(text_content, output_path, width=800, height=600):
    """Create a visual representation of terminal output."""
    
    # Create a black background image
    img = Image.new('RGB', (width, height), color='black')
    draw = ImageDraw.Draw(img)
    
    # Try to use a monospace font
    try:
        # Try to find a monospace font
        font = ImageFont.truetype("/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf", 12)
    except:
        try:
            font = ImageFont.truetype("/usr/share/fonts/truetype/liberation/LiberationMono-Regular.ttf", 12)
        except:
            # Fall back to default font
            font = ImageFont.load_default()
    
    # Split text into lines
    lines = text_content.split('\n')
    
    # Draw text line by line
    y = 10
    line_height = 15
    
    for line in lines:
        # Handle ANSI color codes by removing them for now
        clean_line = line
        # Remove common ANSI escape sequences
        import re
        clean_line = re.sub(r'\x1b\[[0-9;]*m', '', clean_line)
        
        # Draw the line
        draw.text((10, y), clean_line, fill='white', font=font)
        y += line_height
        
        # Stop if we exceed the image height
        if y > height - 20:
            break
    
    # Save the image
    img.save(output_path)
    print(f"Created visual screenshot: {output_path}")

def main():
    screenshots_dir = '/home/ubuntu/screenshots'
    
    # Process all text files
    for filename in os.listdir(screenshots_dir):
        if filename.endswith('_text.txt'):
            text_path = os.path.join(screenshots_dir, filename)
            png_path = os.path.join(screenshots_dir, filename.replace('_text.txt', '_visual.png'))
            
            # Read text content
            with open(text_path, 'r') as f:
                content = f.read()
            
            # Create visual screenshot
            create_terminal_screenshot(content, png_path)
    
    print("Visual screenshots created!")

if __name__ == "__main__":
    main()

