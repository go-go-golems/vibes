#!/usr/bin/env python3
import os
import sys
from PIL import Image, ImageDraw, ImageFont
import textwrap

def text_to_png(text_file, output_file, width=1200, height=800):
    """Convert text file to PNG with terminal-like styling"""
    
    # Read the text file
    with open(text_file, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # Create image with dark background (terminal-like)
    img = Image.new('RGB', (width, height), color='#1a1a1a')
    draw = ImageDraw.Draw(img)
    
    # Try to use a monospace font
    try:
        # Try common monospace fonts
        font_paths = [
            '/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf',
            '/usr/share/fonts/truetype/liberation/LiberationMono-Regular.ttf',
            '/usr/share/fonts/truetype/ubuntu/UbuntuMono-R.ttf',
            '/System/Library/Fonts/Monaco.ttf',  # macOS
            'C:/Windows/Fonts/consola.ttf',      # Windows
        ]
        
        font = None
        for font_path in font_paths:
            if os.path.exists(font_path):
                font = ImageFont.truetype(font_path, 14)
                break
        
        if font is None:
            font = ImageFont.load_default()
            
    except Exception:
        font = ImageFont.load_default()
    
    # Split content into lines
    lines = content.split('\n')
    
    # Terminal colors mapping
    colors = {
        'default': '#ffffff',
        'bright': '#ffffff',
        'dim': '#888888',
        'title': '#7D56F4',
        'success': '#04B575',
        'error': '#FF5F87',
        'domain': '#04B575',
        'help': '#626262'
    }
    
    # Draw text line by line
    y_offset = 20
    line_height = 18
    
    for line in lines:
        if y_offset > height - 30:
            break
            
        # Simple color detection based on content
        color = colors['default']
        
        if '🎨 Graph Generator' in line or '✏️' in line or '📊' in line:
            color = colors['title']
        elif '[Business]' in line or '[Technology]' in line or '[Process]' in line:
            color = colors['domain']
        elif 'Error:' in line:
            color = colors['error']
        elif '✓ Success' in line:
            color = colors['success']
        elif 'Enter to select' in line or 'Ctrl+G' in line or 'Esc to' in line:
            color = colors['help']
        elif line.startswith('│') or line.startswith('┃'):
            color = colors['bright']
        elif line.strip().startswith('┌') or line.strip().startswith('└') or line.strip().startswith('╭') or line.strip().startswith('╰'):
            color = colors['dim']
        
        # Draw the line
        draw.text((20, y_offset), line, font=font, fill=color)
        y_offset += line_height
    
    # Save the image
    img.save(output_file, 'PNG')
    print(f"Converted {text_file} to {output_file}")

def main():
    if len(sys.argv) != 3:
        print("Usage: python3 text_to_png.py <input_text_file> <output_png_file>")
        sys.exit(1)
    
    input_file = sys.argv[1]
    output_file = sys.argv[2]
    
    if not os.path.exists(input_file):
        print(f"Error: Input file {input_file} does not exist")
        sys.exit(1)
    
    text_to_png(input_file, output_file)

if __name__ == "__main__":
    main()

