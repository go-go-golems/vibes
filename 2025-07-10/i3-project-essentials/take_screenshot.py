#!/usr/bin/env python3

import os
import subprocess
import sys

def take_screenshot(filename):
    """Take a screenshot using xwd and convert to PNG"""
    try:
        # Set DISPLAY environment variable
        os.environ['DISPLAY'] = ':99'
        
        # Use xwd to capture the root window
        xwd_cmd = ['xwd', '-root', '-out', '/tmp/screenshot.xwd']
        subprocess.run(xwd_cmd, check=True)
        
        # Convert XWD to PNG using ImageMagick (if available) or try other methods
        try:
            convert_cmd = ['convert', '/tmp/screenshot.xwd', filename]
            subprocess.run(convert_cmd, check=True)
            print(f"Screenshot saved to {filename}")
        except (subprocess.CalledProcessError, FileNotFoundError):
            # If ImageMagick is not available, try using Python PIL
            try:
                from PIL import Image
                import struct
                
                # Read XWD file and convert to PNG
                with open('/tmp/screenshot.xwd', 'rb') as f:
                    data = f.read()
                
                # This is a simplified XWD parser - for production use a proper library
                print(f"XWD file created at /tmp/screenshot.xwd ({len(data)} bytes)")
                print("ImageMagick not available for conversion to PNG")
                return False
                
            except ImportError:
                print("PIL not available for image conversion")
                return False
        
        # Clean up temporary file
        try:
            os.remove('/tmp/screenshot.xwd')
        except:
            pass
            
        return True
        
    except subprocess.CalledProcessError as e:
        print(f"Error taking screenshot: {e}")
        return False
    except Exception as e:
        print(f"Unexpected error: {e}")
        return False

if __name__ == "__main__":
    filename = sys.argv[1] if len(sys.argv) > 1 else "/home/ubuntu/i3_screenshot_1.png"
    success = take_screenshot(filename)
    sys.exit(0 if success else 1)

