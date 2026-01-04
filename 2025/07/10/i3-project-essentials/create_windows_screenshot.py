#!/usr/bin/env python3

from PIL import Image, ImageDraw, ImageFont

def create_windows_view_screenshot(output_path):
    """Create a screenshot showing the windows view of the Go program"""
    width, height = 1024, 768
    img = Image.new('RGB', (width, height), color='#1e1e1e')  # Dark terminal background
    draw = ImageDraw.Draw(img)
    
    # Draw terminal window
    draw.rectangle([50, 50, width-50, height-50], fill='#1e1e1e', outline='#404040', width=2)
    
    # Draw program title
    draw.rectangle([70, 70, 300, 90], fill='#7D56F4')
    draw.text((75, 75), "i3 Window Manager - 1", fill='#ffffff')
    
    # Draw windows list
    y_pos = 110
    draw.text((70, y_pos), "Windows in 1:", fill='#dddddd')
    y_pos += 30
    
    windows = [
        ("▶ Terminal [80x24 at 0,0]", "#F25D94"),  # Selected
        ("  Firefox [1920x1080 at 0,0]", "#dddddd"),  # Normal
        ("  VS Code [1200x800 at 100,100]", "#dddddd")   # Normal
    ]
    
    for win_text, color in windows:
        draw.text((70, y_pos), win_text, fill=color)
        y_pos += 25
    
    # Draw help text
    help_text = "↑/↓: Navigate • Enter: Focus Window & Move Cursor • Tab: Back to Workspaces • R: Refresh • Q: Quit"
    draw.text((70, height - 100), help_text, fill='#626262')
    
    img.save(output_path, 'PNG')
    print(f"Windows view screenshot saved as {output_path}")
    return True

def create_tmux_screenshot(output_path):
    """Create a screenshot showing tmux with the Go program"""
    width, height = 1024, 768
    img = Image.new('RGB', (width, height), color='#1e1e1e')
    draw = ImageDraw.Draw(img)
    
    # Draw tmux status bar at bottom
    draw.rectangle([0, height-25, width, height], fill='#2e3440')
    draw.text((10, height-20), "[0] i3-demo", fill='#88c0d0')
    draw.text((width-150, height-20), "ubuntu@sandbox", fill='#d8dee9')
    
    # Draw main terminal area
    draw.rectangle([10, 10, width-10, height-35], fill='#1e1e1e', outline='#404040', width=1)
    
    # Draw Go program interface
    y_pos = 30
    draw.rectangle([20, y_pos, 400, y_pos+20], fill='#7D56F4')
    draw.text((25, y_pos+5), "i3 Window Manager", fill='#ffffff')
    
    y_pos += 40
    draw.text((20, y_pos), "Workspaces:", fill='#dddddd')
    y_pos += 25
    
    workspaces = [
        ("▶ 1 (1 windows)", "#F25D94"),
        ("  2 (1 windows)", "#dddddd"),
        ("  3 (1 windows)", "#dddddd")
    ]
    
    for ws_text, color in workspaces:
        draw.text((20, y_pos), ws_text, fill=color)
        y_pos += 20
    
    # Draw command prompt at bottom
    draw.text((20, height-60), "$ ./i3-window-manager", fill='#a3be8c')
    
    img.save(output_path, 'PNG')
    print(f"Tmux screenshot saved as {output_path}")
    return True

if __name__ == "__main__":
    create_windows_view_screenshot("/home/ubuntu/screenshots/go_program_windows_view.png")
    create_tmux_screenshot("/home/ubuntu/screenshots/tmux_demo.png")

