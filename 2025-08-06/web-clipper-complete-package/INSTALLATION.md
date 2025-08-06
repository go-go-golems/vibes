# Web Clipper Extension - Installation Guide

## Prerequisites

- Go 1.24.5 or later
- Chrome or Firefox browser
- Linux/macOS/Windows system

## Installation Steps

### 1. Build the Go Backend

```bash
cd clipper-extension/backend
go build -o clipper-backend main.go
```

### 2. Install Native Messaging Hosts

#### For Chrome:
```bash
mkdir -p ~/.config/google-chrome/NativeMessagingHosts
cp native-messaging/chrome-host.json ~/.config/google-chrome/NativeMessagingHosts/com.clipper.host.json
```

#### For Firefox:
```bash
mkdir -p ~/.mozilla/native-messaging-hosts
cp native-messaging/firefox-host.json ~/.mozilla/native-messaging-hosts/com.clipper.host.json
```

### 3. Install Browser Extension

#### Chrome:
1. Open `chrome://extensions/`
2. Enable "Developer mode"
3. Click "Load unpacked"
4. Select the `extension/chrome` directory

#### Firefox:
1. Open `about:debugging`
2. Click "This Firefox"
3. Click "Load Temporary Add-on"
4. Select the `extension/firefox/manifest.json` file

## Usage

1. Navigate to any webpage
2. Optionally select text you want to clip
3. Click the Web Clipper extension icon
4. Fill in the form:
   - **Title**: Custom title for your clip
   - **Category**: Choose from TIL, article, thought, or quote
   - **Notes**: Add your personal notes
5. Click "Save Clip"

## File Location

Clips are saved in the `backend/clips/` directory, organized by date:

```
clips/
├── 2025-08-06/
│   ├── TIL-example-clip.md
│   ├── article-another-clip.md
│   └── quote-interesting-quote.md
```

## Troubleshooting

### Extension not working:
- Check that the Go backend binary is executable
- Verify native messaging host manifests are in correct locations
- Check browser console for error messages

### Native messaging errors:
- Ensure the path in the host manifest points to the correct binary location
- Check that the binary has execute permissions
- Verify the host manifest JSON is valid

### Permission issues:
- Make sure the extension has the required permissions
- Check that native messaging is enabled in browser settings

## Uninstallation

1. Remove the extension from browser extensions page
2. Delete the native messaging host manifests
3. Remove the project directory

## Security Notes

- All data is stored locally on your machine
- No network requests are made by the extension
- Native messaging provides secure communication between browser and backend

