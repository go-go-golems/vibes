# Web Clipper Extension

A cross-browser extension for clipping web content with rich metadata and local storage via native messaging.

## Features

- **Cross-Browser Support**: Works on Chrome and Firefox
- **Rich Content Clipping**: Capture URLs, selected text, notes, and custom titles
- **Four Categories**: TIL, article, thought, quote
- **Local Storage**: All data saved locally as organized markdown files
- **Privacy-First**: No cloud services, no data collection
- **Native Messaging**: Secure communication between browser and backend

## Quick Start

### 1. Build Backend
```bash
cd backend
go build -o clipper-backend main.go
```

### 2. Install Native Messaging
```bash
# Chrome
mkdir -p ~/.config/google-chrome/NativeMessagingHosts
cp native-messaging/chrome-host.json ~/.config/google-chrome/NativeMessagingHosts/com.clipper.host.json

# Firefox  
mkdir -p ~/.mozilla/native-messaging-hosts
cp native-messaging/firefox-host.json ~/.mozilla/native-messaging-hosts/com.clipper.host.json
```

### 3. Load Extension
- **Chrome**: Go to `chrome://extensions/`, enable Developer mode, click "Load unpacked", select `extension/chrome`
- **Firefox**: Go to `about:debugging`, click "This Firefox", click "Load Temporary Add-on", select `extension/firefox/manifest.json`

## Usage

1. Navigate to any webpage
2. Optionally select text to clip
3. Click the Web Clipper extension icon
4. Fill in the form (title, category, notes)
5. Click "Save Clip"

Clips are saved in `backend/clips/` organized by date.

## Project Structure

```
├── extension/
│   ├── chrome/          # Chrome extension files
│   ├── firefox/         # Firefox extension files
│   └── shared/          # Shared extension code
├── backend/
│   └── main.go          # Go native messaging host
├── native-messaging/    # Browser host configurations
├── docs/               # Documentation
└── scripts/            # Build and utility scripts
```

## Documentation

- [Installation Guide](docs/INSTALLATION.md) - Detailed setup instructions
- [Project Summary](docs/PROJECT_SUMMARY.md) - Architecture and features overview
- [Design Document](docs/DESIGN.md) - Technical design decisions

## Requirements

- Go 1.24.5 or later
- Chrome or Firefox browser
- Linux, macOS, or Windows

## License

MIT License - see LICENSE file for details

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test thoroughly
5. Submit a pull request

## Support

For issues and questions, please check the documentation or create an issue in the repository.

