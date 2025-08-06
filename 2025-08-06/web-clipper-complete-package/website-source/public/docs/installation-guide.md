# Web Clipper Extension - Complete Installation Guide

## Overview

The Web Clipper Extension is a cross-browser tool that enables users to capture, organize, and store web content locally with complete privacy. This comprehensive installation guide will walk you through every step of the setup process, from downloading the extension to configuring native messaging and testing the complete workflow.

## System Requirements

Before beginning the installation process, ensure your system meets the following requirements:

### Operating System Support
- **Linux**: Ubuntu 18.04+, Debian 10+, CentOS 7+, or equivalent distributions
- **macOS**: macOS 10.14 (Mojave) or later
- **Windows**: Windows 10 or Windows 11

### Browser Compatibility
- **Google Chrome**: Version 88 or later
- **Mozilla Firefox**: Version 78 or later
- **Chromium-based browsers**: Microsoft Edge, Brave, Opera (Chrome extension compatible)

### Development Dependencies
- **Go Programming Language**: Version 1.24.5 or later
- **Command Line Access**: Terminal (Linux/macOS) or Command Prompt/PowerShell (Windows)
- **File System Permissions**: Ability to create directories and execute binaries

## Pre-Installation Preparation

### Downloading the Extension Package

The Web Clipper Extension is distributed as a complete package containing all necessary components. Download the latest release from the official repository:

1. Navigate to the project's release page
2. Download the `web-clipper-extension.zip` file (approximately 8.1 MB)
3. Verify the download integrity using the provided checksums
4. Extract the archive to your preferred installation directory

The extracted package contains the following structure:

```
web-clipper-extension/
├── extension/
│   ├── chrome/          # Chrome-specific extension files
│   ├── firefox/         # Firefox-specific extension files
│   └── shared/          # Common extension components
├── backend/
│   ├── main.go          # Go backend source code
│   └── go.mod           # Go module dependencies
├── native-messaging/    # Browser host configurations
├── scripts/             # Installation and build scripts
├── docs/               # Documentation files
├── README.md           # Quick start guide
└── LICENSE             # MIT license terms
```

### Setting Up the Go Environment

The Web Clipper Extension requires Go to build the native messaging backend. If Go is not already installed on your system, follow these steps:

#### Linux Installation
```bash
# Download and install Go
wget https://go.dev/dl/go1.24.5.linux-amd64.tar.gz
sudo tar -C /usr/local -xzf go1.24.5.linux-amd64.tar.gz

# Add Go to your PATH
echo 'export PATH=$PATH:/usr/local/go/bin' >> ~/.bashrc
source ~/.bashrc

# Verify installation
go version
```

#### macOS Installation
```bash
# Using Homebrew (recommended)
brew install go

# Or download from official site
curl -O https://go.dev/dl/go1.24.5.darwin-amd64.tar.gz
sudo tar -C /usr/local -xzf go1.24.5.darwin-amd64.tar.gz
echo 'export PATH=$PATH:/usr/local/go/bin' >> ~/.zshrc
source ~/.zshrc
```

#### Windows Installation
1. Download the Windows installer from https://go.dev/dl/
2. Run the installer and follow the setup wizard
3. Verify installation by opening Command Prompt and running `go version`

## Step-by-Step Installation Process

### Step 1: Building the Backend Service

The native messaging backend is the core component that handles communication between the browser extension and your local file system. Navigate to the extracted extension directory and build the backend:

```bash
cd web-clipper-extension
chmod +x scripts/build.sh
./scripts/build.sh
```

The build script performs the following operations:
- Initializes the Go module if not already present
- Downloads required dependencies
- Compiles the backend binary with appropriate optimizations
- Creates the clips storage directory
- Sets proper file permissions

After successful compilation, you should see the `clipper-backend` executable in the `backend/` directory. The binary size is typically around 3-4 MB and includes all necessary dependencies for standalone operation.

### Step 2: Configuring Native Messaging

Native messaging enables secure communication between browser extensions and local applications. The Web Clipper Extension uses this mechanism to save clips directly to your file system without requiring network access or cloud services.

#### Automated Installation
The easiest method is to use the provided installation script:

```bash
chmod +x scripts/install-native-messaging.sh
./scripts/install-native-messaging.sh
```

This script automatically:
- Detects installed browsers (Chrome and Firefox)
- Creates appropriate configuration directories
- Updates manifest files with correct binary paths
- Sets proper permissions for native messaging hosts

#### Manual Installation

If you prefer manual installation or need to customize the setup, follow these browser-specific instructions:

##### Chrome/Chromium Configuration

Create the native messaging host directory:
```bash
# Linux
mkdir -p ~/.config/google-chrome/NativeMessagingHosts

# macOS
mkdir -p ~/Library/Application\ Support/Google/Chrome/NativeMessagingHosts

# Windows
mkdir "%LOCALAPPDATA%\Google\Chrome\User Data\NativeMessagingHosts"
```

Copy and configure the host manifest:
```bash
# Update the manifest with your installation path
sed "s|/path/to/clipper-backend|$(pwd)/backend/clipper-backend|g" \
    native-messaging/chrome-host.json > \
    ~/.config/google-chrome/NativeMessagingHosts/com.clipper.host.json
```

##### Firefox Configuration

Create the Firefox native messaging directory:
```bash
# Linux
mkdir -p ~/.mozilla/native-messaging-hosts

# macOS
mkdir -p ~/Library/Application\ Support/Mozilla/NativeMessagingHosts

# Windows
mkdir "%APPDATA%\Mozilla\NativeMessagingHosts"
```

Install the Firefox host manifest:
```bash
sed "s|/path/to/clipper-backend|$(pwd)/backend/clipper-backend|g" \
    native-messaging/firefox-host.json > \
    ~/.mozilla/native-messaging-hosts/com.clipper.host.json
```

### Step 3: Loading the Browser Extension

With the backend configured, the next step is to load the extension into your browser. Since this is a development extension not distributed through official stores, you'll need to enable developer mode.

#### Chrome Installation

1. Open Google Chrome and navigate to `chrome://extensions/`
2. Enable "Developer mode" using the toggle in the top-right corner
3. Click "Load unpacked" button
4. Navigate to the `extension/chrome/` directory in your installation
5. Select the directory and click "Open"

The extension should now appear in your extensions list with the Web Clipper icon. If you see any errors, check the console for detailed error messages and verify that all files are present in the extension directory.

#### Firefox Installation

Firefox handles unpacked extensions differently than Chrome:

1. Open Firefox and navigate to `about:debugging`
2. Click "This Firefox" in the left sidebar
3. Click "Load Temporary Add-on"
4. Navigate to `extension/firefox/manifest.json`
5. Select the manifest file and click "Open"

Note that Firefox temporary add-ons are removed when the browser restarts. For persistent installation, you would need to package the extension as an XPI file and install it through Firefox's add-on management system.

### Step 4: Verification and Testing

After completing the installation, it's crucial to verify that all components are working correctly. This testing process ensures that the extension can communicate with the backend and save clips successfully.

#### Backend Communication Test

First, test the native messaging backend directly:

```bash
cd backend
echo '{"action":"saveClip","data":{"timestamp":"2025-08-06T12:00:00Z","url":"https://example.com","title":"Test Clip","category":"TIL","selectedText":"Test content","note":"Installation test","pageTitle":"Test Page","domain":"example.com"}}' | ./clipper-backend
```

You should see a JSON response indicating success:
```json
{"success":true,"message":"Clip saved successfully"}
```

Check that a clip file was created:
```bash
ls -la clips/$(date +%Y-%m-%d)/
```

#### Browser Extension Test

1. Navigate to any webpage in your browser
2. Click the Web Clipper extension icon in the toolbar
3. Fill out the clip form with test data:
   - Title: "Installation Test"
   - Category: "TIL"
   - Notes: "Testing the extension installation"
4. Click "Save Clip"

If successful, you should see a confirmation message, and a new markdown file should appear in the `backend/clips/` directory organized by date.

#### Troubleshooting Common Issues

**Extension not appearing in browser:**
- Verify that developer mode is enabled
- Check that all extension files are present and readable
- Review browser console for JavaScript errors

**Native messaging errors:**
- Confirm that the backend binary is executable (`chmod +x clipper-backend`)
- Verify that host manifest paths point to the correct binary location
- Check that native messaging directories exist and are writable

**Permission denied errors:**
- Ensure the backend binary has execute permissions
- Verify that the clips directory is writable
- Check that native messaging manifests have correct file permissions

## Advanced Configuration Options

### Custom Storage Locations

By default, clips are saved in the `backend/clips/` directory. You can customize this location by modifying the backend source code or using environment variables:

```bash
export CLIPPER_STORAGE_PATH="/path/to/custom/clips/directory"
```

### Browser-Specific Customizations

#### Chrome Extension Permissions

The Chrome extension requests minimal permissions for optimal security:
- `activeTab`: Access to the currently active tab for content extraction
- `nativeMessaging`: Communication with the local backend service
- `storage`: Local storage for extension preferences

#### Firefox Extension Compatibility

The Firefox version uses Manifest V2 for broader compatibility:
- `tabs`: Access to tab information and content
- `nativeMessaging`: Backend communication capability
- `storage`: Local preference storage

### Security Considerations

The Web Clipper Extension is designed with privacy and security as primary concerns:

**Local Data Storage**: All clips are stored locally on your machine, never transmitted to external servers or cloud services.

**Native Messaging Security**: Communication between the browser and backend uses the browser's native messaging API, which provides process isolation and permission controls.

**Minimal Permissions**: The extension requests only the minimum permissions necessary for functionality, reducing potential attack surface.

**Open Source Transparency**: All source code is available for review, ensuring no hidden functionality or data collection.

## Maintenance and Updates

### Updating the Extension

To update to a newer version:

1. Download the latest release package
2. Stop any running backend processes
3. Extract the new version to your installation directory
4. Rebuild the backend using `./scripts/build.sh`
5. Reload the extension in your browser

### Backup and Migration

Your clips are stored as standard markdown files, making backup and migration straightforward:

```bash
# Create a backup
tar -czf clips-backup-$(date +%Y%m%d).tar.gz backend/clips/

# Restore from backup
tar -xzf clips-backup-YYYYMMDD.tar.gz
```

### Performance Optimization

For optimal performance with large numbers of clips:

- Regularly archive old clips to separate directories
- Consider using symbolic links for frequently accessed clip categories
- Monitor disk space usage in the clips directory

## Conclusion

The Web Clipper Extension installation process, while involving several steps, provides a robust and privacy-focused solution for web content management. The combination of browser extension technology with native messaging creates a powerful tool that operates entirely on your local machine.

By following this comprehensive guide, you should have a fully functional Web Clipper Extension that enables efficient capture and organization of web content while maintaining complete control over your data. The modular architecture ensures that the system can be customized and extended to meet specific workflow requirements.

For additional support, troubleshooting, or feature requests, consult the project documentation or community resources. The open-source nature of the project encourages community contributions and ensures long-term sustainability of the tool.

