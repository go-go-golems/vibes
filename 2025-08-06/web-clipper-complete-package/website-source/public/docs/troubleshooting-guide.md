# Web Clipper Extension - Troubleshooting Guide

## Introduction

The Web Clipper Extension is designed for reliability and ease of use, but like any software system involving multiple components, users may occasionally encounter issues. This comprehensive troubleshooting guide addresses common problems, provides systematic diagnostic approaches, and offers solutions for various scenarios you might encounter.

This guide is organized by problem category and includes step-by-step diagnostic procedures, common solutions, and preventive measures to avoid future issues. Whether you're experiencing installation problems, communication errors, or performance issues, this guide will help you identify and resolve the problem quickly.

## Diagnostic Methodology

### Systematic Problem Identification

When encountering issues with the Web Clipper Extension, follow this systematic approach to identify the root cause:

**Step 1: Problem Isolation** - Determine which component is experiencing the issue. Is it the browser extension, the native messaging communication, or the backend service?

**Step 2: Error Message Analysis** - Collect and analyze any error messages displayed in the browser, extension popup, or system logs.

**Step 3: Component Testing** - Test each component individually to isolate the failing element.

**Step 4: Environment Verification** - Verify that your system environment meets all requirements and that configurations are correct.

**Step 5: Solution Application** - Apply targeted solutions based on the identified problem category.

### Information Gathering

Before attempting solutions, gather the following diagnostic information:

**System Information**: Operating system version, browser type and version, Go version (if applicable).

**Extension Status**: Whether the extension appears in the browser, any error badges or warnings, and the last successful clip operation.

**File System Status**: Permissions on the clips directory, available disk space, and recent file system changes.

**Process Status**: Whether the backend service is running, native messaging host configuration, and any related error messages.

## Installation and Setup Issues

### Extension Not Appearing in Browser

This is one of the most common initial issues, typically occurring during the extension loading process.

#### Chrome-Specific Issues

**Developer Mode Not Enabled**: The most frequent cause is that Developer Mode is not enabled in Chrome's extension management interface.

*Solution*: Navigate to `chrome://extensions/`, locate the "Developer mode" toggle in the top-right corner, and ensure it's enabled. The toggle should be blue/active, and you should see additional buttons like "Load unpacked" appear.

**Incorrect Directory Selection**: Users sometimes select the wrong directory when loading the unpacked extension.

*Solution*: Ensure you're selecting the `extension/chrome/` directory specifically, not the parent directory or the shared components directory. The selected directory must contain the `manifest.json` file.

**Manifest Validation Errors**: Chrome may reject the extension if the manifest file contains errors or unsupported features.

*Solution*: Check the Chrome extensions page for error messages. Common issues include missing files referenced in the manifest or permission conflicts. Verify that all files listed in the manifest exist in the extension directory.

#### Firefox-Specific Issues

**Temporary Add-on Limitations**: Firefox handles unpacked extensions as temporary add-ons, which have different behavior than permanent installations.

*Solution*: Remember that temporary add-ons in Firefox are removed when the browser restarts. This is normal behavior, not a bug. For persistent installation, you would need to package the extension as an XPI file.

**Manifest Version Compatibility**: Firefox uses Manifest V2, while Chrome uses Manifest V3, leading to potential compatibility issues if files are mixed.

*Solution*: Ensure you're loading the Firefox-specific manifest from the `extension/firefox/` directory, not the Chrome version.

### Native Messaging Configuration Problems

Native messaging configuration is often the source of communication issues between the browser and backend service.

#### Host Manifest Installation Issues

**Incorrect Installation Paths**: Native messaging hosts must be installed in specific directories that vary by operating system and browser.

*Chrome Linux*: `~/.config/google-chrome/NativeMessagingHosts/`
*Chrome macOS*: `~/Library/Application Support/Google/Chrome/NativeMessagingHosts/`
*Chrome Windows*: `%LOCALAPPDATA%\Google\Chrome\User Data\NativeMessagingHosts\`

*Firefox Linux*: `~/.mozilla/native-messaging-hosts/`
*Firefox macOS*: `~/Library/Application Support/Mozilla/NativeMessagingHosts/`
*Firefox Windows*: `%APPDATA%\Mozilla\NativeMessagingHosts\`

*Solution*: Verify that the host manifest files are installed in the correct directories for your operating system and browser combination. Use the provided installation script or manually copy files to the appropriate locations.

**Path Configuration Errors**: The host manifest must contain the correct absolute path to the backend binary.

*Solution*: Edit the host manifest files to ensure the "path" field points to the actual location of your `clipper-backend` executable. Use absolute paths, not relative paths or shell shortcuts like `~`.

**Permission Issues**: The host manifest files and backend binary must have appropriate permissions.

*Solution*: Ensure the backend binary is executable (`chmod +x clipper-backend` on Unix systems) and that the host manifest files are readable by the browser process.

#### Backend Binary Issues

**Go Build Failures**: The backend service may fail to compile due to missing dependencies or Go environment issues.

*Solution*: Verify that Go is properly installed and accessible in your PATH. Run `go version` to confirm. If the build fails, check for error messages about missing modules or compilation errors. Ensure you're in the correct directory when running the build command.

**Binary Execution Problems**: The compiled binary may fail to run due to missing system dependencies or permission issues.

*Solution*: Test the backend binary directly by running it from the command line. If it fails to start, check for missing shared libraries or permission issues. On some systems, you may need to install additional runtime dependencies.

## Communication and Messaging Issues

### Extension Popup Not Responding

When the extension icon appears but clicking it doesn't open the popup or the popup appears empty, this typically indicates communication or loading issues.

#### JavaScript Errors

**Content Script Loading Failures**: The content script may fail to load on certain websites due to Content Security Policy restrictions or JavaScript conflicts.

*Solution*: Open the browser's developer tools, navigate to the Console tab, and look for JavaScript errors related to the Web Clipper Extension. Common issues include CSP violations or conflicts with other extensions.

**Popup Script Errors**: The popup interface may encounter JavaScript errors that prevent proper rendering.

*Solution*: Right-click on the extension icon and select "Inspect popup" (Chrome) or use the browser's extension debugging tools to examine the popup's console for error messages.

#### Resource Loading Issues

**Missing Extension Files**: Some extension files may be missing or corrupted, preventing proper loading.

*Solution*: Verify that all files are present in the extension directory by comparing with the original package contents. Re-extract the extension files if necessary.

**Browser Cache Issues**: Browser caching may serve outdated extension files after updates.

*Solution*: Disable and re-enable the extension, or use the browser's extension reload functionality. In Chrome, you can click the reload button on the extension's card in the extensions management page.

### Native Messaging Communication Failures

Communication failures between the browser extension and backend service are often the most complex issues to diagnose.

#### Backend Service Status

**Service Not Running**: The backend service may not be running or may have crashed.

*Solution*: Check if the backend process is running using system monitoring tools (`ps aux | grep clipper-backend` on Unix systems or Task Manager on Windows). If not running, start it manually and check for startup errors.

**Port or Socket Issues**: Native messaging uses stdin/stdout communication, but system-level issues can interfere.

*Solution*: Test the backend service directly by sending a test message via command line:
```bash
echo '{"action":"saveClip","data":{"timestamp":"2025-08-06T12:00:00Z","url":"https://test.com","title":"Test","category":"TIL","selectedText":"","note":"","pageTitle":"Test","domain":"test.com"}}' | ./clipper-backend
```

#### Message Format Issues

**JSON Parsing Errors**: Malformed messages can cause communication failures.

*Solution*: Enable debug logging in both the extension and backend to examine the exact messages being sent and received. Look for JSON syntax errors or missing required fields.

**Message Size Limits**: Extremely large messages may exceed native messaging size limits.

*Solution*: If clipping very large text selections, try clipping smaller portions or check if the backend properly handles message size limits.

## File System and Storage Issues

### Permission and Access Problems

File system issues often manifest as clips not being saved or error messages about file access.

#### Directory Creation Failures

**Parent Directory Permissions**: The backend may lack permissions to create the clips directory structure.

*Solution*: Verify that the user account running the backend service has write permissions to the intended clips directory. Create the directory manually if necessary and set appropriate permissions.

**Disk Space Issues**: Insufficient disk space can prevent file creation.

*Solution*: Check available disk space using system tools (`df -h` on Unix systems or File Explorer properties on Windows). Free up space if necessary or configure the backend to use a different storage location.

#### File Writing Problems

**File Locking Issues**: Other processes may have locks on clip files, preventing updates.

*Solution*: Check if any text editors or other applications have clip files open. Close these applications and retry the clipping operation.

**Filename Conflicts**: Although rare due to timestamp-based naming, filename conflicts can occur.

*Solution*: The backend should handle filename conflicts automatically by appending numbers to duplicate names. If this fails, check the backend logs for specific error messages.

### Storage Organization Issues

**Date Directory Problems**: The automatic date-based directory creation may fail under certain conditions.

*Solution*: Verify that the system date and time are correct, as the backend uses the current date for directory organization. Check that the date format is compatible with your file system's naming restrictions.

**File Encoding Issues**: Text encoding problems can cause display issues when viewing saved clips.

*Solution*: The backend saves files in UTF-8 encoding by default. If you encounter encoding issues, verify that your text editor or file viewer supports UTF-8 encoding.

## Performance and Resource Issues

### Slow Response Times

Performance issues can affect the user experience and may indicate underlying system problems.

#### Memory Usage Problems

**High Memory Consumption**: The backend service may consume excessive memory with large clip collections.

*Solution*: Monitor memory usage using system tools. If memory consumption is high, consider archiving older clips or implementing memory optimization strategies in the backend configuration.

**Browser Memory Issues**: The browser extension may consume excessive memory, particularly with large text selections.

*Solution*: Restart the browser to clear memory usage. If the problem persists, consider clipping smaller text selections or breaking large clips into multiple smaller ones.

#### Disk I/O Performance

**Slow File Operations**: File writing operations may be slow on certain storage systems.

*Solution*: Consider moving the clips directory to faster storage (SSD instead of HDD) or implementing batch writing strategies for multiple clips.

**Network Storage Issues**: If clips are stored on network-attached storage, network latency can affect performance.

*Solution*: Use local storage for clips when possible, or ensure that network storage has adequate performance characteristics for frequent small file operations.

## Browser-Specific Issues

### Chrome-Specific Problems

**Manifest V3 Compatibility**: Chrome's transition to Manifest V3 introduces new restrictions and requirements.

*Solution*: Ensure you're using the Chrome-specific version of the extension, which is designed for Manifest V3 compatibility. Avoid mixing Chrome and Firefox extension files.

**Service Worker Issues**: Chrome extensions use service workers instead of background pages, which can affect extension lifecycle.

*Solution*: If the extension becomes unresponsive, try disabling and re-enabling it to restart the service worker. Check the service worker console for error messages.

### Firefox-Specific Problems

**Temporary Add-on Persistence**: Firefox temporary add-ons are removed on browser restart.

*Solution*: This is expected behavior for development installations. For permanent installation, the extension would need to be packaged and signed through Mozilla's add-on distribution system.

**Permission Handling Differences**: Firefox handles extension permissions differently than Chrome.

*Solution*: Verify that all required permissions are granted in Firefox's add-on management interface. Some permissions may require explicit user approval.

## Advanced Troubleshooting Techniques

### Log Analysis and Debugging

**Extension Console Logging**: Enable detailed logging in the browser extension to trace execution flow and identify issues.

*Method*: Open browser developer tools, navigate to the Extensions or Add-ons debugging section, and examine console output from the Web Clipper Extension components.

**Backend Service Logging**: Implement or enable detailed logging in the Go backend service.

*Method*: Modify the backend service to output detailed logs about message processing, file operations, and error conditions. Use structured logging to make analysis easier.

**System-Level Monitoring**: Use system monitoring tools to observe resource usage and process behavior.

*Method*: Monitor CPU usage, memory consumption, disk I/O, and process status using tools like `htop`, `iotop`, or Windows Task Manager.

### Network and Communication Analysis

**Message Tracing**: Capture and analyze the messages exchanged between extension and backend.

*Method*: Implement message logging in both components to trace the exact content and timing of communication. Look for message format issues, timing problems, or unexpected message sequences.

**Process Communication Monitoring**: Monitor the native messaging communication channel for issues.

*Method*: Use system tools to monitor process communication, stdin/stdout usage, and inter-process communication patterns.

## Preventive Measures and Best Practices

### Regular Maintenance

**Extension Updates**: Keep the extension updated to the latest version to benefit from bug fixes and improvements.

**System Updates**: Maintain current operating system and browser versions to ensure compatibility and security.

**Dependency Management**: Keep Go and other development dependencies updated if you're building from source.

### Backup and Recovery

**Clip Backup Strategies**: Implement regular backup procedures for your clip collection.

*Method*: Create automated backups of the clips directory using system backup tools or custom scripts. Store backups in multiple locations to prevent data loss.

**Configuration Backup**: Backup extension configurations and native messaging host settings.

*Method*: Document your installation configuration and backup any customized settings or configurations.

### Monitoring and Alerting

**Usage Monitoring**: Monitor extension usage patterns to identify potential issues before they become problems.

**Error Tracking**: Implement error tracking to identify recurring issues or patterns in error occurrence.

**Performance Monitoring**: Track performance metrics like clip save times, file sizes, and resource usage to identify degradation over time.

## Getting Additional Help

### Community Resources

**Documentation Review**: Consult the complete documentation set for detailed information about specific features or configurations.

**Issue Reporting**: Report bugs or issues through the project's issue tracking system, including detailed diagnostic information and reproduction steps.

### Professional Support

**System Administration**: For enterprise or organizational deployments, consider involving system administrators who can provide environment-specific expertise.

**Development Support**: For customization or integration needs, consider engaging with developers familiar with browser extension development and Go programming.

## Conclusion

The Web Clipper Extension is designed for reliability and ease of use, but the complexity of modern browser environments and system configurations can occasionally lead to issues. This troubleshooting guide provides systematic approaches to identifying and resolving common problems.

Remember that most issues fall into a few common categories: installation and configuration problems, communication failures, or file system issues. By following the diagnostic methodology outlined in this guide and applying targeted solutions, you should be able to resolve most problems quickly and effectively.

When encountering persistent issues, don't hesitate to seek help from community resources or professional support. The modular architecture of the Web Clipper Extension makes it possible to isolate and resolve issues without affecting your existing clip collection or losing valuable data.

Regular maintenance, proper backup procedures, and staying current with updates will help prevent many issues from occurring in the first place. By following the best practices outlined in this guide, you can ensure reliable, long-term operation of your Web Clipper Extension installation.

