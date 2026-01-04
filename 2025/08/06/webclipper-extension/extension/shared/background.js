// Background script for Web Clipper extension

// Native messaging host name
const NATIVE_HOST = 'com.clipper.host';

// Handle messages from popup
chrome.runtime.onMessage.addListener(function(request, sender, sendResponse) {
  if (request.action === 'saveClip') {
    saveClipViaNativeMessaging(request.data)
      .then(result => {
        sendResponse({ success: true, result: result });
      })
      .catch(error => {
        console.error('Error saving clip:', error);
        sendResponse({ success: false, error: error.message });
      });
    
    return true; // Keep message channel open for async response
  }
});

async function saveClipViaNativeMessaging(clipData) {
  return new Promise((resolve, reject) => {
    try {
      // Connect to native messaging host
      const port = chrome.runtime.connectNative(NATIVE_HOST);
      
      let responseReceived = false;
      
      // Handle response from native host
      port.onMessage.addListener(function(response) {
        responseReceived = true;
        if (response.success) {
          resolve(response);
        } else {
          reject(new Error(response.error || 'Unknown error from native host'));
        }
      });
      
      // Handle connection errors
      port.onDisconnect.addListener(function() {
        if (!responseReceived) {
          const error = chrome.runtime.lastError;
          if (error) {
            reject(new Error(`Native messaging error: ${error.message}`));
          } else {
            reject(new Error('Native messaging host disconnected unexpectedly'));
          }
        }
      });
      
      // Send clip data to native host
      port.postMessage({
        action: 'saveClip',
        data: clipData
      });
      
    } catch (error) {
      reject(new Error(`Failed to connect to native host: ${error.message}`));
    }
  });
}

// Optional: Handle extension installation/startup
chrome.runtime.onInstalled.addListener(function(details) {
  if (details.reason === 'install') {
    console.log('Web Clipper extension installed');
  } else if (details.reason === 'update') {
    console.log('Web Clipper extension updated');
  }
});

// Optional: Handle extension startup
chrome.runtime.onStartup.addListener(function() {
  console.log('Web Clipper extension started');
});

