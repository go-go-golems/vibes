// Content script for Web Clipper extension
(function() {
  'use strict';

  let lastSelectedText = '';

  // Listen for text selection changes
  document.addEventListener('selectionchange', function() {
    const selection = window.getSelection();
    lastSelectedText = selection.toString().trim();
  });

  // Listen for messages from popup
  chrome.runtime.onMessage.addListener(function(request, sender, sendResponse) {
    if (request.action === 'getSelectedText') {
      // Get current selection
      const selection = window.getSelection();
      const selectedText = selection.toString().trim();
      
      sendResponse({
        selectedText: selectedText || lastSelectedText,
        pageTitle: document.title,
        url: window.location.href
      });
    }
    
    return true; // Keep message channel open for async response
  });

  // Optional: Add visual feedback for selection (could be useful for debugging)
  if (false) { // Set to true for debugging
    document.addEventListener('mouseup', function() {
      const selection = window.getSelection();
      const selectedText = selection.toString().trim();
      
      if (selectedText) {
        console.log('Text selected:', selectedText.substring(0, 100) + (selectedText.length > 100 ? '...' : ''));
      }
    });
  }
})();

