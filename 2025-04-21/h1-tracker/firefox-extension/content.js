// Content script for H1 Tracker extension
// Detects H1 elements on web pages and sends them to background script

// Function to process an H1 element
function processH1Element(h1) {
  // Extract text content
  const text = h1.textContent.trim();
  
  // Skip empty headings
  if (!text) return;
  
  // Send to background script
  browser.runtime.sendMessage({
    type: "h1_found",
    text: text
  }).then(response => {
    console.log("H1 heading sent:", text, response);
  }).catch(error => {
    console.error("Error sending H1 heading:", error);
  });
}

// Process all existing H1 elements when page loads
function processExistingH1s() {
  const h1Elements = document.querySelectorAll('h1');
  h1Elements.forEach(processH1Element);
}

// Set up MutationObserver to detect dynamically added H1 elements
function setupMutationObserver() {
  // Create a new observer
  const observer = new MutationObserver(mutations => {
    mutations.forEach(mutation => {
      // Check for added nodes
      if (mutation.addedNodes && mutation.addedNodes.length > 0) {
        for (let node of mutation.addedNodes) {
          // Check if the added node is an H1
          if (node.nodeName && node.nodeName.toLowerCase() === 'h1') {
            processH1Element(node);
          }
          
          // Check if the added node contains H1 elements
          if (node.querySelectorAll) {
            const h1s = node.querySelectorAll('h1');
            h1s.forEach(processH1Element);
          }
        }
      }
    });
  });
  
  // Start observing the document with the configured parameters
  observer.observe(document.body, {
    childList: true,
    subtree: true
  });
  
  return observer;
}

// Initialize when the page is loaded
document.addEventListener('DOMContentLoaded', () => {
  // Process existing H1s
  processExistingH1s();
  
  // Set up observer for future H1s
  const observer = setupMutationObserver();
  
  // Clean up observer when page is unloaded
  window.addEventListener('unload', () => {
    if (observer) {
      observer.disconnect();
    }
  });
});

// Run immediately in case DOMContentLoaded already fired
if (document.readyState === 'interactive' || document.readyState === 'complete') {
  processExistingH1s();
  setupMutationObserver();
}
