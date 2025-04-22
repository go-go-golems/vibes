// Background script for H1 Tracker extension
// Handles communication between content script and server

// Server configuration
const SERVER_URL = "http://localhost:8080";
const BATCH_SIZE = 5;
const BATCH_TIMEOUT = 10000; // 10 seconds

// Store for collected H1 headings
let pendingEvents = [];
let batchTimer = null;

// Listen for messages from content script
browser.runtime.onMessage.addListener((message, sender) => {
  if (message.type === "h1_found") {
    // Add URL information to the event
    const event = {
      url: sender.tab.url,
      text: message.text,
      timestamp: new Date().toISOString()
    };
    
    // Add to pending events
    pendingEvents.push(event);
    
    // If we've reached batch size, send immediately
    if (pendingEvents.length >= BATCH_SIZE) {
      sendBatchToServer();
    } 
    // Otherwise, set a timer to send after timeout
    else if (!batchTimer) {
      batchTimer = setTimeout(sendBatchToServer, BATCH_TIMEOUT);
    }
    
    return Promise.resolve({status: "received"});
  }
});

// Function to send batch of events to server
function sendBatchToServer() {
  // Clear the timer
  if (batchTimer) {
    clearTimeout(batchTimer);
    batchTimer = null;
  }
  
  // If no events, do nothing
  if (pendingEvents.length === 0) {
    return;
  }
  
  // Create a copy of current events
  const eventsToSend = [...pendingEvents];
  pendingEvents = [];
  
  // Determine if we should use batch or single endpoint
  let url, data;
  if (eventsToSend.length > 1) {
    url = `${SERVER_URL}/batch`;
    data = { events: eventsToSend };
  } else {
    url = `${SERVER_URL}/h1`;
    data = eventsToSend[0];
  }
  
  // Send to server
  fetch(url, {
    method: "POST",
    headers: {
      "Content-Type": "application/json"
    },
    body: JSON.stringify(data)
  })
  .then(response => response.json())
  .then(result => {
    // Show notification on success
    browser.notifications.create({
      type: "basic",
      iconUrl: browser.runtime.getURL("icons/h1-tracker-48.png"),
      title: "H1 Tracker",
      message: `Successfully sent ${eventsToSend.length} H1 heading(s) to server`
    });
    console.log("Sent to server:", result);
  })
  .catch(error => {
    // Put events back in queue if failed
    pendingEvents = [...eventsToSend, ...pendingEvents];
    console.error("Error sending to server:", error);
    
    // Show error notification
    browser.notifications.create({
      type: "basic",
      iconUrl: browser.runtime.getURL("icons/h1-tracker-48.png"),
      title: "H1 Tracker - Error",
      message: "Failed to send H1 headings to server. Will retry later."
    });
  });
}

// Send any remaining events when browser is closing
browser.runtime.onSuspend.addListener(() => {
  if (pendingEvents.length > 0) {
    sendBatchToServer();
  }
});
