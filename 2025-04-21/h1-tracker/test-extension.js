// Simulated test script for Firefox extension
// This script simulates the behavior of the extension on different websites

// Sample H1 headings from popular news sites
const testCases = [
  {
    url: "https://www.nytimes.com",
    h1: "The New York Times"
  },
  {
    url: "https://www.bbc.com/news",
    h1: "BBC News"
  },
  {
    url: "https://www.cnn.com",
    h1: "CNN - Breaking News, Latest News and Videos"
  },
  {
    url: "https://www.theguardian.com",
    h1: "News, sport and opinion from the Guardian's global edition"
  },
  {
    url: "https://www.reuters.com",
    h1: "Reuters | Breaking International News & Views"
  }
];

// Function to simulate sending a single H1 event
async function simulateSingleEvent(testCase) {
  const response = await fetch("http://localhost:8080/h1", {
    method: "POST",
    headers: {
      "Content-Type": "application/json"
    },
    body: JSON.stringify({
      url: testCase.url,
      text: testCase.h1
    })
  });
  
  return response.json();
}

// Function to simulate sending batch events
async function simulateBatchEvents(testCases) {
  const events = testCases.map(tc => ({
    url: tc.url,
    text: tc.h1
  }));
  
  const response = await fetch("http://localhost:8080/batch", {
    method: "POST",
    headers: {
      "Content-Type": "application/json"
    },
    body: JSON.stringify({
      events: events
    })
  });
  
  return response.json();
}

// Run the tests
async function runTests() {
  console.log("Starting extension simulation tests...");
  
  // Test 1: Send individual events
  console.log("\nTest 1: Sending individual H1 events");
  for (const testCase of testCases) {
    try {
      const result = await simulateSingleEvent(testCase);
      console.log(`✓ Sent H1 from ${testCase.url}: "${testCase.h1}" - Result:`, result);
    } catch (error) {
      console.error(`✗ Failed to send H1 from ${testCase.url}:`, error);
    }
  }
  
  // Test 2: Send batch events
  console.log("\nTest 2: Sending batch of H1 events");
  try {
    const result = await simulateBatchEvents(testCases);
    console.log(`✓ Sent batch of ${testCases.length} H1 events - Result:`, result);
  } catch (error) {
    console.error("✗ Failed to send batch events:", error);
  }
  
  console.log("\nTests completed!");
}

// Execute the tests
runTests();
