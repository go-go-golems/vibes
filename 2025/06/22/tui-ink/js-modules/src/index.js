import React, { useState, useEffect } from 'react';
import { render, Text, Box, useInput, useApp } from 'ink';

const Counter = () => {
  const [count, setCount] = useState(0);
  const { exit } = useApp();

  useInput((input, key) => {
    if (input === '+') {
      setCount(count + 1);
    } else if (input === '-') {
      setCount(count - 1);
    } else if (input === 'q' || key.escape) {
      exit();
    }
  });

  return (
    <Box flexDirection="column" padding={1}>
      <Text color="cyan" bold>
        🚀 Ink.js + Goja Test TUI
      </Text>
      <Text>
        Counter: <Text color="green" bold>{count}</Text>
      </Text>
      <Text color="gray">
        Press '+' to increment, '-' to decrement, 'q' to quit
      </Text>
    </Box>
  );
};

const App = () => {
  return <Counter />;
};

// Export for goja
const InkApp = {
  render: () => {
    return render(<App />);
  },
  App: App,
  Counter: Counter
};

// For webpack
if (typeof module !== 'undefined' && module.exports) {
  module.exports = InkApp;
}

// For browser/goja global
if (typeof window !== 'undefined') {
  window.InkApp = InkApp;
} else if (typeof global !== 'undefined') {
  global.InkApp = InkApp;
}

