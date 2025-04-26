import React from 'react';
import { Provider } from 'react-redux';
import store from './store';
import TaskNetworkFlow from './components/TaskNetworkFlow';
import ControlPanel from './components/ControlPanel';
import './App.css';

function App() {
  return (
    <Provider store={store}>
      <div className="App">
        <header className="App-header">
          <h1>Hierarchical Task Planning Network</h1>
        </header>
        <main>
          <ControlPanel />
          <TaskNetworkFlow />
        </main>
        <footer>
          <p>Demo application for visualizing agent task planning networks</p>
        </footer>
      </div>
    </Provider>
  );
}

export default App;
