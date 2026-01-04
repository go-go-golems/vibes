import React from 'react';
import Header from '../components/Header';
import './App.css';

function App() {
  return (
    <div className="App">
      <Header 
        title="Welcome to Test App" 
        subtitle="A modern React application with improved UI"
      />
      <main className="App-main">
        <section className="hero-section">
          <h2>Features</h2>
          <div className="features-grid">
            <div className="feature-card">
              <h3>Modern Design</h3>
              <p>Clean and responsive user interface</p>
            </div>
            <div className="feature-card">
              <h3>Fast Performance</h3>
              <p>Optimized for speed and efficiency</p>
            </div>
            <div className="feature-card">
              <h3>Easy to Use</h3>
              <p>Intuitive navigation and user experience</p>
            </div>
          </div>
        </section>
      </main>
    </div>
  );
}

export default App;

