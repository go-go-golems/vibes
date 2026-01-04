import React, { useState } from 'react';
import styled from 'styled-components';

const PanelContainer = styled.div`
  background: rgba(255, 255, 255, 0.1);
  border-radius: 10px;
  padding: 20px;
  margin-bottom: 20px;
  backdrop-filter: blur(5px);
  border: 1px solid rgba(255, 255, 255, 0.2);
`;

const ControlGroup = styled.div`
  margin-bottom: 15px;
`;

const Label = styled.label`
  display: block;
  color: #fff;
  font-size: 14px;
  margin-bottom: 5px;
  font-weight: 500;
`;

const Slider = styled.input`
  width: 100%;
  height: 6px;
  border-radius: 3px;
  background: rgba(255, 255, 255, 0.2);
  outline: none;
  -webkit-appearance: none;
  
  &::-webkit-slider-thumb {
    -webkit-appearance: none;
    width: 18px;
    height: 18px;
    border-radius: 50%;
    background: #4CAF50;
    cursor: pointer;
    border: 2px solid #fff;
  }
  
  &::-moz-range-thumb {
    width: 18px;
    height: 18px;
    border-radius: 50%;
    background: #4CAF50;
    cursor: pointer;
    border: 2px solid #fff;
  }
`;

const ValueDisplay = styled.span`
  color: #4CAF50;
  font-weight: bold;
  float: right;
`;

const Button = styled.button`
  background: linear-gradient(45deg, #4CAF50, #45a049);
  color: white;
  border: none;
  padding: 12px 24px;
  border-radius: 25px;
  cursor: pointer;
  font-size: 14px;
  font-weight: bold;
  width: 100%;
  margin-top: 10px;
  transition: all 0.3s ease;
  
  &:hover {
    transform: translateY(-2px);
    box-shadow: 0 4px 8px rgba(76, 175, 80, 0.3);
  }
  
  &:active {
    transform: translateY(0);
  }
`;

const ControlPanel: React.FC = () => {
  const [flapSpeed, setFlapSpeed] = useState(1);
  const [wingSpan, setWingSpan] = useState(2.5);
  const [airDensity, setAirDensity] = useState(1.225);
  const [windSpeed, setWindSpeed] = useState(0);
  const [isSimulating, setIsSimulating] = useState(false);

  const handleStartSimulation = () => {
    setIsSimulating(!isSimulating);
  };

  const resetToDefaults = () => {
    setFlapSpeed(1);
    setWingSpan(2.5);
    setAirDensity(1.225);
    setWindSpeed(0);
  };

  return (
    <PanelContainer>
      <h3 style={{ color: '#fff', marginTop: 0, marginBottom: '20px' }}>
        🎛️ Flight Controls
      </h3>
      
      <ControlGroup>
        <Label>
          Flap Speed (Hz)
          <ValueDisplay>{flapSpeed.toFixed(1)}</ValueDisplay>
        </Label>
        <Slider
          type="range"
          min="0.1"
          max="5"
          step="0.1"
          value={flapSpeed}
          onChange={(e) => setFlapSpeed(parseFloat(e.target.value))}
        />
      </ControlGroup>

      <ControlGroup>
        <Label>
          Wing Span (m)
          <ValueDisplay>{wingSpan.toFixed(1)}</ValueDisplay>
        </Label>
        <Slider
          type="range"
          min="1"
          max="5"
          step="0.1"
          value={wingSpan}
          onChange={(e) => setWingSpan(parseFloat(e.target.value))}
        />
      </ControlGroup>

      <ControlGroup>
        <Label>
          Air Density (kg/m³)
          <ValueDisplay>{airDensity.toFixed(3)}</ValueDisplay>
        </Label>
        <Slider
          type="range"
          min="0.5"
          max="2"
          step="0.001"
          value={airDensity}
          onChange={(e) => setAirDensity(parseFloat(e.target.value))}
        />
      </ControlGroup>

      <ControlGroup>
        <Label>
          Wind Speed (m/s)
          <ValueDisplay>{windSpeed.toFixed(1)}</ValueDisplay>
        </Label>
        <Slider
          type="range"
          min="0"
          max="20"
          step="0.5"
          value={windSpeed}
          onChange={(e) => setWindSpeed(parseFloat(e.target.value))}
        />
      </ControlGroup>

      <Button onClick={handleStartSimulation}>
        {isSimulating ? '⏸️ Pause Simulation' : '▶️ Start Simulation'}
      </Button>

      <Button 
        onClick={resetToDefaults}
        style={{ 
          background: 'linear-gradient(45deg, #ff6b6b, #ee5a5a)',
          marginTop: '10px'
        }}
      >
        🔄 Reset to Defaults
      </Button>
    </PanelContainer>
  );
};

export default ControlPanel;
