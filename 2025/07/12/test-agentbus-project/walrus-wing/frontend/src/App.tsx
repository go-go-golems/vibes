import React from 'react';
import { Canvas } from '@react-three/fiber';
import { OrbitControls, Stats } from '@react-three/drei';
import styled from 'styled-components';
import WalrusWing3D from './components/WalrusWing3D';
import AerodynamicsPanel from './components/AerodynamicsPanel';
import ControlPanel from './components/ControlPanel';
import WindVisualization from './components/WindVisualization';

const AppContainer = styled.div`
  width: 100vw;
  height: 100vh;
  background: linear-gradient(135deg, #1e3c72 0%, #2a5298 100%);
  position: relative;
  overflow: hidden;
`;

const CanvasContainer = styled.div`
  width: 70%;
  height: 100%;
  position: absolute;
  left: 0;
  top: 0;
`;

const UIContainer = styled.div`
  width: 30%;
  height: 100%;
  position: absolute;
  right: 0;
  top: 0;
  background: rgba(20, 25, 40, 0.9);
  backdrop-filter: blur(10px);
  display: flex;
  flex-direction: column;
  padding: 20px;
  box-sizing: border-box;
`;

function App() {
  return (
    <AppContainer>
      <CanvasContainer>
        <Canvas camera={{ position: [5, 5, 5], fov: 75 }}>
          <ambientLight intensity={0.6} />
          <pointLight position={[10, 10, 10]} intensity={1} />
          <directionalLight position={[-10, 10, 5]} intensity={0.8} />
          
          <WalrusWing3D />
          <WindVisualization windSpeed={2} particleCount={500} />
          
          <OrbitControls 
            enablePan={true}
            enableZoom={true}
            enableRotate={true}
            maxDistance={20}
            minDistance={2}
          />
          <Stats />
        </Canvas>
      </CanvasContainer>
      
      <UIContainer>
        <h1 style={{ color: '#fff', marginBottom: '20px', fontSize: '24px' }}>
          🦭 Walrus Wing Aerodynamics
        </h1>
        <ControlPanel />
        <AerodynamicsPanel />
      </UIContainer>
    </AppContainer>
  );
}

export default App;
