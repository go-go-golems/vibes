import React, { useRef, useMemo } from 'react';
import { useFrame } from '@react-three/fiber';
import { Points, PointsMaterial } from 'three';
import * as THREE from 'three';

interface WindVisualizationProps {
  windSpeed?: number;
  particleCount?: number;
}

const WindVisualization: React.FC<WindVisualizationProps> = ({
  windSpeed = 5,
  particleCount = 1000
}) => {
  const pointsRef = useRef<Points>(null);

  const particlePositions = useMemo(() => {
    const positions = new Float32Array(particleCount * 3);
    
    for (let i = 0; i < particleCount; i++) {
      const i3 = i * 3;
      positions[i3] = (Math.random() - 0.5) * 20; // x
      positions[i3 + 1] = (Math.random() - 0.5) * 10; // y
      positions[i3 + 2] = (Math.random() - 0.5) * 20; // z
    }
    
    return positions;
  }, [particleCount]);

  const particleGeometry = useMemo(() => {
    const geometry = new THREE.BufferGeometry();
    geometry.setAttribute('position', new THREE.BufferAttribute(particlePositions, 3));
    return geometry;
  }, [particlePositions]);

  useFrame((state, delta) => {
    if (pointsRef.current) {
      const positions = pointsRef.current.geometry.attributes.position.array as Float32Array;
      
      for (let i = 0; i < particleCount; i++) {
        const i3 = i * 3;
        
        // Move particles in wind direction (positive Z)
        positions[i3 + 2] += windSpeed * delta;
        
        // Add some turbulence
        positions[i3] += Math.sin(state.clock.elapsedTime + i) * 0.01;
        positions[i3 + 1] += Math.cos(state.clock.elapsedTime + i) * 0.01;
        
        // Reset particles that have moved too far
        if (positions[i3 + 2] > 10) {
          positions[i3 + 2] = -10;
          positions[i3] = (Math.random() - 0.5) * 20;
          positions[i3 + 1] = (Math.random() - 0.5) * 10;
        }
      }
      
      pointsRef.current.geometry.attributes.position.needsUpdate = true;
    }
  });

  return (
    <points ref={pointsRef} geometry={particleGeometry}>
      <pointsMaterial
        size={0.05}
        color="#00ffff"
        transparent
        opacity={0.6}
        vertexColors={false}
        sizeAttenuation={true}
      />
    </points>
  );
};

export default WindVisualization;
