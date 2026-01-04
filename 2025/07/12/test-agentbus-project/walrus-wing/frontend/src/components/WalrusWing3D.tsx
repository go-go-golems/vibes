import React, { useRef, useState, useMemo } from 'react';
import { useFrame } from '@react-three/fiber';
import { Mesh, Vector3, BufferGeometry, Float32BufferAttribute } from 'three';
import { Text } from '@react-three/drei';

interface WalrusWingProps {
  wingSpan?: number;
  flapSpeed?: number;
  airDensity?: number;
  windSpeed?: number;
}

const WalrusWing3D: React.FC<WalrusWingProps> = ({
  wingSpan = 2.5,
  flapSpeed = 1,
  airDensity = 1.225,
  windSpeed = 0
}) => {
  const leftWingRef = useRef<Mesh>(null);
  const rightWingRef = useRef<Mesh>(null);
  const [time, setTime] = useState(0);

  // Generate wing geometry using bezier curves for realistic shape
  const wingGeometry = useMemo(() => {
    const geometry = new BufferGeometry();
    const vertices = [];
    const indices = [];
    
    // Create wing surface with realistic walrus flipper shape
    const wingWidth = wingSpan;
    const wingLength = wingSpan * 0.8;
    const segments = 20;
    
    for (let i = 0; i <= segments; i++) {
      for (let j = 0; j <= segments; j++) {
        const u = i / segments;
        const v = j / segments;
        
        // Walrus flipper shape - wider at base, tapered at tip
        const baseWidth = wingWidth * (1 - u * 0.7);
        const thickness = 0.1 * Math.sin(Math.PI * u) * (1 - v * 0.5);
        
        const x = (v - 0.5) * baseWidth;
        const y = thickness * Math.sin(Math.PI * v);
        const z = u * wingLength;
        
        vertices.push(x, y, z);
        
        if (i < segments && j < segments) {
          const a = i * (segments + 1) + j;
          const b = (i + 1) * (segments + 1) + j;
          const c = (i + 1) * (segments + 1) + (j + 1);
          const d = i * (segments + 1) + (j + 1);
          
          indices.push(a, b, d);
          indices.push(b, c, d);
        }
      }
    }
    
    geometry.setIndex(indices);
    geometry.setAttribute('position', new Float32BufferAttribute(vertices, 3));
    geometry.computeVertexNormals();
    
    return geometry;
  }, [wingSpan]);

  // Calculate aerodynamic forces
  const calculateLift = (flapAngle: number) => {
    const angleOfAttack = Math.abs(flapAngle) * 0.5;
    const liftCoefficient = 2 * Math.sin(angleOfAttack) * Math.cos(angleOfAttack);
    return 0.5 * airDensity * Math.pow(windSpeed + flapSpeed, 2) * wingSpan * liftCoefficient;
  };

  useFrame((state, delta) => {
    setTime(time + delta);
    
    if (leftWingRef.current && rightWingRef.current) {
      // Realistic wing flapping motion
      const flapAngle = Math.sin(time * flapSpeed * 2) * 0.8;
      const upstroke = Math.sin(time * flapSpeed * 2 + Math.PI) * 0.3;
      
      // Left wing (negative rotation)
      leftWingRef.current.rotation.z = flapAngle;
      leftWingRef.current.rotation.x = upstroke;
      leftWingRef.current.position.y = Math.abs(flapAngle) * 0.2;
      
      // Right wing (positive rotation)
      rightWingRef.current.rotation.z = -flapAngle;
      rightWingRef.current.rotation.x = upstroke;
      rightWingRef.current.position.y = Math.abs(flapAngle) * 0.2;
    }
  });

  return (
    <group>
      {/* Walrus body */}
      <mesh position={[0, 0, 0]}>
        <cylinderGeometry args={[0.8, 1.2, 3, 16]} />
        <meshStandardMaterial color="#8B4513" />
      </mesh>
      
      {/* Left wing */}
      <mesh
        ref={leftWingRef}
        position={[-1.5, 0, 0]}
        geometry={wingGeometry}
      >
        <meshStandardMaterial 
          color="#654321" 
          transparent 
          opacity={0.9}
          roughness={0.8}
        />
      </mesh>
      
      {/* Right wing */}
      <mesh
        ref={rightWingRef}
        position={[1.5, 0, 0]}
        geometry={wingGeometry}
        scale={[-1, 1, 1]}
      >
        <meshStandardMaterial 
          color="#654321" 
          transparent 
          opacity={0.9}
          roughness={0.8}
        />
      </mesh>
      
      {/* Airflow visualization */}
      <group>
        {Array.from({ length: 10 }, (_, i) => (
          <mesh key={i} position={[0, 0, -5 + i * 0.5]}>
            <sphereGeometry args={[0.02]} />
            <meshBasicMaterial color="#00ffff" />
          </mesh>
        ))}
      </group>
      
      {/* Force vectors display */}
      <Text
        position={[0, 3, 0]}
        fontSize={0.3}
        color="white"
        anchorX="center"
        anchorY="middle"
      >
        Lift: {calculateLift(Math.sin(time * flapSpeed * 2)).toFixed(2)}N
      </Text>
    </group>
  );
};

export default WalrusWing3D;
