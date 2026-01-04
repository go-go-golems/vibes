import React, { useState, useEffect } from 'react';
import styled from 'styled-components';

const PanelContainer = styled.div`
  background: rgba(255, 255, 255, 0.1);
  border-radius: 10px;
  padding: 20px;
  margin-bottom: 20px;
  backdrop-filter: blur(5px);
  border: 1px solid rgba(255, 255, 255, 0.2);
  flex: 1;
`;

const DataGrid = styled.div`
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 15px;
  margin-bottom: 20px;
`;

const DataCard = styled.div`
  background: rgba(0, 0, 0, 0.3);
  border-radius: 8px;
  padding: 15px;
  text-align: center;
`;

const DataLabel = styled.div`
  color: #ccc;
  font-size: 12px;
  margin-bottom: 5px;
  text-transform: uppercase;
  letter-spacing: 1px;
`;

const DataValue = styled.div`
  color: #4CAF50;
  font-size: 18px;
  font-weight: bold;
`;

const DataUnit = styled.span`
  color: #888;
  font-size: 12px;
  font-weight: normal;
`;

const StatusIndicator = styled.div<{ status: 'good' | 'warning' | 'danger' }>`
  width: 12px;
  height: 12px;
  border-radius: 50%;
  background-color: ${props => 
    props.status === 'good' ? '#4CAF50' :
    props.status === 'warning' ? '#FF9800' : '#F44336'
  };
  display: inline-block;
  margin-right: 8px;
`;

const PerformanceBar = styled.div`
  width: 100%;
  height: 8px;
  background: rgba(255, 255, 255, 0.2);
  border-radius: 4px;
  overflow: hidden;
  margin-top: 10px;
`;

const PerformanceFill = styled.div<{ percentage: number }>`
  height: 100%;
  width: ${props => props.percentage}%;
  background: linear-gradient(90deg, #4CAF50, #8BC34A);
  transition: width 0.3s ease;
`;

const AerodynamicsPanel: React.FC = () => {
  const [liftForce, setLiftForce] = useState(0);
  const [dragForce, setDragForce] = useState(0);
  const [efficiency, setEfficiency] = useState(0);
  const [powerRequired, setPowerRequired] = useState(0);
  const [angleOfAttack, setAngleOfAttack] = useState(0);
  const [reynoldsNumber, setReynoldsNumber] = useState(0);

  // Simulate real-time aerodynamics data
  useEffect(() => {
    const interval = setInterval(() => {
      const time = Date.now() / 1000;
      const flapCycle = Math.sin(time * 2);
      
      // Realistic aerodynamic calculations
      setLiftForce(15 + Math.abs(flapCycle) * 8 + Math.random() * 2);
      setDragForce(3 + Math.abs(flapCycle) * 2 + Math.random() * 0.5);
      setAngleOfAttack(flapCycle * 25 + Math.random() * 2);
      setPowerRequired(50 + Math.abs(flapCycle) * 30 + Math.random() * 5);
      setReynoldsNumber(150000 + Math.random() * 10000);
      
      const currentEfficiency = liftForce / (dragForce + 0.1);
      setEfficiency(currentEfficiency);
    }, 100);

    return () => clearInterval(interval);
  }, [liftForce, dragForce]);

  const getStatusForEfficiency = (eff: number) => {
    if (eff > 8) return 'good';
    if (eff > 5) return 'warning';
    return 'danger';
  };

  return (
    <PanelContainer>
      <h3 style={{ color: '#fff', marginTop: 0, marginBottom: '20px' }}>
        📊 Aerodynamics Data
      </h3>
      
      <DataGrid>
        <DataCard>
          <DataLabel>Lift Force</DataLabel>
          <DataValue>
            {liftForce.toFixed(1)} <DataUnit>N</DataUnit>
          </DataValue>
        </DataCard>
        
        <DataCard>
          <DataLabel>Drag Force</DataLabel>
          <DataValue>
            {dragForce.toFixed(1)} <DataUnit>N</DataUnit>
          </DataValue>
        </DataCard>
        
        <DataCard>
          <DataLabel>Power Required</DataLabel>
          <DataValue>
            {powerRequired.toFixed(0)} <DataUnit>W</DataUnit>
          </DataValue>
        </DataCard>
        
        <DataCard>
          <DataLabel>Angle of Attack</DataLabel>
          <DataValue>
            {angleOfAttack.toFixed(1)}<DataUnit>°</DataUnit>
          </DataValue>
        </DataCard>
      </DataGrid>

      <div style={{ marginBottom: '15px' }}>
        <DataLabel style={{ marginBottom: '10px' }}>
          <StatusIndicator status={getStatusForEfficiency(efficiency)} />
          L/D Ratio: {efficiency.toFixed(2)}
        </DataLabel>
        <PerformanceBar>
          <PerformanceFill percentage={Math.min((efficiency / 10) * 100, 100)} />
        </PerformanceBar>
      </div>

      <div style={{ marginBottom: '15px' }}>
        <DataLabel style={{ marginBottom: '10px' }}>
          Reynolds Number: {reynoldsNumber.toFixed(0)}
        </DataLabel>
        <PerformanceBar>
          <PerformanceFill percentage={(reynoldsNumber / 200000) * 100} />
        </PerformanceBar>
      </div>

      <div style={{ 
        background: 'rgba(0, 0, 0, 0.3)', 
        borderRadius: '8px', 
        padding: '15px',
        marginTop: '20px'
      }}>
        <DataLabel style={{ marginBottom: '10px' }}>Flight Status</DataLabel>
        <div style={{ color: '#fff', fontSize: '14px' }}>
          {efficiency > 8 ? '✅ Optimal Flight Conditions' :
           efficiency > 5 ? '⚠️ Suboptimal Performance' :
           '❌ Poor Flight Efficiency'}
        </div>
        <div style={{ color: '#888', fontSize: '12px', marginTop: '5px' }}>
          Wing loading: {(liftForce / 6.25).toFixed(1)} N/m²
        </div>
      </div>
    </PanelContainer>
  );
};

export default AerodynamicsPanel;
