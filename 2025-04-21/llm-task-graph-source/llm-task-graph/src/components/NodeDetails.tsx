import React, { useState } from 'react';
import styled from 'styled-components';
import { NodeData, Event } from '../types/types';

// Kawaii pink theme colors
const colors = {
  background: '#FFF0F5', // LavenderBlush
  cardBg: '#FFFFFF',
  cardBorder: '#FF69B4', // Hot pink
  cardShadow: 'rgba(255, 105, 180, 0.3)',
  headerBg: '#FFCCE5', // Light pink
  textPrimary: '#8B008B', // Dark magenta
  textSecondary: '#DB7093', // PaleVioletRed
  
  // Event type colors
  llmCall: '#FF69B4', // Hot pink
  toolCall: '#FF1493', // Deep pink
  memoryFetch: '#FFB6C1', // Light pink
  userInput: '#FFC0CB', // Pink
  systemEvent: '#DB7093', // PaleVioletRed
};

interface NodeDetailsProps {
  node: NodeData | null;
  onClose: () => void;
}

const DetailsContainer = styled.div`
  position: absolute;
  top: 20px;
  right: 20px;
  width: 350px;
  max-height: 80vh;
  background-color: ${colors.cardBg};
  border: 2px solid ${colors.cardBorder};
  border-radius: 12px;
  box-shadow: 0 4px 12px ${colors.cardShadow};
  overflow-y: auto;
  z-index: 1000;
  animation: slideIn 0.3s ease-out;
  
  @keyframes slideIn {
    from { transform: translateX(100%); opacity: 0; }
    to { transform: translateX(0); opacity: 1; }
  }
`;

const Header = styled.div`
  background-color: ${colors.headerBg};
  padding: 12px 16px;
  border-top-left-radius: 10px;
  border-top-right-radius: 10px;
  display: flex;
  justify-content: space-between;
  align-items: center;
`;

const Title = styled.h2`
  margin: 0;
  color: ${colors.textPrimary};
  font-size: 18px;
  font-weight: 600;
`;

const CloseButton = styled.button`
  background: none;
  border: none;
  color: ${colors.textPrimary};
  font-size: 20px;
  cursor: pointer;
  padding: 0;
  display: flex;
  align-items: center;
  justify-content: center;
  width: 24px;
  height: 24px;
  border-radius: 50%;
  
  &:hover {
    background-color: rgba(255, 105, 180, 0.1);
  }
`;

const Content = styled.div`
  padding: 16px;
`;

const Section = styled.div`
  margin-bottom: 16px;
`;

const SectionTitle = styled.h3`
  margin: 0 0 8px 0;
  color: ${colors.textPrimary};
  font-size: 16px;
  font-weight: 600;
  border-bottom: 1px solid ${colors.headerBg};
  padding-bottom: 4px;
`;

const InfoItem = styled.div`
  margin-bottom: 8px;
`;

const Label = styled.span`
  color: ${colors.textSecondary};
  font-weight: 500;
  margin-right: 8px;
`;

const Value = styled.span`
  color: ${colors.textPrimary};
`;

const Badge = styled.span<{ type: string }>`
  display: inline-block;
  padding: 2px 8px;
  border-radius: 12px;
  font-size: 12px;
  font-weight: 500;
  background-color: ${props => {
    switch(props.type) {
      case 'llm_call': return colors.llmCall;
      case 'tool_call': return colors.toolCall;
      case 'memory_fetch': return colors.memoryFetch;
      case 'user_input': return colors.userInput;
      case 'system_event': return colors.systemEvent;
      default: return colors.headerBg;
    }
  }};
  color: white;
  margin-right: 8px;
`;

const EventCard = styled.div`
  background-color: ${colors.cardBg};
  border: 1px solid ${colors.headerBg};
  border-radius: 8px;
  padding: 12px;
  margin-bottom: 12px;
  box-shadow: 0 2px 4px ${colors.cardShadow};
`;

const EventHeader = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 8px;
`;

const EventTitle = styled.div`
  display: flex;
  align-items: center;
`;

const EventTime = styled.span`
  color: ${colors.textSecondary};
  font-size: 12px;
`;

const EventDescription = styled.p`
  margin: 0 0 8px 0;
  color: ${colors.textPrimary};
`;

const EventDetails = styled.pre`
  background-color: ${colors.background};
  border-radius: 4px;
  padding: 8px;
  font-size: 12px;
  color: ${colors.textPrimary};
  overflow-x: auto;
  margin: 0;
`;

const formatDate = (dateString: string) => {
  const date = new Date(dateString);
  return date.toLocaleString();
};

const NodeDetails: React.FC<NodeDetailsProps> = ({ node, onClose }) => {
  if (!node) return null;

  return (
    <DetailsContainer>
      <Header>
        <Title>{node.text}</Title>
        <CloseButton onClick={onClose}>×</CloseButton>
      </Header>
      <Content>
        <Section>
          <SectionTitle>Task Information</SectionTitle>
          <InfoItem>
            <Label>ID:</Label>
            <Value>{node.id}</Value>
          </InfoItem>
          <InfoItem>
            <Label>Status:</Label>
            <Value>{node.status.replace('_', ' ')}</Value>
          </InfoItem>
          <InfoItem>
            <Label>Type:</Label>
            <Value>{node.type.replace('_', ' ')}</Value>
          </InfoItem>
          <InfoItem>
            <Label>Timestamp:</Label>
            <Value>{formatDate(node.timestamp)}</Value>
          </InfoItem>
        </Section>
        
        <Section>
          <SectionTitle>Metadata</SectionTitle>
          <InfoItem>
            <Label>Description:</Label>
            <Value>{node.metadata.description}</Value>
          </InfoItem>
          {node.metadata.dependencies.length > 0 && (
            <InfoItem>
              <Label>Dependencies:</Label>
              <Value>{node.metadata.dependencies.join(', ')}</Value>
            </InfoItem>
          )}
          {Object.entries(node.metadata)
            .filter(([key]) => !['description', 'dependencies'].includes(key))
            .map(([key, value]) => (
              <InfoItem key={key}>
                <Label>{key}:</Label>
                <Value>{typeof value === 'object' ? JSON.stringify(value) : String(value)}</Value>
              </InfoItem>
            ))}
        </Section>
        
        {node.events.length > 0 && (
          <Section>
            <SectionTitle>Related Events ({node.events.length})</SectionTitle>
            {node.events.map((event: Event) => (
              <EventCard key={event.id}>
                <EventHeader>
                  <EventTitle>
                    <Badge type={event.type}>{event.type.replace('_', ' ')}</Badge>
                  </EventTitle>
                  <EventTime>{formatDate(event.timestamp)}</EventTime>
                </EventHeader>
                <EventDescription>{event.description}</EventDescription>
                <EventDetails>{JSON.stringify(event.details, null, 2)}</EventDetails>
              </EventCard>
            ))}
          </Section>
        )}
      </Content>
    </DetailsContainer>
  );
};

export default NodeDetails;
