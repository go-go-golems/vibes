import React, { useState } from 'react';
import { Modal, Button, Form, Tabs, Tab } from 'react-bootstrap';
import { FaUpload, FaFileImport } from 'react-icons/fa';

const ImportModal = ({ show, onHide, onImport }) => {
  const [importType, setImportType] = useState('json');
  const [fileContent, setFileContent] = useState('');
  const [fileName, setFileName] = useState('');
  const [isLoading, setIsLoading] = useState(false);
  
  const handleFileChange = (e) => {
    const file = e.target.files[0];
    if (!file) return;
    
    setFileName(file.name);
    const reader = new FileReader();
    reader.onload = (event) => {
      setFileContent(event.target.result);
    };
    reader.readAsText(file);
  };
  
  const handleImport = () => {
    if (!fileContent) return;
    
    setIsLoading(true);
    
    try {
      let data;
      if (importType === 'json') {
        data = JSON.parse(fileContent);
      } else {
        // For CSV, we'll pass the raw content and let the backend handle parsing
        data = fileContent;
      }
      
      onImport(data, importType);
      setIsLoading(false);
      onHide();
    } catch (error) {
      alert(`Error parsing file: ${error.message}`);
      setIsLoading(false);
    }
  };
  
  return (
    <Modal show={show} onHide={onHide} centered>
      <Modal.Header closeButton>
        <Modal.Title>Import Twitter Data</Modal.Title>
      </Modal.Header>
      <Modal.Body>
        <Tabs
          activeKey={importType}
          onSelect={(k) => setImportType(k)}
          className="mb-3"
        >
          <Tab eventKey="json" title="JSON">
            <p>Import tweets and authors from a JSON file.</p>
            <Form.Group controlId="jsonFile" className="mb-3">
              <Form.Label>Select JSON File</Form.Label>
              <Form.Control 
                type="file" 
                accept=".json" 
                onChange={handleFileChange}
              />
            </Form.Group>
          </Tab>
          <Tab eventKey="csv" title="CSV">
            <p>Import tweets or authors from a CSV file.</p>
            <Form.Group controlId="csvFile" className="mb-3">
              <Form.Label>Select CSV File</Form.Label>
              <Form.Control 
                type="file" 
                accept=".csv" 
                onChange={handleFileChange}
              />
            </Form.Group>
            <Form.Group controlId="csvType" className="mb-3">
              <Form.Label>CSV Content Type</Form.Label>
              <Form.Select>
                <option value="tweets">Tweets</option>
                <option value="authors">Authors</option>
              </Form.Select>
            </Form.Group>
          </Tab>
        </Tabs>
        
        {fileName && (
          <div className="alert alert-info">
            Selected file: {fileName}
          </div>
        )}
      </Modal.Body>
      <Modal.Footer>
        <Button variant="secondary" onClick={onHide}>
          Cancel
        </Button>
        <Button 
          variant="primary" 
          onClick={handleImport}
          disabled={!fileContent || isLoading}
        >
          {isLoading ? 'Importing...' : 'Import Data'}
          <FaFileImport className="ms-2" />
        </Button>
      </Modal.Footer>
    </Modal>
  );
};

export default ImportModal;
