// Pelican Genome Sequencer Frontend
class PelicanSequencer {
    constructor() {
        this.currentEventSource = null;
        this.currentJobId = null;
        this.totalRecords = 200; // Default total records
        
        this.initializeEventListeners();
    }
    
    initializeEventListeners() {
        const form = document.getElementById('seqForm');
        if (form) {
            form.addEventListener('submit', (e) => this.handleFormSubmit(e));
        }
    }
    
    async handleFormSubmit(event) {
        event.preventDefault();
        
        const species = document.getElementById('species').value;
        const submitButton = event.target.querySelector('button[type="submit"]');
        
        // Disable form during submission
        submitButton.disabled = true;
        submitButton.innerHTML = '🔄 Starting...';
        
        try {
            const response = await fetch('/jobs', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify({ species: species })
            });
            
            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }
            
            const data = await response.json();
            this.startListening(data.job_id, species);
            
        } catch (error) {
            console.error('Failed to start job:', error);
            alert('Failed to start sequencing job. Please try again.');
        } finally {
            // Re-enable form
            submitButton.disabled = false;
            submitButton.innerHTML = '🧬 Sequence Genome';
        }
    }
    
    startListening(jobId, species) {
        this.currentJobId = jobId;
        
        // Show progress section
        const progressSection = document.getElementById('progress');
        progressSection.classList.remove('d-none');
        
        // Update job info
        const jobInfo = document.getElementById('jobInfo');
        jobInfo.innerHTML = `
            <div class="d-flex justify-content-between align-items-center">
                <div>
                    <strong>Job ID:</strong> <code>${jobId}</code><br>
                    <strong>Species:</strong> ${this.getSpeciesDisplayName(species)}
                </div>
                <button class="btn btn-sm btn-outline-danger" onclick="sequencer.stopJob()">
                    Stop Job
                </button>
            </div>
        `;
        
        // Reset progress indicators
        this.updateProgress(0, 0, 'fetch');
        this.clearLog();
        this.hideRateLimitWarning();
        
        // Start SSE connection
        this.connectEventSource(jobId);
    }
    
    connectEventSource(jobId) {
        // Close existing connection if any
        if (this.currentEventSource) {
            this.currentEventSource.close();
        }
        
        const eventSource = new EventSource(`/jobs/${jobId}/events`);
        this.currentEventSource = eventSource;
        
        eventSource.onmessage = (event) => {
            try {
                const data = JSON.parse(event.data);
                this.handleProgressEvent(data);
            } catch (error) {
                console.error('Failed to parse event data:', error);
            }
        };
        
        eventSource.onerror = (error) => {
            console.error('EventSource error:', error);
            this.logMessage('Connection error - attempting to reconnect...', 'error');
            
            // Attempt to reconnect after a delay
            setTimeout(() => {
                if (this.currentJobId === jobId) {
                    this.connectEventSource(jobId);
                }
            }, 2000);
        };
        
        eventSource.onopen = () => {
            this.logMessage('Connected to live progress stream', 'info');
        };
    }
    
    handleProgressEvent(event) {
        const { stage, fetched, indexed, rate_limited, err, ts, message } = event;
        
        // Handle connection message
        if (stage === 'connected') {
            this.logMessage(message || 'Connected to live progress stream', 'info');
            return;
        }
        
        // Update progress indicators
        this.updateProgress(fetched, indexed, stage);
        
        // Log the event
        const timestamp = new Date(ts).toLocaleTimeString();
        let logMessage = '';
        let type = 'info';
        
        switch (stage) {
            case 'fetch':
                if (rate_limited) {
                    logMessage = `[${timestamp}] 🐌 FETCH (Rate Limited): ${fetched}/${this.totalRecords} records`;
                    this.showRateLimitWarning();
                    type = 'warning';
                } else {
                    logMessage = `[${timestamp}] 📥 FETCH: ${fetched}/${this.totalRecords} records`;
                    this.hideRateLimitWarning();
                }
                break;
                
            case 'analyze':
                logMessage = `[${timestamp}] 🧬 ANALYZE: ${indexed}/${this.totalRecords} records indexed`;
                break;
                
            case 'done':
                logMessage = `[${timestamp}] ✅ COMPLETED: ${fetched} fetched, ${indexed} indexed`;
                type = 'success';
                this.onJobComplete();
                break;
                
            case 'error':
                logMessage = `[${timestamp}] ❌ ERROR: ${err}`;
                type = 'error';
                this.onJobError();
                break;
        }
        
        if (logMessage) {
            this.logMessage(logMessage, type);
        }
    }
    
    updateProgress(fetched, indexed, stage) {
        const totalProgress = fetched + indexed;
        const percentage = Math.round((totalProgress / (this.totalRecords * 2)) * 100);
        
        // Update progress bar
        const progressBar = document.getElementById('progressBar');
        progressBar.style.width = `${percentage}%`;
        progressBar.textContent = `${percentage}%`;
        
        // Update color based on stage
        progressBar.className = 'progress-bar progress-bar-striped';
        if (stage === 'done') {
            progressBar.classList.add('bg-success');
            progressBar.classList.remove('progress-bar-animated');
        } else if (stage === 'error') {
            progressBar.classList.add('bg-danger');
            progressBar.classList.remove('progress-bar-animated');
        } else {
            progressBar.classList.add('progress-bar-animated');
        }
        
        // Update counters
        document.getElementById('fetchedCount').textContent = fetched;
        document.getElementById('indexedCount').textContent = indexed;
    }
    
    logMessage(message, type = 'info') {
        const logContainer = document.getElementById('log');
        const logEntry = document.createElement('div');
        
        // Add color coding based on type
        let className = '';
        switch (type) {
            case 'error':
                className = 'text-danger';
                break;
            case 'warning':
                className = 'text-warning';
                break;
            case 'success':
                className = 'text-success';
                break;
            default:
                className = 'text-light';
        }
        
        logEntry.className = className;
        logEntry.textContent = message;
        
        logContainer.appendChild(logEntry);
        
        // Auto-scroll to bottom
        logContainer.scrollTop = logContainer.scrollHeight;
        
        // Limit log entries to prevent memory issues
        while (logContainer.children.length > 100) {
            logContainer.removeChild(logContainer.firstChild);
        }
    }
    
    clearLog() {
        const logContainer = document.getElementById('log');
        logContainer.innerHTML = '';
    }
    
    showRateLimitWarning() {
        const warning = document.getElementById('rateLimitWarning');
        warning.classList.remove('d-none');
    }
    
    hideRateLimitWarning() {
        const warning = document.getElementById('rateLimitWarning');
        warning.classList.add('d-none');
    }
    
    stopJob() {
        if (this.currentEventSource) {
            this.currentEventSource.close();
            this.currentEventSource = null;
        }
        
        this.logMessage('Job stopped by user', 'warning');
        this.currentJobId = null;
    }
    
    onJobComplete() {
        if (this.currentEventSource) {
            this.currentEventSource.close();
            this.currentEventSource = null;
        }
        
        // Show completion notification
        setTimeout(() => {
            alert('🎉 Genome sequencing completed successfully!');
        }, 500);
    }
    
    onJobError() {
        if (this.currentEventSource) {
            this.currentEventSource.close();
            this.currentEventSource = null;
        }
    }
    
    getSpeciesDisplayName(species) {
        const speciesMap = {
            'brown_pelican': 'Brown Pelican (Pelecanus occidentalis)',
            'peruvian_pelican': 'Peruvian Pelican (Pelecanus thagus)',
            'dalmatian_pelican': 'Dalmatian Pelican (Pelecanus crispus)',
            'american_white_pelican': 'American White Pelican (Pelecanus erythrorhynchos)',
            'australian_pelican': 'Australian Pelican (Pelecanus conspicillatus)'
        };
        
        return speciesMap[species] || species;
    }
}

// Initialize the sequencer when the page loads
const sequencer = new PelicanSequencer();

// Add some utility functions for debugging
window.pelicanDebug = {
    getCurrentJobId: () => sequencer.currentJobId,
    getEventSourceState: () => sequencer.currentEventSource?.readyState,
    testConnection: async () => {
        try {
            const response = await fetch('/health');
            const data = await response.json();
            console.log('Health check:', data);
            return data;
        } catch (error) {
            console.error('Health check failed:', error);
            return null;
        }
    }
};

