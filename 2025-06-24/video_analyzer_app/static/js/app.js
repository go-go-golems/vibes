// Enhanced Video Analyzer Frontend JavaScript

class VideoAnalyzerApp {
    constructor() {
        this.eventSource = null;
        this.currentSessionId = null;
        this.stepCount = 0;
        this.apiCallCount = 0;
        this.startTime = null;
        this.elapsedTimer = null;
        
        this.initializeApp();
    }
    
    initializeApp() {
        console.log('🚀 Initializing Video Analyzer App');
        
        // Bind event listeners
        this.bindEventListeners();
        
        // Initialize connection status
        this.updateConnectionStatus('disconnected');
        
        // Check server health
        this.checkServerHealth();
    }
    
    bindEventListeners() {
        // Form submission
        const form = document.getElementById('analysisForm');
        form.addEventListener('submit', (e) => this.handleFormSubmit(e));
        
        // Download buttons
        document.getElementById('downloadResults').addEventListener('click', () => this.downloadResults());
        document.getElementById('downloadTracking').addEventListener('click', () => this.downloadTracking());
    }
    
    async checkServerHealth() {
        try {
            const response = await fetch('/health');
            if (response.ok) {
                console.log('✅ Server is healthy');
                this.updateConnectionStatus('connected');
            } else {
                console.warn('⚠️ Server health check failed');
                this.updateConnectionStatus('disconnected');
            }
        } catch (error) {
            console.error('❌ Server health check error:', error);
            this.updateConnectionStatus('disconnected');
        }
    }
    
    updateConnectionStatus(status) {
        // Remove existing status indicator
        const existing = document.querySelector('.connection-status');
        if (existing) existing.remove();
        
        // Create new status indicator
        const indicator = document.createElement('div');
        indicator.className = `connection-status ${status}`;
        
        const icons = {
            connected: 'bi-wifi',
            disconnected: 'bi-wifi-off',
            connecting: 'bi-arrow-clockwise'
        };
        
        const messages = {
            connected: 'Connected',
            disconnected: 'Disconnected',
            connecting: 'Connecting...'
        };
        
        indicator.innerHTML = `<i class="bi ${icons[status]} me-1"></i>${messages[status]}`;
        document.body.appendChild(indicator);
        
        // Auto-hide after 3 seconds if connected
        if (status === 'connected') {
            setTimeout(() => {
                if (indicator.parentNode) {
                    indicator.style.opacity = '0';
                    setTimeout(() => indicator.remove(), 300);
                }
            }, 3000);
        }
    }
    
    async handleFormSubmit(event) {
        event.preventDefault();
        
        const youtubeUrl = document.getElementById('youtubeUrl').value;
        const apiKey = document.getElementById('apiKey').value;
        const mode = document.getElementById('analysisMode').value;
        
        if (!youtubeUrl || !apiKey) {
            this.showAlert('Please fill in all required fields', 'danger');
            return;
        }
        
        // Validate YouTube URL
        if (!this.isValidYouTubeUrl(youtubeUrl)) {
            this.showAlert('Please enter a valid YouTube URL', 'danger');
            return;
        }
        
        try {
            // Show loading state
            this.setLoadingState(true);
            this.resetProgress();
            this.hideResults();
            
            // Start SSE connection
            this.connectToEventStream();
            
            // Submit analysis request
            const response = await fetch('/analyze', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify({
                    youtube_url: youtubeUrl,
                    api_key: apiKey,
                    mode: mode
                })
            });
            
            const result = await response.json();
            
            if (response.ok) {
                this.currentSessionId = result.session_id;
                document.getElementById('sessionId').textContent = result.session_id;
                this.updateStatus('Analysis started successfully');
                this.startElapsedTimer();
                
                console.log('✅ Analysis started:', result);
            } else {
                throw new Error(result.error || 'Analysis failed to start');
            }
            
        } catch (error) {
            console.error('❌ Analysis error:', error);
            this.showAlert(`Error: ${error.message}`, 'danger');
            this.setLoadingState(false);
        }
    }
    
    isValidYouTubeUrl(url) {
        const youtubeRegex = /^(https?:\/\/)?(www\.)?(youtube\.com\/watch\?v=|youtu\.be\/)[\w-]+/;
        return youtubeRegex.test(url);
    }
    
    connectToEventStream() {
        if (this.eventSource) {
            this.eventSource.close();
        }
        
        console.log('🔌 Connecting to event stream...');
        this.updateConnectionStatus('connecting');
        
        this.eventSource = new EventSource('/events');
        
        this.eventSource.onopen = () => {
            console.log('✅ SSE connection opened');
            this.updateConnectionStatus('connected');
        };
        
        this.eventSource.onmessage = (event) => {
            try {
                const data = JSON.parse(event.data);
                this.handleSSEEvent(data);
            } catch (error) {
                console.error('❌ SSE message parsing error:', error);
            }
        };
        
        this.eventSource.onerror = (error) => {
            console.error('❌ SSE connection error:', error);
            this.updateConnectionStatus('disconnected');
            
            // Attempt to reconnect after 5 seconds
            setTimeout(() => {
                if (this.currentSessionId) {
                    console.log('🔄 Attempting to reconnect...');
                    this.connectToEventStream();
                }
            }, 5000);
        };
    }
    
    handleSSEEvent(data) {
        console.log('📨 SSE Event:', data.type, data);
        
        switch (data.type) {
            case 'step':
                this.handleStepEvent(data.step);
                break;
            case 'api_call':
                this.handleApiCallEvent(data.api_call);
                break;
            case 'completion':
                this.handleCompletionEvent(data.summary);
                break;
            case 'result':
                this.handleResultEvent(data.result);
                break;
            case 'error':
                this.handleErrorEvent(data.error);
                break;
            case 'heartbeat':
                // Keep connection alive
                break;
            default:
                console.log('Unknown SSE event type:', data.type);
        }
    }
    
    handleStepEvent(step) {
        this.stepCount++;
        document.getElementById('stepCount').textContent = this.stepCount;
        
        this.updateStatus(step.description);
        this.addStepToTracking(step, 'step');
        
        // Update model info if available
        if (step.data && step.data.model) {
            document.getElementById('modelName').textContent = step.data.model;
            document.getElementById('modelInfo').classList.remove('d-none');
        }
    }
    
    handleApiCallEvent(apiCall) {
        this.apiCallCount++;
        document.getElementById('apiCallCount').textContent = this.apiCallCount;
        
        this.addStepToTracking({
            step_id: `API-${apiCall.call_id}`,
            description: `API call to ${apiCall.model}`,
            timestamp: apiCall.timestamp,
            elapsed_time: apiCall.elapsed_time,
            data: {
                model: apiCall.model,
                response_length: apiCall.response_length
            }
        }, 'api-call');
    }
    
    handleCompletionEvent(summary) {
        this.updateStatus('Analysis completed successfully');
        
        this.addStepToTracking({
            step_id: 'COMPLETE',
            description: 'Analysis completed',
            timestamp: new Date().toISOString(),
            elapsed_time: summary.total_duration,
            data: summary
        }, 'completion');
        
        this.setLoadingState(false);
        this.stopElapsedTimer();
    }
    
    handleResultEvent(result) {
        console.log('📊 Analysis result received:', result);
        this.displayResults(result);
    }
    
    handleErrorEvent(error) {
        console.error('❌ Analysis error:', error);
        this.showAlert(`Analysis error: ${error}`, 'danger');
        this.setLoadingState(false);
        this.stopElapsedTimer();
        
        this.addStepToTracking({
            step_id: 'ERROR',
            description: `Error: ${error}`,
            timestamp: new Date().toISOString(),
            elapsed_time: this.getElapsedTime()
        }, 'error');
    }
    
    addStepToTracking(step, type) {
        const container = document.getElementById('stepTracking');
        
        // Remove placeholder if it exists
        const placeholder = container.querySelector('.text-muted');
        if (placeholder && placeholder.parentNode) {
            placeholder.parentNode.remove();
        }
        
        // Create step element
        const stepElement = document.createElement('div');
        stepElement.className = `step-item ${type}`;
        
        const iconMap = {
            'step': step.step_id,
            'api-call': 'API',
            'completion': '✓',
            'error': '✗'
        };
        
        stepElement.innerHTML = `
            <div class="step-icon ${type}">
                ${iconMap[type]}
            </div>
            <div class="step-content">
                <div class="step-title">${step.description}</div>
                <div class="step-description">
                    ${step.data ? this.formatStepData(step.data) : ''}
                </div>
                <div class="step-meta">
                    <span><i class="bi bi-clock me-1"></i>${this.formatElapsedTime(step.elapsed_time)}</span>
                    <span><i class="bi bi-calendar me-1"></i>${this.formatTimestamp(step.timestamp)}</span>
                </div>
            </div>
        `;
        
        // Add to container (newest at top)
        container.insertBefore(stepElement, container.firstChild);
        
        // Limit to 20 steps to prevent performance issues
        const steps = container.querySelectorAll('.step-item');
        if (steps.length > 20) {
            steps[steps.length - 1].remove();
        }
    }
    
    formatStepData(data) {
        if (!data) return '';
        
        const parts = [];
        if (data.model) parts.push(`Model: ${data.model}`);
        if (data.response_length) parts.push(`Response: ${data.response_length} chars`);
        if (data.prompt_length) parts.push(`Prompt: ${data.prompt_length} chars`);
        if (data.video_url) parts.push(`URL: ${data.video_url.substring(0, 50)}...`);
        if (data.mode) parts.push(`Mode: ${data.mode}`);
        
        return parts.join(' • ');
    }
    
    formatElapsedTime(seconds) {
        if (!seconds) return '0s';
        
        if (seconds < 60) {
            return `${seconds.toFixed(1)}s`;
        } else {
            const minutes = Math.floor(seconds / 60);
            const remainingSeconds = seconds % 60;
            return `${minutes}m ${remainingSeconds.toFixed(0)}s`;
        }
    }
    
    formatTimestamp(timestamp) {
        if (!timestamp) return '';
        
        const date = new Date(timestamp);
        return date.toLocaleTimeString();
    }
    
    displayResults(result) {
        // Update result summary
        document.getElementById('resultResponseLength').textContent = result.response_length.toLocaleString();
        document.getElementById('resultTotalSteps').textContent = result.tracking.total_steps;
        document.getElementById('resultDuration').textContent = this.formatElapsedTime(result.tracking.total_duration || this.getElapsedTime());
        document.getElementById('resultModel').textContent = result.model_used;
        
        // Display analysis content
        document.getElementById('analysisContent').textContent = result.raw_analysis;
        
        // Show results section
        this.showResults();
        
        // Store result for downloads
        this.currentResult = result;
    }
    
    showResults() {
        document.getElementById('resultsSection').style.display = 'block';
        document.getElementById('resultsSection').scrollIntoView({ behavior: 'smooth' });
    }
    
    hideResults() {
        document.getElementById('resultsSection').style.display = 'none';
    }
    
    setLoadingState(loading) {
        const btn = document.getElementById('analyzeBtn');
        const alert = document.getElementById('statusAlert');
        
        if (loading) {
            btn.disabled = true;
            btn.innerHTML = '<span class="spinner-border spinner-border-sm me-2"></span>Analyzing...';
            alert.classList.remove('d-none');
        } else {
            btn.disabled = false;
            btn.innerHTML = '<i class="bi bi-play-fill me-2"></i>Start Analysis';
            alert.classList.add('d-none');
        }
    }
    
    updateStatus(message) {
        document.getElementById('statusText').textContent = message;
        document.getElementById('currentStatus').textContent = message;
    }
    
    resetProgress() {
        this.stepCount = 0;
        this.apiCallCount = 0;
        document.getElementById('stepCount').textContent = '0';
        document.getElementById('apiCallCount').textContent = '0';
        document.getElementById('elapsedTime').textContent = '0s';
        document.getElementById('currentStatus').textContent = 'Starting...';
        
        // Clear step tracking
        const container = document.getElementById('stepTracking');
        container.innerHTML = `
            <div class="text-muted text-center py-4">
                <i class="bi bi-clock-history display-4"></i>
                <p class="mt-2">Analysis steps will appear here in real-time</p>
            </div>
        `;
    }
    
    startElapsedTimer() {
        this.startTime = Date.now();
        this.elapsedTimer = setInterval(() => {
            const elapsed = this.getElapsedTime();
            document.getElementById('elapsedTime').textContent = this.formatElapsedTime(elapsed);
        }, 1000);
    }
    
    stopElapsedTimer() {
        if (this.elapsedTimer) {
            clearInterval(this.elapsedTimer);
            this.elapsedTimer = null;
        }
    }
    
    getElapsedTime() {
        if (!this.startTime) return 0;
        return (Date.now() - this.startTime) / 1000;
    }
    
    showAlert(message, type = 'info') {
        // Remove existing alerts
        const existingAlerts = document.querySelectorAll('.alert-dismissible');
        existingAlerts.forEach(alert => alert.remove());
        
        // Create new alert
        const alert = document.createElement('div');
        alert.className = `alert alert-${type} alert-dismissible fade show`;
        alert.innerHTML = `
            ${message}
            <button type="button" class="btn-close" data-bs-dismiss="alert"></button>
        `;
        
        // Insert at top of main content
        const container = document.querySelector('.container-fluid');
        container.insertBefore(alert, container.children[1]);
        
        // Auto-dismiss after 5 seconds
        setTimeout(() => {
            if (alert.parentNode) {
                alert.classList.remove('show');
                setTimeout(() => alert.remove(), 150);
            }
        }, 5000);
    }
    
    async downloadResults() {
        if (!this.currentResult) {
            this.showAlert('No results available to download', 'warning');
            return;
        }
        
        try {
            const blob = new Blob([JSON.stringify(this.currentResult, null, 2)], {
                type: 'application/json'
            });
            
            const url = URL.createObjectURL(blob);
            const a = document.createElement('a');
            a.href = url;
            a.download = `video_analysis_${this.currentSessionId}.json`;
            document.body.appendChild(a);
            a.click();
            document.body.removeChild(a);
            URL.revokeObjectURL(url);
            
            this.showAlert('Results downloaded successfully', 'success');
        } catch (error) {
            console.error('Download error:', error);
            this.showAlert('Failed to download results', 'danger');
        }
    }
    
    async downloadTracking() {
        if (!this.currentSessionId) {
            this.showAlert('No tracking data available', 'warning');
            return;
        }
        
        try {
            const response = await fetch(`/results/${this.currentSessionId}`);
            if (!response.ok) throw new Error('Failed to fetch tracking data');
            
            const trackingData = await response.json();
            
            const blob = new Blob([JSON.stringify(trackingData, null, 2)], {
                type: 'application/json'
            });
            
            const url = URL.createObjectURL(blob);
            const a = document.createElement('a');
            a.href = url;
            a.download = `tracking_data_${this.currentSessionId}.json`;
            document.body.appendChild(a);
            a.click();
            document.body.removeChild(a);
            URL.revokeObjectURL(url);
            
            this.showAlert('Tracking data downloaded successfully', 'success');
        } catch (error) {
            console.error('Download error:', error);
            this.showAlert('Failed to download tracking data', 'danger');
        }
    }
}

// Initialize app when DOM is loaded
document.addEventListener('DOMContentLoaded', () => {
    window.videoAnalyzerApp = new VideoAnalyzerApp();
});

// Handle page unload
window.addEventListener('beforeunload', () => {
    if (window.videoAnalyzerApp && window.videoAnalyzerApp.eventSource) {
        window.videoAnalyzerApp.eventSource.close();
    }
});

