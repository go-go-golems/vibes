class Terminal {
    constructor() {
        this.ws = null;
        this.terminalContent = document.getElementById('terminal-content');
        this.terminal = document.getElementById('terminal');
        this.cursor = document.getElementById('cursor');
        this.isConnected = false;
        
        this.init();
    }

    init() {
        this.connect();
        this.setupEventListeners();
        this.setupResizeHandler();
    }

    connect() {
        const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
        const wsUrl = `${protocol}//${window.location.host}/ws`;
        
        this.ws = new WebSocket(wsUrl);
        
        this.ws.onopen = () => {
            console.log('WebSocket connected');
            this.isConnected = true;
            this.terminalContent.innerHTML = '';
            this.sendResize();
        };
        
        this.ws.onmessage = (event) => {
            try {
                const message = JSON.parse(event.data);
                this.handleMessage(message);
            } catch (error) {
                console.error('Error parsing WebSocket message:', error);
            }
        };
        
        this.ws.onclose = () => {
            console.log('WebSocket disconnected');
            this.isConnected = false;
            this.terminalContent.innerHTML = '<div class="loading">Connection lost. Reconnecting...</div>';
            
            // Attempt to reconnect after 3 seconds
            setTimeout(() => {
                this.connect();
            }, 3000);
        };
        
        this.ws.onerror = (error) => {
            console.error('WebSocket error:', error);
        };
    }

    handleMessage(message) {
        switch (message.type) {
            case 'render':
                this.render(message.data.content);
                break;
            default:
                console.log('Unknown message type:', message.type);
        }
    }

    render(content) {
        // Convert ANSI escape sequences to HTML
        const htmlContent = this.ansiToHtml(content);
        this.terminalContent.innerHTML = htmlContent;
        
        // Scroll to bottom if needed
        this.terminal.scrollTop = this.terminal.scrollHeight;
    }

    ansiToHtml(text) {
        // Basic ANSI escape sequence handling
        // This is a simplified version - a full implementation would be more complex
        
        // Remove ANSI escape sequences for now and just return the text
        // In a full implementation, you'd convert these to HTML with CSS classes
        return text
            .replace(/\x1b\[[0-9;]*m/g, '') // Remove color codes
            .replace(/\x1b\[[0-9;]*[A-Za-z]/g, '') // Remove other escape sequences
            .replace(/&/g, '&amp;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;')
            .replace(/\n/g, '<br>');
    }

    setupEventListeners() {
        // Make the terminal focusable
        this.terminal.setAttribute('tabindex', '0');
        this.terminal.focus();
        
        // Handle keyboard events
        document.addEventListener('keydown', (event) => {
            if (!this.isConnected) return;
            
            // Prevent default behavior for most keys
            const allowedKeys = ['F5', 'F12'];
            if (!allowedKeys.includes(event.key)) {
                event.preventDefault();
            }
            
            this.sendKeyEvent(event);
        });
        
        // Handle clicks to focus the terminal
        this.terminal.addEventListener('click', () => {
            this.terminal.focus();
        });
        
        // Prevent context menu
        this.terminal.addEventListener('contextmenu', (event) => {
            event.preventDefault();
        });
    }

    setupResizeHandler() {
        window.addEventListener('resize', () => {
            this.sendResize();
        });
    }

    sendKeyEvent(event) {
        if (!this.ws || this.ws.readyState !== WebSocket.OPEN) return;
        
        const message = {
            type: 'keypress',
            data: {
                key: event.key,
                ctrlKey: event.ctrlKey,
                altKey: event.altKey,
                shiftKey: event.shiftKey,
                metaKey: event.metaKey,
                code: event.code
            }
        };
        
        this.ws.send(JSON.stringify(message));
    }

    sendResize() {
        if (!this.ws || this.ws.readyState !== WebSocket.OPEN) return;
        
        // Calculate terminal size based on character dimensions
        const style = window.getComputedStyle(this.terminal);
        const fontSize = parseInt(style.fontSize);
        const lineHeight = parseFloat(style.lineHeight) * fontSize;
        
        const width = Math.floor(this.terminal.clientWidth / (fontSize * 0.6)); // Approximate character width
        const height = Math.floor(this.terminal.clientHeight / lineHeight);
        
        const message = {
            type: 'resize',
            data: {
                width: width,
                height: height
            }
        };
        
        this.ws.send(JSON.stringify(message));
    }
}

// Initialize the terminal when the page loads
document.addEventListener('DOMContentLoaded', () => {
    new Terminal();
});

// Add some helpful keyboard shortcuts info
document.addEventListener('DOMContentLoaded', () => {
    const shortcuts = [
        { key: 'F1', action: 'Show this help' },
        { key: 'Ctrl+C', action: 'Interrupt (disabled in web)' }
    ];
    
    // You could add a help modal here if desired
});

