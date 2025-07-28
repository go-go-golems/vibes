class FullscreenTerminal {
    constructor() {
        this.ws = null;
        this.term = null;
        this.isConnected = false;
        this.reconnectAttempts = 0;
        this.maxReconnectAttempts = 5;
        this.reconnectDelay = 1000;
        
        this.statusIndicator = document.getElementById('status-indicator');
        this.loadingOverlay = document.getElementById('loading-overlay');
        
        this.init();
    }

    init() {
        this.setupTerminal();
        this.connect();
        this.setupEventListeners();
    }

    setupTerminal() {
        // Create xterm.js terminal with optimal settings
        this.term = new Terminal({
            cursorBlink: true,
            cursorStyle: 'block',
            fontFamily: '"SF Mono", "Monaco", "Inconsolata", "Fira Code", "Droid Sans Mono", monospace',
            fontSize: 14,
            lineHeight: 1.2,
            theme: {
                background: '#000000',
                foreground: '#ffffff',
                cursor: '#ffffff',
                cursorAccent: '#000000',
                selection: 'rgba(255, 255, 255, 0.3)',
                black: '#000000',
                red: '#ff5555',
                green: '#50fa7b',
                yellow: '#f1fa8c',
                blue: '#bd93f9',
                magenta: '#ff79c6',
                cyan: '#8be9fd',
                white: '#f8f8f2',
                brightBlack: '#44475a',
                brightRed: '#ff6e6e',
                brightGreen: '#69ff94',
                brightYellow: '#ffffa5',
                brightBlue: '#d6acff',
                brightMagenta: '#ff92df',
                brightCyan: '#a4ffff',
                brightWhite: '#ffffff'
            },
            allowTransparency: false,
            convertEol: true,
            scrollback: 1000,
            tabStopWidth: 4
        });

        // Open terminal in the container
        this.term.open(document.getElementById('terminal'));
        
        // Handle terminal input
        this.term.onData((data) => {
            if (this.isConnected) {
                this.sendInput(data);
            }
        });

        // Handle terminal resize
        this.term.onResize((size) => {
            if (this.isConnected) {
                this.sendResize(size.cols, size.rows);
            }
        });

        // Fit terminal to container
        this.fitTerminal();
    }

    connect() {
        const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
        const wsUrl = `${protocol}//${window.location.host}/ws`;
        
        this.updateStatus('connecting');
        
        try {
            this.ws = new WebSocket(wsUrl);
            
            this.ws.onopen = () => {
                console.log('WebSocket connected');
                this.isConnected = true;
                this.reconnectAttempts = 0;
                this.updateStatus('connected');
                this.hideLoading();
                
                // Send initial resize to match terminal size
                this.sendResize(this.term.cols, this.term.rows);
                
                // Focus the terminal
                this.term.focus();
            };
            
            this.ws.onmessage = (event) => {
                try {
                    const message = JSON.parse(event.data);
                    this.handleMessage(message);
                } catch (error) {
                    console.error('Error parsing WebSocket message:', error);
                    // If it's not JSON, treat as raw terminal data
                    this.term.write(event.data);
                }
            };
            
            this.ws.onclose = (event) => {
                console.log('WebSocket disconnected:', event.code, event.reason);
                this.isConnected = false;
                this.updateStatus('disconnected');
                
                if (event.code !== 1000) { // Not a normal closure
                    this.attemptReconnect();
                }
            };
            
            this.ws.onerror = (error) => {
                console.error('WebSocket error:', error);
                this.updateStatus('error');
            };
            
        } catch (error) {
            console.error('Failed to create WebSocket:', error);
            this.updateStatus('error');
            this.attemptReconnect();
        }
    }

    handleMessage(message) {
        switch (message.type) {
            case 'render':
                // Clear terminal and write new content
                this.term.clear();
                this.term.write(message.data.content);
                break;
            case 'output':
                // Append output to terminal
                this.term.write(message.data);
                break;
            case 'clear':
                this.term.clear();
                break;
            default:
                console.log('Unknown message type:', message.type);
        }
    }

    sendInput(data) {
        if (!this.ws || this.ws.readyState !== WebSocket.OPEN) return;
        
        // Convert terminal input to key events
        const message = {
            type: 'keypress',
            data: this.convertInputToKeyEvent(data)
        };
        
        this.ws.send(JSON.stringify(message));
    }

    convertInputToKeyEvent(data) {
        // Handle special keys and sequences
        const keyMap = {
            '\r': { key: 'Enter' },
            '\x7f': { key: 'Backspace' },
            '\x1b': { key: 'Escape' },
            '\t': { key: 'Tab' },
            '\x1b[A': { key: 'ArrowUp' },
            '\x1b[B': { key: 'ArrowDown' },
            '\x1b[C': { key: 'ArrowRight' },
            '\x1b[D': { key: 'ArrowLeft' },
            '\x1b[H': { key: 'Home' },
            '\x1b[F': { key: 'End' },
            '\x1b[3~': { key: 'Delete' },
            '\x1b[5~': { key: 'PageUp' },
            '\x1b[6~': { key: 'PageDown' },
            ' ': { key: ' ' }
        };

        if (keyMap[data]) {
            return keyMap[data];
        }

        // Handle Ctrl combinations
        if (data.length === 1 && data.charCodeAt(0) < 32) {
            const ctrlKey = String.fromCharCode(data.charCodeAt(0) + 64);
            return {
                key: ctrlKey.toLowerCase(),
                ctrlKey: true
            };
        }

        // Regular character
        return {
            key: data,
            ctrlKey: false,
            altKey: false,
            shiftKey: false,
            metaKey: false
        };
    }

    sendResize(cols, rows) {
        if (!this.ws || this.ws.readyState !== WebSocket.OPEN) return;
        
        const message = {
            type: 'resize',
            data: {
                width: cols,
                height: rows
            }
        };
        
        this.ws.send(JSON.stringify(message));
    }

    updateStatus(status) {
        this.statusIndicator.className = `status-indicator ${status}`;
    }

    hideLoading() {
        this.loadingOverlay.classList.add('hidden');
        setTimeout(() => {
            this.loadingOverlay.style.display = 'none';
        }, 500);
    }

    showLoading() {
        this.loadingOverlay.style.display = 'flex';
        this.loadingOverlay.classList.remove('hidden');
    }

    attemptReconnect() {
        if (this.reconnectAttempts >= this.maxReconnectAttempts) {
            console.error('Max reconnection attempts reached');
            this.updateStatus('error');
            return;
        }

        this.reconnectAttempts++;
        const delay = this.reconnectDelay * Math.pow(2, this.reconnectAttempts - 1);
        
        console.log(`Attempting to reconnect in ${delay}ms (attempt ${this.reconnectAttempts})`);
        this.updateStatus('connecting');
        
        setTimeout(() => {
            this.connect();
        }, delay);
    }

    fitTerminal() {
        // Calculate optimal terminal size
        const container = document.getElementById('terminal-container');
        const containerRect = container.getBoundingClientRect();
        
        // Account for padding
        const padding = 32; // 1rem on each side
        const availableWidth = containerRect.width - padding;
        const availableHeight = containerRect.height - padding;
        
        // Estimate character dimensions
        const charWidth = 9; // Approximate character width in pixels
        const charHeight = 17; // Approximate character height in pixels
        
        const cols = Math.floor(availableWidth / charWidth);
        const rows = Math.floor(availableHeight / charHeight);
        
        // Resize terminal
        this.term.resize(cols, rows);
    }

    setupEventListeners() {
        // Handle window resize
        window.addEventListener('resize', () => {
            setTimeout(() => {
                this.fitTerminal();
            }, 100);
        });

        // Handle visibility change (tab switching)
        document.addEventListener('visibilitychange', () => {
            if (!document.hidden && this.term) {
                this.term.focus();
            }
        });

        // Handle page focus
        window.addEventListener('focus', () => {
            if (this.term) {
                this.term.focus();
            }
        });

        // Prevent context menu on right click
        document.addEventListener('contextmenu', (e) => {
            e.preventDefault();
        });

        // Handle keyboard shortcuts
        document.addEventListener('keydown', (e) => {
            // Prevent browser shortcuts that might interfere
            if (e.ctrlKey || e.metaKey) {
                switch (e.key) {
                    case 'r': // Refresh
                    case 'w': // Close tab
                    case 't': // New tab
                    case 'n': // New window
                        // Allow these to work normally
                        break;
                    default:
                        // Prevent other shortcuts and let terminal handle them
                        if (this.term && this.term.hasSelection()) {
                            // Allow copy/paste
                            if (e.key === 'c' || e.key === 'v') {
                                break;
                            }
                        }
                        e.preventDefault();
                        break;
                }
            }
        });
    }
}

// Initialize the terminal when the page loads
document.addEventListener('DOMContentLoaded', () => {
    new FullscreenTerminal();
});

