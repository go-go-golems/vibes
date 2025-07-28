package websocket

import (
	"encoding/json"
	"log"
	"net/http"
	"sync"
	"time"

	"github.com/gorilla/websocket"
	"bubbletea-todo-app/internal/todo"
)

var enhancedUpgrader = websocket.Upgrader{
	CheckOrigin: func(r *http.Request) bool {
		return true // Allow all origins for development
	},
}

// EnhancedMessage represents a websocket message with enhanced features
type EnhancedMessage struct {
	Type string      `json:"type"`
	Data interface{} `json:"data"`
}

// EnhancedKeyEvent represents a keyboard event from the web client
type EnhancedKeyEvent struct {
	Key       string `json:"key"`
	CtrlKey   bool   `json:"ctrlKey"`
	AltKey    bool   `json:"altKey"`
	ShiftKey  bool   `json:"shiftKey"`
	MetaKey   bool   `json:"metaKey"`
}

// EnhancedTerminalSession represents a websocket session with enhanced terminal
type EnhancedTerminalSession struct {
	conn     *websocket.Conn
	model    *todo.EnhancedModel
	send     chan []byte
	done     chan bool
	mutex    sync.RWMutex
}

// NewEnhancedTerminalSession creates a new enhanced terminal session
func NewEnhancedTerminalSession(conn *websocket.Conn) *EnhancedTerminalSession {
	return &EnhancedTerminalSession{
		conn:  conn,
		model: &todo.EnhancedModel{},
		send:  make(chan []byte, 256),
		done:  make(chan bool),
	}
}

// HandleEnhancedWebSocket handles enhanced websocket connections
func HandleEnhancedWebSocket(w http.ResponseWriter, r *http.Request) {
	conn, err := enhancedUpgrader.Upgrade(w, r, nil)
	if err != nil {
		log.Printf("WebSocket upgrade error: %v", err)
		return
	}
	defer conn.Close()

	session := NewEnhancedTerminalSession(conn)
	
	// Initialize the enhanced model
	*session.model = todo.NewEnhancedModel()
	
	// Add some sample todos with variety
	session.model.TodoList.AddItem("🚀 Learn Bubbletea framework")
	session.model.TodoList.AddItem("🎨 Build a colorful todo app")
	session.model.TodoList.AddItem("🌐 Add enhanced web interface")
	session.model.TodoList.AddItem("✨ Test ANSI color support")
	session.model.TodoList.SetSelectedIndex(0)

	// Start goroutines for handling the session
	go session.writePump()
	go session.readPump()

	// Send initial render
	session.sendEnhancedRender()

	// Wait for session to end
	<-session.done
}

// readPump handles incoming messages from the websocket
func (s *EnhancedTerminalSession) readPump() {
	defer func() {
		s.done <- true
	}()

	s.conn.SetReadLimit(512)
	s.conn.SetReadDeadline(time.Now().Add(60 * time.Second))
	s.conn.SetPongHandler(func(string) error {
		s.conn.SetReadDeadline(time.Now().Add(60 * time.Second))
		return nil
	})

	for {
		_, message, err := s.conn.ReadMessage()
		if err != nil {
			if websocket.IsUnexpectedCloseError(err, websocket.CloseGoingAway, websocket.CloseAbnormalClosure) {
				log.Printf("WebSocket error: %v", err)
			}
			break
		}

		var msg EnhancedMessage
		if err := json.Unmarshal(message, &msg); err != nil {
			log.Printf("JSON unmarshal error: %v", err)
			continue
		}

		s.handleEnhancedMessage(msg)
	}
}

// writePump handles outgoing messages to the websocket
func (s *EnhancedTerminalSession) writePump() {
	ticker := time.NewTicker(54 * time.Second)
	defer func() {
		ticker.Stop()
		s.conn.Close()
	}()

	for {
		select {
		case message, ok := <-s.send:
			s.conn.SetWriteDeadline(time.Now().Add(10 * time.Second))
			if !ok {
				s.conn.WriteMessage(websocket.CloseMessage, []byte{})
				return
			}

			if err := s.conn.WriteMessage(websocket.TextMessage, message); err != nil {
				log.Printf("WebSocket write error: %v", err)
				return
			}

		case <-ticker.C:
			s.conn.SetWriteDeadline(time.Now().Add(10 * time.Second))
			if err := s.conn.WriteMessage(websocket.PingMessage, nil); err != nil {
				return
			}
		}
	}
}

// handleEnhancedMessage processes incoming messages
func (s *EnhancedTerminalSession) handleEnhancedMessage(msg EnhancedMessage) {
	s.mutex.Lock()
	defer s.mutex.Unlock()

	switch msg.Type {
	case "keypress":
		if keyData, ok := msg.Data.(map[string]interface{}); ok {
			keyEvent := EnhancedKeyEvent{}
			if keyBytes, err := json.Marshal(keyData); err == nil {
				json.Unmarshal(keyBytes, &keyEvent)
			}

			// Process the key event
			s.processEnhancedKeyEvent(keyEvent)
			
			// Send updated render
			s.sendEnhancedRender()
		}
	case "resize":
		if resizeData, ok := msg.Data.(map[string]interface{}); ok {
			if width, ok := resizeData["width"].(float64); ok {
				if height, ok := resizeData["height"].(float64); ok {
					s.model.Width = int(width)
					s.model.Height = int(height)
					s.sendEnhancedRender()
				}
			}
		}
	}
}

// processEnhancedKeyEvent converts web key events to model updates
func (s *EnhancedTerminalSession) processEnhancedKeyEvent(keyEvent EnhancedKeyEvent) {
	key := keyEvent.Key

	// Handle special key combinations
	if keyEvent.CtrlKey && key == "c" {
		// For web interface, we don't actually quit, just ignore
		return
	}

	// Convert web keys to the format expected by the TUI
	switch key {
	case "ArrowUp":
		s.model.TodoList.MoveSelectionUp()
	case "ArrowDown":
		s.model.TodoList.MoveSelectionDown()
	case "Enter":
		switch s.model.Mode {
		case todo.ModeList:
			if len(s.model.TodoList.Items) > 0 {
				s.model.TodoList.ToggleItem(s.model.TodoList.GetSelectedIndex())
			}
		case todo.ModeAdd:
			if len(s.model.InputText) > 0 {
				s.model.TodoList.AddItem(s.model.InputText)
			}
			s.model.Mode = todo.ModeList
			s.model.InputText = ""
		}
	case " ":
		if s.model.Mode == todo.ModeList && len(s.model.TodoList.Items) > 0 {
			s.model.TodoList.ToggleItem(s.model.TodoList.GetSelectedIndex())
		} else if s.model.Mode == todo.ModeAdd {
			s.model.InputText += " "
		}
	case "Escape":
		if s.model.Mode == todo.ModeAdd {
			s.model.Mode = todo.ModeList
			s.model.InputText = ""
		}
	case "Backspace":
		if s.model.Mode == todo.ModeAdd && len(s.model.InputText) > 0 {
			s.model.InputText = s.model.InputText[:len(s.model.InputText)-1]
		}
	case "a":
		if s.model.Mode == todo.ModeList {
			s.model.Mode = todo.ModeAdd
			s.model.InputText = ""
		}
	case "d", "x":
		if s.model.Mode == todo.ModeList && len(s.model.TodoList.Items) > 0 {
			s.model.TodoList.DeleteItem(s.model.TodoList.GetSelectedIndex())
		}
	case "k":
		if s.model.Mode == todo.ModeList {
			s.model.TodoList.MoveSelectionUp()
		}
	case "j":
		if s.model.Mode == todo.ModeList {
			s.model.TodoList.MoveSelectionDown()
		}
	default:
		// Handle regular character input in add mode
		if s.model.Mode == todo.ModeAdd && len(key) == 1 {
			s.model.InputText += key
		}
	}
}

// sendEnhancedRender sends the current enhanced terminal render to the client
func (s *EnhancedTerminalSession) sendEnhancedRender() {
	rendered := s.model.View()
	
	response := EnhancedMessage{
		Type: "render",
		Data: map[string]interface{}{
			"content": rendered,
			"mode":    s.model.Mode,
			"stats": map[string]interface{}{
				"total":     s.model.TodoList.GetTotalCount(),
				"completed": s.model.TodoList.GetCompletedCount(),
				"selected":  s.model.TodoList.GetSelectedIndex(),
			},
		},
	}

	if data, err := json.Marshal(response); err == nil {
		select {
		case s.send <- data:
		default:
			// Channel is full, skip this render
		}
	}
}

