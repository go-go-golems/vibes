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

var upgrader = websocket.Upgrader{
	CheckOrigin: func(r *http.Request) bool {
		return true // Allow all origins for development
	},
}

// Message represents a websocket message
type Message struct {
	Type string      `json:"type"`
	Data interface{} `json:"data"`
}

// KeyEvent represents a keyboard event from the web client
type KeyEvent struct {
	Key       string `json:"key"`
	CtrlKey   bool   `json:"ctrlKey"`
	AltKey    bool   `json:"altKey"`
	ShiftKey  bool   `json:"shiftKey"`
	MetaKey   bool   `json:"metaKey"`
}

// TerminalSession represents a websocket session with a terminal
type TerminalSession struct {
	conn     *websocket.Conn
	model    *todo.Model
	send     chan []byte
	done     chan bool
	mutex    sync.RWMutex
}

// NewTerminalSession creates a new terminal session
func NewTerminalSession(conn *websocket.Conn) *TerminalSession {
	return &TerminalSession{
		conn:  conn,
		model: &todo.Model{},
		send:  make(chan []byte, 256),
		done:  make(chan bool),
	}
}

// HandleWebSocket handles websocket connections
func HandleWebSocket(w http.ResponseWriter, r *http.Request) {
	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		log.Printf("WebSocket upgrade error: %v", err)
		return
	}
	defer conn.Close()

	session := NewTerminalSession(conn)
	
	// Initialize the model
	*session.model = todo.NewModel()
	
	// Add some sample todos
	session.model.TodoList.AddItem("Learn Bubbletea")
	session.model.TodoList.AddItem("Build a todo app")
	session.model.TodoList.AddItem("Add web interface")
	session.model.TodoList.SetSelectedIndex(0)

	// Start goroutines for handling the session
	go session.writePump()
	go session.readPump()

	// Send initial render
	session.sendRender()

	// Wait for session to end
	<-session.done
}

// readPump handles incoming messages from the websocket
func (s *TerminalSession) readPump() {
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

		var msg Message
		if err := json.Unmarshal(message, &msg); err != nil {
			log.Printf("JSON unmarshal error: %v", err)
			continue
		}

		s.handleMessage(msg)
	}
}

// writePump handles outgoing messages to the websocket
func (s *TerminalSession) writePump() {
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

// handleMessage processes incoming messages
func (s *TerminalSession) handleMessage(msg Message) {
	s.mutex.Lock()
	defer s.mutex.Unlock()

	switch msg.Type {
	case "keypress":
		if keyData, ok := msg.Data.(map[string]interface{}); ok {
			keyEvent := KeyEvent{}
			if keyBytes, err := json.Marshal(keyData); err == nil {
				json.Unmarshal(keyBytes, &keyEvent)
			}

			// Process the key event
			s.processKeyEvent(keyEvent)
			
			// Send updated render
			s.sendRender()
		}
	case "resize":
		if resizeData, ok := msg.Data.(map[string]interface{}); ok {
			if width, ok := resizeData["width"].(float64); ok {
				if height, ok := resizeData["height"].(float64); ok {
					s.model.Width = int(width)
					s.model.Height = int(height)
					s.sendRender()
				}
			}
		}
	}
}

// processKeyEvent converts web key events to model updates
func (s *TerminalSession) processKeyEvent(keyEvent KeyEvent) {
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

// sendRender sends the current terminal render to the client
func (s *TerminalSession) sendRender() {
	rendered := s.model.View()
	
	response := Message{
		Type: "render",
		Data: map[string]interface{}{
			"content": rendered,
			"mode":    s.model.Mode,
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

