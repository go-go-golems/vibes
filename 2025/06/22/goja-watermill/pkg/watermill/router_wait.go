package watermill

import (
	"time"
)

// waitForRouter waits for the router to be fully started
func (m *Module) waitForRouter() bool {
	m.routerMutex.Lock()
	if m.routerStarted {
		m.routerMutex.Unlock()
		return true
	}
	m.routerMutex.Unlock()
	
	// Wait for router to start with timeout
	timeout := time.After(5 * time.Second)
	ticker := time.NewTicker(10 * time.Millisecond)
	defer ticker.Stop()
	
	for {
		select {
		case <-timeout:
			m.logger.Warn().Msg("Timeout waiting for router to be ready")
			return false
		case <-ticker.C:
			m.routerMutex.Lock()
			if m.routerStarted {
				m.routerMutex.Unlock()
				return true
			}
			m.routerMutex.Unlock()
		case <-m.ctx.Done():
			return false
		}
	}
}

