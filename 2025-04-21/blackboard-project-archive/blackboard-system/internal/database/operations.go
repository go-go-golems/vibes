package database

import (
	"database/sql"
	"fmt"
	"log"
	"time"

	"github.com/blackboard-system/internal/models"
)

// GetHypothesesByLevel retrieves all hypotheses for a specific level
func GetHypothesesByLevel(level string) ([]models.Hypothesis, error) {
	rows, err := DB.Query(`
		SELECT h.id, h.blackboard_id, h.content, h.confidence, h.time_range, h.owner, h.locked, h.created_at
		FROM hypothesis h
		JOIN blackboard b ON h.blackboard_id = b.id
		WHERE b.level = ?
		ORDER BY h.created_at ASC
	`, level)
	if err != nil {
		return nil, fmt.Errorf("failed to query hypotheses: %v", err)
	}
	defer rows.Close()

	var hypotheses []models.Hypothesis
	for rows.Next() {
		var h models.Hypothesis
		var createdAt string
		if err := rows.Scan(&h.ID, &h.BlackboardID, &h.Content, &h.Confidence, &h.TimeRange, &h.Owner, &h.Locked, &createdAt); err != nil {
			return nil, fmt.Errorf("failed to scan hypothesis: %v", err)
		}
		t, _ := time.Parse("2006-01-02 15:04:05", createdAt)
		h.CreatedAt = t
		hypotheses = append(hypotheses, h)
	}
	return hypotheses, nil
}

// CreateHypothesis creates a new hypothesis
func CreateHypothesis(h models.Hypothesis) error {
	// Get blackboard ID for the level
	var blackboardID int64
	err := DB.QueryRow("SELECT id FROM blackboard WHERE level = ?", getLevelFromID(h.ID)).Scan(&blackboardID)
	if err != nil {
		if err == sql.ErrNoRows {
			// Create blackboard level if it doesn't exist
			result, err := DB.Exec("INSERT INTO blackboard (level) VALUES (?)", getLevelFromID(h.ID))
			if err != nil {
				return fmt.Errorf("failed to create blackboard level: %v", err)
			}
			blackboardID, _ = result.LastInsertId()
		} else {
			return fmt.Errorf("failed to get blackboard ID: %v", err)
		}
	}

	h.BlackboardID = blackboardID

	_, err = DB.Exec(`
		INSERT INTO hypothesis (id, blackboard_id, content, confidence, time_range, owner, locked)
		VALUES (?, ?, ?, ?, ?, ?, ?)
	`, h.ID, h.BlackboardID, h.Content, h.Confidence, h.TimeRange, h.Owner, h.Locked)
	if err != nil {
		return fmt.Errorf("failed to create hypothesis: %v", err)
	}
	return nil
}

// UpdateHypothesis updates an existing hypothesis
func UpdateHypothesis(h models.Hypothesis) error {
	_, err := DB.Exec(`
		UPDATE hypothesis
		SET content = ?, confidence = ?, time_range = ?, owner = ?, locked = ?
		WHERE id = ?
	`, h.Content, h.Confidence, h.TimeRange, h.Owner, h.Locked, h.ID)
	if err != nil {
		return fmt.Errorf("failed to update hypothesis: %v", err)
	}
	return nil
}

// DeleteHypothesis deletes a hypothesis
func DeleteHypothesis(id string) error {
	_, err := DB.Exec("DELETE FROM hypothesis WHERE id = ?", id)
	if err != nil {
		return fmt.Errorf("failed to delete hypothesis: %v", err)
	}
	return nil
}

// GetKnowledgeSources retrieves all knowledge sources
func GetKnowledgeSources() ([]models.KnowledgeSource, error) {
	rows, err := DB.Query(`
		SELECT id, name, status, last_action, bid_value, activation_pattern, created_at
		FROM knowledge_source
		ORDER BY name ASC
	`)
	if err != nil {
		return nil, fmt.Errorf("failed to query knowledge sources: %v", err)
	}
	defer rows.Close()

	var sources []models.KnowledgeSource
	for rows.Next() {
		var ks models.KnowledgeSource
		var createdAt string
		if err := rows.Scan(&ks.ID, &ks.Name, &ks.Status, &ks.LastAction, &ks.BidValue, &ks.ActivationPattern, &createdAt); err != nil {
			return nil, fmt.Errorf("failed to scan knowledge source: %v", err)
		}
		t, _ := time.Parse("2006-01-02 15:04:05", createdAt)
		ks.CreatedAt = t
		sources = append(sources, ks)
	}
	return sources, nil
}

// CreateKnowledgeSource creates a new knowledge source
func CreateKnowledgeSource(ks models.KnowledgeSource) error {
	_, err := DB.Exec(`
		INSERT INTO knowledge_source (id, name, status, last_action, bid_value, activation_pattern)
		VALUES (?, ?, ?, ?, ?, ?)
	`, ks.ID, ks.Name, ks.Status, ks.LastAction, ks.BidValue, ks.ActivationPattern)
	if err != nil {
		return fmt.Errorf("failed to create knowledge source: %v", err)
	}
	return nil
}

// UpdateKnowledgeSource updates an existing knowledge source
func UpdateKnowledgeSource(ks models.KnowledgeSource) error {
	_, err := DB.Exec(`
		UPDATE knowledge_source
		SET name = ?, status = ?, last_action = ?, bid_value = ?, activation_pattern = ?
		WHERE id = ?
	`, ks.Name, ks.Status, ks.LastAction, ks.BidValue, ks.ActivationPattern, ks.ID)
	if err != nil {
		return fmt.Errorf("failed to update knowledge source: %v", err)
	}
	return nil
}

// CreateActivityLog creates a new activity log entry
func CreateActivityLog(al models.ActivityLog) error {
	_, err := DB.Exec(`
		INSERT INTO activity_log (time, knowledge_source_id, action, level)
		VALUES (?, ?, ?, ?)
	`, al.Time, al.KnowledgeSourceID, al.Action, al.Level)
	if err != nil {
		return fmt.Errorf("failed to create activity log: %v", err)
	}
	return nil
}

// GetActivityLog retrieves recent activity log entries
func GetActivityLog(limit int) ([]models.ActivityLog, error) {
	rows, err := DB.Query(`
		SELECT id, time, knowledge_source_id, action, level, created_at
		FROM activity_log
		ORDER BY created_at DESC
		LIMIT ?
	`, limit)
	if err != nil {
		return nil, fmt.Errorf("failed to query activity log: %v", err)
	}
	defer rows.Close()

	var logs []models.ActivityLog
	for rows.Next() {
		var al models.ActivityLog
		var createdAt string
		if err := rows.Scan(&al.ID, &al.Time, &al.KnowledgeSourceID, &al.Action, &al.Level, &createdAt); err != nil {
			return nil, fmt.Errorf("failed to scan activity log: %v", err)
		}
		t, _ := time.Parse("2006-01-02 15:04:05", createdAt)
		al.CreatedAt = t
		logs = append(logs, al)
	}
	return logs, nil
}

// GetFocusControl retrieves the current focus control
func GetFocusControl() (models.FocusControl, error) {
	var fc models.FocusControl
	var createdAt string
	err := DB.QueryRow(`
		SELECT id, current_focus, created_at
		FROM focus_control
		ORDER BY created_at DESC
		LIMIT 1
	`).Scan(&fc.ID, &fc.CurrentFocus, &createdAt)
	if err != nil {
		if err == sql.ErrNoRows {
			// Create default focus control if none exists
			result, err := DB.Exec("INSERT INTO focus_control (current_focus) VALUES ('WORD')")
			if err != nil {
				return fc, fmt.Errorf("failed to create default focus control: %v", err)
			}
			fc.ID, _ = result.LastInsertId()
			fc.CurrentFocus = "WORD"
			fc.CreatedAt = time.Now()
			return fc, nil
		}
		return fc, fmt.Errorf("failed to get focus control: %v", err)
	}
	t, _ := time.Parse("2006-01-02 15:04:05", createdAt)
	fc.CreatedAt = t
	return fc, nil
}

// UpdateFocusControl updates the focus control
func UpdateFocusControl(fc models.FocusControl) error {
	_, err := DB.Exec(`
		UPDATE focus_control
		SET current_focus = ?
		WHERE id = ?
	`, fc.CurrentFocus, fc.ID)
	if err != nil {
		return fmt.Errorf("failed to update focus control: %v", err)
	}
	return nil
}

// GetFocusPriorities retrieves all focus priorities
func GetFocusPriorities(focusControlID int64) (map[string]float64, error) {
	rows, err := DB.Query(`
		SELECT level, priority
		FROM focus_priority
		WHERE focus_control_id = ?
	`, focusControlID)
	if err != nil {
		return nil, fmt.Errorf("failed to query focus priorities: %v", err)
	}
	defer rows.Close()

	priorities := make(map[string]float64)
	for rows.Next() {
		var level string
		var priority float64
		if err := rows.Scan(&level, &priority); err != nil {
			return nil, fmt.Errorf("failed to scan focus priority: %v", err)
		}
		priorities[level] = priority
	}

	// If no priorities exist, create defaults
	if len(priorities) == 0 {
		defaultPriorities := map[string]float64{
			"PARAMETER": 0.15,
			"SEGMENT":   0.25,
			"SYLLABLE":  0.40,
			"WORD":      0.85,
			"PHRASE":    0.75,
		}

		for level, priority := range defaultPriorities {
			_, err := DB.Exec(`
				INSERT INTO focus_priority (level, priority, focus_control_id)
				VALUES (?, ?, ?)
			`, level, priority, focusControlID)
			if err != nil {
				return nil, fmt.Errorf("failed to create default focus priority: %v", err)
			}
			priorities[level] = priority
		}
	}

	return priorities, nil
}

// GetBidQueue retrieves the current bid queue
func GetBidQueue() ([]models.BidQueue, error) {
	rows, err := DB.Query(`
		SELECT id, knowledge_source_id, bid_value, target, action, created_at
		FROM bid_queue
		ORDER BY bid_value DESC
	`)
	if err != nil {
		return nil, fmt.Errorf("failed to query bid queue: %v", err)
	}
	defer rows.Close()

	var bids []models.BidQueue
	for rows.Next() {
		var bid models.BidQueue
		var createdAt string
		if err := rows.Scan(&bid.ID, &bid.KnowledgeSourceID, &bid.BidValue, &bid.Target, &bid.Action, &createdAt); err != nil {
			return nil, fmt.Errorf("failed to scan bid: %v", err)
		}
		t, _ := time.Parse("2006-01-02 15:04:05", createdAt)
		bid.CreatedAt = t
		bids = append(bids, bid)
	}
	return bids, nil
}

// CreateBid adds a new bid to the queue
func CreateBid(bid models.BidQueue) error {
	_, err := DB.Exec(`
		INSERT INTO bid_queue (knowledge_source_id, bid_value, target, action)
		VALUES (?, ?, ?, ?)
	`, bid.KnowledgeSourceID, bid.BidValue, bid.Target, bid.Action)
	if err != nil {
		return fmt.Errorf("failed to create bid: %v", err)
	}
	return nil
}

// DeleteBid removes a bid from the queue
func DeleteBid(id int64) error {
	_, err := DB.Exec("DELETE FROM bid_queue WHERE id = ?", id)
	if err != nil {
		return fmt.Errorf("failed to delete bid: %v", err)
	}
	return nil
}

// GetBlackboardState retrieves the complete state of the blackboard system
func GetBlackboardState() (models.BlackboardState, error) {
	var state models.BlackboardState

	// Get hypotheses for each level
	phrase, err := GetHypothesesByLevel("PHRASE")
	if err != nil {
		return state, err
	}
	state.Phrase = phrase

	word, err := GetHypothesesByLevel("WORD")
	if err != nil {
		return state, err
	}
	state.Word = word

	syllable, err := GetHypothesesByLevel("SYLLABLE")
	if err != nil {
		return state, err
	}
	state.Syllable = syllable

	segment, err := GetHypothesesByLevel("SEGMENT")
	if err != nil {
		return state, err
	}
	state.Segment = segment

	parameter, err := GetHypothesesByLevel("PARAMETER")
	if err != nil {
		return state, err
	}
	state.Parameter = parameter

	// Get knowledge sources
	ksources, err := GetKnowledgeSources()
	if err != nil {
		return state, err
	}
	state.KSources = ksources

	// Get focus control
	fc, err := GetFocusControl()
	if err != nil {
		return state, err
	}
	state.Focus.CurrentFocus = fc.CurrentFocus

	// Get focus priorities
	priorities, err := GetFocusPriorities(fc.ID)
	if err != nil {
		return state, err
	}
	state.Focus.FocusPriorities = priorities

	// Get bid queue
	bids, err := GetBidQueue()
	if err != nil {
		return state, err
	}
	state.Focus.BidQueue = bids

	// Get activity log
	logs, err := GetActivityLog(10)
	if err != nil {
		return state, err
	}
	state.ActivityLog = logs

	return state, nil
}

// Helper function to get level from hypothesis ID
func getLevelFromID(id string) string {
	prefix := id[:3]
	switch prefix {
	case "PHR":
		return "PHRASE"
	case "WRD":
		return "WORD"
	case "SYL":
		return "SYLLABLE"
	case "SEG":
		return "SEGMENT"
	case "PAR":
		return "PARAMETER"
	default:
		return "UNKNOWN"
	}
}

// InitializeDefaultData populates the database with default data if empty
func InitializeDefaultData() error {
	// Check if we have any knowledge sources
	var count int
	err := DB.QueryRow("SELECT COUNT(*) FROM knowledge_source").Scan(&count)
	if err != nil {
		return fmt.Errorf("failed to check knowledge sources: %v", err)
	}

	if count == 0 {
		log.Println("Initializing default data...")

		// Create default knowledge sources
		knowledgeSources := []models.KnowledgeSource{
			{
				ID:                "KS_SEG",
				Name:              "SEG: Segment Creator",
				Status:            "active",
				LastAction:        "Creating segment SEG-004 from raw parameters",
				BidValue:          0.75,
				ActivationPattern: "When parameter hypotheses above 0.9 confidence",
			},
			{
				ID:                "KS_SYL",
				Name:              "POM: Phone to Syllable",
				Status:            "waiting",
				LastAction:        "Waiting for more segment hypotheses",
				BidValue:          0.68,
				ActivationPattern: "When adjacent segments form valid phoneme sequence",
			},
			{
				ID:                "KS_WRD",
				Name:              "MOW: Syllable to Word",
				Status:            "idle",
				LastAction:        "Last created WRD-005 from syllables",
				BidValue:          0.52,
				ActivationPattern: "When syllables match lexicon entries",
			},
			{
				ID:                "KS_PARSE",
				Name:              "PARSE: Syntax Parser",
				Status:            "ready",
				LastAction:        "Ready to parse word sequence",
				BidValue:          0.85,
				ActivationPattern: "When contiguous word sequence exists",
			},
			{
				ID:                "KS_PRED",
				Name:              "PREDICT: Word Predictor",
				Status:            "activating",
				LastAction:        "Generating predictions based on syntax",
				BidValue:          0.78,
				ActivationPattern: "When partial syntactic structure available",
			},
			{
				ID:                "KS_RATE",
				Name:              "RPOL: Hypothesis Rater",
				Status:            "active",
				LastAction:        "Rating credibility of new hypotheses",
				BidValue:          0.92,
				ActivationPattern: "When new hypotheses are created",
			},
		}

		for _, ks := range knowledgeSources {
			if err := CreateKnowledgeSource(ks); err != nil {
				return fmt.Errorf("failed to create knowledge source %s: %v", ks.ID, err)
			}
		}

		// Create default hypotheses
		hypotheses := []models.Hypothesis{
			{
				ID:         "PHR-123",
				Content:    "\"THE PRICE OF BLUE CHIPS DECLINED\"",
				Confidence: 0.85,
				TimeRange:  "0-2500ms",
				Locked:     false,
			},
			{
				ID:         "WRD-001",
				Content:    "THE",
				Confidence: 0.95,
				TimeRange:  "0-200ms",
				Locked:     false,
			},
			{
				ID:         "WRD-002",
				Content:    "PRICE",
				Confidence: 0.88,
				TimeRange:  "200-450ms",
				Owner:      "KS_LEX",
				Locked:     true,
			},
			{
				ID:         "WRD-003",
				Content:    "OF",
				Confidence: 0.92,
				TimeRange:  "450-600ms",
				Locked:     false,
			},
			{
				ID:         "WRD-004",
				Content:    "BLUE",
				Confidence: 0.78,
				TimeRange:  "600-850ms",
				Locked:     false,
			},
			{
				ID:         "WRD-005",
				Content:    "CHIPS",
				Confidence: 0.83,
				TimeRange:  "850-1150ms",
				Locked:     false,
			},
			{
				ID:         "SYL-001",
				Content:    "THE",
				Confidence: 0.96,
				TimeRange:  "0-200ms",
				Locked:     false,
			},
			{
				ID:         "SYL-002",
				Content:    "PRI",
				Confidence: 0.85,
				TimeRange:  "200-350ms",
				Owner:      "KS_SYL",
				Locked:     true,
			},
			{
				ID:         "SYL-003",
				Content:    "CE",
				Confidence: 0.84,
				TimeRange:  "350-450ms",
				Locked:     false,
			},
			{
				ID:         "SYL-004",
				Content:    "BLU",
				Confidence: 0.75,
				TimeRange:  "600-750ms",
				Locked:     false,
			},
			{
				ID:         "SYL-005",
				Content:    "CHIPS",
				Confidence: 0.81,
				TimeRange:  "850-1150ms",
				Locked:     false,
			},
			{
				ID:         "SEG-001",
				Content:    "[DH-AH]",
				Confidence: 0.92,
				TimeRange:  "0-100ms",
				Locked:     false,
			},
			{
				ID:         "SEG-002",
				Content:    "[P-R-AY]",
				Confidence: 0.78,
				TimeRange:  "200-350ms",
				Owner:      "KS_PHON",
				Locked:     true,
			},
			{
				ID:         "SEG-003",
				Content:    "[S]",
				Confidence: 0.88,
				TimeRange:  "350-450ms",
				Locked:     false,
			},
			{
				ID:         "SEG-004",
				Content:    "[B-L-UW]",
				Confidence: 0.72,
				TimeRange:  "600-750ms",
				Locked:     false,
			},
			{
				ID:         "PAR-001",
				Content:    "Frequency: 220Hz-350Hz",
				Confidence: 0.98,
				TimeRange:  "0-50ms",
				Locked:     false,
			},
			{
				ID:         "PAR-002",
				Content:    "Formant: F1=400Hz F2=2000Hz",
				Confidence: 0.95,
				TimeRange:  "50-100ms",
				Locked:     false,
			},
			{
				ID:         "PAR-003",
				Content:    "Energy: Peak=65dB",
				Confidence: 0.97,
				TimeRange:  "100-150ms",
				Locked:     false,
			},
		}

		for _, h := range hypotheses {
			if err := CreateHypothesis(h); err != nil {
				return fmt.Errorf("failed to create hypothesis %s: %v", h.ID, err)
			}
		}

		// Create default activity log entries
		activityLogs := []models.ActivityLog{
			{
				Time:              "14:32:15.123",
				KnowledgeSourceID: "KS_SEG",
				Action:            "Created SEG-004: \"[B-L-UW]\" from parameters",
				Level:             "SEGMENT",
			},
			{
				Time:              "14:32:15.156",
				KnowledgeSourceID: "KS_RATE",
				Action:            "Rated SEG-004 confidence: 0.72",
				Level:             "SEGMENT",
			},
			{
				Time:              "14:32:15.189",
				KnowledgeSourceID: "KS_SYL",
				Action:            "Activated by SEG-004 creation",
				Level:             "SYLLABLE",
			},
			{
				Time:              "14:32:15.223",
				KnowledgeSourceID: "FOCUS",
				Action:            "Shifted focus from SEGMENT to WORD level",
				Level:             "CONTROL",
			},
			{
				Time:              "14:32:15.267",
				KnowledgeSourceID: "KS_PARSE",
				Action:            "Bidding for control (0.85) on word sequence",
				Level:             "WORD",
			},
		}

		for _, al := range activityLogs {
			if err := CreateActivityLog(al); err != nil {
				return fmt.Errorf("failed to create activity log: %v", err)
			}
		}

		// Create default bids
		bids := []models.BidQueue{
			{
				KnowledgeSourceID: "KS_PARSE",
				BidValue:          0.85,
				Target:            "PHR-124",
				Action:            "Create phrase hypothesis",
			},
			{
				KnowledgeSourceID: "KS_PRED",
				BidValue:          0.78,
				Target:            "WRD-006",
				Action:            "Predict next word",
			},
			{
				KnowledgeSourceID: "KS_SEG",
				BidValue:          0.75,
				Target:            "SEG-005",
				Action:            "Create new segment",
			},
		}

		for _, bid := range bids {
			if err := CreateBid(bid); err != nil {
				return fmt.Errorf("failed to create bid: %v", err)
			}
		}

		log.Println("Default data initialized successfully")
	}

	return nil
}
