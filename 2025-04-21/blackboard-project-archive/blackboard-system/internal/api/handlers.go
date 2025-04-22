package api

import (
	"encoding/json"
	"net/http"
	"strconv"
	"time"

	"github.com/blackboard-system/internal/blackboard"
	"github.com/blackboard-system/internal/database"
	"github.com/blackboard-system/internal/models"
	"github.com/gorilla/mux"
)

// Handler contains the dependencies for the API handlers
type Handler struct {
	BB *blackboard.Blackboard
}

// NewHandler creates a new API handler
func NewHandler(bb *blackboard.Blackboard) *Handler {
	return &Handler{
		BB: bb,
	}
}

// GetBlackboardState returns the current state of the blackboard
func (h *Handler) GetBlackboardState(w http.ResponseWriter, r *http.Request) {
	state := h.BB.GetState()
	respondWithJSON(w, http.StatusOK, state)
}

// GetHypothesesByLevel returns all hypotheses for a specific level
func (h *Handler) GetHypothesesByLevel(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	level := vars["level"]

	hypotheses, err := database.GetHypothesesByLevel(level)
	if err != nil {
		respondWithError(w, http.StatusInternalServerError, err.Error())
		return
	}

	respondWithJSON(w, http.StatusOK, hypotheses)
}

// GetHypothesis returns a specific hypothesis
func (h *Handler) GetHypothesis(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	id := vars["id"]

	// Get all hypotheses and find the one with the matching ID
	level := getLevelFromID(id)
	hypotheses, err := database.GetHypothesesByLevel(level)
	if err != nil {
		respondWithError(w, http.StatusInternalServerError, err.Error())
		return
	}

	for _, hyp := range hypotheses {
		if hyp.ID == id {
			respondWithJSON(w, http.StatusOK, hyp)
			return
		}
	}

	respondWithError(w, http.StatusNotFound, "Hypothesis not found")
}

// CreateHypothesis creates a new hypothesis
func (h *Handler) CreateHypothesis(w http.ResponseWriter, r *http.Request) {
	var hyp models.Hypothesis
	if err := json.NewDecoder(r.Body).Decode(&hyp); err != nil {
		respondWithError(w, http.StatusBadRequest, "Invalid request payload")
		return
	}

	if err := h.BB.AddHypothesis(hyp); err != nil {
		respondWithError(w, http.StatusInternalServerError, err.Error())
		return
	}

	respondWithJSON(w, http.StatusCreated, hyp)
}

// UpdateHypothesis updates a hypothesis
func (h *Handler) UpdateHypothesis(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	id := vars["id"]

	var hyp models.Hypothesis
	if err := json.NewDecoder(r.Body).Decode(&hyp); err != nil {
		respondWithError(w, http.StatusBadRequest, "Invalid request payload")
		return
	}

	// Ensure ID in URL matches ID in payload
	if hyp.ID != id {
		respondWithError(w, http.StatusBadRequest, "ID in URL does not match ID in payload")
		return
	}

	if err := h.BB.UpdateHypothesis(hyp); err != nil {
		respondWithError(w, http.StatusInternalServerError, err.Error())
		return
	}

	respondWithJSON(w, http.StatusOK, hyp)
}

// LockHypothesis locks a hypothesis
func (h *Handler) LockHypothesis(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	id := vars["id"]

	var data struct {
		Owner string `json:"owner"`
	}
	if err := json.NewDecoder(r.Body).Decode(&data); err != nil {
		respondWithError(w, http.StatusBadRequest, "Invalid request payload")
		return
	}

	if err := h.BB.LockHypothesis(id, data.Owner); err != nil {
		respondWithError(w, http.StatusInternalServerError, err.Error())
		return
	}

	respondWithJSON(w, http.StatusOK, map[string]string{"message": "Hypothesis locked"})
}

// UnlockHypothesis unlocks a hypothesis
func (h *Handler) UnlockHypothesis(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	id := vars["id"]

	if err := h.BB.UnlockHypothesis(id); err != nil {
		respondWithError(w, http.StatusInternalServerError, err.Error())
		return
	}

	respondWithJSON(w, http.StatusOK, map[string]string{"message": "Hypothesis unlocked"})
}

// GetKnowledgeSources returns all knowledge sources
func (h *Handler) GetKnowledgeSources(w http.ResponseWriter, r *http.Request) {
	sources, err := database.GetKnowledgeSources()
	if err != nil {
		respondWithError(w, http.StatusInternalServerError, err.Error())
		return
	}

	respondWithJSON(w, http.StatusOK, sources)
}

// GetKnowledgeSource returns a specific knowledge source
func (h *Handler) GetKnowledgeSource(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	id := vars["id"]

	sources, err := database.GetKnowledgeSources()
	if err != nil {
		respondWithError(w, http.StatusInternalServerError, err.Error())
		return
	}

	for _, source := range sources {
		if source.ID == id {
			respondWithJSON(w, http.StatusOK, source)
			return
		}
	}

	respondWithError(w, http.StatusNotFound, "Knowledge source not found")
}

// UpdateKnowledgeSource updates a knowledge source
func (h *Handler) UpdateKnowledgeSource(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	id := vars["id"]

	var ks models.KnowledgeSource
	if err := json.NewDecoder(r.Body).Decode(&ks); err != nil {
		respondWithError(w, http.StatusBadRequest, "Invalid request payload")
		return
	}

	// Ensure ID in URL matches ID in payload
	if ks.ID != id {
		respondWithError(w, http.StatusBadRequest, "ID in URL does not match ID in payload")
		return
	}

	if err := h.BB.UpdateKnowledgeSource(ks); err != nil {
		respondWithError(w, http.StatusInternalServerError, err.Error())
		return
	}

	respondWithJSON(w, http.StatusOK, ks)
}

// SubmitBid submits a bid from a knowledge source
func (h *Handler) SubmitBid(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	id := vars["id"]

	var bid models.BidQueue
	if err := json.NewDecoder(r.Body).Decode(&bid); err != nil {
		respondWithError(w, http.StatusBadRequest, "Invalid request payload")
		return
	}

	// Ensure ID in URL matches ID in payload
	if bid.KnowledgeSourceID != id {
		respondWithError(w, http.StatusBadRequest, "ID in URL does not match ID in payload")
		return
	}

	if err := h.BB.AddBid(bid); err != nil {
		respondWithError(w, http.StatusInternalServerError, err.Error())
		return
	}

	respondWithJSON(w, http.StatusCreated, bid)
}

// GetActivityLog returns the activity log
func (h *Handler) GetActivityLog(w http.ResponseWriter, r *http.Request) {
	limit := 10
	if limitStr := r.URL.Query().Get("limit"); limitStr != "" {
		var err error
		limit, err = strconv.Atoi(limitStr)
		if err != nil {
			respondWithError(w, http.StatusBadRequest, "Invalid limit parameter")
			return
		}
	}

	logs, err := database.GetActivityLog(limit)
	if err != nil {
		respondWithError(w, http.StatusInternalServerError, err.Error())
		return
	}

	respondWithJSON(w, http.StatusOK, logs)
}

// AddActivityLog adds an activity log entry
func (h *Handler) AddActivityLog(w http.ResponseWriter, r *http.Request) {
	var log models.ActivityLog
	if err := json.NewDecoder(r.Body).Decode(&log); err != nil {
		respondWithError(w, http.StatusBadRequest, "Invalid request payload")
		return
	}

	// Set the time if not provided
	if log.Time == "" {
		log.Time = time.Now().Format("15:04:05.000")
	}

	if err := h.BB.AddActivityLog(log); err != nil {
		respondWithError(w, http.StatusInternalServerError, err.Error())
		return
	}

	respondWithJSON(w, http.StatusCreated, log)
}

// GetFocusControl returns the current focus control
func (h *Handler) GetFocusControl(w http.ResponseWriter, r *http.Request) {
	fc, err := database.GetFocusControl()
	if err != nil {
		respondWithError(w, http.StatusInternalServerError, err.Error())
		return
	}

	respondWithJSON(w, http.StatusOK, fc)
}

// UpdateFocusControl updates the focus control
func (h *Handler) UpdateFocusControl(w http.ResponseWriter, r *http.Request) {
	var data struct {
		CurrentFocus string `json:"current_focus"`
	}
	if err := json.NewDecoder(r.Body).Decode(&data); err != nil {
		respondWithError(w, http.StatusBadRequest, "Invalid request payload")
		return
	}

	if err := h.BB.UpdateFocus(data.CurrentFocus); err != nil {
		respondWithError(w, http.StatusInternalServerError, err.Error())
		return
	}

	respondWithJSON(w, http.StatusOK, map[string]string{"message": "Focus control updated"})
}

// GetFocusPriorities returns the focus priorities
func (h *Handler) GetFocusPriorities(w http.ResponseWriter, r *http.Request) {
	fc, err := database.GetFocusControl()
	if err != nil {
		respondWithError(w, http.StatusInternalServerError, err.Error())
		return
	}

	priorities, err := database.GetFocusPriorities(fc.ID)
	if err != nil {
		respondWithError(w, http.StatusInternalServerError, err.Error())
		return
	}

	respondWithJSON(w, http.StatusOK, priorities)
}

// GetBidQueue returns the current bid queue
func (h *Handler) GetBidQueue(w http.ResponseWriter, r *http.Request) {
	bids, err := database.GetBidQueue()
	if err != nil {
		respondWithError(w, http.StatusInternalServerError, err.Error())
		return
	}

	respondWithJSON(w, http.StatusOK, bids)
}

// DeleteBid removes a bid from the queue
func (h *Handler) DeleteBid(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	idStr := vars["id"]
	id, err := strconv.ParseInt(idStr, 10, 64)
	if err != nil {
		respondWithError(w, http.StatusBadRequest, "Invalid bid ID")
		return
	}

	if err := h.BB.RemoveBid(id); err != nil {
		respondWithError(w, http.StatusInternalServerError, err.Error())
		return
	}

	respondWithJSON(w, http.StatusOK, map[string]string{"message": "Bid removed"})
}

// GetCycle returns the current cycle count
func (h *Handler) GetCycle(w http.ResponseWriter, r *http.Request) {
	cycle := h.BB.GetCycle()
	respondWithJSON(w, http.StatusOK, map[string]int{"cycle": cycle})
}

// Helper function to get level from hypothesis ID
func getLevelFromID(id string) string {
	if len(id) < 3 {
		return "UNKNOWN"
	}

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

// respondWithError returns an error response
func respondWithError(w http.ResponseWriter, code int, message string) {
	respondWithJSON(w, code, map[string]string{"error": message})
}

// respondWithJSON returns a JSON response
func respondWithJSON(w http.ResponseWriter, code int, payload interface{}) {
	response, _ := json.Marshal(payload)
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(code)
	w.Write(response)
}
