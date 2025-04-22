package agents

import (
	"fmt"
	"log"
	"math/rand"
	"strconv"
	"strings"
	"time"

	"github.com/blackboard-system/internal/blackboard"
	"github.com/blackboard-system/internal/models"
)

// BaseAgent provides common functionality for all agents
type BaseAgent struct {
	id                string
	name              string
	activationPattern string
	status            string
	lastAction        string
	bidValue          float64
}

// ID returns the agent's ID
func (a *BaseAgent) ID() string {
	return a.id
}

// SegmentCreator is the SEG agent that creates segments from parameters
type SegmentCreator struct {
	BaseAgent
	nextSegmentID int
}

// NewSegmentCreator creates a new segment creator agent
func NewSegmentCreator() *SegmentCreator {
	return &SegmentCreator{
		BaseAgent: BaseAgent{
			id:                "KS_SEG",
			name:              "SEG: Segment Creator",
			activationPattern: "When parameter hypotheses above 0.9 confidence",
			status:            "active",
			lastAction:        "Creating segment from raw parameters",
			bidValue:          0.75,
		},
		nextSegmentID: 5, // Start from SEG-005
	}
}

// CanActivate checks if the agent can activate
func (a *SegmentCreator) CanActivate(state models.BlackboardState) (bool, float64) {
	// Check if there are high-confidence parameter hypotheses
	for _, param := range state.Parameter {
		if param.Confidence > 0.9 && !param.Locked {
			// Randomize bid value slightly
			bidValue := a.bidValue + (rand.Float64()*0.1 - 0.05)
			return true, bidValue
		}
	}
	return false, 0
}

// Activate performs the agent's action
func (a *SegmentCreator) Activate(bb *blackboard.Blackboard) error {
	// Get current state
	state := bb.GetState()
	
	// Find a high-confidence parameter to work with
	var selectedParam *models.Hypothesis
	for _, param := range state.Parameter {
		if param.Confidence > 0.9 && !param.Locked {
			selectedParam = &param
			break
		}
	}
	
	if selectedParam == nil {
		return fmt.Errorf("no suitable parameter found")
	}
	
	// Lock the parameter
	if err := bb.LockHypothesis(selectedParam.ID, a.id); err != nil {
		return err
	}
	
	// Create a new segment
	segmentID := fmt.Sprintf("SEG-%03d", a.nextSegmentID)
	a.nextSegmentID++
	
	// Generate segment content based on parameter
	var content string
	switch {
	case selectedParam.Content == "Frequency: 220Hz-350Hz":
		content = "[DH-AH]"
	case selectedParam.Content == "Formant: F1=400Hz F2=2000Hz":
		content = "[P-R-AY]"
	case selectedParam.Content == "Energy: Peak=65dB":
		content = "[S]"
	default:
		content = fmt.Sprintf("[%c-%c-%c]", 
			'A' + byte(rand.Intn(26)), 
			'A' + byte(rand.Intn(26)), 
			'A' + byte(rand.Intn(26)))
	}
	
	// Calculate time range based on parameter
	timeRange := fmt.Sprintf("%s-+100ms", selectedParam.TimeRange)
	
	// Create segment hypothesis
	segment := models.Hypothesis{
		ID:         segmentID,
		Content:    content,
		Confidence: 0.7 + rand.Float64()*0.2, // Random confidence between 0.7 and 0.9
		TimeRange:  timeRange,
		Locked:     false,
	}
	
	if err := bb.AddHypothesis(segment); err != nil {
		// Unlock the parameter
		bb.UnlockHypothesis(selectedParam.ID)
		return err
	}
	
	// Update agent status
	a.lastAction = fmt.Sprintf("Created %s: \"%s\" from parameters", segmentID, content)
	a.status = "active"
	
	// Create activity log
	al := models.ActivityLog{
		Time:              time.Now().Format("15:04:05.000"),
		KnowledgeSourceID: a.id,
		Action:            fmt.Sprintf("Created %s: \"%s\" from parameters", segmentID, content),
		Level:             "SEGMENT",
	}
	
	if err := bb.AddActivityLog(al); err != nil {
		log.Printf("Error adding activity log: %v", err)
	}
	
	// Update knowledge source in blackboard
	ks := models.KnowledgeSource{
		ID:                a.id,
		Name:              a.name,
		Status:            a.status,
		LastAction:        a.lastAction,
		BidValue:          a.bidValue,
		ActivationPattern: a.activationPattern,
	}
	
	if err := bb.UpdateKnowledgeSource(ks); err != nil {
		log.Printf("Error updating knowledge source: %v", err)
	}
	
	// Unlock the parameter
	bb.UnlockHypothesis(selectedParam.ID)
	
	return nil
}

// PhoneToSyllable is the POM agent that creates syllables from segments
type PhoneToSyllable struct {
	BaseAgent
	nextSyllableID int
}

// NewPhoneToSyllable creates a new phone to syllable agent
func NewPhoneToSyllable() *PhoneToSyllable {
	return &PhoneToSyllable{
		BaseAgent: BaseAgent{
			id:                "KS_SYL",
			name:              "POM: Phone to Syllable",
			activationPattern: "When adjacent segments form valid phoneme sequence",
			status:            "waiting",
			lastAction:        "Waiting for more segment hypotheses",
			bidValue:          0.68,
		},
		nextSyllableID: 6, // Start from SYL-006
	}
}

// CanActivate checks if the agent can activate
func (a *PhoneToSyllable) CanActivate(state models.BlackboardState) (bool, float64) {
	// Check if there are enough segments to form a syllable
	if len(state.Segment) >= 2 {
		// Randomize bid value slightly
		bidValue := a.bidValue + (rand.Float64()*0.1 - 0.05)
		return true, bidValue
	}
	return false, 0
}

// Activate performs the agent's action
func (a *PhoneToSyllable) Activate(bb *blackboard.Blackboard) error {
	// Get current state
	state := bb.GetState()
	
	// Find segments to work with
	if len(state.Segment) < 2 {
		return fmt.Errorf("not enough segments")
	}
	
	// Select two adjacent segments
	var seg1, seg2 *models.Hypothesis
	for i := 0; i < len(state.Segment)-1; i++ {
		if !state.Segment[i].Locked && !state.Segment[i+1].Locked {
			seg1 = &state.Segment[i]
			seg2 = &state.Segment[i+1]
			break
		}
	}
	
	if seg1 == nil || seg2 == nil {
		return fmt.Errorf("no suitable segments found")
	}
	
	// Lock the segments
	if err := bb.LockHypothesis(seg1.ID, a.id); err != nil {
		return err
	}
	if err := bb.LockHypothesis(seg2.ID, a.id); err != nil {
		bb.UnlockHypothesis(seg1.ID)
		return err
	}
	
	// Create a new syllable
	syllableID := fmt.Sprintf("SYL-%03d", a.nextSyllableID)
	a.nextSyllableID++
	
	// Generate syllable content based on segments
	content := fmt.Sprintf("%s%s", seg1.Content[1:2], seg2.Content[1:2])
	
	// Calculate time range based on segments
	timeStart := seg1.TimeRange[:strings.Index(seg1.TimeRange, "-")]
	timeEnd := seg2.TimeRange[strings.Index(seg2.TimeRange, "-")+1:]
	timeRange := fmt.Sprintf("%s-%s", timeStart, timeEnd)
	
	// Create syllable hypothesis
	syllable := models.Hypothesis{
		ID:         syllableID,
		Content:    content,
		Confidence: (seg1.Confidence + seg2.Confidence) / 2, // Average confidence
		TimeRange:  timeRange,
		Locked:     false,
	}
	
	if err := bb.AddHypothesis(syllable); err != nil {
		// Unlock the segments
		bb.UnlockHypothesis(seg1.ID)
		bb.UnlockHypothesis(seg2.ID)
		return err
	}
	
	// Update agent status
	a.lastAction = fmt.Sprintf("Created %s: \"%s\" from segments", syllableID, content)
	a.status = "active"
	
	// Create activity log
	al := models.ActivityLog{
		Time:              time.Now().Format("15:04:05.000"),
		KnowledgeSourceID: a.id,
		Action:            fmt.Sprintf("Created %s: \"%s\" from segments %s and %s", 
			syllableID, content, seg1.ID, seg2.ID),
		Level:             "SYLLABLE",
	}
	
	if err := bb.AddActivityLog(al); err != nil {
		log.Printf("Error adding activity log: %v", err)
	}
	
	// Update knowledge source in blackboard
	ks := models.KnowledgeSource{
		ID:                a.id,
		Name:              a.name,
		Status:            a.status,
		LastAction:        a.lastAction,
		BidValue:          a.bidValue,
		ActivationPattern: a.activationPattern,
	}
	
	if err := bb.UpdateKnowledgeSource(ks); err != nil {
		log.Printf("Error updating knowledge source: %v", err)
	}
	
	// Unlock the segments
	bb.UnlockHypothesis(seg1.ID)
	bb.UnlockHypothesis(seg2.ID)
	
	return nil
}

// SyllableToWord is the MOW agent that creates words from syllables
type SyllableToWord struct {
	BaseAgent
	nextWordID int
	lexicon    map[string]bool
}

// NewSyllableToWord creates a new syllable to word agent
func NewSyllableToWord() *SyllableToWord {
	return &SyllableToWord{
		BaseAgent: BaseAgent{
			id:                "KS_WRD",
			name:              "MOW: Syllable to Word",
			activationPattern: "When syllables match lexicon entries",
			status:            "idle",
			lastAction:        "Last created WRD-005 from syllables",
			bidValue:          0.52,
		},
		nextWordID: 6, // Start from WRD-006
		lexicon: map[string]bool{
			"THE":   true,
			"PRICE": true,
			"OF":    true,
			"BLUE":  true,
			"CHIPS": true,
			"STOCK": true,
			"MARKET": true,
			"FELL":  true,
			"ROSE":  true,
			"DECLINED": true,
		},
	}
}

// CanActivate checks if the agent can activate
func (a *SyllableToWord) CanActivate(state models.BlackboardState) (bool, float64) {
	// Check if there are syllables to form words
	if len(state.Syllable) > 0 {
		// Randomize bid value slightly
		bidValue := a.bidValue + (rand.Float64()*0.1 - 0.05)
		return true, bidValue
	}
	return false, 0
}

// Activate performs the agent's action
func (a *SyllableToWord) Activate(bb *blackboard.Blackboard) error {
	// Get current state
	state := bb.GetState()
	
	// Find syllables to work with
	if len(state.Syllable) == 0 {
		return fmt.Errorf("no syllables available")
	}
	
	// Select a syllable
	var selectedSyllable *models.Hypothesis
	for _, syl := range state.Syllable {
		if !syl.Locked {
			selectedSyllable = &syl
			break
		}
	}
	
	if selectedSyllable == nil {
		return fmt.Errorf("no suitable syllable found")
	}
	
	// Lock the syllable
	if err := bb.LockHypothesis(selectedSyllable.ID, a.id); err != nil {
		return err
	}
	
	// Create a new word
	wordID := fmt.Sprintf("WRD-%03d", a.nextWordID)
	a.nextWordID++
	
	// Generate word content based on syllable
	// For simplicity, we'll just use the syllable content as the word
	// In a real system, this would involve dictionary lookup
	content := selectedSyllable.Content
	
	// Check if it's in our lexicon
	if !a.lexicon[content] {
		// If not in lexicon, pick a random word
		words := []string{"THE", "PRICE", "OF", "BLUE", "CHIPS", "STOCK", "MARKET"}
		content = words[rand.Intn(len(words))]
	}
	
	// Create word hypothesis
	word := models.Hypothesis{
		ID:         wordID,
		Content:    content,
		Confidence: selectedSyllable.Confidence * 0.9, // Slightly lower confidence
		TimeRange:  selectedSyllable.TimeRange,
		Locked:     false,
	}
	
	if err := bb.AddHypothesis(word); err != nil {
		// Unlock the syllable
		bb.UnlockHypothesis(selectedSyllable.ID)
		return err
	}
	
	// Update agent status
	a.lastAction = fmt.Sprintf("Created %s: \"%s\" from syllable %s", 
		wordID, content, selectedSyllable.ID)
	a.status = "active"
	
	// Create activity log
	al := models.ActivityLog{
		Time:              time.Now().Format("15:04:05.000"),
		KnowledgeSourceID: a.id,
		Action:            fmt.Sprintf("Created %s: \"%s\" from syllable %s", 
			wordID, content, selectedSyllable.ID),
		Level:             "WORD",
	}
	
	if err := bb.AddActivityLog(al); err != nil {
		log.Printf("Error adding activity log: %v", err)
	}
	
	// Update knowledge source in blackboard
	ks := models.KnowledgeSource{
		ID:                a.id,
		Name:              a.name,
		Status:            a.status,
		LastAction:        a.lastAction,
		BidValue:          a.bidValue,
		ActivationPattern: a.activationPattern,
	}
	
	if err := bb.UpdateKnowledgeSource(ks); err != nil {
		log.Printf("Error updating knowledge source: %v", err)
	}
	
	// Unlock the syllable
	bb.UnlockHypothesis(selectedSyllable.ID)
	
	return nil
}

// SyntaxParser is the PARSE agent that creates phrases from words
type SyntaxParser struct {
	BaseAgent
	nextPhraseID int
}

// NewSyntaxParser creates a new syntax parser agent
func NewSyntaxParser() *SyntaxParser {
	return &SyntaxParser{
		BaseAgent: BaseAgent{
			id:                "KS_PARSE",
			name:              "PARSE: Syntax Parser",
			activationPattern: "When contiguous word sequence exists",
			status:            "ready",
			lastAction:        "Ready to parse word sequence",
			bidValue:          0.85,
		},
		nextPhraseID: 124, // Start from PHR-124
	}
}

// CanActivate checks if the agent can activate
func (a *SyntaxParser) CanActivate(state models.BlackboardState) (bool, float64) {
	// Check if there are enough words to form a phrase
	if len(state.Word) >= 3 {
		// Randomize bid value slightly
		bidValue := a.bidValue + (rand.Float64()*0.1 - 0.05)
		return true, bidValue
	}
	return false, 0
}

// Activate performs the agent's action
func (a *SyntaxParser) Activate(bb *blackboard.Blackboard) error {
	// Get current state
	state := bb.GetState()
	
	// Find words to work with
	if len(state.Word) < 3 {
		return fmt.Errorf("not enough words")
	}
	
	// Select three words
	var words []*models.Hypothesis
	for _, word := range state.Word {
		if !word.Locked {
			words = append(words, &word)
			if len(words) == 3 {
				break
			}
		}
	}
	
	if len(words) < 3 {
		return fmt.Errorf("not enough unlocked words")
	}
	
	// Lock the words
	for _, word := range words {
		if err := bb.LockHypothesis(word.ID, a.id); err != nil {
			// Unlock any words we've already locked
			for _, w := range words {
				if w.ID != word.ID {
					bb.UnlockHypothesis(w.ID)
				}
			}
			return err
		}
	}
	
	// Create a new phrase
	phraseID := fmt.Sprintf("PHR-%d", a.nextPhraseID)
	a.nextPhraseID++
	
	// Generate phrase content based on words
	content := fmt.Sprintf("\"%s %s %s\"", 
		words[0].Content, words[1].Content, words[2].Content)
	
	// Calculate time range based on words
	timeStart := words[0].TimeRange[:strings.Index(words[0].TimeRange, "-")]
	timeEnd := words[2].TimeRange[strings.Index(words[2].TimeRange, "-")+1:]
	timeRange := fmt.Sprintf("%s-%s", timeStart, timeEnd)
	
	// Create phrase hypothesis
	phrase := models.Hypothesis{
		ID:         phraseID,
		Content:    content,
		Confidence: (words[0].Confidence + words[1].Confidence + words[2].Confidence) / 3, // Average confidence
		TimeRange:  timeRange,
		Locked:     false,
	}
	
	if err := bb.AddHypothesis(phrase); err != nil {
		// Unlock the words
		for _, word := range words {
			bb.UnlockHypothesis(word.ID)
		}
		return err
	}
	
	// Update agent status
	a.lastAction = fmt.Sprintf("Created %s: %s from words", phraseID, content)
	a.status = "active"
	
	// Create activity log
	al := models.ActivityLog{
		Time:              time.Now().Format("15:04:05.000"),
		KnowledgeSourceID: a.id,
		Action:            fmt.Sprintf("Created %s: %s from words %s, %s, %s", 
			phraseID, content, words[0].ID, words[1].ID, words[2].ID),
		Level:             "PHRASE",
	}
	
	if err := bb.AddActivityLog(al); err != nil {
		log.Printf("Error adding activity log: %v", err)
	}
	
	// Update knowledge source in blackboard
	ks := models.KnowledgeSource{
		ID:                a.id,
		Name:              a.name,
		Status:            a.status,
		LastAction:        a.lastAction,
		BidValue:          a.bidValue,
		ActivationPattern: a.activationPattern,
	}
	
	if err := bb.UpdateKnowledgeSource(ks); err != nil {
		log.Printf("Error updating knowledge source: %v", err)
	}
	
	// Unlock the words
	for _, word := range words {
		bb.UnlockHypothesis(word.ID)
	}
	
	return nil
}

// WordPredictor is the PREDICT agent that predicts next words
type WordPredictor struct {
	BaseAgent
	nextWordID int
}

// NewWordPredictor creates a new word predictor agent
func NewWordPredictor() *WordPredictor {
	return &WordPredictor{
		BaseAgent: BaseAgent{
			id:                "KS_PRED",
			name:              "PREDICT: Word Predictor",
			activationPattern: "When partial syntactic structure available",
			status:            "waiting",
			lastAction:        "Generating predictions based on syntax",
			bidValue:          0.78,
		},
		nextWordID: 6, // Start from WRD-006
	}
}

// CanActivate checks if the agent can activate
func (a *WordPredictor) CanActivate(state models.BlackboardState) (bool, float64) {
	// Check if there are words to predict from
	if len(state.Word) >= 2 {
		// Randomize bid value slightly
		bidValue := a.bidValue + (rand.Float64()*0.1 - 0.05)
		return true, bidValue
	}
	return false, 0
}

// Activate performs the agent's action
func (a *WordPredictor) Activate(bb *blackboard.Blackboard) error {
	// Get current state
	state := bb.GetState()
	
	// Find words to work with
	if len(state.Word) < 2 {
		return fmt.Errorf("not enough words")
	}
	
	// Select the last two words
	var lastWords []*models.Hypothesis
	for i := len(state.Word) - 1; i >= 0 && len(lastWords) < 2; i-- {
		if !state.Word[i].Locked {
			lastWords = append([]*models.Hypothesis{&state.Word[i]}, lastWords...)
		}
	}
	
	if len(lastWords) < 2 {
		return fmt.Errorf("not enough unlocked words")
	}
	
	// Lock the words
	for _, word := range lastWords {
		if err := bb.LockHypothesis(word.ID, a.id); err != nil {
			// Unlock any words we've already locked
			for _, w := range lastWords {
				if w.ID != word.ID {
					bb.UnlockHypothesis(w.ID)
				}
			}
			return err
		}
	}
	
	// Create a new predicted word
	wordID := fmt.Sprintf("WRD-%03d", a.nextWordID)
	a.nextWordID++
	
	// Generate word content based on previous words
	var content string
	if lastWords[0].Content == "THE" && lastWords[1].Content == "PRICE" {
		content = "OF"
	} else if lastWords[0].Content == "PRICE" && lastWords[1].Content == "OF" {
		content = "BLUE"
	} else if lastWords[0].Content == "OF" && lastWords[1].Content == "BLUE" {
		content = "CHIPS"
	} else if lastWords[0].Content == "BLUE" && lastWords[1].Content == "CHIPS" {
		content = "DECLINED"
	} else {
		// Default prediction
		predictions := []string{"THE", "MARKET", "STOCK", "PRICE", "FELL", "ROSE"}
		content = predictions[rand.Intn(len(predictions))]
	}
	
	// Calculate time range based on last word
	lastTimeRange := lastWords[1].TimeRange
	lastTimeEnd := lastTimeRange[strings.Index(lastTimeRange, "-")+1:]
	timeStart := lastTimeEnd[:strings.Index(lastTimeEnd, "ms")]
	timeEnd := fmt.Sprintf("%d", atoi(timeStart) + 200)
	timeRange := fmt.Sprintf("%s-%sms", timeStart, timeEnd)
	
	// Create word hypothesis
	word := models.Hypothesis{
		ID:         wordID,
		Content:    content,
		Confidence: 0.6 + rand.Float64()*0.2, // Random confidence between 0.6 and 0.8
		TimeRange:  timeRange,
		Locked:     false,
	}
	
	if err := bb.AddHypothesis(word); err != nil {
		// Unlock the words
		for _, w := range lastWords {
			bb.UnlockHypothesis(w.ID)
		}
		return err
	}
	
	// Update agent status
	a.lastAction = fmt.Sprintf("Predicted %s: \"%s\" after \"%s %s\"", 
		wordID, content, lastWords[0].Content, lastWords[1].Content)
	a.status = "active"
	
	// Create activity log
	al := models.ActivityLog{
		Time:              time.Now().Format("15:04:05.000"),
		KnowledgeSourceID: a.id,
		Action:            fmt.Sprintf("Predicted %s: \"%s\" after \"%s %s\"", 
			wordID, content, lastWords[0].Content, lastWords[1].Content),
		Level:             "WORD",
	}
	
	if err := bb.AddActivityLog(al); err != nil {
		log.Printf("Error adding activity log: %v", err)
	}
	
	// Update knowledge source in blackboard
	ks := models.KnowledgeSource{
		ID:                a.id,
		Name:              a.name,
		Status:            a.status,
		LastAction:        a.lastAction,
		BidValue:          a.bidValue,
		ActivationPattern: a.activationPattern,
	}
	
	if err := bb.UpdateKnowledgeSource(ks); err != nil {
		log.Printf("Error updating knowledge source: %v", err)
	}
	
	// Unlock the words
	for _, word := range lastWords {
		bb.UnlockHypothesis(word.ID)
	}
	
	return nil
}

// HypothesisRater is the RPOL agent that rates hypotheses
type HypothesisRater struct {
	BaseAgent
}

// NewHypothesisRater creates a new hypothesis rater agent
func NewHypothesisRater() *HypothesisRater {
	return &HypothesisRater{
		BaseAgent: BaseAgent{
			id:                "KS_RATE",
			name:              "RPOL: Hypothesis Rater",
			activationPattern: "When new hypotheses are created",
			status:            "active",
			lastAction:        "Rating credibility of new hypotheses",
			bidValue:          0.92,
		},
	}
}

// CanActivate checks if the agent can activate
func (a *HypothesisRater) CanActivate(state models.BlackboardState) (bool, float64) {
	// This agent can always activate with high priority
	// Randomize bid value slightly
	bidValue := a.bidValue + (rand.Float64()*0.05 - 0.025)
	return true, bidValue
}

// Activate performs the agent's action
func (a *HypothesisRater) Activate(bb *blackboard.Blackboard) error {
	// Get current state
	state := bb.GetState()
	
	// Find a hypothesis to rate
	var hyp *models.Hypothesis
	var level string
	
	// Try to find a hypothesis with confidence < 0.8
	// Check in order of importance: phrase, word, syllable, segment
	for _, phrase := range state.Phrase {
		if !phrase.Locked && phrase.Confidence < 0.8 {
			hyp = &phrase
			level = "PHRASE"
			break
		}
	}
	
	if hyp == nil {
		for _, word := range state.Word {
			if !word.Locked && word.Confidence < 0.8 {
				hyp = &word
				level = "WORD"
				break
			}
		}
	}
	
	if hyp == nil {
		for _, syllable := range state.Syllable {
			if !syllable.Locked && syllable.Confidence < 0.8 {
				hyp = &syllable
				level = "SYLLABLE"
				break
			}
		}
	}
	
	if hyp == nil {
		for _, segment := range state.Segment {
			if !segment.Locked && segment.Confidence < 0.8 {
				hyp = &segment
				level = "SEGMENT"
				break
			}
		}
	}
	
	if hyp == nil {
		return fmt.Errorf("no suitable hypothesis found")
	}
	
	// Lock the hypothesis
	if err := bb.LockHypothesis(hyp.ID, a.id); err != nil {
		return err
	}
	
	// Rate the hypothesis
	oldConfidence := hyp.Confidence
	newConfidence := oldConfidence + (rand.Float64()*0.2 - 0.05) // Adjust confidence
	if newConfidence > 0.95 {
		newConfidence = 0.95
	} else if newConfidence < 0.5 {
		newConfidence = 0.5
	}
	
	hyp.Confidence = newConfidence
	
	// Update the hypothesis
	if err := bb.UpdateHypothesis(*hyp); err != nil {
		bb.UnlockHypothesis(hyp.ID)
		return err
	}
	
	// Update agent status
	a.lastAction = fmt.Sprintf("Rated %s confidence: %.2f -> %.2f", 
		hyp.ID, oldConfidence, newConfidence)
	a.status = "active"
	
	// Create activity log
	al := models.ActivityLog{
		Time:              time.Now().Format("15:04:05.000"),
		KnowledgeSourceID: a.id,
		Action:            fmt.Sprintf("Rated %s confidence: %.2f -> %.2f", 
			hyp.ID, oldConfidence, newConfidence),
		Level:             level,
	}
	
	if err := bb.AddActivityLog(al); err != nil {
		log.Printf("Error adding activity log: %v", err)
	}
	
	// Update knowledge source in blackboard
	ks := models.KnowledgeSource{
		ID:                a.id,
		Name:              a.name,
		Status:            a.status,
		LastAction:        a.lastAction,
		BidValue:          a.bidValue,
		ActivationPattern: a.activationPattern,
	}
	
	if err := bb.UpdateKnowledgeSource(ks); err != nil {
		log.Printf("Error updating knowledge source: %v", err)
	}
	
	// Unlock the hypothesis
	bb.UnlockHypothesis(hyp.ID)
	
	return nil
}

// Helper function to convert string to int
func atoi(s string) int {
	i, _ := strconv.Atoi(s)
	return i
}
