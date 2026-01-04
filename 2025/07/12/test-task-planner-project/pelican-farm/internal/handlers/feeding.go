package handlers

import (
	"database/sql"
	"encoding/json"
	"html/template"
	"net/http"
	"strconv"
	"strings"
	"time"

	"pelican-farm/internal/models"

	"github.com/gorilla/mux"
)

type FeedingHandler struct {
	DB *sql.DB
}

func NewFeedingHandler(db *sql.DB) *FeedingHandler {
	return &FeedingHandler{DB: db}
}

// ListFeedings handler - show all feeding records with search/filter by pelican, date range
func (fh *FeedingHandler) ListFeedings(w http.ResponseWriter, r *http.Request) {
	// Get query parameters for search and filtering
	pelicanID := r.URL.Query().Get("pelican_id")
	foodType := r.URL.Query().Get("food_type")
	startDate := r.URL.Query().Get("start_date")
	endDate := r.URL.Query().Get("end_date")

	// Build query with optional filters
	query := `
		SELECT fr.id, fr.pelican_id, fr.food_type, fr.amount_kg, fr.feeding_time, fr.notes, fr.created,
			   p.name as pelican_name
		FROM feeding_records fr
		LEFT JOIN pelicans p ON fr.pelican_id = p.id
		WHERE 1=1`
	args := []interface{}{}

	if pelicanID != "" {
		query += " AND fr.pelican_id = ?"
		args = append(args, pelicanID)
	}

	if foodType != "" {
		query += " AND fr.food_type = ?"
		args = append(args, foodType)
	}

	if startDate != "" {
		query += " AND DATE(fr.feeding_time) >= ?"
		args = append(args, startDate)
	}

	if endDate != "" {
		query += " AND DATE(fr.feeding_time) <= ?"
		args = append(args, endDate)
	}

	query += " ORDER BY fr.feeding_time DESC"

	rows, err := fh.DB.Query(query, args...)
	if err != nil {
		http.Error(w, "Failed to fetch feeding records: "+err.Error(), http.StatusInternalServerError)
		return
	}
	defer rows.Close()

	var feedings []map[string]interface{}
	for rows.Next() {
		var fr models.FeedingRecord
		var notes sql.NullString
		var pelicanName sql.NullString

		err := rows.Scan(&fr.ID, &fr.PelicanID, &fr.FoodType, &fr.AmountKg, &fr.FeedingTime, &notes, &fr.Created, &pelicanName)
		if err != nil {
			http.Error(w, "Failed to scan feeding record: "+err.Error(), http.StatusInternalServerError)
			return
		}

		if notes.Valid {
			fr.Notes = &notes.String
		}

		feeding := map[string]interface{}{
			"id":           fr.ID,
			"pelican_id":   fr.PelicanID,
			"pelican_name": pelicanName.String,
			"food_type":    fr.FoodType,
			"amount_kg":    fr.AmountKg,
			"feeding_time": fr.FeedingTime,
			"notes":        fr.Notes,
			"created":      fr.Created,
		}

		feedings = append(feedings, feeding)
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"feedings": feedings,
		"count":    len(feedings),
	})
}

// ShowFeeding handler - display single feeding record details
func (fh *FeedingHandler) ShowFeeding(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	idStr := vars["id"]
	id, err := strconv.Atoi(idStr)
	if err != nil {
		http.Error(w, "Invalid feeding ID", http.StatusBadRequest)
		return
	}

	// Get feeding record details
	var fr models.FeedingRecord
	var notes sql.NullString
	var pelicanName sql.NullString

	err = fh.DB.QueryRow(`
		SELECT fr.id, fr.pelican_id, fr.food_type, fr.amount_kg, fr.feeding_time, fr.notes, fr.created,
			   p.name as pelican_name
		FROM feeding_records fr
		LEFT JOIN pelicans p ON fr.pelican_id = p.id
		WHERE fr.id = ?`, id).Scan(
		&fr.ID, &fr.PelicanID, &fr.FoodType, &fr.AmountKg, &fr.FeedingTime, &notes, &fr.Created, &pelicanName)

	if err != nil {
		if err == sql.ErrNoRows {
			http.Error(w, "Feeding record not found", http.StatusNotFound)
			return
		}
		http.Error(w, "Failed to fetch feeding record: "+err.Error(), http.StatusInternalServerError)
		return
	}

	if notes.Valid {
		fr.Notes = &notes.String
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"feeding":      fr,
		"pelican_name": pelicanName.String,
	})
}

// NewFeeding handler - show form to record new feeding
func (fh *FeedingHandler) NewFeeding(w http.ResponseWriter, r *http.Request) {
	// Get list of pelicans for dropdown
	rows, err := fh.DB.Query("SELECT id, name FROM pelicans ORDER BY name")
	if err != nil {
		http.Error(w, "Failed to fetch pelicans: "+err.Error(), http.StatusInternalServerError)
		return
	}
	defer rows.Close()

	var pelicans []map[string]interface{}
	for rows.Next() {
		var id int
		var name string
		err := rows.Scan(&id, &name)
		if err != nil {
			http.Error(w, "Failed to scan pelican: "+err.Error(), http.StatusInternalServerError)
			return
		}
		pelicans = append(pelicans, map[string]interface{}{
			"id":   id,
			"name": name,
		})
	}

	formHTML := `
<!DOCTYPE html>
<html>
<head>
    <title>New Feeding Record</title>
</head>
<body>
    <h1>Record New Feeding</h1>
    <form method="POST" action="/feedings">
        <div>
            <label for="pelican_id">Pelican:</label>
            <select id="pelican_id" name="pelican_id" required>
                <option value="">Select a pelican...</option>
                {{range .Pelicans}}
                <option value="{{.id}}">{{.name}}</option>
                {{end}}
            </select>
        </div>
        <div>
            <label for="food_type">Food Type:</label>
            <select id="food_type" name="food_type" required>
                <option value="">Select food type...</option>
                <option value="fish">Fish</option>
                <option value="squid">Squid</option>
                <option value="shrimp">Shrimp</option>
                <option value="vitamins">Vitamins</option>
                <option value="medication">Medication</option>
                <option value="other">Other</option>
            </select>
        </div>
        <div>
            <label for="amount_kg">Amount (kg):</label>
            <input type="number" id="amount_kg" name="amount_kg" step="0.01" min="0" required>
        </div>
        <div>
            <label for="feeding_time">Feeding Time:</label>
            <input type="datetime-local" id="feeding_time" name="feeding_time" required>
        </div>
        <div>
            <label for="notes">Notes:</label>
            <textarea id="notes" name="notes" placeholder="Optional notes about this feeding..."></textarea>
        </div>
        <button type="submit">Record Feeding</button>
        <a href="/feedings">Cancel</a>
    </form>
</body>
</html>`

	tmpl, err := template.New("new").Parse(formHTML)
	if err != nil {
		http.Error(w, "Template error: "+err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "text/html")
	tmpl.Execute(w, map[string]interface{}{
		"Pelicans": pelicans,
	})
}

// CreateFeeding handler - process feeding form submission with validation
func (fh *FeedingHandler) CreateFeeding(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	// Parse form data
	err := r.ParseForm()
	if err != nil {
		http.Error(w, "Failed to parse form: "+err.Error(), http.StatusBadRequest)
		return
	}

	// Validate required fields
	pelicanIDStr := r.FormValue("pelican_id")
	foodType := strings.TrimSpace(r.FormValue("food_type"))
	amountStr := r.FormValue("amount_kg")
	feedingTimeStr := r.FormValue("feeding_time")

	if pelicanIDStr == "" || foodType == "" || amountStr == "" || feedingTimeStr == "" {
		http.Error(w, "All required fields must be provided", http.StatusBadRequest)
		return
	}

	pelicanID, err := strconv.Atoi(pelicanIDStr)
	if err != nil || pelicanID <= 0 {
		http.Error(w, "Invalid pelican ID", http.StatusBadRequest)
		return
	}

	// Validate pelican exists
	var exists int
	err = fh.DB.QueryRow("SELECT COUNT(*) FROM pelicans WHERE id = ?", pelicanID).Scan(&exists)
	if err != nil {
		http.Error(w, "Failed to check pelican existence: "+err.Error(), http.StatusInternalServerError)
		return
	}
	if exists == 0 {
		http.Error(w, "Selected pelican does not exist", http.StatusBadRequest)
		return
	}

	// Validate food type
	validFoodTypes := map[string]bool{
		"fish":       true,
		"squid":      true,
		"shrimp":     true,
		"vitamins":   true,
		"medication": true,
		"other":      true,
	}
	if !validFoodTypes[foodType] {
		http.Error(w, "Invalid food type", http.StatusBadRequest)
		return
	}

	amount, err := strconv.ParseFloat(amountStr, 64)
	if err != nil || amount <= 0 {
		http.Error(w, "Invalid amount - must be a positive number", http.StatusBadRequest)
		return
	}

	// Parse feeding time
	feedingTime, err := time.Parse("2006-01-02T15:04", feedingTimeStr)
	if err != nil {
		http.Error(w, "Invalid feeding time format", http.StatusBadRequest)
		return
	}

	// Validate feeding time is not in the future
	if feedingTime.After(time.Now()) {
		http.Error(w, "Feeding time cannot be in the future", http.StatusBadRequest)
		return
	}

	notes := strings.TrimSpace(r.FormValue("notes"))

	// Insert into database
	query := `
		INSERT INTO feeding_records (pelican_id, food_type, amount_kg, feeding_time, notes, created)
		VALUES (?, ?, ?, ?, ?, CURRENT_TIMESTAMP)`

	result, err := fh.DB.Exec(query, pelicanID, foodType, amount, feedingTime,
		func() interface{} {
			if notes == "" {
				return nil
			}
			return notes
		}())

	if err != nil {
		http.Error(w, "Failed to create feeding record: "+err.Error(), http.StatusInternalServerError)
		return
	}

	id, err := result.LastInsertId()
	if err != nil {
		http.Error(w, "Failed to get created feeding ID: "+err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"id":      id,
		"message": "Feeding record created successfully",
	})
}

// EditFeeding handler - show edit form for feeding record
func (fh *FeedingHandler) EditFeeding(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	idStr := vars["id"]
	id, err := strconv.Atoi(idStr)
	if err != nil {
		http.Error(w, "Invalid feeding ID", http.StatusBadRequest)
		return
	}

	// Get existing feeding data
	var fr models.FeedingRecord
	var notes sql.NullString

	err = fh.DB.QueryRow(`
		SELECT id, pelican_id, food_type, amount_kg, feeding_time, notes 
		FROM feeding_records WHERE id = ?`, id).Scan(
		&fr.ID, &fr.PelicanID, &fr.FoodType, &fr.AmountKg, &fr.FeedingTime, &notes)

	if err != nil {
		if err == sql.ErrNoRows {
			http.Error(w, "Feeding record not found", http.StatusNotFound)
			return
		}
		http.Error(w, "Failed to fetch feeding record: "+err.Error(), http.StatusInternalServerError)
		return
	}

	if notes.Valid {
		fr.Notes = &notes.String
	}

	// Get list of pelicans for dropdown
	rows, err := fh.DB.Query("SELECT id, name FROM pelicans ORDER BY name")
	if err != nil {
		http.Error(w, "Failed to fetch pelicans: "+err.Error(), http.StatusInternalServerError)
		return
	}
	defer rows.Close()

	var pelicans []map[string]interface{}
	for rows.Next() {
		var pid int
		var name string
		err := rows.Scan(&pid, &name)
		if err != nil {
			http.Error(w, "Failed to scan pelican: "+err.Error(), http.StatusInternalServerError)
			return
		}
		pelicans = append(pelicans, map[string]interface{}{
			"id":   pid,
			"name": name,
		})
	}

	formTemplate := `
<!DOCTYPE html>
<html>
<head>
    <title>Edit Feeding Record</title>
</head>
<body>
    <h1>Edit Feeding Record</h1>
    <form method="POST" action="/feedings/{{.Feeding.ID}}">
        <input type="hidden" name="_method" value="PUT">
        <div>
            <label for="pelican_id">Pelican:</label>
            <select id="pelican_id" name="pelican_id" required>
                {{range .Pelicans}}
                <option value="{{.id}}" {{if eq .id $.Feeding.PelicanID}}selected{{end}}>{{.name}}</option>
                {{end}}
            </select>
        </div>
        <div>
            <label for="food_type">Food Type:</label>
            <select id="food_type" name="food_type" required>
                <option value="fish" {{if eq .Feeding.FoodType "fish"}}selected{{end}}>Fish</option>
                <option value="squid" {{if eq .Feeding.FoodType "squid"}}selected{{end}}>Squid</option>
                <option value="shrimp" {{if eq .Feeding.FoodType "shrimp"}}selected{{end}}>Shrimp</option>
                <option value="vitamins" {{if eq .Feeding.FoodType "vitamins"}}selected{{end}}>Vitamins</option>
                <option value="medication" {{if eq .Feeding.FoodType "medication"}}selected{{end}}>Medication</option>
                <option value="other" {{if eq .Feeding.FoodType "other"}}selected{{end}}>Other</option>
            </select>
        </div>
        <div>
            <label for="amount_kg">Amount (kg):</label>
            <input type="number" id="amount_kg" name="amount_kg" step="0.01" min="0" value="{{.Feeding.AmountKg}}" required>
        </div>
        <div>
            <label for="feeding_time">Feeding Time:</label>
            <input type="datetime-local" id="feeding_time" name="feeding_time" value="{{.FeedingTimeFormatted}}" required>
        </div>
        <div>
            <label for="notes">Notes:</label>
            <textarea id="notes" name="notes">{{if .Feeding.Notes}}{{.Feeding.Notes}}{{end}}</textarea>
        </div>
        <button type="submit">Update Feeding</button>
        <a href="/feedings/{{.Feeding.ID}}">Cancel</a>
    </form>
</body>
</html>`

	tmpl, err := template.New("edit").Parse(formTemplate)
	if err != nil {
		http.Error(w, "Template error: "+err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "text/html")
	tmpl.Execute(w, map[string]interface{}{
		"Feeding":              fr,
		"FeedingTimeFormatted": fr.FeedingTime.Format("2006-01-02T15:04"),
		"Pelicans":             pelicans,
	})
}

// UpdateFeeding handler - process feeding updates
func (fh *FeedingHandler) UpdateFeeding(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost && r.Method != http.MethodPut {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	vars := mux.Vars(r)
	idStr := vars["id"]
	id, err := strconv.Atoi(idStr)
	if err != nil {
		http.Error(w, "Invalid feeding ID", http.StatusBadRequest)
		return
	}

	// Check if feeding record exists
	var exists int
	err = fh.DB.QueryRow("SELECT COUNT(*) FROM feeding_records WHERE id = ?", id).Scan(&exists)
	if err != nil {
		http.Error(w, "Failed to check feeding record existence: "+err.Error(), http.StatusInternalServerError)
		return
	}
	if exists == 0 {
		http.Error(w, "Feeding record not found", http.StatusNotFound)
		return
	}

	// Parse form data
	err = r.ParseForm()
	if err != nil {
		http.Error(w, "Failed to parse form: "+err.Error(), http.StatusBadRequest)
		return
	}

	// Validate required fields
	pelicanIDStr := r.FormValue("pelican_id")
	foodType := strings.TrimSpace(r.FormValue("food_type"))
	amountStr := r.FormValue("amount_kg")
	feedingTimeStr := r.FormValue("feeding_time")

	if pelicanIDStr == "" || foodType == "" || amountStr == "" || feedingTimeStr == "" {
		http.Error(w, "All required fields must be provided", http.StatusBadRequest)
		return
	}

	pelicanID, err := strconv.Atoi(pelicanIDStr)
	if err != nil || pelicanID <= 0 {
		http.Error(w, "Invalid pelican ID", http.StatusBadRequest)
		return
	}

	// Validate pelican exists
	var pelicanExists int
	err = fh.DB.QueryRow("SELECT COUNT(*) FROM pelicans WHERE id = ?", pelicanID).Scan(&pelicanExists)
	if err != nil {
		http.Error(w, "Failed to check pelican existence: "+err.Error(), http.StatusInternalServerError)
		return
	}
	if pelicanExists == 0 {
		http.Error(w, "Selected pelican does not exist", http.StatusBadRequest)
		return
	}

	// Validate food type
	validFoodTypes := map[string]bool{
		"fish":       true,
		"squid":      true,
		"shrimp":     true,
		"vitamins":   true,
		"medication": true,
		"other":      true,
	}
	if !validFoodTypes[foodType] {
		http.Error(w, "Invalid food type", http.StatusBadRequest)
		return
	}

	amount, err := strconv.ParseFloat(amountStr, 64)
	if err != nil || amount <= 0 {
		http.Error(w, "Invalid amount - must be a positive number", http.StatusBadRequest)
		return
	}

	// Parse feeding time
	feedingTime, err := time.Parse("2006-01-02T15:04", feedingTimeStr)
	if err != nil {
		http.Error(w, "Invalid feeding time format", http.StatusBadRequest)
		return
	}

	notes := strings.TrimSpace(r.FormValue("notes"))

	// Update database
	query := `
		UPDATE feeding_records 
		SET pelican_id = ?, food_type = ?, amount_kg = ?, feeding_time = ?, notes = ?
		WHERE id = ?`

	_, err = fh.DB.Exec(query, pelicanID, foodType, amount, feedingTime,
		func() interface{} {
			if notes == "" {
				return nil
			}
			return notes
		}(), id)

	if err != nil {
		http.Error(w, "Failed to update feeding record: "+err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"id":      id,
		"message": "Feeding record updated successfully",
	})
}

// DeleteFeeding handler - remove feeding record
func (fh *FeedingHandler) DeleteFeeding(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodDelete && r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	vars := mux.Vars(r)
	idStr := vars["id"]
	id, err := strconv.Atoi(idStr)
	if err != nil {
		http.Error(w, "Invalid feeding ID", http.StatusBadRequest)
		return
	}

	// Check if feeding record exists
	var exists int
	err = fh.DB.QueryRow("SELECT COUNT(*) FROM feeding_records WHERE id = ?", id).Scan(&exists)
	if err != nil {
		http.Error(w, "Failed to check feeding record existence: "+err.Error(), http.StatusInternalServerError)
		return
	}
	if exists == 0 {
		http.Error(w, "Feeding record not found", http.StatusNotFound)
		return
	}

	// Delete feeding record
	_, err = fh.DB.Exec("DELETE FROM feeding_records WHERE id = ?", id)
	if err != nil {
		http.Error(w, "Failed to delete feeding record: "+err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"id":      id,
		"message": "Feeding record deleted successfully",
	})
}

// ScheduledFeedings handler - show/manage feeding schedules
func (fh *FeedingHandler) ScheduledFeedings(w http.ResponseWriter, r *http.Request) {
	// Get query parameters for filtering
	pelicanID := r.URL.Query().Get("pelican_id")
	showCompleted := r.URL.Query().Get("show_completed") == "true"

	// Build query with optional filters
	query := `
		SELECT fs.id, fs.pelican_id, fs.scheduled_time, fs.food_type, fs.amount_kg, fs.completed, fs.created,
			   p.name as pelican_name
		FROM feeding_schedules fs
		LEFT JOIN pelicans p ON fs.pelican_id = p.id
		WHERE 1=1`
	args := []interface{}{}

	if pelicanID != "" {
		query += " AND fs.pelican_id = ?"
		args = append(args, pelicanID)
	}

	if !showCompleted {
		query += " AND fs.completed = 0"
	}

	query += " ORDER BY fs.scheduled_time ASC"

	rows, err := fh.DB.Query(query, args...)
	if err != nil {
		http.Error(w, "Failed to fetch feeding schedules: "+err.Error(), http.StatusInternalServerError)
		return
	}
	defer rows.Close()

	var schedules []map[string]interface{}
	for rows.Next() {
		var fs models.FeedingSchedule
		var pelicanName sql.NullString

		err := rows.Scan(&fs.ID, &fs.PelicanID, &fs.ScheduledTime, &fs.FoodType, &fs.AmountKg, &fs.Completed, &fs.Created, &pelicanName)
		if err != nil {
			http.Error(w, "Failed to scan feeding schedule: "+err.Error(), http.StatusInternalServerError)
			return
		}

		schedule := map[string]interface{}{
			"id":             fs.ID,
			"pelican_id":     fs.PelicanID,
			"pelican_name":   pelicanName.String,
			"scheduled_time": fs.ScheduledTime,
			"food_type":      fs.FoodType,
			"amount_kg":      fs.AmountKg,
			"completed":      fs.Completed,
			"created":        fs.Created,
		}

		schedules = append(schedules, schedule)
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"schedules": schedules,
		"count":     len(schedules),
	})
}
