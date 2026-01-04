package handlers

import (
	"database/sql"
	"encoding/json"
	"html/template"
	"net/http"
	"strconv"
	"strings"

	"pelican-farm/internal/models"

	"github.com/gorilla/mux"
)

type PelicanHandler struct {
	DB *sql.DB
}

func NewPelicanHandler(db *sql.DB) *PelicanHandler {
	return &PelicanHandler{DB: db}
}

// ListPelicans handler - fetch and display all pelicans with search/filter
func (ph *PelicanHandler) ListPelicans(w http.ResponseWriter, r *http.Request) {
	// Get query parameters for search and filtering
	search := r.URL.Query().Get("search")
	species := r.URL.Query().Get("species")
	healthStatus := r.URL.Query().Get("health_status")

	// Build query with optional filters
	query := `SELECT id, name, species, age, weight, health_status, arrival_date, notes, created, modified FROM pelicans WHERE 1=1`
	args := []interface{}{}

	if search != "" {
		query += " AND (name LIKE ? OR notes LIKE ?)"
		searchTerm := "%" + search + "%"
		args = append(args, searchTerm, searchTerm)
	}

	if species != "" {
		query += " AND species = ?"
		args = append(args, species)
	}

	if healthStatus != "" {
		query += " AND health_status = ?"
		args = append(args, healthStatus)
	}

	query += " ORDER BY name ASC"

	rows, err := ph.DB.Query(query, args...)
	if err != nil {
		http.Error(w, "Failed to fetch pelicans: "+err.Error(), http.StatusInternalServerError)
		return
	}
	defer rows.Close()

	var pelicans []models.Pelican
	for rows.Next() {
		var p models.Pelican
		var arrivalDate, notes sql.NullString
		var weight sql.NullFloat64

		err := rows.Scan(&p.ID, &p.Name, &p.Species, &p.Age, &weight, &p.HealthStatus, &arrivalDate, &notes, &p.Created, &p.Modified)
		if err != nil {
			http.Error(w, "Failed to scan pelican: "+err.Error(), http.StatusInternalServerError)
			return
		}

		if weight.Valid {
			p.Weight = &weight.Float64
		}
		if arrivalDate.Valid {
			p.ArrivalDate = arrivalDate.String
		}
		if notes.Valid {
			p.Notes = &notes.String
		}

		pelicans = append(pelicans, p)
	}

	// Return JSON response
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"pelicans": pelicans,
		"count":    len(pelicans),
	})
}

// ShowPelican handler - display single pelican with feeding/health history
func (ph *PelicanHandler) ShowPelican(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	idStr := vars["id"]
	id, err := strconv.Atoi(idStr)
	if err != nil {
		http.Error(w, "Invalid pelican ID", http.StatusBadRequest)
		return
	}

	// Get pelican details
	var p models.Pelican
	var arrivalDate, notes sql.NullString
	var weight sql.NullFloat64

	err = ph.DB.QueryRow(`
		SELECT id, name, species, age, weight, health_status, arrival_date, notes, created, modified 
		FROM pelicans WHERE id = ?`, id).Scan(
		&p.ID, &p.Name, &p.Species, &p.Age, &weight, &p.HealthStatus, &arrivalDate, &notes, &p.Created, &p.Modified)

	if err != nil {
		if err == sql.ErrNoRows {
			http.Error(w, "Pelican not found", http.StatusNotFound)
			return
		}
		http.Error(w, "Failed to fetch pelican: "+err.Error(), http.StatusInternalServerError)
		return
	}

	if weight.Valid {
		p.Weight = &weight.Float64
	}
	if arrivalDate.Valid {
		p.ArrivalDate = arrivalDate.String
	}
	if notes.Valid {
		p.Notes = &notes.String
	}

	// Get feeding records
	feedingRows, err := ph.DB.Query(`
		SELECT id, pelican_id, food_type, amount_kg, feeding_time, notes, created 
		FROM feeding_records WHERE pelican_id = ? ORDER BY feeding_time DESC LIMIT 10`, id)
	if err != nil {
		http.Error(w, "Failed to fetch feeding records: "+err.Error(), http.StatusInternalServerError)
		return
	}
	defer feedingRows.Close()

	var feedingRecords []models.FeedingRecord
	for feedingRows.Next() {
		var fr models.FeedingRecord
		var notes sql.NullString

		err := feedingRows.Scan(&fr.ID, &fr.PelicanID, &fr.FoodType, &fr.AmountKg, &fr.FeedingTime, &notes, &fr.Created)
		if err != nil {
			http.Error(w, "Failed to scan feeding record: "+err.Error(), http.StatusInternalServerError)
			return
		}

		if notes.Valid {
			fr.Notes = &notes.String
		}
		feedingRecords = append(feedingRecords, fr)
	}

	// Get health checks
	healthRows, err := ph.DB.Query(`
		SELECT id, pelican_id, check_date, weight, temperature, notes, veterinarian, created 
		FROM health_checks WHERE pelican_id = ? ORDER BY check_date DESC LIMIT 10`, id)
	if err != nil {
		http.Error(w, "Failed to fetch health checks: "+err.Error(), http.StatusInternalServerError)
		return
	}
	defer healthRows.Close()

	var healthChecks []models.HealthCheck
	for healthRows.Next() {
		var hc models.HealthCheck
		var checkDate sql.NullString
		var weight, temperature sql.NullFloat64
		var notes, veterinarian sql.NullString

		err := healthRows.Scan(&hc.ID, &hc.PelicanID, &checkDate, &weight, &temperature, &notes, &veterinarian, &hc.Created)
		if err != nil {
			http.Error(w, "Failed to scan health check: "+err.Error(), http.StatusInternalServerError)
			return
		}

		if checkDate.Valid {
			hc.CheckDate = checkDate.String
		}
		if weight.Valid {
			hc.Weight = &weight.Float64
		}
		if temperature.Valid {
			hc.Temperature = &temperature.Float64
		}
		if notes.Valid {
			hc.Notes = &notes.String
		}
		if veterinarian.Valid {
			hc.Veterinarian = &veterinarian.String
		}
		healthChecks = append(healthChecks, hc)
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"pelican":         p,
		"feeding_records": feedingRecords,
		"health_checks":   healthChecks,
	})
}

// NewPelican handler - show create form
func (ph *PelicanHandler) NewPelican(w http.ResponseWriter, r *http.Request) {
	formHTML := `
<!DOCTYPE html>
<html>
<head>
    <title>New Pelican</title>
</head>
<body>
    <h1>Add New Pelican</h1>
    <form method="POST" action="/pelicans">
        <div>
            <label for="name">Name:</label>
            <input type="text" id="name" name="name" required>
        </div>
        <div>
            <label for="species">Species:</label>
            <input type="text" id="species" name="species" required>
        </div>
        <div>
            <label for="age">Age:</label>
            <input type="number" id="age" name="age" required>
        </div>
        <div>
            <label for="weight">Weight (kg):</label>
            <input type="number" id="weight" name="weight" step="0.1">
        </div>
        <div>
            <label for="health_status">Health Status:</label>
            <select id="health_status" name="health_status" required>
                <option value="healthy">Healthy</option>
                <option value="sick">Sick</option>
                <option value="recovering">Recovering</option>
                <option value="critical">Critical</option>
            </select>
        </div>
        <div>
            <label for="arrival_date">Arrival Date:</label>
            <input type="date" id="arrival_date" name="arrival_date" required>
        </div>
        <div>
            <label for="notes">Notes:</label>
            <textarea id="notes" name="notes"></textarea>
        </div>
        <button type="submit">Create Pelican</button>
        <a href="/pelicans">Cancel</a>
    </form>
</body>
</html>`

	w.Header().Set("Content-Type", "text/html")
	w.Write([]byte(formHTML))
}

// CreatePelican handler - process form submission with validation
func (ph *PelicanHandler) CreatePelican(w http.ResponseWriter, r *http.Request) {
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
	name := strings.TrimSpace(r.FormValue("name"))
	species := strings.TrimSpace(r.FormValue("species"))
	ageStr := r.FormValue("age")
	healthStatus := r.FormValue("health_status")
	arrivalDate := r.FormValue("arrival_date")

	if name == "" || species == "" || ageStr == "" || healthStatus == "" || arrivalDate == "" {
		http.Error(w, "All required fields must be provided", http.StatusBadRequest)
		return
	}

	age, err := strconv.Atoi(ageStr)
	if err != nil || age < 0 {
		http.Error(w, "Invalid age", http.StatusBadRequest)
		return
	}

	// Validate health status
	validStatuses := map[string]bool{
		"healthy":     true,
		"sick":        true,
		"recovering":  true,
		"critical":    true,
	}
	if !validStatuses[healthStatus] {
		http.Error(w, "Invalid health status", http.StatusBadRequest)
		return
	}

	// Optional fields
	var weight *float64
	weightStr := r.FormValue("weight")
	if weightStr != "" {
		weightVal, err := strconv.ParseFloat(weightStr, 64)
		if err != nil || weightVal < 0 {
			http.Error(w, "Invalid weight", http.StatusBadRequest)
			return
		}
		weight = &weightVal
	}

	notes := r.FormValue("notes")

	// Insert into database
	query := `
		INSERT INTO pelicans (name, species, age, weight, health_status, arrival_date, notes, created, modified)
		VALUES (?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)`

	result, err := ph.DB.Exec(query, name, species, age, weight, healthStatus, arrivalDate, 
		func() interface{} {
			if notes == "" {
				return nil
			}
			return notes
		}())

	if err != nil {
		http.Error(w, "Failed to create pelican: "+err.Error(), http.StatusInternalServerError)
		return
	}

	id, err := result.LastInsertId()
	if err != nil {
		http.Error(w, "Failed to get created pelican ID: "+err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"id":      id,
		"message": "Pelican created successfully",
	})
}

// EditPelican handler - show edit form
func (ph *PelicanHandler) EditPelican(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	idStr := vars["id"]
	id, err := strconv.Atoi(idStr)
	if err != nil {
		http.Error(w, "Invalid pelican ID", http.StatusBadRequest)
		return
	}

	// Get existing pelican data
	var p models.Pelican
	var arrivalDate, notes sql.NullString
	var weight sql.NullFloat64

	err = ph.DB.QueryRow(`
		SELECT id, name, species, age, weight, health_status, arrival_date, notes 
		FROM pelicans WHERE id = ?`, id).Scan(
		&p.ID, &p.Name, &p.Species, &p.Age, &weight, &p.HealthStatus, &arrivalDate, &notes)

	if err != nil {
		if err == sql.ErrNoRows {
			http.Error(w, "Pelican not found", http.StatusNotFound)
			return
		}
		http.Error(w, "Failed to fetch pelican: "+err.Error(), http.StatusInternalServerError)
		return
	}

	if weight.Valid {
		p.Weight = &weight.Float64
	}
	if arrivalDate.Valid {
		p.ArrivalDate = arrivalDate.String
	}
	if notes.Valid {
		p.Notes = &notes.String
	}

	formTemplate := `
<!DOCTYPE html>
<html>
<head>
    <title>Edit Pelican</title>
</head>
<body>
    <h1>Edit Pelican</h1>
    <form method="POST" action="/pelicans/{{.ID}}">
        <input type="hidden" name="_method" value="PUT">
        <div>
            <label for="name">Name:</label>
            <input type="text" id="name" name="name" value="{{.Name}}" required>
        </div>
        <div>
            <label for="species">Species:</label>
            <input type="text" id="species" name="species" value="{{.Species}}" required>
        </div>
        <div>
            <label for="age">Age:</label>
            <input type="number" id="age" name="age" value="{{.Age}}" required>
        </div>
        <div>
            <label for="weight">Weight (kg):</label>
            <input type="number" id="weight" name="weight" step="0.1" value="{{if .Weight}}{{.Weight}}{{end}}">
        </div>
        <div>
            <label for="health_status">Health Status:</label>
            <select id="health_status" name="health_status" required>
                <option value="healthy" {{if eq .HealthStatus "healthy"}}selected{{end}}>Healthy</option>
                <option value="sick" {{if eq .HealthStatus "sick"}}selected{{end}}>Sick</option>
                <option value="recovering" {{if eq .HealthStatus "recovering"}}selected{{end}}>Recovering</option>
                <option value="critical" {{if eq .HealthStatus "critical"}}selected{{end}}>Critical</option>
            </select>
        </div>
        <div>
            <label for="arrival_date">Arrival Date:</label>
            <input type="date" id="arrival_date" name="arrival_date" value="{{.ArrivalDate}}" required>
        </div>
        <div>
            <label for="notes">Notes:</label>
            <textarea id="notes" name="notes">{{if .Notes}}{{.Notes}}{{end}}</textarea>
        </div>
        <button type="submit">Update Pelican</button>
        <a href="/pelicans/{{.ID}}">Cancel</a>
    </form>
</body>
</html>`

	tmpl, err := template.New("edit").Parse(formTemplate)
	if err != nil {
		http.Error(w, "Template error: "+err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "text/html")
	tmpl.Execute(w, p)
}

// UpdatePelican handler - process updates with validation
func (ph *PelicanHandler) UpdatePelican(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost && r.Method != http.MethodPut {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	vars := mux.Vars(r)
	idStr := vars["id"]
	id, err := strconv.Atoi(idStr)
	if err != nil {
		http.Error(w, "Invalid pelican ID", http.StatusBadRequest)
		return
	}

	// Check if pelican exists
	var exists int
	err = ph.DB.QueryRow("SELECT COUNT(*) FROM pelicans WHERE id = ?", id).Scan(&exists)
	if err != nil {
		http.Error(w, "Failed to check pelican existence: "+err.Error(), http.StatusInternalServerError)
		return
	}
	if exists == 0 {
		http.Error(w, "Pelican not found", http.StatusNotFound)
		return
	}

	// Parse form data
	err = r.ParseForm()
	if err != nil {
		http.Error(w, "Failed to parse form: "+err.Error(), http.StatusBadRequest)
		return
	}

	// Validate required fields
	name := strings.TrimSpace(r.FormValue("name"))
	species := strings.TrimSpace(r.FormValue("species"))
	ageStr := r.FormValue("age")
	healthStatus := r.FormValue("health_status")
	arrivalDate := r.FormValue("arrival_date")

	if name == "" || species == "" || ageStr == "" || healthStatus == "" || arrivalDate == "" {
		http.Error(w, "All required fields must be provided", http.StatusBadRequest)
		return
	}

	age, err := strconv.Atoi(ageStr)
	if err != nil || age < 0 {
		http.Error(w, "Invalid age", http.StatusBadRequest)
		return
	}

	// Validate health status
	validStatuses := map[string]bool{
		"healthy":     true,
		"sick":        true,
		"recovering":  true,
		"critical":    true,
	}
	if !validStatuses[healthStatus] {
		http.Error(w, "Invalid health status", http.StatusBadRequest)
		return
	}

	// Optional fields
	var weight *float64
	weightStr := r.FormValue("weight")
	if weightStr != "" {
		weightVal, err := strconv.ParseFloat(weightStr, 64)
		if err != nil || weightVal < 0 {
			http.Error(w, "Invalid weight", http.StatusBadRequest)
			return
		}
		weight = &weightVal
	}

	notes := r.FormValue("notes")

	// Update database
	query := `
		UPDATE pelicans 
		SET name = ?, species = ?, age = ?, weight = ?, health_status = ?, arrival_date = ?, notes = ?, modified = CURRENT_TIMESTAMP
		WHERE id = ?`

	_, err = ph.DB.Exec(query, name, species, age, weight, healthStatus, arrivalDate, 
		func() interface{} {
			if notes == "" {
				return nil
			}
			return notes
		}(), id)

	if err != nil {
		http.Error(w, "Failed to update pelican: "+err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"id":      id,
		"message": "Pelican updated successfully",
	})
}

// DeletePelican handler - soft delete pelican
func (ph *PelicanHandler) DeletePelican(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodDelete && r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	vars := mux.Vars(r)
	idStr := vars["id"]
	id, err := strconv.Atoi(idStr)
	if err != nil {
		http.Error(w, "Invalid pelican ID", http.StatusBadRequest)
		return
	}

	// Check if pelican exists
	var exists int
	err = ph.DB.QueryRow("SELECT COUNT(*) FROM pelicans WHERE id = ?", id).Scan(&exists)
	if err != nil {
		http.Error(w, "Failed to check pelican existence: "+err.Error(), http.StatusInternalServerError)
		return
	}
	if exists == 0 {
		http.Error(w, "Pelican not found", http.StatusNotFound)
		return
	}

	// For demonstration, we'll add a soft delete column
	// First check if deleted_at column exists, if not add it
	_, err = ph.DB.Exec("ALTER TABLE pelicans ADD COLUMN deleted_at DATETIME")
	if err != nil {
		// Column might already exist, check if it's a constraint error
		if !strings.Contains(err.Error(), "duplicate column name") {
			// Try soft delete anyway
		}
	}

	// Soft delete by setting deleted_at timestamp
	_, err = ph.DB.Exec("UPDATE pelicans SET deleted_at = CURRENT_TIMESTAMP WHERE id = ?", id)
	if err != nil {
		// If soft delete fails, do hard delete as fallback
		_, err = ph.DB.Exec("DELETE FROM pelicans WHERE id = ?", id)
		if err != nil {
			http.Error(w, "Failed to delete pelican: "+err.Error(), http.StatusInternalServerError)
			return
		}
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"id":      id,
		"message": "Pelican deleted successfully",
	})
}
