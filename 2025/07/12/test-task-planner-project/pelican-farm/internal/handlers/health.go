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

type HealthHandler struct {
	DB *sql.DB
}

func NewHealthHandler(db *sql.DB) *HealthHandler {
	return &HealthHandler{DB: db}
}

// ListHealthChecks handler - show all health check records with search/filter
func (hh *HealthHandler) ListHealthChecks(w http.ResponseWriter, r *http.Request) {
	// Get query parameters for search and filtering
	pelicanID := r.URL.Query().Get("pelican_id")
	veterinarian := r.URL.Query().Get("veterinarian")
	startDate := r.URL.Query().Get("start_date")
	endDate := r.URL.Query().Get("end_date")

	// Build query with optional filters
	query := `
		SELECT hc.id, hc.pelican_id, hc.check_date, hc.weight, hc.temperature, hc.notes, hc.veterinarian, hc.created,
			   p.name as pelican_name
		FROM health_checks hc
		LEFT JOIN pelicans p ON hc.pelican_id = p.id
		WHERE 1=1`
	args := []interface{}{}

	if pelicanID != "" {
		query += " AND hc.pelican_id = ?"
		args = append(args, pelicanID)
	}

	if veterinarian != "" {
		query += " AND hc.veterinarian LIKE ?"
		args = append(args, "%"+veterinarian+"%")
	}

	if startDate != "" {
		query += " AND DATE(hc.check_date) >= ?"
		args = append(args, startDate)
	}

	if endDate != "" {
		query += " AND DATE(hc.check_date) <= ?"
		args = append(args, endDate)
	}

	query += " ORDER BY hc.check_date DESC"

	rows, err := hh.DB.Query(query, args...)
	if err != nil {
		http.Error(w, "Failed to fetch health checks: "+err.Error(), http.StatusInternalServerError)
		return
	}
	defer rows.Close()

	var healthChecks []map[string]interface{}
	for rows.Next() {
		var hc models.HealthCheck
		var checkDate sql.NullString
		var weight, temperature sql.NullFloat64
		var notes, veterinarian sql.NullString
		var pelicanName sql.NullString

		err := rows.Scan(&hc.ID, &hc.PelicanID, &checkDate, &weight, &temperature, &notes, &veterinarian, &hc.Created, &pelicanName)
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

		healthCheck := map[string]interface{}{
			"id":           hc.ID,
			"pelican_id":   hc.PelicanID,
			"pelican_name": pelicanName.String,
			"check_date":   hc.CheckDate,
			"weight":       hc.Weight,
			"temperature":  hc.Temperature,
			"notes":        hc.Notes,
			"veterinarian": hc.Veterinarian,
			"created":      hc.Created,
		}

		healthChecks = append(healthChecks, healthCheck)
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"health_checks": healthChecks,
		"count":         len(healthChecks),
	})
}

// ShowHealthCheck handler - display single health check details
func (hh *HealthHandler) ShowHealthCheck(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	idStr := vars["id"]
	id, err := strconv.Atoi(idStr)
	if err != nil {
		http.Error(w, "Invalid health check ID", http.StatusBadRequest)
		return
	}

	// Get health check details
	var hc models.HealthCheck
	var checkDate sql.NullString
	var weight, temperature sql.NullFloat64
	var notes, veterinarian sql.NullString
	var pelicanName sql.NullString

	err = hh.DB.QueryRow(`
		SELECT hc.id, hc.pelican_id, hc.check_date, hc.weight, hc.temperature, hc.notes, hc.veterinarian, hc.created,
			   p.name as pelican_name
		FROM health_checks hc
		LEFT JOIN pelicans p ON hc.pelican_id = p.id
		WHERE hc.id = ?`, id).Scan(
		&hc.ID, &hc.PelicanID, &checkDate, &weight, &temperature, &notes, &veterinarian, &hc.Created, &pelicanName)

	if err != nil {
		if err == sql.ErrNoRows {
			http.Error(w, "Health check not found", http.StatusNotFound)
			return
		}
		http.Error(w, "Failed to fetch health check: "+err.Error(), http.StatusInternalServerError)
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

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"health_check": hc,
		"pelican_name": pelicanName.String,
	})
}

// NewHealthCheck handler - show form to record new health check
func (hh *HealthHandler) NewHealthCheck(w http.ResponseWriter, r *http.Request) {
	// Get list of pelicans for dropdown
	rows, err := hh.DB.Query("SELECT id, name FROM pelicans ORDER BY name")
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
    <title>New Health Check</title>
</head>
<body>
    <h1>Record New Health Check</h1>
    <form method="POST" action="/health">
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
            <label for="check_date">Check Date:</label>
            <input type="date" id="check_date" name="check_date" required>
        </div>
        <div>
            <label for="weight">Weight (kg):</label>
            <input type="number" id="weight" name="weight" step="0.01" min="0">
        </div>
        <div>
            <label for="temperature">Temperature (°C):</label>
            <input type="number" id="temperature" name="temperature" step="0.1" min="0">
        </div>
        <div>
            <label for="veterinarian">Veterinarian:</label>
            <input type="text" id="veterinarian" name="veterinarian">
        </div>
        <div>
            <label for="notes">Notes:</label>
            <textarea id="notes" name="notes" placeholder="Health check observations, treatments, recommendations..."></textarea>
        </div>
        <button type="submit">Record Health Check</button>
        <a href="/health">Cancel</a>
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

// CreateHealthCheck handler - process health check form submission
func (hh *HealthHandler) CreateHealthCheck(w http.ResponseWriter, r *http.Request) {
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
	checkDate := r.FormValue("check_date")

	if pelicanIDStr == "" || checkDate == "" {
		http.Error(w, "Pelican and check date are required", http.StatusBadRequest)
		return
	}

	pelicanID, err := strconv.Atoi(pelicanIDStr)
	if err != nil || pelicanID <= 0 {
		http.Error(w, "Invalid pelican ID", http.StatusBadRequest)
		return
	}

	// Validate pelican exists
	var exists int
	err = hh.DB.QueryRow("SELECT COUNT(*) FROM pelicans WHERE id = ?", pelicanID).Scan(&exists)
	if err != nil {
		http.Error(w, "Failed to check pelican existence: "+err.Error(), http.StatusInternalServerError)
		return
	}
	if exists == 0 {
		http.Error(w, "Selected pelican does not exist", http.StatusBadRequest)
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

	var temperature *float64
	temperatureStr := r.FormValue("temperature")
	if temperatureStr != "" {
		tempVal, err := strconv.ParseFloat(temperatureStr, 64)
		if err != nil || tempVal < 0 {
			http.Error(w, "Invalid temperature", http.StatusBadRequest)
			return
		}
		temperature = &tempVal
	}

	veterinarian := strings.TrimSpace(r.FormValue("veterinarian"))
	notes := strings.TrimSpace(r.FormValue("notes"))

	// Insert into database
	query := `
		INSERT INTO health_checks (pelican_id, check_date, weight, temperature, veterinarian, notes, created)
		VALUES (?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)`

	result, err := hh.DB.Exec(query, pelicanID, checkDate, weight, temperature,
		func() interface{} {
			if veterinarian == "" {
				return nil
			}
			return veterinarian
		}(),
		func() interface{} {
			if notes == "" {
				return nil
			}
			return notes
		}())

	if err != nil {
		http.Error(w, "Failed to create health check: "+err.Error(), http.StatusInternalServerError)
		return
	}

	id, err := result.LastInsertId()
	if err != nil {
		http.Error(w, "Failed to get created health check ID: "+err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"id":      id,
		"message": "Health check created successfully",
	})
}

// EditHealthCheck handler - show edit form for health check
func (hh *HealthHandler) EditHealthCheck(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	idStr := vars["id"]
	id, err := strconv.Atoi(idStr)
	if err != nil {
		http.Error(w, "Invalid health check ID", http.StatusBadRequest)
		return
	}

	// Get existing health check data
	var hc models.HealthCheck
	var checkDate sql.NullString
	var weight, temperature sql.NullFloat64
	var notes, veterinarian sql.NullString

	err = hh.DB.QueryRow(`
		SELECT id, pelican_id, check_date, weight, temperature, veterinarian, notes 
		FROM health_checks WHERE id = ?`, id).Scan(
		&hc.ID, &hc.PelicanID, &checkDate, &weight, &temperature, &veterinarian, &notes)

	if err != nil {
		if err == sql.ErrNoRows {
			http.Error(w, "Health check not found", http.StatusNotFound)
			return
		}
		http.Error(w, "Failed to fetch health check: "+err.Error(), http.StatusInternalServerError)
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

	// Get list of pelicans for dropdown
	rows, err := hh.DB.Query("SELECT id, name FROM pelicans ORDER BY name")
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
    <title>Edit Health Check</title>
</head>
<body>
    <h1>Edit Health Check</h1>
    <form method="POST" action="/health/{{.HealthCheck.ID}}">
        <input type="hidden" name="_method" value="PUT">
        <div>
            <label for="pelican_id">Pelican:</label>
            <select id="pelican_id" name="pelican_id" required>
                {{range .Pelicans}}
                <option value="{{.id}}" {{if eq .id $.HealthCheck.PelicanID}}selected{{end}}>{{.name}}</option>
                {{end}}
            </select>
        </div>
        <div>
            <label for="check_date">Check Date:</label>
            <input type="date" id="check_date" name="check_date" value="{{.HealthCheck.CheckDate}}" required>
        </div>
        <div>
            <label for="weight">Weight (kg):</label>
            <input type="number" id="weight" name="weight" step="0.01" min="0" value="{{if .HealthCheck.Weight}}{{.HealthCheck.Weight}}{{end}}">
        </div>
        <div>
            <label for="temperature">Temperature (°C):</label>
            <input type="number" id="temperature" name="temperature" step="0.1" min="0" value="{{if .HealthCheck.Temperature}}{{.HealthCheck.Temperature}}{{end}}">
        </div>
        <div>
            <label for="veterinarian">Veterinarian:</label>
            <input type="text" id="veterinarian" name="veterinarian" value="{{if .HealthCheck.Veterinarian}}{{.HealthCheck.Veterinarian}}{{end}}">
        </div>
        <div>
            <label for="notes">Notes:</label>
            <textarea id="notes" name="notes">{{if .HealthCheck.Notes}}{{.HealthCheck.Notes}}{{end}}</textarea>
        </div>
        <button type="submit">Update Health Check</button>
        <a href="/health/{{.HealthCheck.ID}}">Cancel</a>
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
		"HealthCheck": hc,
		"Pelicans":    pelicans,
	})
}

// UpdateHealthCheck handler - process health check updates
func (hh *HealthHandler) UpdateHealthCheck(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost && r.Method != http.MethodPut {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	vars := mux.Vars(r)
	idStr := vars["id"]
	id, err := strconv.Atoi(idStr)
	if err != nil {
		http.Error(w, "Invalid health check ID", http.StatusBadRequest)
		return
	}

	// Check if health check exists
	var exists int
	err = hh.DB.QueryRow("SELECT COUNT(*) FROM health_checks WHERE id = ?", id).Scan(&exists)
	if err != nil {
		http.Error(w, "Failed to check health check existence: "+err.Error(), http.StatusInternalServerError)
		return
	}
	if exists == 0 {
		http.Error(w, "Health check not found", http.StatusNotFound)
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
	checkDate := r.FormValue("check_date")

	if pelicanIDStr == "" || checkDate == "" {
		http.Error(w, "Pelican and check date are required", http.StatusBadRequest)
		return
	}

	pelicanID, err := strconv.Atoi(pelicanIDStr)
	if err != nil || pelicanID <= 0 {
		http.Error(w, "Invalid pelican ID", http.StatusBadRequest)
		return
	}

	// Validate pelican exists
	var pelicanExists int
	err = hh.DB.QueryRow("SELECT COUNT(*) FROM pelicans WHERE id = ?", pelicanID).Scan(&pelicanExists)
	if err != nil {
		http.Error(w, "Failed to check pelican existence: "+err.Error(), http.StatusInternalServerError)
		return
	}
	if pelicanExists == 0 {
		http.Error(w, "Selected pelican does not exist", http.StatusBadRequest)
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

	var temperature *float64
	temperatureStr := r.FormValue("temperature")
	if temperatureStr != "" {
		tempVal, err := strconv.ParseFloat(temperatureStr, 64)
		if err != nil || tempVal < 0 {
			http.Error(w, "Invalid temperature", http.StatusBadRequest)
			return
		}
		temperature = &tempVal
	}

	veterinarian := strings.TrimSpace(r.FormValue("veterinarian"))
	notes := strings.TrimSpace(r.FormValue("notes"))

	// Update database
	query := `
		UPDATE health_checks 
		SET pelican_id = ?, check_date = ?, weight = ?, temperature = ?, veterinarian = ?, notes = ?
		WHERE id = ?`

	_, err = hh.DB.Exec(query, pelicanID, checkDate, weight, temperature,
		func() interface{} {
			if veterinarian == "" {
				return nil
			}
			return veterinarian
		}(),
		func() interface{} {
			if notes == "" {
				return nil
			}
			return notes
		}(), id)

	if err != nil {
		http.Error(w, "Failed to update health check: "+err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"id":      id,
		"message": "Health check updated successfully",
	})
}

// DeleteHealthCheck handler - remove health check record
func (hh *HealthHandler) DeleteHealthCheck(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodDelete && r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	vars := mux.Vars(r)
	idStr := vars["id"]
	id, err := strconv.Atoi(idStr)
	if err != nil {
		http.Error(w, "Invalid health check ID", http.StatusBadRequest)
		return
	}

	// Check if health check exists
	var exists int
	err = hh.DB.QueryRow("SELECT COUNT(*) FROM health_checks WHERE id = ?", id).Scan(&exists)
	if err != nil {
		http.Error(w, "Failed to check health check existence: "+err.Error(), http.StatusInternalServerError)
		return
	}
	if exists == 0 {
		http.Error(w, "Health check not found", http.StatusNotFound)
		return
	}

	// Delete health check
	_, err = hh.DB.Exec("DELETE FROM health_checks WHERE id = ?", id)
	if err != nil {
		http.Error(w, "Failed to delete health check: "+err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]interface{}{
		"id":      id,
		"message": "Health check deleted successfully",
	})
}
