package tests

import (
	"bytes"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"testing"
	
	"../backend/api"
)

func TestGetUsers(t *testing.T) {
	req, err := http.NewRequest("GET", "/api/users", nil)
	if err != nil {
		t.Fatal(err)
	}

	rr := httptest.NewRecorder()
	handler := http.HandlerFunc(api.GetUsers)

	handler.ServeHTTP(rr, req)

	if status := rr.Code; status != http.StatusOK {
		t.Errorf("handler returned wrong status code: got %v want %v",
			status, http.StatusOK)
	}

	var users []map[string]interface{}
	if err := json.Unmarshal(rr.Body.Bytes(), &users); err != nil {
		t.Errorf("Could not parse response: %v", err)
	}

	if len(users) == 0 {
		t.Error("Expected at least one user, got none")
	}
}

func TestCreateUser(t *testing.T) {
	user := map[string]string{
		"name":  "Test User",
		"email": "test@example.com",
	}
	
	jsonData, _ := json.Marshal(user)
	req, err := http.NewRequest("POST", "/api/users", bytes.NewBuffer(jsonData))
	if err != nil {
		t.Fatal(err)
	}
	
	req.Header.Set("Content-Type", "application/json")

	rr := httptest.NewRecorder()
	handler := http.HandlerFunc(api.CreateUser)

	handler.ServeHTTP(rr, req)

	if status := rr.Code; status != http.StatusCreated {
		t.Errorf("handler returned wrong status code: got %v want %v",
			status, http.StatusCreated)
	}

	var createdUser map[string]interface{}
	if err := json.Unmarshal(rr.Body.Bytes(), &createdUser); err != nil {
		t.Errorf("Could not parse response: %v", err)
	}

	if createdUser["name"] != user["name"] {
		t.Errorf("Expected name %v, got %v", user["name"], createdUser["name"])
	}
}

