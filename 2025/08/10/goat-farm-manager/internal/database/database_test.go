package database

import (
	"context"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/farm/goat-manager/ent"
)

func TestDatabaseConnection(t *testing.T) {
	ctx := context.Background()
	
	// Create temporary directory for test database
	tempDir, err := os.MkdirTemp("", "goat-farm-test-*")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}
	defer os.RemoveAll(tempDir)

	// Create test configuration
	config := &Config{
		DatabasePath: tempDir,
		DatabaseName: "test_goat_farm",
		CommitName:   "Test User",
		CommitEmail:  "test@example.com",
	}

	// Test database connection
	client, err := OpenEnt(ctx, config)
	if err != nil {
		t.Fatalf("Failed to open database: %v", err)
	}
	defer client.Close()

	// Test database initialization
	if err := InitializeDatabase(ctx, client); err != nil {
		t.Fatalf("Failed to initialize database: %v", err)
	}
}

func TestGoatCRUD(t *testing.T) {
	ctx := context.Background()
	client, cleanup := setupTestDB(t)
	defer cleanup()

	// Test creating a goat
	goat, err := client.Goat.
		Create().
		SetID("TEST001").
		SetName("Test Goat").
		SetBreed("alpine").
		SetGender("female").
		SetStatus("active").
		Save(ctx)
	if err != nil {
		t.Fatalf("Failed to create goat: %v", err)
	}

	if goat.ID != "TEST001" {
		t.Errorf("Expected goat ID 'TEST001', got '%s'", goat.ID)
	}

	// Test reading the goat
	retrievedGoat, err := client.Goat.Get(ctx, "TEST001")
	if err != nil {
		t.Fatalf("Failed to retrieve goat: %v", err)
	}

	if retrievedGoat.Name != "Test Goat" {
		t.Errorf("Expected goat name 'Test Goat', got '%s'", retrievedGoat.Name)
	}

	// Test updating the goat
	updatedGoat, err := client.Goat.
		UpdateOneID("TEST001").
		SetWeight(65.5).
		Save(ctx)
	if err != nil {
		t.Fatalf("Failed to update goat: %v", err)
	}

	if updatedGoat.Weight != 65.5 {
		t.Errorf("Expected goat weight 65.5, got %f", updatedGoat.Weight)
	}

	// Test listing goats
	goats, err := client.Goat.Query().All(ctx)
	if err != nil {
		t.Fatalf("Failed to list goats: %v", err)
	}

	if len(goats) != 1 {
		t.Errorf("Expected 1 goat, got %d", len(goats))
	}

	// Test deleting the goat
	err = client.Goat.DeleteOneID("TEST001").Exec(ctx)
	if err != nil {
		t.Fatalf("Failed to delete goat: %v", err)
	}

	// Verify deletion
	goats, err = client.Goat.Query().All(ctx)
	if err != nil {
		t.Fatalf("Failed to list goats after deletion: %v", err)
	}

	if len(goats) != 0 {
		t.Errorf("Expected 0 goats after deletion, got %d", len(goats))
	}
}

func TestMilkRecordCRUD(t *testing.T) {
	ctx := context.Background()
	client, cleanup := setupTestDB(t)
	defer cleanup()

	// Create a goat first
	_, err := client.Goat.
		Create().
		SetID("TEST001").
		SetName("Test Goat").
		SetBreed("alpine").
		SetGender("female").
		SetStatus("lactating").
		Save(ctx)
	if err != nil {
		t.Fatalf("Failed to create goat: %v", err)
	}

	// Test creating a milk record
	milkTime := time.Now()
	record, err := client.MilkRecord.
		Create().
		SetGoatTag("TEST001").
		SetMilkingTime(milkTime).
		SetMilkingSession("morning").
		SetVolumeLiters(3.2).
		SetQualityGrade("A").
		Save(ctx)
	if err != nil {
		t.Fatalf("Failed to create milk record: %v", err)
	}

	if record.GoatTag != "TEST001" {
		t.Errorf("Expected goat tag 'TEST001', got '%s'", record.GoatTag)
	}

	if record.VolumeLiters != 3.2 {
		t.Errorf("Expected volume 3.2, got %f", record.VolumeLiters)
	}

	// Test querying milk records
	records, err := client.MilkRecord.Query().All(ctx)
	if err != nil {
		t.Fatalf("Failed to query milk records: %v", err)
	}

	if len(records) != 1 {
		t.Errorf("Expected 1 milk record, got %d", len(records))
	}
}

func TestVersionControl(t *testing.T) {
	ctx := context.Background()
	client, cleanup := setupTestDB(t)
	defer cleanup()

	// Test committing changes
	err := CommitChanges(ctx, client, "Test commit")
	if err != nil {
		t.Fatalf("Failed to commit changes: %v", err)
	}

	// Test creating a branch
	err = CreateBranch(ctx, client, "test-branch")
	if err != nil {
		t.Fatalf("Failed to create branch: %v", err)
	}

	// Test switching branches
	err = SwitchBranch(ctx, client, "test-branch")
	if err != nil {
		t.Fatalf("Failed to switch branch: %v", err)
	}

	// Test getting branches
	branches, err := GetBranches(ctx, client)
	if err != nil {
		t.Fatalf("Failed to get branches: %v", err)
	}

	// Should have at least main and test-branch
	if len(branches) < 2 {
		t.Errorf("Expected at least 2 branches, got %d", len(branches))
	}

	// Test getting commit history
	commits, err := GetCommitHistory(ctx, client, 5)
	if err != nil {
		t.Fatalf("Failed to get commit history: %v", err)
	}

	if len(commits) == 0 {
		t.Error("Expected at least one commit in history")
	}
}

func TestWorkflowManager(t *testing.T) {
	ctx := context.Background()
	client, cleanup := setupTestDB(t)
	defer cleanup()

	wm := NewWorkflowManager(client)

	// Test creating a feature branch
	err := wm.CreateFeatureBranch(ctx, "feature-test", "Test feature branch")
	if err != nil {
		t.Fatalf("Failed to create feature branch: %v", err)
	}

	// Test getting branch status
	status, err := wm.GetBranchStatus(ctx, "feature-test")
	if err != nil {
		t.Fatalf("Failed to get branch status: %v", err)
	}

	if status.Name != "feature-test" {
		t.Errorf("Expected branch name 'feature-test', got '%s'", status.Name)
	}

	// Test creating a backup
	backupBranch, err := wm.CreateBackup(ctx, "Test backup")
	if err != nil {
		t.Fatalf("Failed to create backup: %v", err)
	}

	if backupBranch == "" {
		t.Error("Expected backup branch name, got empty string")
	}
}

// setupTestDB creates a test database and returns a client and cleanup function
func setupTestDB(t *testing.T) (*ent.Client, func()) {
	ctx := context.Background()
	
	// Create temporary directory for test database
	tempDir, err := os.MkdirTemp("", "goat-farm-test-*")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %v", err)
	}

	// Create test configuration
	config := &Config{
		DatabasePath: tempDir,
		DatabaseName: "test_goat_farm",
		CommitName:   "Test User",
		CommitEmail:  "test@example.com",
	}

	// Open database connection
	client, err := OpenEnt(ctx, config)
	if err != nil {
		t.Fatalf("Failed to open test database: %v", err)
	}

	// Initialize database
	if err := InitializeDatabase(ctx, client); err != nil {
		t.Fatalf("Failed to initialize test database: %v", err)
	}

	// Return client and cleanup function
	return client, func() {
		client.Close()
		os.RemoveAll(tempDir)
	}
}

func BenchmarkGoatCreation(b *testing.B) {
	ctx := context.Background()
	client, cleanup := setupTestDB(&testing.T{})
	defer cleanup()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		tagID := fmt.Sprintf("BENCH%04d", i)
		_, err := client.Goat.
			Create().
			SetID(tagID).
			SetName(fmt.Sprintf("Bench Goat %d", i)).
			SetBreed("alpine").
			SetGender("female").
			SetStatus("active").
			Save(ctx)
		if err != nil {
			b.Fatalf("Failed to create goat: %v", err)
		}
	}
}

func BenchmarkMilkRecordCreation(b *testing.B) {
	ctx := context.Background()
	client, cleanup := setupTestDB(&testing.T{})
	defer cleanup()

	// Create a test goat
	_, err := client.Goat.
		Create().
		SetID("BENCH001").
		SetName("Bench Goat").
		SetBreed("alpine").
		SetGender("female").
		SetStatus("lactating").
		Save(ctx)
	if err != nil {
		b.Fatalf("Failed to create test goat: %v", err)
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, err := client.MilkRecord.
			Create().
			SetGoatTag("BENCH001").
			SetMilkingTime(time.Now()).
			SetMilkingSession("morning").
			SetVolumeLiters(3.0).
			SetQualityGrade("A").
			Save(ctx)
		if err != nil {
			b.Fatalf("Failed to create milk record: %v", err)
		}
	}
}

