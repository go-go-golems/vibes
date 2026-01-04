package database

import (
	"gorm.io/driver/sqlite"
	"gorm.io/gorm"
	"pelican-farm/internal/models"
)

func Initialize() (*gorm.DB, error) {
	db, err := gorm.Open(sqlite.Open("pelican_farm.db"), &gorm.Config{})
	if err != nil {
		return nil, err
	}

	// Auto-migrate the schema
	err = db.AutoMigrate(&models.Pelican{}, &models.Farm{})
	if err != nil {
		return nil, err
	}

	return db, nil
}
