package database

import (
	"gorm.io/gorm"
	"pelican-farm/internal/models"
)

type PelicanRepository struct {
	db *gorm.DB
}

func NewPelicanRepository(db *gorm.DB) *PelicanRepository {
	return &PelicanRepository{db: db}
}

func (r *PelicanRepository) Create(pelican *models.Pelican) error {
	return r.db.Create(pelican).Error
}

func (r *PelicanRepository) GetByID(id uint) (*models.Pelican, error) {
	var pelican models.Pelican
	err := r.db.First(&pelican, id).Error
	return &pelican, err
}

func (r *PelicanRepository) GetAll() ([]models.Pelican, error) {
	var pelicans []models.Pelican
	err := r.db.Find(&pelicans).Error
	return pelicans, err
}

func (r *PelicanRepository) Filter(filter models.PelicanFilter) ([]models.Pelican, error) {
	var pelicans []models.Pelican
	query := r.db

	if filter.Species != nil && *filter.Species != "" {
		query = query.Where("species = ?", *filter.Species)
	}
	if filter.Health != nil {
		query = query.Where("health = ?", *filter.Health)
	}
	if filter.Location != nil && *filter.Location != "" {
		query = query.Where("location = ?", *filter.Location)
	}
	if filter.MinAge != nil {
		query = query.Where("age >= ?", *filter.MinAge)
	}
	if filter.MaxAge != nil {
		query = query.Where("age <= ?", *filter.MaxAge)
	}
	if filter.MinWeight != nil {
		query = query.Where("weight >= ?", *filter.MinWeight)
	}
	if filter.MaxWeight != nil {
		query = query.Where("weight <= ?", *filter.MaxWeight)
	}

	err := query.Find(&pelicans).Error
	return pelicans, err
}

func (r *PelicanRepository) Update(id uint, updates models.PelicanUpdate) error {
	return r.db.Model(&models.Pelican{}).Where("id = ?", id).Updates(updates).Error
}

func (r *PelicanRepository) Delete(id uint) error {
	return r.db.Delete(&models.Pelican{}, id).Error
}

func (r *PelicanRepository) GetStats() (*models.PelicanStats, error) {
	var stats models.PelicanStats
	
	r.db.Model(&models.Pelican{}).Count(&stats.Total)
	r.db.Model(&models.Pelican{}).Where("health = ?", models.HealthStatusHealthy).Count(&stats.Healthy)
	r.db.Model(&models.Pelican{}).Where("health = ?", models.HealthStatusSick).Count(&stats.Sick)
	r.db.Model(&models.Pelican{}).Where("health = ?", models.HealthStatusInjured).Count(&stats.Injured)

	return &stats, nil
}
