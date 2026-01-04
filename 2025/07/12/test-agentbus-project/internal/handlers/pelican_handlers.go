package handlers

import (
	"net/http"
	"strconv"

	"github.com/gin-gonic/gin"
	"pelican-farm/internal/database"
	"pelican-farm/internal/models"
)

type PelicanHandler struct {
	repo *database.PelicanRepository
}

func NewPelicanHandler(repo *database.PelicanRepository) *PelicanHandler {
	return &PelicanHandler{repo: repo}
}

func (h *PelicanHandler) CreatePelican(c *gin.Context) {
	var pelican models.Pelican
	if err := c.ShouldBindJSON(&pelican); err != nil {
		c.JSON(http.StatusBadRequest, gin.H{
			"error":   "Invalid request body",
			"details": err.Error(),
		})
		return
	}

	if err := h.repo.Create(&pelican); err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{
			"error":   "Failed to create pelican",
			"details": err.Error(),
		})
		return
	}

	c.JSON(http.StatusCreated, gin.H{
		"message": "Pelican created successfully",
		"data":    pelican,
	})
}

func (h *PelicanHandler) GetPelicans(c *gin.Context) {
	// Check for filter parameters
	species := c.Query("species")
	health := c.Query("health")
	location := c.Query("location")
	minAge := c.Query("min_age")
	maxAge := c.Query("max_age")
	minWeight := c.Query("min_weight")
	maxWeight := c.Query("max_weight")

	filter := models.PelicanFilter{}
	
	if species != "" {
		filter.Species = &species
	}
	if health != "" {
		healthStatus := models.HealthStatus(health)
		filter.Health = &healthStatus
	}
	if location != "" {
		filter.Location = &location
	}
	if minAge != "" {
		if age, err := strconv.Atoi(minAge); err == nil {
			filter.MinAge = &age
		}
	}
	if maxAge != "" {
		if age, err := strconv.Atoi(maxAge); err == nil {
			filter.MaxAge = &age
		}
	}
	if minWeight != "" {
		if weight, err := strconv.ParseFloat(minWeight, 64); err == nil {
			filter.MinWeight = &weight
		}
	}
	if maxWeight != "" {
		if weight, err := strconv.ParseFloat(maxWeight, 64); err == nil {
			filter.MaxWeight = &weight
		}
	}

	var pelicans []models.Pelican
	var err error

	// Use filter if any parameters provided, otherwise get all
	if filter != (models.PelicanFilter{}) {
		pelicans, err = h.repo.Filter(filter)
	} else {
		pelicans, err = h.repo.GetAll()
	}

	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{
			"error":   "Failed to retrieve pelicans",
			"details": err.Error(),
		})
		return
	}

	c.JSON(http.StatusOK, gin.H{
		"message": "Pelicans retrieved successfully",
		"data":    pelicans,
		"count":   len(pelicans),
	})
}

func (h *PelicanHandler) GetPelican(c *gin.Context) {
	idStr := c.Param("id")
	id, err := strconv.ParseUint(idStr, 10, 32)
	if err != nil {
		c.JSON(http.StatusBadRequest, gin.H{
			"error":   "Invalid pelican ID",
			"details": "ID must be a positive integer",
		})
		return
	}

	pelican, err := h.repo.GetByID(uint(id))
	if err != nil {
		c.JSON(http.StatusNotFound, gin.H{
			"error":   "Pelican not found",
			"details": err.Error(),
		})
		return
	}

	c.JSON(http.StatusOK, gin.H{
		"message": "Pelican retrieved successfully",
		"data":    pelican,
	})
}

func (h *PelicanHandler) UpdatePelican(c *gin.Context) {
	idStr := c.Param("id")
	id, err := strconv.ParseUint(idStr, 10, 32)
	if err != nil {
		c.JSON(http.StatusBadRequest, gin.H{
			"error":   "Invalid pelican ID",
			"details": "ID must be a positive integer",
		})
		return
	}

	var updates models.PelicanUpdate
	if err := c.ShouldBindJSON(&updates); err != nil {
		c.JSON(http.StatusBadRequest, gin.H{
			"error":   "Invalid request body",
			"details": err.Error(),
		})
		return
	}

	if err := h.repo.Update(uint(id), updates); err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{
			"error":   "Failed to update pelican",
			"details": err.Error(),
		})
		return
	}

	// Fetch updated pelican to return
	updatedPelican, err := h.repo.GetByID(uint(id))
	if err != nil {
		c.JSON(http.StatusOK, gin.H{
			"message": "Pelican updated successfully",
		})
		return
	}

	c.JSON(http.StatusOK, gin.H{
		"message": "Pelican updated successfully",
		"data":    updatedPelican,
	})
}

func (h *PelicanHandler) DeletePelican(c *gin.Context) {
	idStr := c.Param("id")
	id, err := strconv.ParseUint(idStr, 10, 32)
	if err != nil {
		c.JSON(http.StatusBadRequest, gin.H{
			"error":   "Invalid pelican ID",
			"details": "ID must be a positive integer",
		})
		return
	}

	if err := h.repo.Delete(uint(id)); err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{
			"error":   "Failed to delete pelican",
			"details": err.Error(),
		})
		return
	}

	c.JSON(http.StatusOK, gin.H{
		"message": "Pelican deleted successfully",
	})
}

func (h *PelicanHandler) GetPelicanStats(c *gin.Context) {
	stats, err := h.repo.GetStats()
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{
			"error":   "Failed to retrieve pelican statistics",
			"details": err.Error(),
		})
		return
	}

	c.JSON(http.StatusOK, gin.H{
		"message": "Pelican statistics retrieved successfully",
		"data":    stats,
	})
}
