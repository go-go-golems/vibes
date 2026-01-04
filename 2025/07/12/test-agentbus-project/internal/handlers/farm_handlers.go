package handlers

import (
	"net/http"
	"strconv"
	"time"

	"github.com/gin-gonic/gin"
	"gorm.io/gorm"
	"pelican-farm/internal/models"
)

type FarmHandler struct {
	db *gorm.DB
}

func NewFarmHandler(db *gorm.DB) *FarmHandler {
	return &FarmHandler{db: db}
}

func (h *FarmHandler) CreateFarm(c *gin.Context) {
	var farm models.Farm
	if err := c.ShouldBindJSON(&farm); err != nil {
		c.JSON(http.StatusBadRequest, gin.H{
			"error":   "Invalid request body",
			"details": err.Error(),
		})
		return
	}

	farm.CreatedAt = time.Now()
	farm.UpdatedAt = time.Now()

	if err := h.db.Create(&farm).Error; err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{
			"error":   "Failed to create farm",
			"details": err.Error(),
		})
		return
	}

	c.JSON(http.StatusCreated, gin.H{
		"message": "Farm created successfully",
		"data":    farm,
	})
}

func (h *FarmHandler) GetFarms(c *gin.Context) {
	// Check for filter parameters
	farmType := c.Query("type")
	location := c.Query("location")
	minCapacity := c.Query("min_capacity")
	maxCapacity := c.Query("max_capacity")
	hasVacancy := c.Query("has_vacancy")

	query := h.db.Model(&models.Farm{})

	if farmType != "" {
		query = query.Where("type = ?", farmType)
	}
	if location != "" {
		query = query.Where("location LIKE ?", "%"+location+"%")
	}
	if minCapacity != "" {
		if capacity, err := strconv.Atoi(minCapacity); err == nil {
			query = query.Where("capacity >= ?", capacity)
		}
	}
	if maxCapacity != "" {
		if capacity, err := strconv.Atoi(maxCapacity); err == nil {
			query = query.Where("capacity <= ?", capacity)
		}
	}
	if hasVacancy == "true" {
		query = query.Where("current_count < capacity")
	} else if hasVacancy == "false" {
		query = query.Where("current_count >= capacity")
	}

	var farms []models.Farm
	if err := query.Find(&farms).Error; err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{
			"error":   "Failed to retrieve farms",
			"details": err.Error(),
		})
		return
	}

	c.JSON(http.StatusOK, gin.H{
		"message": "Farms retrieved successfully",
		"data":    farms,
		"count":   len(farms),
	})
}

func (h *FarmHandler) GetFarm(c *gin.Context) {
	idStr := c.Param("id")
	id, err := strconv.ParseUint(idStr, 10, 32)
	if err != nil {
		c.JSON(http.StatusBadRequest, gin.H{
			"error":   "Invalid farm ID",
			"details": "ID must be a positive integer",
		})
		return
	}

	var farm models.Farm
	if err := h.db.First(&farm, id).Error; err != nil {
		c.JSON(http.StatusNotFound, gin.H{
			"error":   "Farm not found",
			"details": err.Error(),
		})
		return
	}

	c.JSON(http.StatusOK, gin.H{
		"message": "Farm retrieved successfully",
		"data":    farm,
	})
}

func (h *FarmHandler) UpdateFarm(c *gin.Context) {
	idStr := c.Param("id")
	id, err := strconv.ParseUint(idStr, 10, 32)
	if err != nil {
		c.JSON(http.StatusBadRequest, gin.H{
			"error":   "Invalid farm ID",
			"details": "ID must be a positive integer",
		})
		return
	}

	var updates models.FarmUpdate
	if err := c.ShouldBindJSON(&updates); err != nil {
		c.JSON(http.StatusBadRequest, gin.H{
			"error":   "Invalid request body",
			"details": err.Error(),
		})
		return
	}

	updateData := map[string]interface{}{
		"updated_at": time.Now(),
	}
	
	if updates.Name != nil {
		updateData["name"] = *updates.Name
	}
	if updates.Type != nil {
		updateData["type"] = *updates.Type
	}
	if updates.Location != nil {
		updateData["location"] = *updates.Location
	}
	if updates.Capacity != nil {
		updateData["capacity"] = *updates.Capacity
	}
	if updates.ManagerName != nil {
		updateData["manager_name"] = *updates.ManagerName
	}
	if updates.ManagerEmail != nil {
		updateData["manager_email"] = *updates.ManagerEmail
	}

	if err := h.db.Model(&models.Farm{}).Where("id = ?", id).Updates(updateData).Error; err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{
			"error":   "Failed to update farm",
			"details": err.Error(),
		})
		return
	}

	// Fetch updated farm to return
	var updatedFarm models.Farm
	if err := h.db.First(&updatedFarm, id).Error; err != nil {
		c.JSON(http.StatusOK, gin.H{
			"message": "Farm updated successfully",
		})
		return
	}

	c.JSON(http.StatusOK, gin.H{
		"message": "Farm updated successfully",
		"data":    updatedFarm,
	})
}

func (h *FarmHandler) DeleteFarm(c *gin.Context) {
	idStr := c.Param("id")
	id, err := strconv.ParseUint(idStr, 10, 32)
	if err != nil {
		c.JSON(http.StatusBadRequest, gin.H{
			"error":   "Invalid farm ID",
			"details": "ID must be a positive integer",
		})
		return
	}

	if err := h.db.Delete(&models.Farm{}, id).Error; err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{
			"error":   "Failed to delete farm",
			"details": err.Error(),
		})
		return
	}

	c.JSON(http.StatusOK, gin.H{
		"message": "Farm deleted successfully",
	})
}

func (h *FarmHandler) GetFarmStats(c *gin.Context) {
	idStr := c.Param("id")
	farmID, err := strconv.ParseUint(idStr, 10, 32)
	if err != nil {
		c.JSON(http.StatusBadRequest, gin.H{
			"error":   "Invalid farm ID",
			"details": "ID must be a positive integer",
		})
		return
	}

	// Get farm details
	var farm models.Farm
	if err := h.db.First(&farm, farmID).Error; err != nil {
		c.JSON(http.StatusNotFound, gin.H{
			"error":   "Farm not found",
			"details": err.Error(),
		})
		return
	}

	// Calculate farm statistics
	var totalPelicans int64
	h.db.Model(&models.Assignment{}).Where("farm_id = ?", farmID).Count(&totalPelicans)

	// Get health status counts
	healthCounts := make(map[models.HealthStatus]int)
	var assignments []models.Assignment
	h.db.Where("farm_id = ?", farmID).Find(&assignments)
	
	for _, assignment := range assignments {
		var pelican models.Pelican
		if err := h.db.First(&pelican, assignment.PelicanID).Error; err == nil {
			healthCounts[pelican.Health]++
		}
	}

	// Get species counts
	speciesCounts := make(map[string]int)
	for _, assignment := range assignments {
		var pelican models.Pelican
		if err := h.db.First(&pelican, assignment.PelicanID).Error; err == nil {
			speciesCounts[pelican.Species]++
		}
	}

	// Calculate average age and weight
	var avgAge, avgWeight float64
	if totalPelicans > 0 {
		var sumAge, sumWeight float64
		for _, assignment := range assignments {
			var pelican models.Pelican
			if err := h.db.First(&pelican, assignment.PelicanID).Error; err == nil {
				sumAge += float64(pelican.Age)
				sumWeight += pelican.Weight
			}
		}
		avgAge = sumAge / float64(totalPelicans)
		avgWeight = sumWeight / float64(totalPelicans)
	}

	// Calculate capacity usage
	capacityUsage := float64(totalPelicans) / float64(farm.Capacity) * 100

	stats := models.FarmStats{
		TotalPelicans: int(totalPelicans),
		HealthyCounts: healthCounts,
		SpeciesCounts: speciesCounts,
		AverageAge:    avgAge,
		AverageWeight: avgWeight,
		CapacityUsage: capacityUsage,
	}

	c.JSON(http.StatusOK, gin.H{
		"message": "Farm statistics retrieved successfully",
		"data":    stats,
	})
}

func (h *FarmHandler) AssignPelican(c *gin.Context) {
	farmIDStr := c.Param("id")
	pelicanIDStr := c.Param("pelican_id")

	farmID, err := strconv.ParseUint(farmIDStr, 10, 32)
	if err != nil {
		c.JSON(http.StatusBadRequest, gin.H{
			"error":   "Invalid farm ID",
			"details": "Farm ID must be a positive integer",
		})
		return
	}

	pelicanID, err := strconv.ParseUint(pelicanIDStr, 10, 32)
	if err != nil {
		c.JSON(http.StatusBadRequest, gin.H{
			"error":   "Invalid pelican ID",
			"details": "Pelican ID must be a positive integer",
		})
		return
	}

	// Check if farm exists and has capacity
	var farm models.Farm
	if err := h.db.First(&farm, farmID).Error; err != nil {
		c.JSON(http.StatusNotFound, gin.H{
			"error":   "Farm not found",
			"details": err.Error(),
		})
		return
	}

	if farm.CurrentCount >= farm.Capacity {
		c.JSON(http.StatusBadRequest, gin.H{
			"error":   "Farm is at full capacity",
			"details": "Cannot assign more pelicans to this farm",
		})
		return
	}

	// Check if pelican exists
	var pelican models.Pelican
	if err := h.db.First(&pelican, pelicanID).Error; err != nil {
		c.JSON(http.StatusNotFound, gin.H{
			"error":   "Pelican not found",
			"details": err.Error(),
		})
		return
	}

	// Check if assignment already exists
	var existingAssignment models.Assignment
	result := h.db.Where("pelican_id = ? AND farm_id = ?", pelicanID, farmID).First(&existingAssignment)
	if result.Error == nil {
		c.JSON(http.StatusBadRequest, gin.H{
			"error":   "Pelican is already assigned to this farm",
			"details": "Assignment already exists",
		})
		return
	}

	// Get notes from request body if provided
	var requestBody struct {
		Notes string `json:"notes"`
	}
	c.ShouldBindJSON(&requestBody)

	// Create assignment
	assignment := models.Assignment{
		PelicanID:  int(pelicanID),
		FarmID:     int(farmID),
		AssignedAt: time.Now(),
		Notes:      requestBody.Notes,
	}

	if err := h.db.Create(&assignment).Error; err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{
			"error":   "Failed to assign pelican to farm",
			"details": err.Error(),
		})
		return
	}

	// Update farm current count
	h.db.Model(&farm).Update("current_count", farm.CurrentCount+1)

	c.JSON(http.StatusOK, gin.H{
		"message": "Pelican assigned to farm successfully",
		"data":    assignment,
	})
}

func (h *FarmHandler) UnassignPelican(c *gin.Context) {
	farmIDStr := c.Param("id")
	pelicanIDStr := c.Param("pelican_id")

	farmID, err := strconv.ParseUint(farmIDStr, 10, 32)
	if err != nil {
		c.JSON(http.StatusBadRequest, gin.H{
			"error":   "Invalid farm ID",
			"details": "Farm ID must be a positive integer",
		})
		return
	}

	pelicanID, err := strconv.ParseUint(pelicanIDStr, 10, 32)
	if err != nil {
		c.JSON(http.StatusBadRequest, gin.H{
			"error":   "Invalid pelican ID",
			"details": "Pelican ID must be a positive integer",
		})
		return
	}

	// Check if assignment exists
	var assignment models.Assignment
	if err := h.db.Where("pelican_id = ? AND farm_id = ?", pelicanID, farmID).First(&assignment).Error; err != nil {
		c.JSON(http.StatusNotFound, gin.H{
			"error":   "Assignment not found",
			"details": "Pelican is not assigned to this farm",
		})
		return
	}

	// Delete assignment
	if err := h.db.Delete(&assignment).Error; err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{
			"error":   "Failed to unassign pelican from farm",
			"details": err.Error(),
		})
		return
	}

	// Update farm current count
	var farm models.Farm
	if err := h.db.First(&farm, farmID).Error; err == nil && farm.CurrentCount > 0 {
		h.db.Model(&farm).Update("current_count", farm.CurrentCount-1)
	}

	c.JSON(http.StatusOK, gin.H{
		"message": "Pelican unassigned from farm successfully",
	})
}
