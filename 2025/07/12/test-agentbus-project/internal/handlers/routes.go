package handlers

import (
	"github.com/gin-gonic/gin"
	"gorm.io/gorm"
	"pelican-farm/internal/database"
)

func SetupRoutes(r *gin.Engine, db *gorm.DB) {
	pelicanRepo := database.NewPelicanRepository(db)
	pelicanHandler := NewPelicanHandler(pelicanRepo)
	farmHandler := NewFarmHandler(db)

	api := r.Group("/api/v1")
	{
		// Pelican routes
		pelicans := api.Group("/pelicans")
		{
			pelicans.POST("", pelicanHandler.CreatePelican)
			pelicans.GET("", pelicanHandler.GetPelicans)
			pelicans.GET("/:id", pelicanHandler.GetPelican)
			pelicans.PUT("/:id", pelicanHandler.UpdatePelican)
			pelicans.DELETE("/:id", pelicanHandler.DeletePelican)
			pelicans.GET("/stats", pelicanHandler.GetPelicanStats)
		}

		// Farm routes
		farms := api.Group("/farms")
		{
			farms.POST("", farmHandler.CreateFarm)
			farms.GET("", farmHandler.GetFarms)
			farms.GET("/:id", farmHandler.GetFarm)
			farms.PUT("/:id", farmHandler.UpdateFarm)
			farms.DELETE("/:id", farmHandler.DeleteFarm)
			farms.GET("/:id/stats", farmHandler.GetFarmStats)
			farms.POST("/:id/assign/:pelican_id", farmHandler.AssignPelican)
			farms.DELETE("/:id/unassign/:pelican_id", farmHandler.UnassignPelican)
		}
	}
}
