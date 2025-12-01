package pdfjobs

import (
	"bytes"
	"context"
	"fmt"
	"time"

	"github.com/pkg/errors"
	"github.com/rs/zerolog"
	"photobook-backend-go/internal/photos"
	"photobook-backend-go/internal/storage"
	"photobook-backend-go/pkg/types"
)

// Worker processes PDF generation jobs
type Worker struct {
	repo      Repository
	photoRepo photos.Repository
	generator *Generator
	storage   storage.Storage
	logger    zerolog.Logger
}

// NewWorker creates a new PDF worker
func NewWorker(
	repo Repository,
	photoRepo photos.Repository,
	storage storage.Storage,
	logger zerolog.Logger,
) *Worker {
	generator := NewGenerator(storage)
	return &Worker{
		repo:      repo,
		photoRepo: photoRepo,
		generator: generator,
		storage:   storage,
		logger:    logger,
	}
}

// Run starts the worker loop, polling for jobs every 10 seconds
func (w *Worker) Run(ctx context.Context) {
	ticker := time.NewTicker(10 * time.Second)
	defer ticker.Stop()

	// Process immediately on startup
	w.processPending(ctx)

	for {
		select {
		case <-ticker.C:
			w.processPending(ctx)
		case <-ctx.Done():
			w.logger.Info().Msg("PDF worker stopping")
			return
		}
	}
}

// processPending processes pending PDF jobs
func (w *Worker) processPending(ctx context.Context) {
	// Claim up to 5 pending jobs atomically
	jobs, err := w.repo.ClaimPendingJobs(ctx, 5)
	if err != nil {
		w.logger.Error().Err(err).Msg("failed to claim pending jobs")
		return
	}

	if len(jobs) == 0 {
		return
	}

	w.logger.Info().Int("count", len(jobs)).Msg("claimed PDF jobs for processing")

	// Process each job
	for _, job := range jobs {
		if err := w.ProcessJob(ctx, job); err != nil {
			w.logger.Error().
				Err(err).
				Int64("job_id", job.ID).
				Msg("failed to process PDF job")
			// Mark job as failed
			if markErr := w.repo.MarkFailed(ctx, job.ID, err.Error()); markErr != nil {
				w.logger.Error().
					Err(markErr).
					Int64("job_id", job.ID).
					Msg("failed to mark job as failed")
			}
		}
	}
}

// ProcessJob processes a single PDF job (public for CLI access)
func (w *Worker) ProcessJob(ctx context.Context, job *types.PdfJob) error {
	logger := w.logger.With().Int64("job_id", job.ID).Int64("user_id", job.UserID).Logger()
	logger.Info().Msg("processing PDF job")

	// Fetch photos for the user
	allPhotos, err := w.photoRepo.GetByUserID(ctx, job.UserID)
	if err != nil {
		return errors.Wrap(err, "failed to fetch user photos")
	}

	// Filter to requested photo IDs and maintain order
	photoMap := make(map[int64]*types.Photo)
	for _, photo := range allPhotos {
		photoMap[photo.ID] = photo
	}

	var orderedPhotos []*types.Photo
	for _, photoID := range job.PhotoIDs {
		if photo, ok := photoMap[photoID]; ok {
			orderedPhotos = append(orderedPhotos, photo)
		} else {
			logger.Warn().Int64("photo_id", photoID).Msg("photo not found for user, skipping")
		}
	}

	if len(orderedPhotos) == 0 {
		return fmt.Errorf("no valid photos found for job")
	}

	logger.Info().Int("photo_count", len(orderedPhotos)).Msg("found photos for PDF generation")

	// Generate PDF
	pdfBytes, err := w.generator.GeneratePDF(ctx, orderedPhotos)
	if err != nil {
		return errors.Wrap(err, "failed to generate PDF")
	}

	logger.Info().Int("pdf_size", len(pdfBytes)).Msg("PDF generated successfully")

	// Upload PDF to storage
	fileKey := fmt.Sprintf("user-%d/pdfs/%d-photobook.pdf", job.UserID, time.Now().Unix())
	pdfReader := bytes.NewReader(pdfBytes)
	url, err := w.storage.Put(ctx, fileKey, pdfReader, "application/pdf")
	if err != nil {
		return errors.Wrap(err, "failed to upload PDF")
	}

	logger.Info().Str("url", url).Str("file_key", fileKey).Msg("PDF uploaded to storage")

	// Mark job as completed
	if err := w.repo.MarkCompleted(ctx, job.ID, fileKey, url); err != nil {
		return errors.Wrap(err, "failed to mark job as completed")
	}

	logger.Info().Msg("PDF job completed successfully")
	return nil
}

