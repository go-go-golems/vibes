package pdfjobs

import (
	"bytes"
	"context"
	"fmt"
	"image"
	_ "image/jpeg"
	_ "image/png"
	"io"
	"strings"

	"github.com/jung-kurt/gofpdf/v2"
	"github.com/pkg/errors"
	"photobook-backend-go/internal/storage"
	"photobook-backend-go/pkg/types"
)

// Generator handles PDF generation from photos
type Generator struct {
	storage storage.Storage
}

// NewGenerator creates a new PDF generator
func NewGenerator(storage storage.Storage) *Generator {
	return &Generator{
		storage: storage,
	}
}

// GeneratePDF generates a PDF from the given photos
// Returns the PDF bytes and any error
func (g *Generator) GeneratePDF(ctx context.Context, photos []*types.Photo) ([]byte, error) {
	if len(photos) == 0 {
		return nil, fmt.Errorf("no photos provided")
	}

	// Create PDF document
	// A4 portrait: 210mm × 297mm
	pdf := gofpdf.New("P", "mm", "A4", "")
	
	// Page dimensions
	pageWidth := 210.0  // mm
	pageHeight := 297.0 // mm
	margin := 10.0     // mm
	imageWidth := pageWidth - 2*margin
	imageHeight := pageHeight - 2*margin
	pageAspectRatio := imageWidth / imageHeight

	// Process each photo
	for i, photo := range photos {
		// Add new page (except for first photo)
		if i > 0 {
			pdf.AddPage()
		}

		// Download image from storage
		reader, err := g.storage.Open(ctx, photo.FileKey)
		if err != nil {
			return nil, errors.Wrapf(err, "failed to open photo %d", photo.ID)
		}
		defer reader.Close()

		// Decode image to get dimensions
		img, format, err := image.Decode(reader)
		if err != nil {
			return nil, errors.Wrapf(err, "failed to decode photo %d", photo.ID)
		}

		// Get image dimensions
		bounds := img.Bounds()
		imgWidth := float64(bounds.Dx())
		imgHeight := float64(bounds.Dy())
		imgAspectRatio := imgWidth / imgHeight

		// Calculate aspect-fit dimensions
		var finalWidth, finalHeight, xOffset, yOffset float64

		if imgAspectRatio > pageAspectRatio {
			// Image is wider - fit to width
			finalWidth = imageWidth
			finalHeight = imageWidth / imgAspectRatio
			xOffset = margin
			yOffset = margin + (imageHeight-finalHeight)/2
		} else {
			// Image is taller - fit to height
			finalWidth = imageHeight * imgAspectRatio
			finalHeight = imageHeight
			xOffset = margin + (imageWidth-finalWidth)/2
			yOffset = margin
		}

		// Reset reader to beginning for PDF
		reader.Close()
		reader, err = g.storage.Open(ctx, photo.FileKey)
		if err != nil {
			return nil, errors.Wrapf(err, "failed to reopen photo %d", photo.ID)
		}
		defer reader.Close()

		// Read image data into memory
		imageData, err := io.ReadAll(reader)
		if err != nil {
			return nil, errors.Wrapf(err, "failed to read photo %d", photo.ID)
		}

		// Determine image type from format string
		imageType := "jpg" // default
		if strings.HasPrefix(format, "png") {
			imageType = "png"
		}

		// Register image in PDF
		opt := gofpdf.ImageOptions{
			ImageType: imageType,
		}

		// Register image with a unique name
		imageName := fmt.Sprintf("photo_%d", photo.ID)
		pdf.RegisterImageOptionsReader(imageName, opt, bytes.NewReader(imageData))

		// Add image to PDF page
		pdf.ImageOptions(imageName, xOffset, yOffset, finalWidth, finalHeight, false, opt, 0, "")
	}

	// Generate PDF bytes
	var buf bytes.Buffer
	if err := pdf.Output(&buf); err != nil {
		return nil, errors.Wrap(err, "failed to generate PDF")
	}

	return buf.Bytes(), nil
}

