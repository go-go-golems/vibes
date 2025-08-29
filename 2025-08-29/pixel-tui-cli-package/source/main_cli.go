package main

import (
    "flag"
    "fmt"
    "image"
    "image/color"
    "image/gif"
    "image/draw"
    "image/png"
    _ "image/jpeg"
    "math"
    "log"
    "os"
    "path/filepath"
    "strconv"
    "strings"
    "time"

    "github.com/charmbracelet/bubbletea"
    "github.com/charmbracelet/lipgloss"
)

// ColorSampling defines how colors are processed
type ColorSampling int

const (
	SamplingNearest ColorSampling = iota // Use nearest color from original image
	SamplingQuantized                    // Quantize colors to reduce palette
	SamplingInterpolated                 // Allow intermediate colors
)

// ProcessedImage represents a processed image ready for terminal display
type ProcessedImage struct {
	Filename   string     `json:"filename"`
	Width      int        `json:"width"`
	Height     int        `json:"height"`
	Pixels     [][]int    `json:"pixels"`
	Palette    []string   `json:"palette"`
	ColorCount int        `json:"color_count"`
}

// ProcessedGIF represents a processed animated GIF
type ProcessedGIF struct {
	Filename       string            `json:"filename"`
	OriginalWidth  int               `json:"original_width"`
	OriginalHeight int               `json:"original_height"`
	IsAnimated     bool              `json:"is_animated"`
	FrameCount     int               `json:"frame_count"`
	Frames         []ProcessedImage  `json:"frames"`
}

// Config holds the application configuration
type Config struct {
    InputFile     string
    OutputWidth   int
    OutputHeight  int
    ColorSampling ColorSampling
    ExportScale   int
    Verbose       bool
    RenderMode    RenderMode
    MaxColors     int
    DownscaleFactor int
    SpeedMS         int
    StepMode        bool
    TransparentBG   bool
}

// Model represents the TUI application state
type Model struct {
    config        Config
    staticImage   *ProcessedImage
    animatedGIF   *ProcessedGIF
    currentFrame  int
    animationView bool
    playing       bool
    animationSpeed time.Duration
    width         int
    height        int
}

// Animation tick message
type tickMsg time.Time

func doTick(d time.Duration) tea.Cmd {
	return tea.Tick(d, func(t time.Time) tea.Msg {
		return tickMsg(t)
	})
}

// RenderMode selects how pixels are mapped to terminal cells
type RenderMode int

const (
    RenderBlock RenderMode = iota // 1 pixel per cell using background color
    RenderHalf                    // 2 vertical pixels per cell using '▀' (fg/bg)
)

func parseRenderMode(s string) RenderMode {
    switch strings.ToLower(s) {
    case "half", "subpixel":
        return RenderHalf
    default:
        return RenderBlock
    }
}

// parseColorSampling converts string to ColorSampling enum
func parseColorSampling(s string) ColorSampling {
	switch strings.ToLower(s) {
	case "nearest":
		return SamplingNearest
	case "quantized":
		return SamplingQuantized
	case "interpolated":
		return SamplingInterpolated
	default:
		return SamplingNearest
	}
}

// rgbToHex converts RGB values to hex string
func rgbToHex(r, g, b uint8) string {
	return fmt.Sprintf("#%02x%02x%02x", r, g, b)
}

// hexToRGB converts hex color string to RGB values
func hexToRGB(hex string) (uint8, uint8, uint8) {
	hex = strings.TrimPrefix(hex, "#")
	if len(hex) != 6 {
		return 0, 0, 0
	}
	
	r, _ := strconv.ParseUint(hex[0:2], 16, 8)
	g, _ := strconv.ParseUint(hex[2:4], 16, 8)
	b, _ := strconv.ParseUint(hex[4:6], 16, 8)
	
	return uint8(r), uint8(g), uint8(b)
}

// processImageWithSampling processes an image with specified sampling method
func processImageWithSampling(img image.Image, width, height int, sampling ColorSampling, verbose bool) ProcessedImage {
	if verbose {
		fmt.Printf("Processing image: %dx%d -> %dx%d, sampling: %v\n", 
			img.Bounds().Dx(), img.Bounds().Dy(), width, height, sampling)
	}

    // Choose resizing strategy: nearest for upscaling, block-mode for downscaling
    var resized image.Image
    if img.Bounds().Dx() > width || img.Bounds().Dy() > height {
        resized = downsampleImageBlockMode(img, width, height)
    } else {
        resized = resizeImage(img, width, height)
    }
	
	pixels := make([][]int, height)
	colorMap := make(map[string]int)
	palette := []string{}
	
	for y := 0; y < height; y++ {
		pixels[y] = make([]int, width)
		for x := 0; x < width; x++ {
			originalColor := resized.At(x, y)
			r, g, b, _ := originalColor.RGBA()
			
			// Convert to 8-bit values
			r8, g8, b8 := uint8(r>>8), uint8(g>>8), uint8(b>>8)
			
			// Apply color sampling strategy
			var finalColor string
			switch sampling {
			case SamplingNearest:
				// Use exact color from original
				finalColor = rgbToHex(r8, g8, b8)
			case SamplingQuantized:
				// Quantize to reduce color variations
				r8 = (r8 / 32) * 32
				g8 = (g8 / 32) * 32
				b8 = (b8 / 32) * 32
				finalColor = rgbToHex(r8, g8, b8)
			case SamplingInterpolated:
				// Allow intermediate colors (smooth gradients)
				finalColor = rgbToHex(r8, g8, b8)
			}
			
			// Add to palette if new
			if _, exists := colorMap[finalColor]; !exists {
				colorMap[finalColor] = len(palette)
				palette = append(palette, finalColor)
			}
			
			pixels[y][x] = colorMap[finalColor]
		}
	}
	
	if verbose {
		fmt.Printf("Generated palette with %d colors\n", len(palette))
	}
	
    return ProcessedImage{
        Width:      width,
        Height:     height,
        Pixels:     pixels,
        Palette:    palette,
        ColorCount: len(palette),
    }
}

// reducePaletteUniform re-quantizes pixels to a uniformly reduced palette ceiling at maxColors.
func reducePaletteUniform(pixels [][]int, palette []string, maxColors int) ([][]int, []string) {
    if maxColors <= 0 || len(palette) <= maxColors {
        return pixels, palette
    }
    // Choose per-channel step so bucket count <= maxColors (try simple steps)
    steps := []int{4, 8, 16, 32, 64}
    chosen := 16
    for _, s := range steps {
        buckets := int(math.Ceil(256.0/float64(s)))
        if buckets*buckets*buckets <= maxColors {
            chosen = s
            break
        }
    }
    colorMap := make(map[string]int)
    newPalette := []string{}
    h := len(pixels)
    w := 0
    if h > 0 {
        w = len(pixels[0])
    }
    newPixels := make([][]int, h)
    for y := 0; y < h; y++ {
        newPixels[y] = make([]int, w)
        for x := 0; x < w; x++ {
            idx := pixels[y][x]
            var hex string
            if idx >= 0 && idx < len(palette) {
                r, g, b := hexToRGB(palette[idx])
                rq := (int(r) / chosen) * chosen
                gq := (int(g) / chosen) * chosen
                bq := (int(b) / chosen) * chosen
                if rq > 255 { rq = 255 }
                if gq > 255 { gq = 255 }
                if bq > 255 { bq = 255 }
                hex = rgbToHex(uint8(rq), uint8(gq), uint8(bq))
            } else {
                hex = "#000000"
            }
            if _, ok := colorMap[hex]; !ok {
                colorMap[hex] = len(newPalette)
                newPalette = append(newPalette, hex)
            }
            newPixels[y][x] = colorMap[hex]
        }
    }
    return newPixels, newPalette
}

// resizeImage resizes an image using nearest neighbor interpolation
func resizeImage(src image.Image, newWidth, newHeight int) image.Image {
	srcBounds := src.Bounds()
	srcWidth := srcBounds.Dx()
	srcHeight := srcBounds.Dy()
	
	dst := image.NewRGBA(image.Rect(0, 0, newWidth, newHeight))
	
	for y := 0; y < newHeight; y++ {
		for x := 0; x < newWidth; x++ {
			// Calculate source coordinates using nearest neighbor
			srcX := (x * srcWidth) / newWidth
			srcY := (y * srcHeight) / newHeight
			
			// Ensure we don't go out of bounds
			if srcX >= srcWidth {
				srcX = srcWidth - 1
			}
			if srcY >= srcHeight {
				srcY = srcHeight - 1
			}
			
			srcColor := src.At(srcBounds.Min.X+srcX, srcBounds.Min.Y+srcY)
			dst.Set(x, y, srcColor)
		}
	}
	
	return dst
}

// downsampleImageBlockMode reduces image size by selecting the dominant color
// within each source region that maps to a destination pixel. This preserves
// crisp pixel-art edges compared to naive nearest-neighbor downsampling.
func downsampleImageBlockMode(src image.Image, newWidth, newHeight int) image.Image {
    srcBounds := src.Bounds()
    srcWidth := srcBounds.Dx()
    srcHeight := srcBounds.Dy()

    dst := image.NewRGBA(image.Rect(0, 0, newWidth, newHeight))

    for y := 0; y < newHeight; y++ {
        sy0 := (y * srcHeight) / newHeight
        sy1 := ((y + 1) * srcHeight) / newHeight
        if sy1 <= sy0 {
            sy1 = sy0 + 1
        }
        for x := 0; x < newWidth; x++ {
            sx0 := (x * srcWidth) / newWidth
            sx1 := ((x + 1) * srcWidth) / newWidth
            if sx1 <= sx0 {
                sx1 = sx0 + 1
            }
            // Count colors in the block and pick the most frequent
            counts := make(map[uint32]int)
            var bestKey uint32
            var bestCount int
            for yy := sy0; yy < sy1; yy++ {
                for xx := sx0; xx < sx1; xx++ {
                    r, g, b, _ := src.At(srcBounds.Min.X+xx, srcBounds.Min.Y+yy).RGBA()
                    r8, g8, b8 := uint8(r>>8), uint8(g>>8), uint8(b>>8)
                    key := uint32(r8)<<16 | uint32(g8)<<8 | uint32(b8)
                    counts[key]++
                    if counts[key] > bestCount {
                        bestCount = counts[key]
                        bestKey = key
                    }
                }
            }
            r8 := uint8((bestKey >> 16) & 0xFF)
            g8 := uint8((bestKey >> 8) & 0xFF)
            b8 := uint8(bestKey & 0xFF)
            dst.Set(x, y, color.RGBA{r8, g8, b8, 255})
        }
    }
    return dst
}

// mseBetween computes mean squared error between two same-sized images (RGB only)
func mseBetween(a, b image.Image) float64 {
    ba := a.Bounds()
    bb := b.Bounds()
    if ba.Dx() != bb.Dx() || ba.Dy() != bb.Dy() {
        return 1e30 // incompatible sizes; treat as very bad
    }
    var sum float64
    var n float64
    for y := 0; y < ba.Dy(); y++ {
        for x := 0; x < ba.Dx(); x++ {
            ar, ag, ab, _ := a.At(ba.Min.X+x, ba.Min.Y+y).RGBA()
            br, bg, bb2, _ := b.At(bb.Min.X+x, bb.Min.Y+y).RGBA()
            dr := float64(int(ar>>8) - int(br>>8))
            dg := float64(int(ag>>8) - int(bg>>8))
            db := float64(int(ab>>8) - int(bb2>>8))
            sum += dr*dr + dg*dg + db*db
            n += 3.0
        }
    }
    if n == 0 {
        return 0
    }
    return sum / n
}

// chooseDownsampleDims tries integer-like reductions near the requested size
// using the first frame to minimize reconstruction error after downsample+nearest.
func chooseDownsampleDims(first image.Image, desiredW, desiredH int, verbose bool) (int, int) {
    sw := first.Bounds().Dx()
    sh := first.Bounds().Dy()
    if desiredW <= 0 || desiredH <= 0 {
        return desiredW, desiredH
    }
    // If not actually downscaling, keep requested
    if sw <= desiredW && sh <= desiredH {
        return desiredW, desiredH
    }

    type cand struct{ w, h int }
    candidates := make([]cand, 0, 32)

    // Always consider requested target
    candidates = append(candidates, cand{desiredW, desiredH})

    // Consider integer factors of original size near requested
    maxF := 64
    if sw < maxF { maxF = sw }
    if sh < maxF { if sh < maxF { maxF = sh } }
    for f := 1; f <= maxF; f++ {
        w := sw / f
        h := sh / f
        if w <= 0 || h <= 0 {
            break
        }
        candidates = append(candidates, cand{w, h})
    }

    // Also add small neighborhood around desired dims
    for dw := -2; dw <= 2; dw++ {
        for dh := -2; dh <= 2; dh++ {
            w := desiredW + dw
            h := desiredH + dh
            if w > 0 && h > 0 {
                candidates = append(candidates, cand{w, h})
            }
        }
    }

    // Evaluate by downsample -> upsample (nearest) MSE, with distance tie-breaker
    bestW, bestH := desiredW, desiredH
    bestScore := 1e30
    for _, c := range candidates {
        // Skip if change is extreme (>50% away from desired)
        if c.w < desiredW/2 || c.h < desiredH/2 || c.w > desiredW*3/2 || c.h > desiredH*3/2 {
            continue
        }
        ds := downsampleImageBlockMode(first, c.w, c.h)
        up := resizeImage(ds, sw, sh)
        score := mseBetween(first, up)
        // Favor closeness to desired size when scores are similar
        dist := float64(absInt(c.w-desiredW) + absInt(c.h-desiredH))
        adjusted := score * (1.0 + 0.01*dist)
        if adjusted < bestScore {
            bestScore = adjusted
            bestW, bestH = c.w, c.h
        }
    }
    if verbose {
        fmt.Printf("Tuned downsample size: requested %dx%d -> chosen %dx%d (first frame)\n", desiredW, desiredH, bestW, bestH)
    }
    return bestW, bestH
}

func absInt(x int) int { if x < 0 { return -x }; return x }

// computeDesiredDims derives target width/height using the original size,
// an optional downscale factor, and aspect-ratio preservation when one side is unspecified.
func computeDesiredDims(origW, origH int, cfg Config) (int, int) {
    if origW <= 0 || origH <= 0 {
        return maxInt(1, cfg.OutputWidth), maxInt(1, cfg.OutputHeight)
    }
    baseW, baseH := origW, origH
    if cfg.DownscaleFactor > 1 {
        baseW = maxInt(1, origW/cfg.DownscaleFactor)
        baseH = maxInt(1, origH/cfg.DownscaleFactor)
    }
    w, h := cfg.OutputWidth, cfg.OutputHeight
    switch {
    case w > 0 && h > 0:
        return w, h
    case w > 0 && h <= 0:
        // preserve aspect based on base dimensions
        newH := int(math.Round(float64(w) * float64(baseH) / float64(baseW)))
        return w, maxInt(1, newH)
    case h > 0 && w <= 0:
        newW := int(math.Round(float64(h) * float64(baseW) / float64(baseH)))
        return maxInt(1, newW), h
    default:
        // both unspecified -> use base dimensions
        return baseW, baseH
    }
}

func maxInt(a, b int) int { if a > b { return a }; return b }

// loadImage loads an image from file
func loadImage(filename string) (image.Image, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()
	
	img, _, err := image.Decode(file)
	return img, err
}

// loadGIF loads a GIF and processes all frames
func loadGIF(filename string, config Config) (*ProcessedGIF, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()
	
	gifImg, err := gif.DecodeAll(file)
	if err != nil {
		return nil, err
	}
	
    if config.Verbose {
        fmt.Printf("Loading GIF: %d frames, %dx%d\n", 
            len(gifImg.Image), gifImg.Config.Width, gifImg.Config.Height)
    }
	
    // Composite frames according to disposal to avoid artifacts
    composited := compositeGIFFrames(gifImg)

    // Determine desired dimensions using original size, downscale factor, and aspect rules
    desiredW, desiredH := config.OutputWidth, config.OutputHeight
    if len(composited) > 0 {
        ow := composited[0].Bounds().Dx()
        oh := composited[0].Bounds().Dy()
        desiredW, desiredH = computeDesiredDims(ow, oh, config)
    }
    // Tune dimensions using first composited frame if downscaling
    tunedW, tunedH := desiredW, desiredH
    if len(composited) > 0 {
        tunedW, tunedH = chooseDownsampleDims(composited[0], desiredW, desiredH, config.Verbose)
    }

    frames := make([]ProcessedImage, len(composited))

    for i, frame := range composited {
        if config.Verbose {
            fmt.Printf("Processing frame %d/%d\n", i+1, len(gifImg.Image))
        }

        processed := processImageWithSampling(frame, tunedW, tunedH, 
            config.ColorSampling, config.Verbose)
        if config.MaxColors > 0 {
            processed.Pixels, processed.Palette = reducePaletteUniform(processed.Pixels, processed.Palette, config.MaxColors)
            processed.ColorCount = len(processed.Palette)
        }
        processed.Filename = fmt.Sprintf("%s_frame_%d", filepath.Base(filename), i+1)
        frames[i] = processed
    }
	
	return &ProcessedGIF{
		Filename:       filepath.Base(filename),
		OriginalWidth:  gifImg.Config.Width,
		OriginalHeight: gifImg.Config.Height,
		IsAnimated:     len(gifImg.Image) > 1,
		FrameCount:     len(gifImg.Image),
		Frames:         frames,
	}, nil
}

// cloneRGBA makes a deep copy of an RGBA image
func cloneRGBA(src *image.RGBA) *image.RGBA {
    dst := image.NewRGBA(src.Bounds())
    copy(dst.Pix, src.Pix)
    return dst
}

// compositeGIFFrames composites paletted frames into full RGBA frames using disposal methods.
func compositeGIFFrames(g *gif.GIF) []image.Image {
    w, h := g.Config.Width, g.Config.Height
    canvas := image.NewRGBA(image.Rect(0, 0, w, h))
    // Background color from global palette if available
    var bg color.Color = color.RGBA{0, 0, 0, 0}
    if pal, ok := g.Config.ColorModel.(color.Palette); ok && int(g.BackgroundIndex) < len(pal) {
        bg = pal[g.BackgroundIndex]
    }
    // Clear canvas
    draw.Draw(canvas, canvas.Bounds(), &image.Uniform{C: bg}, image.Point{}, draw.Src)

    frames := make([]image.Image, 0, len(g.Image))
    var prevBounds image.Rectangle
    var prevBackup *image.RGBA
    var prevDisposal byte

    // Helper to get disposal for frame i, default 0 (None)
    getDisposal := func(i int) byte {
        if g.Disposal != nil && i >= 0 && i < len(g.Disposal) {
            return g.Disposal[i]
        }
        return 0
    }

    for i, pal := range g.Image {
        // Apply disposal for previous frame before drawing current
        if i > 0 {
            switch prevDisposal {
            case 2: // DisposalBackground
                draw.Draw(canvas, prevBounds, &image.Uniform{C: bg}, image.Point{}, draw.Src)
            case 3: // DisposalPrevious
                if prevBackup != nil {
                    draw.Draw(canvas, prevBackup.Bounds(), prevBackup, image.Point{}, draw.Src)
                }
            }
        }

        // Backup before drawing this frame (for DisposalPrevious use in next step)
        prevBackup = cloneRGBA(canvas)
        prevBounds = pal.Bounds()
        prevDisposal = getDisposal(i)

        // Draw current frame onto canvas respecting transparency
        draw.Draw(canvas, pal.Bounds(), pal, pal.Bounds().Min, draw.Over)

        // Store a copy of the composited frame
        frames = append(frames, cloneRGBA(canvas))
    }
    return frames
}

// exportImageToPNG exports the current image to a PNG file
func exportImageToPNG(pixels [][]int, palette []string, filename string, scale int) error {
	if len(pixels) == 0 || len(pixels[0]) == 0 {
		return fmt.Errorf("empty pixel data")
	}
	
	height := len(pixels)
	width := len(pixels[0])
	
	// Create image with scaling
	img := image.NewRGBA(image.Rect(0, 0, width*scale, height*scale))
	
	// Fill the image
	for y := 0; y < height; y++ {
		for x := 0; x < width; x++ {
			colorIndex := pixels[y][x]
			var c color.RGBA
			
			if colorIndex < len(palette) {
				r, g, b := hexToRGB(palette[colorIndex])
				c = color.RGBA{r, g, b, 255}
			} else {
				c = color.RGBA{0, 0, 0, 255}
			}
			
			// Scale the pixel
			for sy := 0; sy < scale; sy++ {
				for sx := 0; sx < scale; sx++ {
					img.Set(x*scale+sx, y*scale+sy, c)
				}
			}
		}
	}
	
	// Save to file
	file, err := os.Create(filename)
	if err != nil {
		return err
	}
	defer file.Close()
	
	return png.Encode(file, img)
}

// Init initializes the model
func (m Model) Init() tea.Cmd {
    if m.animatedGIF != nil && m.animatedGIF.IsAnimated && m.animationView && m.playing {
        return doTick(m.animationSpeed)
    }
    return nil
}

// Update handles messages and updates the model
func (m Model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
    case tea.KeyMsg:
        switch msg.String() {
        case "ctrl+c", "q":
            return m, tea.Quit
        case "a":
            // Toggle animation mode
            if m.animatedGIF != nil && m.animatedGIF.IsAnimated {
                m.animationView = !m.animationView
                if m.animationView {
                    if !m.playing { m.playing = true }
                    return m, doTick(m.animationSpeed)
                }
            }
        case "s":
            // Save current view as PNG
            if m.animationView && m.animatedGIF != nil && m.currentFrame < len(m.animatedGIF.Frames) {
                frame := m.animatedGIF.Frames[m.currentFrame]
                filename := fmt.Sprintf("export_%s_frame_%d.png", 
                    strings.TrimSuffix(m.animatedGIF.Filename, filepath.Ext(m.animatedGIF.Filename)), 
                    m.currentFrame+1)
                err := exportImageToPNG(frame.Pixels, frame.Palette, filename, m.config.ExportScale)
                if err == nil {
                    fmt.Printf("Exported animation frame to %s\n", filename)
                }
            } else if !m.animationView && m.staticImage != nil {
                filename := fmt.Sprintf("export_%s.png", 
                    strings.TrimSuffix(m.staticImage.Filename, filepath.Ext(m.staticImage.Filename)))
                err := exportImageToPNG(m.staticImage.Pixels, m.staticImage.Palette, filename, m.config.ExportScale)
                if err == nil {
                    fmt.Printf("Exported image to %s\n", filename)
                }
            }
        case "=", "+":
            // Speed up animation
            if m.animationSpeed > 50*time.Millisecond {
                m.animationSpeed -= 50 * time.Millisecond
            }
        case "-", "_":
            // Slow down animation
            if m.animationSpeed < 2*time.Second {
                m.animationSpeed += 50 * time.Millisecond
            }
        case " ":
            // Pause/resume playback (in animation view)
            if m.animatedGIF != nil && m.animatedGIF.IsAnimated {
                if !m.animationView {
                    m.animationView = true
                }
                m.playing = !m.playing
                if m.playing {
                    return m, doTick(m.animationSpeed)
                }
            }
        case ",", "<", "left", "h":
            // Previous frame (enter animation view, pause)
            if m.animatedGIF != nil && m.animatedGIF.IsAnimated {
                m.animationView = true
                m.playing = false
                if m.currentFrame > 0 {
                    m.currentFrame--
                } else if m.animatedGIF.FrameCount > 0 {
                    m.currentFrame = m.animatedGIF.FrameCount - 1
                }
            }
        case ".", ">", "right", "l":
            // Next frame (enter animation view, pause)
            if m.animatedGIF != nil && m.animatedGIF.IsAnimated {
                m.animationView = true
                m.playing = false
                if m.animatedGIF.FrameCount > 0 {
                    m.currentFrame = (m.currentFrame + 1) % m.animatedGIF.FrameCount
                }
            }
        }
    case tea.WindowSizeMsg:
        m.width = msg.Width
        m.height = msg.Height
    case tickMsg:
        if m.animationView && m.playing && m.animatedGIF != nil && m.animatedGIF.IsAnimated {
            m.currentFrame = (m.currentFrame + 1) % m.animatedGIF.FrameCount
            return m, doTick(m.animationSpeed)
        }
    }
    return m, nil
}

// renderPixelImage renders a pixel image using lipgloss
func renderPixelImage(pixels [][]int, palette []string) string {
	var result strings.Builder
	
	// Create a style for each color in the palette
	styles := make([]lipgloss.Style, len(palette))
	for i, color := range palette {
		styles[i] = lipgloss.NewStyle().Background(lipgloss.Color(color))
	}
	
	// Render each pixel as a colored block
	for _, row := range pixels {
		for _, colorIndex := range row {
			if colorIndex < len(styles) {
				result.WriteString(styles[colorIndex].Render("  "))
			} else {
				result.WriteString("  ")
			}
		}
		result.WriteString("\n")
	}
	
	return result.String()
}

// computeConnectedBGMask flood-fills from (0,0) and marks only the connected
// region that matches the top-left color as transparent. Prevents removing
// interior pixels of the same color that are not connected to the border area.
func computeConnectedBGMask(pixels [][]int) [][]bool {
    h := len(pixels)
    if h == 0 { return nil }
    w := len(pixels[0])
    if w == 0 { return nil }
    target := pixels[0][0]
    mask := make([][]bool, h)
    for y := 0; y < h; y++ {
        mask[y] = make([]bool, w)
    }
    // BFS
    type pt struct{ x, y int }
    q := make([]pt, 0, h*w/4+1)
    push := func(p pt) { q = append(q, p) }
    pop := func() pt { p := q[0]; q = q[1:]; return p }
    if w > 0 && h > 0 {
        mask[0][0] = true
        push(pt{0, 0})
    }
    dirs := [][2]int{{1,0},{-1,0},{0,1},{0,-1}}
    for len(q) > 0 {
        p := pop()
        for _, d := range dirs {
            nx, ny := p.x+d[0], p.y+d[1]
            if nx < 0 || ny < 0 || nx >= w || ny >= h { continue }
            if mask[ny][nx] { continue }
            if pixels[ny][nx] == target {
                mask[ny][nx] = true
                push(pt{nx, ny})
            }
        }
    }
    return mask
}

// renderPixelImageWithMask renders blocks with a transparency mask.
func renderPixelImageWithMask(pixels [][]int, palette []string, mask [][]bool) string {
    if mask == nil { return renderPixelImage(pixels, palette) }
    var result strings.Builder
    styles := make([]lipgloss.Style, len(palette))
    for i, hex := range palette {
        styles[i] = lipgloss.NewStyle().Background(lipgloss.Color(hex))
    }
    for y, row := range pixels {
        for x, colorIndex := range row {
            if y < len(mask) && x < len(mask[y]) && mask[y][x] {
                result.WriteString("  ")
                continue
            }
            if colorIndex < len(styles) {
                result.WriteString(styles[colorIndex].Render("  "))
            } else {
                result.WriteString("  ")
            }
        }
        result.WriteString("\n")
    }
    return result.String()
}

// renderPixelImageHalfBlock renders using '▀' with FG=top color, BG=bottom color, packing 2 rows per cell.
func renderPixelImageHalfBlock(pixels [][]int, palette []string) string {
    var result strings.Builder
    height := len(pixels)
    if height == 0 {
        return ""
    }
    width := len(pixels[0])
    for y := 0; y < height; y += 2 {
        for x := 0; x < width; x++ {
            topIdx := 0
            bottomIdx := 0
            if x < len(pixels[y]) {
                topIdx = pixels[y][x]
            }
            if y+1 < height && x < len(pixels[y+1]) {
                bottomIdx = pixels[y+1][x]
            } else {
                // if no bottom row, fill with top color for full block appearance
                bottomIdx = topIdx
            }
            top := "#000000"
            bottom := "#000000"
            if topIdx >= 0 && topIdx < len(palette) {
                top = palette[topIdx]
            }
            if bottomIdx >= 0 && bottomIdx < len(palette) {
                bottom = palette[bottomIdx]
            }
            style := lipgloss.NewStyle().Foreground(lipgloss.Color(top)).Background(lipgloss.Color(bottom))
            result.WriteString(style.Render("▀"))
        }
        result.WriteString("\n")
    }
    return result.String()
}

// renderPixelImageHalfBlockWithBG renders half-block mode with bgHex treated as transparent.
func renderPixelImageHalfBlockWithMask(pixels [][]int, palette []string, mask [][]bool) string {
    if mask == nil { return renderPixelImageHalfBlock(pixels, palette) }
    var result strings.Builder
    height := len(pixels)
    if height == 0 { return "" }
    width := len(pixels[0])
    for y := 0; y < height; y += 2 {
        for x := 0; x < width; x++ {
            topIdx := 0
            bottomIdx := 0
            if x < len(pixels[y]) { topIdx = pixels[y][x] }
            if y+1 < height && x < len(pixels[y+1]) { bottomIdx = pixels[y+1][x] } else { bottomIdx = topIdx }
            topTrans := y < len(mask) && x < len(mask[y]) && mask[y][x]
            botTrans := false
            if y+1 < len(mask) && x < len(mask[y+1]) { botTrans = mask[y+1][x] }
            if topTrans && botTrans {
                result.WriteString(" ")
                continue
            }
            top := "#000000"
            bottom := "#000000"
            if topIdx >= 0 && topIdx < len(palette) { top = palette[topIdx] }
            if bottomIdx >= 0 && bottomIdx < len(palette) { bottom = palette[bottomIdx] }
            if topTrans && !botTrans {
                style := lipgloss.NewStyle().Foreground(lipgloss.Color(bottom))
                result.WriteString(style.Render("▄"))
            } else if !topTrans && botTrans {
                style := lipgloss.NewStyle().Foreground(lipgloss.Color(top))
                result.WriteString(style.Render("▀"))
            } else {
                style := lipgloss.NewStyle().Foreground(lipgloss.Color(top)).Background(lipgloss.Color(bottom))
                result.WriteString(style.Render("▀"))
            }
        }
        result.WriteString("\n")
    }
    return result.String()
}

// View renders the current view
func (m Model) View() string {
	// Header style
	headerStyle := lipgloss.NewStyle().
		Bold(true).
		Foreground(lipgloss.Color("#FAFAFA")).
		Background(lipgloss.Color("#7D56F4")).
		Padding(0, 1).
		MarginBottom(1)
	
	// Info style
	infoStyle := lipgloss.NewStyle().
		Foreground(lipgloss.Color("#626262")).
		MarginBottom(1)
	
	// Controls style
	controlsStyle := lipgloss.NewStyle().
		Foreground(lipgloss.Color("#04B575")).
		MarginTop(1)
	
	// Build the view
	var view strings.Builder
	
    if m.animationView && m.animatedGIF != nil {
        // Animation mode
        header := fmt.Sprintf("Pixel Art Viewer - Animation Mode - Frame %d/%d", 
            m.currentFrame+1, m.animatedGIF.FrameCount)
        view.WriteString(headerStyle.Render(header))
        view.WriteString("\n")
		
		// Animation info
		info := fmt.Sprintf("File: %s | Original: %dx%d | Display: %dx%d | Speed: %v | Sampling: %v", 
			m.animatedGIF.Filename, 
			m.animatedGIF.OriginalWidth, 
			m.animatedGIF.OriginalHeight,
			m.animatedGIF.Frames[0].Width,
			m.animatedGIF.Frames[0].Height,
			m.animationSpeed,
			m.config.ColorSampling)
		view.WriteString(infoStyle.Render(info))
		view.WriteString("\n")
		
        // Render current frame
        if m.currentFrame < len(m.animatedGIF.Frames) {
            frame := m.animatedGIF.Frames[m.currentFrame]
            // determine transparent background mask if enabled (connected area from top-left)
            var mask [][]bool
            if m.config.TransparentBG && len(frame.Pixels) > 0 && len(frame.Pixels[0]) > 0 {
                mask = computeConnectedBGMask(frame.Pixels)
            }
            switch m.config.RenderMode {
            case RenderHalf:
                view.WriteString(renderPixelImageHalfBlockWithMask(frame.Pixels, frame.Palette, mask))
            default:
                view.WriteString(renderPixelImageWithMask(frame.Pixels, frame.Palette, mask))
            }
			
			// Frame palette
			view.WriteString(fmt.Sprintf("\nFrame Palette (%d colors):\n", len(frame.Palette)))
			paletteStyle := lipgloss.NewStyle().MarginBottom(1)
			var palette strings.Builder
			for i, color := range frame.Palette {
				if i >= 32 { // Limit palette display
					palette.WriteString("...")
					break
				}
				colorStyle := lipgloss.NewStyle().
					Background(lipgloss.Color(color)).
					Foreground(lipgloss.Color("#000000"))
				if i > 0 && i%8 == 0 {
					palette.WriteString("\n")
				}
				palette.WriteString(colorStyle.Render(fmt.Sprintf(" %02d ", i)))
				palette.WriteString(" ")
			}
			view.WriteString(paletteStyle.Render(palette.String()))
			view.WriteString("\n")
		}
		
        // Animation controls
        controls := "Controls: SPACE (play/pause) | ,/< (prev) | ./> (next) | +/- (speed) | s (save PNG) | a (exit) | q (quit)"
        view.WriteString(controlsStyle.Render(controls))

    } else {
        // Static image mode
        if m.staticImage == nil && m.animatedGIF == nil {
            return "No image loaded.\n"
        }
		
		var img *ProcessedImage
		if m.staticImage != nil {
			img = m.staticImage
		} else if m.animatedGIF != nil && len(m.animatedGIF.Frames) > 0 {
			img = &m.animatedGIF.Frames[0]
		}
		
		if img == nil {
			return "No image data available.\n"
		}
		
		header := "Pixel Art Viewer - Static Image"
		view.WriteString(headerStyle.Render(header))
		view.WriteString("\n")
		
		// Image info
		var originalSize string
		if m.animatedGIF != nil {
			originalSize = fmt.Sprintf("%dx%d", m.animatedGIF.OriginalWidth, m.animatedGIF.OriginalHeight)
		} else {
			originalSize = "Unknown"
		}
		
		info := fmt.Sprintf("File: %s | Original: %s | Display: %dx%d | Colors: %d | Sampling: %v", 
			img.Filename, originalSize, img.Width, img.Height, img.ColorCount, m.config.ColorSampling)
		view.WriteString(infoStyle.Render(info))
		view.WriteString("\n")
		
        // Render the pixel image
        var mask [][]bool
        if m.config.TransparentBG && len(img.Pixels) > 0 && len(img.Pixels[0]) > 0 {
            mask = computeConnectedBGMask(img.Pixels)
        }
        switch m.config.RenderMode {
        case RenderHalf:
            view.WriteString(renderPixelImageHalfBlockWithMask(img.Pixels, img.Palette, mask))
        default:
            view.WriteString(renderPixelImageWithMask(img.Pixels, img.Palette, mask))
        }
		
		// Color palette
		view.WriteString(fmt.Sprintf("\nColor Palette (%d colors):\n", len(img.Palette)))
		paletteStyle := lipgloss.NewStyle().MarginBottom(1)
		var palette strings.Builder
		for i, color := range img.Palette {
			if i >= 32 { // Limit palette display
				palette.WriteString("...")
				break
			}
			colorStyle := lipgloss.NewStyle().
				Background(lipgloss.Color(color)).
				Foreground(lipgloss.Color("#000000"))
			if i > 0 && i%8 == 0 {
				palette.WriteString("\n")
			}
			palette.WriteString(colorStyle.Render(fmt.Sprintf(" %02d ", i)))
			palette.WriteString(" ")
		}
		view.WriteString(paletteStyle.Render(palette.String()))
		view.WriteString("\n")
		
		// Controls
        animText := ""
        if m.animatedGIF != nil && m.animatedGIF.IsAnimated {
            animText = " | a (animation) | ./> (next frame) | ,/< (prev frame)"
        }
        controls := fmt.Sprintf("Controls: s (save PNG)%s | q (quit)", animText)
        view.WriteString(controlsStyle.Render(controls))
    }
	
	return view.String()
}

func main() {
    // Parse command line arguments
    var config Config
    var colorSamplingStr string
    var renderModeStr string
    var speedMS int
	
	flag.StringVar(&config.InputFile, "input", "", "Input image file (PNG or GIF)")
	flag.StringVar(&config.InputFile, "i", "", "Input image file (PNG or GIF) (shorthand)")
    flag.IntVar(&config.OutputWidth, "width", 0, "Output width in pixels (0=auto)")
    flag.IntVar(&config.OutputWidth, "w", 0, "Output width in pixels (shorthand, 0=auto)")
    flag.IntVar(&config.OutputHeight, "height", 0, "Output height in pixels (0=auto)")
    flag.IntVar(&config.OutputHeight, "h", 0, "Output height in pixels (shorthand, 0=auto)")
	flag.StringVar(&colorSamplingStr, "sampling", "nearest", "Color sampling method: nearest, quantized, interpolated")
    flag.StringVar(&colorSamplingStr, "s", "nearest", "Color sampling method (shorthand)")
    flag.StringVar(&renderModeStr, "render", "block", "Render mode: block, half")
    flag.StringVar(&renderModeStr, "r", "block", "Render mode (shorthand): block, half")
    flag.IntVar(&config.MaxColors, "maxcolors", 0, "Reduce to at most this many colors (0=off)")
    flag.IntVar(&config.DownscaleFactor, "downscale", 1, "Downscale original by factor before sizing (>=1)")
    flag.IntVar(&speedMS, "speed", 200, "Animation speed in milliseconds per frame")
    flag.BoolVar(&config.StepMode, "step", false, "Start in animation view paused for manual frame stepping (GIF only)")
    flag.BoolVar(&config.TransparentBG, "transparent", false, "Treat top-left color as transparent background in TUI")
	flag.IntVar(&config.ExportScale, "scale", 10, "Export PNG scale factor")
	flag.BoolVar(&config.Verbose, "verbose", false, "Verbose output")
	flag.BoolVar(&config.Verbose, "v", false, "Verbose output (shorthand)")
	
	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "Pixel Art TUI Renderer - CLI Version\n\n")
		fmt.Fprintf(os.Stderr, "Usage: %s [options]\n\n", os.Args[0])
		fmt.Fprintf(os.Stderr, "Options:\n")
		flag.PrintDefaults()
		fmt.Fprintf(os.Stderr, "\nColor Sampling Methods:\n")
		fmt.Fprintf(os.Stderr, "  nearest      - Use exact colors from original image (default)\n")
		fmt.Fprintf(os.Stderr, "  quantized    - Reduce color variations by quantization\n")
		fmt.Fprintf(os.Stderr, "  interpolated - Allow intermediate colors for smooth gradients\n")
        fmt.Fprintf(os.Stderr, "\nExamples:\n")
        fmt.Fprintf(os.Stderr, "  %s -i image.png -w 64 -h 48 -s nearest\n", os.Args[0])
        fmt.Fprintf(os.Stderr, "  %s --input animation.gif --width 32 --sampling quantized --render half --maxcolors 64 --speed 120\n", os.Args[0])
        }
	
	flag.Parse()
	
    // Validate arguments
    if config.InputFile == "" {
        fmt.Fprintf(os.Stderr, "Error: Input file is required\n\n")
        flag.Usage()
        os.Exit(1)
    }
    if config.DownscaleFactor < 1 { config.DownscaleFactor = 1 }
	
    config.ColorSampling = parseColorSampling(colorSamplingStr)
    config.RenderMode = parseRenderMode(renderModeStr)
    if speedMS <= 0 { speedMS = 200 }
    config.SpeedMS = speedMS
	
	if config.Verbose {
		fmt.Printf("Configuration:\n")
		fmt.Printf("  Input: %s\n", config.InputFile)
		fmt.Printf("  Output size: %dx%d\n", config.OutputWidth, config.OutputHeight)
		fmt.Printf("  Color sampling: %v\n", config.ColorSampling)
        fmt.Printf("  Export scale: %dx\n", config.ExportScale)
        fmt.Printf("  Render mode: %v\n", config.RenderMode)
        if config.MaxColors > 0 { fmt.Printf("  Max colors: %d\n", config.MaxColors) }
        fmt.Printf("  Downscale factor: %d\n", config.DownscaleFactor)
        fmt.Printf("  Speed: %dms/frame\n", config.SpeedMS)
    }
	
	// Check if file exists
	if _, err := os.Stat(config.InputFile); os.IsNotExist(err) {
		log.Fatalf("Error: File '%s' does not exist", config.InputFile)
	}
	
	// Determine file type and process accordingly
	ext := strings.ToLower(filepath.Ext(config.InputFile))
	
    var model Model
    model.config = config
    model.animationSpeed = time.Duration(config.SpeedMS) * time.Millisecond
    model.animationView = false
    model.playing = false
	
	switch ext {
case ".gif":
		if config.Verbose {
			fmt.Println("Processing GIF file...")
		}
		
        processedGIF, err := loadGIF(config.InputFile, config)
        if err != nil {
            log.Fatalf("Error processing GIF: %v", err)
        }

        model.animatedGIF = processedGIF
        // Initialize viewing mode for GIFs
        if config.StepMode {
            model.animationView = true
            model.playing = false
        } else {
            model.animationView = false
            model.playing = false
        }
		
		if config.Verbose {
            fmt.Printf("Loaded GIF: %d frames, %dx%d -> %dx%d\n", 
                processedGIF.FrameCount, 
                processedGIF.OriginalWidth, processedGIF.OriginalHeight,
                config.OutputWidth, config.OutputHeight)
        }
		
case ".png", ".jpg", ".jpeg":
		if config.Verbose {
			fmt.Println("Processing static image...")
		}
		
		img, err := loadImage(config.InputFile)
		if err != nil {
			log.Fatalf("Error loading image: %v", err)
		}
		
        // Determine desired dimensions from original, downscale, and aspect rules
        ow := img.Bounds().Dx()
        oh := img.Bounds().Dy()
        dw, dh := computeDesiredDims(ow, oh, model.config)
        processed := processImageWithSampling(img, dw, dh, 
            config.ColorSampling, config.Verbose)
        if config.MaxColors > 0 {
            processed.Pixels, processed.Palette = reducePaletteUniform(processed.Pixels, processed.Palette, config.MaxColors)
            processed.ColorCount = len(processed.Palette)
        }
		processed.Filename = filepath.Base(config.InputFile)
		
		model.staticImage = &processed
		
		if config.Verbose {
			fmt.Printf("Loaded image: %dx%d -> %dx%d, %d colors\n", 
				img.Bounds().Dx(), img.Bounds().Dy(),
				processed.Width, processed.Height, processed.ColorCount)
		}
		
	default:
		log.Fatalf("Error: Unsupported file format '%s'. Supported formats: PNG, JPG, GIF", ext)
	}
	
	// Start the TUI
	if config.Verbose {
		fmt.Println("Starting TUI...")
	}
	
	p := tea.NewProgram(model, tea.WithAltScreen())
	if _, err := p.Run(); err != nil {
		fmt.Printf("Error running program: %v", err)
		os.Exit(1)
	}
}
