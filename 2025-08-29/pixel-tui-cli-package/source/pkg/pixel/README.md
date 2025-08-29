# Pixel Library — Implementation & Usage Guide

A small, dependency‑light Go library for pixel‑art‑friendly image processing and GIF handling. It extracts the core algorithms used by the CLI into reusable functions.

## Import

- Local (inside this repo’s module):
  - `import "pixel-tui/pkg/pixel"`
- Go version: Go 1.23+ (matching this module’s `go.mod`).

## Key Concepts

- Block‑mode downsampling: preserves crisp edges by choosing the dominant color in each source block instead of averaging.
- GIF compositing: produces full frames using disposal methods to avoid artifacts.
- Size planning: compute target dimensions from desired width/height and a downscale factor, and optionally tune to “clean” sizes.
- Palette utilities: lightweight hex helpers and uniform palette reduction for TUI‑friendly output.
- Connected transparency mask: flood‑fills from the top‑left color so only the background region becomes transparent.

## API Overview

- Resizing/Downsampling
  - `ResizeNearest(src image.Image, w, h int) image.Image`
  - `DownsampleImageBlockMode(src image.Image, w, h int) image.Image`
- GIF
  - `CompositeGIFFrames(g *gif.GIF) []image.Image`
- Sizing
  - `ComputeDesiredDims(origW, origH, outW, outH, downscale int) (int, int)`
  - `ChooseDownsampleDims(first image.Image, desiredW, desiredH int) (int, int)`
- Palette/Color
  - `ReducePaletteUniform(pixels [][]int, palette []string, maxColors int) ([][]int, []string)`
  - `RGBToHex(r, g, b uint8) string`, `HexToRGB(hex string) (uint8, uint8, uint8)`
- Metrics/Mask
  - `MSEBetween(a, b image.Image) float64`
  - `ComputeConnectedBGMask(pixels [][]int) [][]bool`

## Usage Examples

### 1) Composite GIF frames
```go
f, _ := os.Open("anim.gif")
defer f.Close()
g, _ := gif.DecodeAll(f)
frames := pixel.CompositeGIFFrames(g) // []image.Image, RGBA full frames
```

### 2) Plan output size
```go
origW, origH := frames[0].Bounds().Dx(), frames[0].Bounds().Dy()
// Keep aspect if one side is zero; pre‑shrink by factor 2.
outW, outH := pixel.ComputeDesiredDims(origW, origH, 64, 0, 2)
// Optionally tune to divisor‑like size that upscales cleanly
outW, outH = pixel.ChooseDownsampleDims(frames[0], outW, outH)
```

### 3) Downsample or resize
```go
var out image.Image
if origW > outW || origH > outH {
    out = pixel.DownsampleImageBlockMode(frames[0], outW, outH) // crisp downscale
} else {
    out = pixel.ResizeNearest(frames[0], outW, outH) // nearest for upscaling
}
```

### 4) Build a palette + pixel grid
```go
w, h := out.Bounds().Dx(), out.Bounds().Dy()
pixels := make([][]int, h)
colorMap := map[string]int{}
palette := []string{}
for y := 0; y < h; y++ {
    pixels[y] = make([]int, w)
    for x := 0; x < w; x++ {
        r, g, b, _ := out.At(out.Bounds().Min.X+x, out.Bounds().Min.Y+y).RGBA()
        hex := pixel.RGBToHex(uint8(r>>8), uint8(g>>8), uint8(b>>8))
        idx, ok := colorMap[hex]
        if !ok {
            idx = len(palette)
            colorMap[hex] = idx
            palette = append(palette, hex)
        }
        pixels[y][x] = idx
    }
}
```

### 5) Reduce palette (optional)
```go
pixels, palette = pixel.ReducePaletteUniform(pixels, palette, 64)
```

### 6) Connected transparency mask from top‑left
```go
mask := pixel.ComputeConnectedBGMask(pixels)
// In your renderer, treat mask[y][x] == true as transparent.
```

## Implementation Notes

- Block downsampling iterates per destination pixel over its source block and picks the most frequent RGB triplet — ideal for pixel art and sprites.
- GIF compositing honors disposal modes 0 (None), 2 (Background), 3 (Previous) and global background color.
- Tuning evaluates candidate sizes via MSE after downsample→nearest upsample, biasing toward values close to your target.
- Palette reduction uses uniform per‑channel quantization (4/8/16/32/64 steps) to cap total colors quickly without external dependencies.
- The mask is computed on the processed pixel grid (post‑resize), which aligns with how you render in a terminal.

## Tips

- For TUI subpixel (half‑block) rendering, pair this library with a renderer that maps two rows per terminal cell and uses foreground/background colors accordingly.
- For very small originals, prefer `ChooseDownsampleDims` to avoid jittery resizing.
- Keep `maxColors` modest (e.g., 32–128) for consistent terminal output.

## License & Contributions

This library is part of the CLI module. PRs welcome to extend quantization strategies, connectivity modes, or add tests. Place tests under `source/pkg/pixel` with Go’s `testing` package.
