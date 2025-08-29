package pixel

import (
    "image"
    "image/color"
)

// ResizeNearest resizes an image using nearest neighbor interpolation.
func ResizeNearest(src image.Image, newWidth, newHeight int) image.Image {
    srcBounds := src.Bounds()
    srcWidth := srcBounds.Dx()
    srcHeight := srcBounds.Dy()

    dst := image.NewRGBA(image.Rect(0, 0, newWidth, newHeight))

    for y := 0; y < newHeight; y++ {
        for x := 0; x < newWidth; x++ {
            sx := (x * srcWidth) / newWidth
            sy := (y * srcHeight) / newHeight
            if sx >= srcWidth { sx = srcWidth - 1 }
            if sy >= srcHeight { sy = srcHeight - 1 }
            dst.Set(x, y, src.At(srcBounds.Min.X+sx, srcBounds.Min.Y+sy))
        }
    }
    return dst
}

// DownsampleImageBlockMode reduces image size by selecting the dominant color
// within each source region that maps to a destination pixel. This preserves
// crisp pixel-art edges compared to naive nearest-neighbor downsampling.
func DownsampleImageBlockMode(src image.Image, newWidth, newHeight int) image.Image {
    srcBounds := src.Bounds()
    srcWidth := srcBounds.Dx()
    srcHeight := srcBounds.Dy()

    dst := image.NewRGBA(image.Rect(0, 0, newWidth, newHeight))

    for y := 0; y < newHeight; y++ {
        sy0 := (y * srcHeight) / newHeight
        sy1 := ((y + 1) * srcHeight) / newHeight
        if sy1 <= sy0 { sy1 = sy0 + 1 }
        for x := 0; x < newWidth; x++ {
            sx0 := (x * srcWidth) / newWidth
            sx1 := ((x + 1) * srcWidth) / newWidth
            if sx1 <= sx0 { sx1 = sx0 + 1 }
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

// MSEBetween computes mean squared error between two same-sized images (RGB only).
func MSEBetween(a, b image.Image) float64 {
    ba := a.Bounds()
    bb := b.Bounds()
    if ba.Dx() != bb.Dx() || ba.Dy() != bb.Dy() {
        return 1e30
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
    if n == 0 { return 0 }
    return sum / n
}

// RGBToHex converts RGB values to hex string.
func RGBToHex(r, g, b uint8) string {
    return fmtHex(r, g, b)
}

// HexToRGB converts hex color string to RGB values.
func HexToRGB(hex string) (uint8, uint8, uint8) {
    return parseHex(hex)
}

// internal helpers reused by package
func fmtHex(r, g, b uint8) string { return formatHex(r, g, b) }
func parseHex(hex string) (uint8, uint8, uint8) {
    // simple reimplementation to avoid external deps
    if len(hex) == 0 { return 0,0,0 }
    if hex[0] == '#' { hex = hex[1:] }
    if len(hex) != 6 { return 0,0,0 }
    var toU8 = func(b byte) int {
        switch {
        case b >= '0' && b <= '9': return int(b - '0')
        case b >= 'a' && b <= 'f': return int(b - 'a' + 10)
        case b >= 'A' && b <= 'F': return int(b - 'A' + 10)
        default: return 0
        }
    }
    h := func(i int) uint8 { return uint8(toU8(hex[i])<<4 | toU8(hex[i+1])) }
    return h(0), h(2), h(4)
}

func formatHex(r, g, b uint8) string {
    const hexdigits = "0123456789abcdef"
    out := [7]byte{'#', 0, 0, 0, 0, 0, 0}
    vals := []uint8{r, g, b}
    for i, v := range vals {
        out[1+2*i] = hexdigits[v>>4]
        out[2+2*i] = hexdigits[v&0x0F]
    }
    return string(out[:])
}

