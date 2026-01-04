package pixel

import (
    "math"
)

// ReducePaletteUniform re-quantizes pixels to a uniformly reduced palette ceiling at maxColors.
func ReducePaletteUniform(pixels [][]int, palette []string, maxColors int) ([][]int, []string) {
    if maxColors <= 0 || len(palette) <= maxColors { return pixels, palette }
    steps := []int{4, 8, 16, 32, 64}
    chosen := 16
    for _, s := range steps {
        buckets := int(math.Ceil(256.0/float64(s)))
        if buckets*buckets*buckets <= maxColors { chosen = s; break }
    }
    colorMap := make(map[string]int)
    newPalette := []string{}
    h := len(pixels)
    w := 0
    if h > 0 { w = len(pixels[0]) }
    newPixels := make([][]int, h)
    for y := 0; y < h; y++ {
        newPixels[y] = make([]int, w)
        for x := 0; x < w; x++ {
            idx := pixels[y][x]
            var hex string
            if idx >= 0 && idx < len(palette) {
                r, g, b := HexToRGB(palette[idx])
                rq := (int(r) / chosen) * chosen
                gq := (int(g) / chosen) * chosen
                bq := (int(b) / chosen) * chosen
                if rq > 255 { rq = 255 }
                if gq > 255 { gq = 255 }
                if bq > 255 { bq = 255 }
                hex = RGBToHex(uint8(rq), uint8(gq), uint8(bq))
            } else { hex = "#000000" }
            if _, ok := colorMap[hex]; !ok {
                colorMap[hex] = len(newPalette)
                newPalette = append(newPalette, hex)
            }
            newPixels[y][x] = colorMap[hex]
        }
    }
    return newPixels, newPalette
}

