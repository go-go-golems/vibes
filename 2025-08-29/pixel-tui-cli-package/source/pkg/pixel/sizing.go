package pixel

import (
    "image"
    "math"
)

// ComputeDesiredDims derives target width/height using the original size,
// an optional downscale factor, and aspect-ratio preservation when one side is unspecified.
func ComputeDesiredDims(origW, origH int, outW, outH, downscale int) (int, int) {
    if origW <= 0 || origH <= 0 { return maxInt(1, outW), maxInt(1, outH) }
    baseW, baseH := origW, origH
    if downscale < 1 { downscale = 1 }
    if downscale > 1 {
        baseW = maxInt(1, origW/downscale)
        baseH = maxInt(1, origH/downscale)
    }
    switch {
    case outW > 0 && outH > 0:
        return outW, outH
    case outW > 0 && outH <= 0:
        nh := int(math.Round(float64(outW) * float64(baseH) / float64(baseW)))
        return outW, maxInt(1, nh)
    case outH > 0 && outW <= 0:
        nw := int(math.Round(float64(outH) * float64(baseW) / float64(baseH)))
        return maxInt(1, nw), outH
    default:
        return baseW, baseH
    }
}

// ChooseDownsampleDims tries integer-like reductions near the requested size
// using the first frame to minimize reconstruction error after downsample+nearest.
func ChooseDownsampleDims(first image.Image, desiredW, desiredH int) (int, int) {
    sw := first.Bounds().Dx()
    sh := first.Bounds().Dy()
    if desiredW <= 0 || desiredH <= 0 { return desiredW, desiredH }
    if sw <= desiredW && sh <= desiredH { return desiredW, desiredH }
    type cand struct{ w, h int }
    candidates := make([]cand, 0, 64)
    candidates = append(candidates, cand{desiredW, desiredH})
    maxF := 64
    if sw < maxF { maxF = sw }
    if sh < maxF { if sh < maxF { maxF = sh } }
    for f := 1; f <= maxF; f++ {
        w := sw / f
        h := sh / f
        if w <= 0 || h <= 0 { break }
        candidates = append(candidates, cand{w, h})
    }
    for dw := -2; dw <= 2; dw++ {
        for dh := -2; dh <= 2; dh++ {
            w := desiredW + dw
            h := desiredH + dh
            if w > 0 && h > 0 { candidates = append(candidates, cand{w, h}) }
        }
    }
    bestW, bestH := desiredW, desiredH
    bestScore := 1e30
    for _, c := range candidates {
        if c.w < desiredW/2 || c.h < desiredH/2 || c.w > desiredW*3/2 || c.h > desiredH*3/2 { continue }
        ds := DownsampleImageBlockMode(first, c.w, c.h)
        up := ResizeNearest(ds, sw, sh)
        score := MSEBetween(first, up)
        dist := float64(absInt(c.w-desiredW) + absInt(c.h-desiredH))
        adjusted := score * (1.0 + 0.01*dist)
        if adjusted < bestScore {
            bestScore = adjusted
            bestW, bestH = c.w, c.h
        }
    }
    return bestW, bestH
}

func absInt(x int) int { if x < 0 { return -x }; return x }
func maxInt(a, b int) int { if a > b { return a }; return b }

