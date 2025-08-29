package pixel

import (
    "image"
    "image/color"
    "image/draw"
    "image/gif"
)

// CompositeGIFFrames composites paletted frames into full RGBA frames using disposal methods.
func CompositeGIFFrames(g *gif.GIF) []image.Image {
    w, h := g.Config.Width, g.Config.Height
    canvas := image.NewRGBA(image.Rect(0, 0, w, h))
    var bg color.Color = color.RGBA{0, 0, 0, 0}
    if pal, ok := g.Config.ColorModel.(color.Palette); ok && int(g.BackgroundIndex) < len(pal) {
        bg = pal[g.BackgroundIndex]
    }
    draw.Draw(canvas, canvas.Bounds(), &image.Uniform{C: bg}, image.Point{}, draw.Src)

    frames := make([]image.Image, 0, len(g.Image))
    var prevBounds image.Rectangle
    var prevBackup *image.RGBA
    var prevDisposal byte
    getDisposal := func(i int) byte {
        if g.Disposal != nil && i >= 0 && i < len(g.Disposal) {
            return g.Disposal[i]
        }
        return 0
    }
    for i, pal := range g.Image {
        if i > 0 {
            switch prevDisposal {
            case 2:
                draw.Draw(canvas, prevBounds, &image.Uniform{C: bg}, image.Point{}, draw.Src)
            case 3:
                if prevBackup != nil {
                    draw.Draw(canvas, prevBackup.Bounds(), prevBackup, image.Point{}, draw.Src)
                }
            }
        }
        prevBackup = cloneRGBA(canvas)
        prevBounds = pal.Bounds()
        prevDisposal = getDisposal(i)
        draw.Draw(canvas, pal.Bounds(), pal, pal.Bounds().Min, draw.Over)
        frames = append(frames, cloneRGBA(canvas))
    }
    return frames
}

func cloneRGBA(src *image.RGBA) *image.RGBA {
    dst := image.NewRGBA(src.Bounds())
    copy(dst.Pix, src.Pix)
    return dst
}

