package pixel

// ComputeConnectedBGMask flood-fills from (0,0) and marks only the connected
// region that matches the top-left color as transparent. Prevents removing
// interior pixels of the same color that are not connected to the border area.
func ComputeConnectedBGMask(pixels [][]int) [][]bool {
    h := len(pixels)
    if h == 0 { return nil }
    w := len(pixels[0])
    if w == 0 { return nil }
    target := pixels[0][0]
    mask := make([][]bool, h)
    for y := 0; y < h; y++ { mask[y] = make([]bool, w) }
    type pt struct{ x, y int }
    q := make([]pt, 0, h*w/4+1)
    push := func(p pt) { q = append(q, p) }
    pop := func() pt { p := q[0]; q = q[1:]; return p }
    // Seed from all border pixels that match the target color
    for x := 0; x < w; x++ {
        if pixels[0][x] == target && !mask[0][x] { mask[0][x] = true; push(pt{x, 0}) }
        if pixels[h-1][x] == target && !mask[h-1][x] { mask[h-1][x] = true; push(pt{x, h - 1}) }
    }
    for y := 1; y < h-1; y++ {
        if pixels[y][0] == target && !mask[y][0] { mask[y][0] = true; push(pt{0, y}) }
        if pixels[y][w-1] == target && !mask[y][w-1] { mask[y][w-1] = true; push(pt{w - 1, y}) }
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
