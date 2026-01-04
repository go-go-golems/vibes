package sprites

import (
	"cyberpunk-tui/animations"
	"cyberpunk-tui/models"
	"cyberpunk-tui/styles"
	"math/rand"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss/v2"
)

// SpriteSystem manages all floating sprites
type SpriteSystem struct {
	models.BaseModel
	matrixRain *MatrixRain
	creatures  []*FloatingCreature
	maxCreatures int
	spawnTimer   time.Time
}

// NewSpriteSystem creates a new sprite system
func NewSpriteSystem(width, height int) *SpriteSystem {
	return &SpriteSystem{
		BaseModel:    models.NewBaseModel(width, height),
		matrixRain:   NewMatrixRain(width, height),
		creatures:    make([]*FloatingCreature, 0),
		maxCreatures: 8,
		spawnTimer:   time.Now().Add(time.Duration(rand.Intn(5000)+3000) * time.Millisecond),
	}
}

// Init implements tea.Model
func (m *SpriteSystem) Init() tea.Cmd {
	return nil
}

// Update implements tea.Model
func (m *SpriteSystem) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case models.ResizeMsg:
		m.Resize(msg.Width, msg.Height)
		m.matrixRain.Resize(msg.Width, msg.Height)
	}
	return m, nil
}

// View implements tea.Model
func (m *SpriteSystem) View() string {
	// This system renders as overlay, so it returns empty string
	// The actual rendering is done by GetOverlay()
	return ""
}

// GetOverlay returns the sprite overlay for the main view
func (m *SpriteSystem) GetOverlay() [][]string {
	overlay := make([][]string, m.Height)
	for i := range overlay {
		overlay[i] = make([]string, m.Width)
	}
	
	// Render matrix rain
	matrixDrops := m.matrixRain.GetDrops()
	for _, drop := range matrixDrops {
		if drop.Y >= 0 && drop.Y < m.Height && drop.X >= 0 && drop.X < m.Width {
			overlay[drop.Y][drop.X] = styles.MatrixGreenStyle.Render(string(drop.Char))
		}
	}
	
	// Render creatures
	for _, creature := range m.creatures {
		creatureSprite := creature.GetSprite()
		for dy, row := range creatureSprite {
			for dx, char := range row {
				y := creature.Y + dy
				x := creature.X + dx
				if y >= 0 && y < m.Height && x >= 0 && x < m.Width && char != "" {
					overlay[y][x] = char
				}
			}
		}
	}
	
	return overlay
}

// UpdateAnimation implements Animatable
func (m *SpriteSystem) UpdateAnimation(clock *animations.AnimationClock) {
	m.matrixRain.UpdateAnimation(clock)
	
	// Update existing creatures
	for i := len(m.creatures) - 1; i >= 0; i-- {
		creature := m.creatures[i]
		creature.UpdateAnimation(clock)
		
		// Remove creatures that are off-screen or expired
		if creature.ShouldRemove(m.Width, m.Height) {
			m.creatures = append(m.creatures[:i], m.creatures[i+1:]...)
		}
	}
	
	// Spawn new creatures
	if time.Now().After(m.spawnTimer) && len(m.creatures) < m.maxCreatures {
		m.spawnCreature()
		m.spawnTimer = time.Now().Add(time.Duration(rand.Intn(7000)+3000) * time.Millisecond)
	}
}

// spawnCreature spawns a new floating creature
func (m *SpriteSystem) spawnCreature() {
	creatureTypes := []string{"skull", "ghost", "lightning"}
	creatureType := creatureTypes[rand.Intn(len(creatureTypes))]
	
	// Random spawn position (avoid UI areas)
	x := rand.Intn(m.Width - 16) + 8  // Avoid sidebars
	y := rand.Intn(m.Height - 8) + 4  // Avoid top/bottom bars
	
	creature := NewFloatingCreature(creatureType, x, y)
	m.creatures = append(m.creatures, creature)
}

// MatrixRain manages the matrix rain effect
type MatrixRain struct {
	width  int
	height int
	drops  []*MatrixDrop
}

// MatrixDrop represents a single matrix rain drop
type MatrixDrop struct {
	X     int
	Y     int
	Char  rune
	Speed int
	Life  int
}

// NewMatrixRain creates a new matrix rain system
func NewMatrixRain(width, height int) *MatrixRain {
	return &MatrixRain{
		width:  width,
		height: height,
		drops:  make([]*MatrixDrop, 0),
	}
}

// Resize updates the matrix rain dimensions
func (mr *MatrixRain) Resize(width, height int) {
	mr.width = width
	mr.height = height
}

// UpdateAnimation updates the matrix rain
func (mr *MatrixRain) UpdateAnimation(clock *animations.AnimationClock) {
	// Spawn new drops (5% chance per column per tick)
	for col := 0; col < mr.width; col++ {
		if rand.Float64() < 0.05 {
			char := styles.GlitchChars[rand.Intn(len(styles.GlitchChars))]
			drop := &MatrixDrop{
				X:     col,
				Y:     -1,
				Char:  []rune(char)[0],
				Speed: rand.Intn(4) + 1, // 1-4 rows per tick
				Life:  mr.height + 5,
			}
			mr.drops = append(mr.drops, drop)
		}
	}
	
	// Update existing drops
	for i := len(mr.drops) - 1; i >= 0; i-- {
		drop := mr.drops[i]
		drop.Y += drop.Speed
		drop.Life--
		
		// Remove drops that are off-screen or expired
		if drop.Y > mr.height+5 || drop.Life <= 0 {
			mr.drops = append(mr.drops[:i], mr.drops[i+1:]...)
		}
	}
}

// GetDrops returns the current matrix drops
func (mr *MatrixRain) GetDrops() []*MatrixDrop {
	return mr.drops
}

// FloatingCreature represents a floating creature sprite
type FloatingCreature struct {
	Type       string
	X, Y       int
	frames     [][][]string
	frameState *animations.AnimationState
	moveState  *animations.AnimationState
	life       int
	maxLife    int
}

// NewFloatingCreature creates a new floating creature
func NewFloatingCreature(creatureType string, x, y int) *FloatingCreature {
	creature := &FloatingCreature{
		Type:    creatureType,
		X:       x,
		Y:       y,
		life:    300, // 30 seconds at 10 FPS
		maxLife: 300,
	}
	
	switch creatureType {
	case "skull":
		creature.frames = [][][]string{
			{
				{"░░▓▓▓▓░░", "░▓████▓░", "▓██▓▓██▓", "▓██░░██▓", "▓██▓▓██▓", "░▓▓▓▓▓▓░", "░░▓░░▓░░", "░░░░░░░░"},
				{"░░▓▓▓▓░░", "░▓████▓░", "▓██░░██▓", "▓██▓▓██▓", "▓██▓▓██▓", "░▓▓▓▓▓▓░", "░░▓░░▓░░", "░░░░░░░░"},
			},
		}
		creature.frameState = animations.NewAnimationState(2, 30, 0) // 3 second blink
		creature.moveState = animations.NewAnimationState(2, 20, 0)  // 2 second bounce
		
	case "ghost":
		creature.frames = [][][]string{
			{
				{"░░▓▓▓▓░░", "░▓████▓░", "▓██▓▓██▓", "▓██░░██▓", "▓██████▓", "▓██████▓", "▓▓▓░▓▓▓▓", "░░░░░░░░"},
				{"░░▓▓▓▓░░", "░▓░██░▓░", "▓██▓▓██▓", "▓██░░██▓", "▓██████▓", "▓██████▓", "▓▓▓▓░▓▓▓", "░░░░░░░░"},
			},
		}
		creature.frameState = animations.NewAnimationState(2, 20, 0) // 2 second phase
		creature.moveState = animations.NewAnimationState(4, 30, 0)  // 3 second drift
		
	case "lightning":
		creature.frames = [][][]string{
			{
				{"░░░▓░░░░", "░░▓█▓░░░", "░▓███▓░░", "▓█████▓░", "░▓███▓░░", "░░▓█▓░░░", "░░░▓░░░░", "░░░░░░░░"},
				{"░░▓░░░░░", "░▓█▓░░░░", "▓███▓░░░", "█████▓░░", "▓███▓░░░", "░▓█▓░░░░", "░░▓░░░░░", "░░░░░░░░"},
			},
		}
		creature.frameState = animations.NewAnimationState(2, 2, 0) // 200ms strike
		creature.moveState = animations.NewAnimationState(1, 1, 0)  // No movement
	}
	
	return creature
}

// UpdateAnimation updates the creature animation
func (fc *FloatingCreature) UpdateAnimation(clock *animations.AnimationClock) {
	fc.frameState.Update(clock)
	fc.moveState.Update(clock)
	fc.life--
	
	// Apply movement based on type
	switch fc.Type {
	case "skull":
		// Bounce effect
		if fc.moveState.GetFrame() == 1 {
			fc.Y-- // Move up slightly
		} else {
			fc.Y++ // Move down slightly
		}
		
	case "ghost":
		// Horizontal drift
		movePattern := []int{-2, -1, 0, 1, 2, 1, 0, -1}
		fc.X += movePattern[fc.moveState.GetFrame()%len(movePattern)]
		
	case "lightning":
		// No movement, just flicker
	}
}

// GetSprite returns the current sprite frame
func (fc *FloatingCreature) GetSprite() [][]string {
	if len(fc.frames) == 0 {
		return [][]string{}
	}
	
	frameSet := fc.frames[0]
	frame := frameSet[fc.frameState.GetFrame()]
	
	// Apply styling based on type
	styledFrame := make([][]string, len(frame))
	for i, row := range frame {
		styledFrame[i] = make([]string, len(row))
		for j, char := range row {
			if char != '░' && char != 0 {
				switch fc.Type {
				case "skull":
					styledFrame[i][j] = styles.HotPinkStyle.Render(string(char))
				case "ghost":
					styledFrame[i][j] = styles.DeepPurpleStyle.Render(string(char))
				case "lightning":
					styledFrame[i][j] = lipgloss.NewStyle().Foreground(lipgloss.Color("#FFFF00")).Render(string(char))
				}
			}
		}
	}
	
	return styledFrame
}

// ShouldRemove returns whether the creature should be removed
func (fc *FloatingCreature) ShouldRemove(screenWidth, screenHeight int) bool {
	return fc.life <= 0 || fc.X < -8 || fc.X > screenWidth+8 || fc.Y < -8 || fc.Y > screenHeight+8
}

