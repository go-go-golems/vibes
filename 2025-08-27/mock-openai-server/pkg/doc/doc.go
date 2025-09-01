package doc

import (
    "embed"
    "encoding/json"
    "io/fs"
    "net/http"
    "path/filepath"
    "strings"
    yaml "gopkg.in/yaml.v3"
    "github.com/gorilla/mux"
    glaze_help "github.com/go-go-golems/glazed/pkg/help"
)

//go:embed help/*.md
var helpFS embed.FS

type HelpSection struct {
    Title         string   `yaml:"Title" json:"title"`
    Slug          string   `yaml:"Slug" json:"slug"`
    Short         string   `yaml:"Short" json:"short"`
    Topics        []string `yaml:"Topics" json:"topics"`
    Commands      []string `yaml:"Commands" json:"commands"`
    Flags         []string `yaml:"Flags" json:"flags"`
    IsTopLevel    bool     `yaml:"IsTopLevel" json:"isTopLevel"`
    IsTemplate    bool     `yaml:"IsTemplate" json:"isTemplate"`
    ShowPerDefault bool    `yaml:"ShowPerDefault" json:"showPerDefault"`
    SectionType   string   `yaml:"SectionType" json:"sectionType"`
    Content       string   `json:"content"`
}

var helpIndex = map[string]*HelpSection{}

func LoadHelpSections() error {
    entries, err := fs.ReadDir(helpFS, "help")
    if err != nil { return err }
    for _, e := range entries {
        if e.IsDir() { continue }
        name := e.Name()
        if !strings.HasSuffix(name, ".md") { continue }
        b, err := helpFS.ReadFile(filepath.Join("help", name))
        if err != nil { continue }
        section, err := parseHelpMarkdown(string(b))
        if err != nil { continue }
        if section.Slug == "" {
            // derive slug from filename
            section.Slug = strings.TrimSuffix(name, ".md")
        }
        helpIndex[section.Slug] = section
    }
    return nil
}

func parseHelpMarkdown(s string) (*HelpSection, error) {
    s = strings.TrimSpace(s)
    var meta HelpSection
    content := s
    if strings.HasPrefix(s, "---\n") {
        parts := strings.SplitN(s, "\n---\n", 2)
        if len(parts) == 2 {
            fm := strings.TrimPrefix(parts[0], "---\n")
            if err := yaml.Unmarshal([]byte(fm), &meta); err == nil {
                content = parts[1]
            }
        }
    }
    meta.Content = strings.TrimSpace(content)
    if meta.Title == "" {
        // try to grab first heading
        for _, line := range strings.Split(meta.Content, "\n") {
            if strings.HasPrefix(line, "# ") {
                meta.Title = strings.TrimPrefix(line, "# ")
                break
            }
        }
    }
    return &meta, nil
}

func handleHelpList(w http.ResponseWriter, r *http.Request) {
    type item struct { Title, Slug, Short, SectionType string; Topics []string }
    var out []item
    for _, s := range helpIndex {
        out = append(out, item{Title: s.Title, Slug: s.Slug, Short: s.Short, SectionType: s.SectionType, Topics: s.Topics})
    }
    w.Header().Set("Content-Type", "application/json")
    json.NewEncoder(w).Encode(out)
}

func handleHelpGet(w http.ResponseWriter, r *http.Request) {
    slug := strings.TrimPrefix(r.URL.Path, "/help/")
    if slug == "help" || slug == "" { http.NotFound(w, r); return }
    sec, ok := helpIndex[slug]
    if !ok { http.NotFound(w, r); return }
    w.Header().Set("Content-Type", "application/json")
    json.NewEncoder(w).Encode(sec)
}

func RegisterHelpRoutes(r *mux.Router) {
    _ = LoadHelpSections()
    r.HandleFunc("/help", handleHelpList).Methods("GET")
    r.HandleFunc("/help/{slug}", func(w http.ResponseWriter, r *http.Request) {
        vars := mux.Vars(r)
        slug := vars["slug"]
        sec, ok := helpIndex[slug]
        if !ok { http.NotFound(w, r); return }
        w.Header().Set("Content-Type", "application/json")
        json.NewEncoder(w).Encode(sec)
    }).Methods("GET")
}

// Utilities for CLI integration
func ListSections() map[string]*HelpSection { return helpIndex }
func GetSection(slug string) (*HelpSection, bool) { s, ok := helpIndex[slug]; return s, ok }

// AddDocToHelpSystem wires embedded docs into the Glazed help system.
// Note: We keep a custom parser for front matter and content, then
//       add minimal sections to the Glazed system.
func AddDocToHelpSystem(hs *glaze_help.HelpSystem) error {
    // Load all embedded markdown files under help/ into the Glazed help system
    return hs.LoadSectionsFromFS(helpFS, "help")
}
