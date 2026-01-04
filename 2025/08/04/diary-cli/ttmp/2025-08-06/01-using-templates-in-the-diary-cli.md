# Refactoring `diary-cli` to Use Go Templates for Entry Rendering

## 1 – Why Templates?
* Current formatting logic for default/markdown/task is embedded in Go (`pkg/storage/markdown.go` → `formatDefaultEntry`, `formatMarkdownEntry`, `formatTaskEntry`).
* Changing formats requires recompilation → low flexibility.
* Using standard `text/template` (or `template/parse` with `goldmark`) allows:
  * User-customisable output without code changes.
  * Re-use across new export formats (HTML, PDF, etc.).
  * Easier testing (compare rendered strings).

## 2 – Target Architecture
```
📦 diary-cli
 ├─ pkg/
 │  ├─ rendering/            # NEW – template engine & helpers
 │  │   ├─ renderer.go       # Template lookup, default funcs, execution
 │  │   └─ templates.go      # Built-in template strings (embedded via go:embed or constants)
│  └─ …
```

## 3 – Config Extension
```yaml
# ~/.config/diary-cli/config.yaml
rendering:
  templates:
    default: |
      ## {{ title . }}
      {{ if .SubtitleSlug }}### {{ .SubtitleSlug }}{{ end }}
      {{ if .Title }}{{ .Content }}
      {{ end }}
      *Added: {{ .Date.Format "2006-01-02 15:04" }}*
    markdown: |
      ## {{ title . }}
      **Type:** {{ .Type }}  
      **Date:** {{ .Date.Format "2006-01-02 15:04" }}  
      {{ if .Tags }}**Tags:** {{ join .Tags ", " }}  {{ end }}
      ---
      {{ .Content }}
    task: |
      - [ ] **{{ upper .Type }}**: {{ .Content }} #toProcess #{{ .Type }}
      - Added: {{ .Date.Format "2006-01-02 15:04" }}
```


## 4 – Renderer API (`pkg/rendering/renderer.go`)
```go
var builtIns = map[string]string{
    "default.md.tmpl": defaultTemplate,
    "markdown.md.tmpl": markdownTemplate,
    "task.md.tmpl": taskTemplate,
}

type Renderer struct {
    cfg       *config.Config
    templates *template.Template
}

func NewRenderer(cfg *config.Config) (*Renderer, error) {
    r := &Renderer{cfg: cfg}
    // 1. Parse built-ins
    base := template.New("base").Funcs(funcMap())
    for name, src := range builtIns {
        base.New(name).Parse(src)
    }
    // 2. User-supplied overrides from config
    for name, src := range cfg.Rendering.Templates {
        base.New(name).Parse(src)
    }
    r.templates = base
    return r, nil
}

func (r *Renderer) Render(tmpl string, data any) (string, error) {
    var b bytes.Buffer
    err := r.templates.ExecuteTemplate(&b, tmpl, data)
    return b.String(), err
}
```

## 5 – Replacing Current Formatters
1. Delete `formatDefaultEntry`, `formatMarkdownEntry`, `formatTaskEntry`.
2. In `MarkdownStorage.formatEntry`:
```go
out, _ := r.Render(string(entry.Format)+".md.tmpl", entry)
return out
```
(The Renderer is created once and injected into storage.)

## 6 – Default Templates (markdown examples)
`default.md.tmpl`
```gotemplate
## {{ title . }}
{{ if .SubtitleSlug }}### {{ .SubtitleSlug }}{{ end }}
{{ if .Title }}{{ .Content }}
{{ end }}
*Added: {{ .Date.Format "2006-01-02 15:04" }}*
```
Utility funcs like `title` are provided via `funcMap()`.

## 7 – Overriding Templates
1. User edits the `rendering.templates` section in their config file.
2. Provide the full Go template string using YAML literal (`|`) style for readability.
3. At startup, `Renderer` parses these strings and overrides the built-ins with the same key.
4. Hot-reload via fsnotify can be added later (future work).

## 8 – Implementation Steps (Checklist)
- [ ] Create `pkg/rendering` with embedded built-ins.
- [ ] Add `RenderingConfig` to `pkg/config.Config` + `yaml:"rendering"`.
- [ ] Wire `Renderer` into `main.go` and `MarkdownStorage`.
- [ ] Delete old formatter methods; replace with template execution.
- [ ] Provide default templates (commit under `pkg/rendering/templates`).
- [ ] Update docs + ARCHITECTURE.md.
- [ ] Add unit tests `renderer_test.go` (render each entry type).

## 9 – Future Improvements
* Template helpers for tags, priorities, obsidian-specific links.
* Flag `--template` to supply ad-hoc template files.
* Support HTML/PDF export by adding more templates.

---
Tight plan ready for execution. Focus next PRs on `renderer.go` creation and wiring storage to it.