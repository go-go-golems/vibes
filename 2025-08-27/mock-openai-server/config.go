package main

import (
    "errors"
    "io/ioutil"
    "log"
    "math/rand"
    "os"
    "path/filepath"
    "regexp"
    "strings"
    "time"

    yaml "gopkg.in/yaml.v3"
)

// Global configuration instance
var botConfig *BotConfig

type BotConfig struct {
    Version   int               `yaml:"version"`
    Server    ServerConfig      `yaml:"server"`
    Models    []ModelConfig     `yaml:"models"`
    Streaming StreamingConfig   `yaml:"streaming"`
    Tools     ToolsConfig       `yaml:"tools"`
    Variables map[string]string `yaml:"variables"`
    Rules     []Rule            `yaml:"rules"`
    Fallback  RespondWrapper    `yaml:"fallback"`
}

type ServerConfig struct {
    Port string `yaml:"port"`
    CORS string `yaml:"cors"`
}

type StreamingConfig struct {
    Enabled      *bool `yaml:"enabled"`
    ChunkDelayMs *int  `yaml:"chunk_delay_ms"`
}

type ModelConfig struct {
    ID      string `yaml:"id"`
    OwnedBy string `yaml:"owned_by"`
}

type StringOrSlice []string

func (s *StringOrSlice) UnmarshalYAML(value *yaml.Node) error {
    switch value.Kind {
    case yaml.ScalarNode:
        var single string
        if err := value.Decode(&single); err != nil {
            return err
        }
        *s = []string{single}
        return nil
    case yaml.SequenceNode:
        var arr []string
        if err := value.Decode(&arr); err != nil {
            return err
        }
        *s = arr
        return nil
    default:
        return errors.New("invalid type for StringOrSlice")
    }
}

type Match struct {
    Endpoint string        `yaml:"endpoint"`
    Model    StringOrSlice `yaml:"model"`
    Role     string        `yaml:"role"`
    Contains []string      `yaml:"contains"`
    Regex    string        `yaml:"regex"`
}

type WeightedText struct {
    Weight int    `yaml:"weight"`
    Text   string `yaml:"text"`
}

type AnnotationOut struct {
    Type  string `yaml:"type"`
    Title string `yaml:"title"`
    URL   string `yaml:"url,omitempty"`
}

type MessageOut struct {
    Text        string           `yaml:"text"`
    Annotations []AnnotationOut  `yaml:"annotations"`
}

type ToolOut struct {
    Type   string `yaml:"type"`
    Status string `yaml:"status,omitempty"`
}

type ErrorOut struct {
    Status  int    `yaml:"status"`
    Code    string `yaml:"code"`
    Message string `yaml:"message"`
}

type RespondWrapper struct {
    // Simple text for chat
    Text   string         `yaml:"text"`
    Choose []WeightedText `yaml:"choose"`

    // Responses API specific
    Tools   []ToolOut  `yaml:"tools"`
    UseTools []string  `yaml:"use_tools"`
    Message MessageOut `yaml:"message"`

    // Error injection
    Error *ErrorOut `yaml:"error"`
}

type Rule struct {
    ID             string           `yaml:"id"`
    Match          Match            `yaml:"match"`
    Respond        RespondWrapper   `yaml:"respond"`
    StreamOverride *StreamingConfig `yaml:"stream_override"`
    Continue       bool             `yaml:"continue"`
    Probability    *float64         `yaml:"probability"`
}

// Tools configuration
type ToolsConfig struct {
    Enabled  []string            `yaml:"enabled"`
    Registry map[string]ToolDef  `yaml:"registry"`
}

type ToolDef struct {
    // Logical tool name is the map key
    CallType string     `yaml:"call_type"` // OutputObject.Type, e.g., web_search_call
    Status   string     `yaml:"status"`
    Message  *MessageOut `yaml:"message"`
}

func LoadConfigFromEnv() {
    path := os.Getenv("MOCK_SERVER_CONFIG")
    if path == "" {
        path = filepath.Join("config", "bot.yaml")
    }
    cfg, err := LoadConfig(path)
    if err != nil {
        log.Printf("[config] No config file found (%v). Using built-in default configuration.", err)
        botConfig = defaultConfig()
        ensureDefaultTools(botConfig)
        return
    }
    botConfig = cfg
    ensureDefaultTools(botConfig)
    log.Printf("[config] Loaded config from %s (version %d)", path, cfg.Version)
}

func LoadConfig(path string) (*BotConfig, error) {
    b, err := ioutil.ReadFile(path)
    if err != nil {
        return nil, err
    }
    var cfg BotConfig
    if err := yaml.Unmarshal(b, &cfg); err != nil {
        return nil, err
    }
    return &cfg, nil
}

func containsString(list []string, s string) bool {
    for _, v := range list { if v == s { return true } }
    return false
}

func ensureDefaultTools(cfg *BotConfig) {
    if cfg.Tools.Registry == nil { cfg.Tools.Registry = map[string]ToolDef{} }
    // Built-in defaults
    if _, ok := cfg.Tools.Registry["web_search"]; !ok {
        cfg.Tools.Registry["web_search"] = ToolDef{
            CallType: "web_search_call",
            Status:   "completed",
            Message: &MessageOut{
                Text: "Based on my web search, here are the latest developments: Mock search results show that AI technology continues to advance rapidly.",
                Annotations: []AnnotationOut{
                    {Title: "AI Technology Advances in 2025", Type: "url_citation", URL: "https://example.com/ai-advances-2025"},
                    {Title: "Language Model Improvements", Type: "url_citation", URL: "https://example.com/language-models"},
                },
            },
        }
    }
    if _, ok := cfg.Tools.Registry["file_search"]; !ok {
        cfg.Tools.Registry["file_search"] = ToolDef{
            CallType: "file_search_call",
            Status:   "completed",
            Message: &MessageOut{
                Text: "Based on the uploaded documents, I found relevant information about your query.",
                Annotations: []AnnotationOut{
                    {Title: "Document Section 3.2", Type: "file_citation"},
                    {Title: "Appendix A - Examples", Type: "file_citation"},
                },
            },
        }
    }
    // If no enabled set, enable all defaults by default
    if len(cfg.Tools.Enabled) == 0 {
        for name := range cfg.Tools.Registry { cfg.Tools.Enabled = append(cfg.Tools.Enabled, name) }
    }
}

func isToolEnabled(name string) bool {
    if botConfig == nil { return false }
    if len(botConfig.Tools.Enabled) == 0 { return true }
    return containsString(botConfig.Tools.Enabled, name)
}

func getToolDef(name string) (ToolDef, bool) {
    if botConfig == nil { return ToolDef{}, false }
    td, ok := botConfig.Tools.Registry[name]
    return td, ok
}

// Default configuration when no YAML file is provided
func defaultConfig() *BotConfig {
    delay := 120
    enabled := true
    cfg := &BotConfig{
        Version: 1,
        Server:  ServerConfig{Port: "3117", CORS: "*"},
        Models: []ModelConfig{
            {ID: "gpt-4o", OwnedBy: "openai"},
            {ID: "gpt-4o-mini", OwnedBy: "openai"},
            {ID: "gpt-3.5-turbo", OwnedBy: "openai"},
        },
        Streaming: StreamingConfig{Enabled: &enabled, ChunkDelayMs: &delay},
        Variables: map[string]string{"bot_name": "Mock OpenAI"},
        Tools: ToolsConfig{
            Enabled:  []string{"web_search", "file_search"},
            Registry: map[string]ToolDef{
                "custom_demo": {
                    CallType: "custom_demo_call",
                    Status:   "completed",
                    Message:  &MessageOut{Text: "Custom tool ran with input: '{{input_text}}'"},
                },
            },
        },
        Rules: []Rule{
            {ID: "greet", Match: Match{Endpoint: "chat", Contains: []string{"hello", "hi"}}, Respond: RespondWrapper{Text: "Hello! I'm {{bot_name}}. How can I help?"}},
            {ID: "jokes", Match: Match{Endpoint: "chat", Contains: []string{"joke"}}, Respond: RespondWrapper{Choose: []WeightedText{{Weight: 1, Text: "Why don't scientists trust atoms? They make up everything!"}, {Weight: 1, Text: "What do you call a fake noodle? An impasta!"}}}},
            {ID: "chat_with_search", Match: Match{Endpoint: "chat", Contains: []string{"search", "latest"}}, Respond: RespondWrapper{UseTools: []string{"web_search"}, Text: "Summary above. Let me know if you want more details."}},
            {ID: "responses_web_search", Match: Match{Endpoint: "responses", Contains: []string{"news", "latest", "AI"}}, Respond: RespondWrapper{UseTools: []string{"web_search"}, Message: MessageOut{Text: "Here are the latest AI headlines with citations."}}},
            {ID: "responses_custom_tool", Match: Match{Endpoint: "responses", Contains: []string{"custom tool"}}, Respond: RespondWrapper{UseTools: []string{"custom_demo"}}},
        },
        Fallback: RespondWrapper{Text: "This is a mock response to: '{{last_user_message}}'."},
    }
    return cfg
}

// Utility: render a minimal template expansion using {{var}} placeholders
func renderTemplate(s string, ctx map[string]string) string {
    out := s
    for k, v := range ctx {
        out = strings.ReplaceAll(out, "{{"+k+"}}", v)
    }
    return out
}

// Rule evaluation helpers
type matchedRule struct {
    Rule  *Rule
    Delay time.Duration
}

func getStreamingDelayMs(global StreamingConfig, override *StreamingConfig, defaultMs int) int {
    if override != nil && override.ChunkDelayMs != nil {
        return *override.ChunkDelayMs
    }
    if global.ChunkDelayMs != nil {
        return *global.ChunkDelayMs
    }
    return defaultMs
}

func pickText(resp RespondWrapper) string {
    if len(resp.Choose) > 0 {
        total := 0
        for _, c := range resp.Choose { total += c.Weight }
        if total <= 0 { total = len(resp.Choose) }
        r := rand.Intn(total)
        sum := 0
        for _, c := range resp.Choose {
            w := c.Weight
            if w <= 0 { w = 1 }
            if r < sum+w { return c.Text }
            sum += w
        }
        return resp.Choose[0].Text
    }
    if resp.Text != "" {
        return resp.Text
    }
    if resp.Message.Text != "" {
        return resp.Message.Text
    }
    return ""
}

func isModelMatch(model string, cand []string) bool {
    if len(cand) == 0 { return true }
    for _, m := range cand { if m == model { return true } }
    return false
}

// Evaluate rules and return the first applicable one. If continue=true, it will
// pick the last matching rule in sequence, allowing overrides.
func evaluateRules(endpoint string, model string, role string, lastUser string, fullText string) *matchedRule {
    if botConfig == nil || len(botConfig.Rules) == 0 { return nil }
    var current *matchedRule
    for i := range botConfig.Rules {
        r := &botConfig.Rules[i]
        if r.Match.Endpoint != "" && r.Match.Endpoint != endpoint {
            continue
        }
        if !isModelMatch(model, r.Match.Model) { continue }
        if r.Match.Role != "" && r.Match.Role != role { continue }

        // contains check against last user and full text
        if len(r.Match.Contains) > 0 {
            found := false
            lu := strings.ToLower(lastUser)
            ft := strings.ToLower(fullText)
            for _, c := range r.Match.Contains {
                c = strings.ToLower(c)
                if strings.Contains(lu, c) || strings.Contains(ft, c) { found = true; break }
            }
            if !found { continue }
        }

        // regex on full text
        if r.Match.Regex != "" {
            re, err := regexp.Compile(r.Match.Regex)
            if err != nil { continue }
            if !re.MatchString(fullText) { continue }
        }

        // probability gate
        if r.Probability != nil {
            p := *r.Probability
            if p <= 0 { continue }
            if p < 1.0 {
                if rand.Float64() > p { continue }
            }
        }

        // matched
        delayMs := getStreamingDelayMs(botConfig.Streaming, r.StreamOverride, 150)
        current = &matchedRule{Rule: r, Delay: time.Duration(delayMs) * time.Millisecond}
        if !r.Continue { break }
    }
    return current
}

// Build template context
func buildTemplateContext(model, lastUser, fullText string) map[string]string {
    ctx := map[string]string{
        "model":            model,
        "last_user_message": lastUser,
        "input_text":       fullText,
        "timestamp":        time.Now().Format(time.RFC3339),
    }
    if botConfig != nil && botConfig.Variables != nil {
        for k, v := range botConfig.Variables { ctx[k] = v }
    }
    return ctx
}
