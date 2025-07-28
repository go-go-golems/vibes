package browser

import (
	"time"
)

// ScreenshotParams represents parameters for taking a screenshot
type ScreenshotParams struct {
	URL      string        `json:"url"`
	Output   string        `json:"output"`
	Width    int           `json:"width"`
	Height   int           `json:"height"`
	FullPage bool          `json:"full_page"`
	Wait     time.Duration `json:"wait"`
	Selector string        `json:"selector"`
}

// ScreenshotResult represents the result of a screenshot operation
type ScreenshotResult struct {
	URL        string        `json:"url"`
	OutputPath string        `json:"output_path"`
	FileSize   int64         `json:"file_size"`
	Width      int           `json:"width"`
	Height     int           `json:"height"`
	FullPage   bool          `json:"full_page"`
	Duration   time.Duration `json:"duration"`
	Timestamp  time.Time     `json:"timestamp"`
	Success    bool          `json:"success"`
	Error      string        `json:"error,omitempty"`
}

// ScrapingParams represents parameters for web scraping
type ScrapingParams struct {
	URL         string            `json:"url"`
	Selectors   map[string]string `json:"selectors"`
	WaitFor     string            `json:"wait_for"`
	Timeout     time.Duration     `json:"timeout"`
	UserAgent   string            `json:"user_agent"`
	Headers     map[string]string `json:"headers"`
	JavaScript  bool              `json:"javascript"`
}

// ScrapingResult represents the result of a web scraping operation
type ScrapingResult struct {
	URL       string                 `json:"url"`
	Data      map[string]interface{} `json:"data"`
	Duration  time.Duration          `json:"duration"`
	Timestamp time.Time              `json:"timestamp"`
	Success   bool                   `json:"success"`
	Error     string                 `json:"error,omitempty"`
}

// BrowserConfig represents browser configuration options
type BrowserConfig struct {
	Headless    bool              `json:"headless"`
	UserAgent   string            `json:"user_agent"`
	Timeout     time.Duration     `json:"timeout"`
	Headers     map[string]string `json:"headers"`
	Proxy       string            `json:"proxy"`
	IgnoreHTTPS bool              `json:"ignore_https"`
}

// Client interface defines the browser automation functionality
type Client interface {
	TakeScreenshot(params ScreenshotParams) (*ScreenshotResult, error)
	ScrapeData(params ScrapingParams) (*ScrapingResult, error)
	NavigateAndWait(url string, selector string, timeout time.Duration) error
	Close() error
}

