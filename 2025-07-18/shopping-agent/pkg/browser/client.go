package browser

import (
	"context"
	"os"
	"path/filepath"
	"time"

	"github.com/go-rod/rod"
	"github.com/go-rod/rod/lib/launcher"
	"github.com/go-rod/rod/lib/proto"
	"github.com/pkg/errors"
	"github.com/rs/zerolog/log"
)

// RodClient implements the Client interface using Rod browser automation
type RodClient struct {
	browser *rod.Browser
	config  BrowserConfig
}

// NewClient creates a new browser client with default configuration
func NewClient() *RodClient {
	return NewClientWithConfig(BrowserConfig{
		Headless:    true,
		UserAgent:   "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
		Timeout:     30 * time.Second,
		IgnoreHTTPS: true,
	})
}

// NewClientWithConfig creates a new browser client with custom configuration
func NewClientWithConfig(config BrowserConfig) *RodClient {
	client := &RodClient{
		config: config,
	}

	// Initialize browser
	if err := client.initBrowser(); err != nil {
		log.Fatal().Err(err).Msg("Failed to initialize browser")
	}

	return client
}

// initBrowser initializes the Rod browser instance
func (c *RodClient) initBrowser() error {
	log.Debug().Msg("Initializing browser")

	// Create launcher
	l := launcher.New().
		Headless(c.config.Headless).
		Set("disable-web-security").
		Set("disable-features", "VizDisplayCompositor")

	if c.config.Proxy != "" {
		l = l.Proxy(c.config.Proxy)
	}

	// Launch browser
	url, err := l.Launch()
	if err != nil {
		return errors.Wrap(err, "failed to launch browser")
	}

	// Connect to browser
	c.browser = rod.New().ControlURL(url)
	if err := c.browser.Connect(); err != nil {
		return errors.Wrap(err, "failed to connect to browser")
	}

	log.Debug().Msg("Browser initialized successfully")
	return nil
}

// TakeScreenshot captures a screenshot of the specified URL
func (c *RodClient) TakeScreenshot(ctx context.Context, params ScreenshotParams) (*ScreenshotResult, error) {
	startTime := time.Now()
	result := &ScreenshotResult{
		URL:       params.URL,
		OutputPath: params.Output,
		Width:     params.Width,
		Height:    params.Height,
		FullPage:  params.FullPage,
		Timestamp: startTime,
	}

	log.Debug().
		Str("url", params.URL).
		Str("output", params.Output).
		Msg("Taking screenshot")

	// Create new page
	page, err := c.browser.Page(proto.TargetCreateTarget{URL: ""})
	if err != nil {
		result.Error = err.Error()
		return result, errors.Wrap(err, "failed to create page")
	}
	defer page.Close()

	// Set viewport size
	if err := page.SetViewport(&proto.EmulationSetDeviceMetricsOverride{
		Width:  params.Width,
		Height: params.Height,
	}); err != nil {
		result.Error = err.Error()
		return result, errors.Wrap(err, "failed to set viewport")
	}

	// Set user agent if configured
	if c.config.UserAgent != "" {
		if err := page.SetUserAgent(&proto.NetworkSetUserAgentOverride{
			UserAgent: c.config.UserAgent,
		}); err != nil {
			log.Warn().Err(err).Msg("Failed to set user agent")
		}
	}

	// Navigate to URL
	if err := page.Navigate(params.URL); err != nil {
		result.Error = err.Error()
		return result, errors.Wrap(err, "failed to navigate to URL")
	}

	// Wait for page to load
	if err := page.WaitLoad(); err != nil {
		result.Error = err.Error()
		return result, errors.Wrap(err, "failed to wait for page load")
	}

	// Wait for specific selector if provided
	if params.Selector != "" {
		log.Debug().Str("selector", params.Selector).Msg("Waiting for selector")
		element, err := page.Timeout(c.config.Timeout).Element(params.Selector)
		if err != nil {
			log.Warn().Err(err).Str("selector", params.Selector).Msg("Failed to wait for selector")
		} else {
			if err := element.WaitVisible(); err != nil {
				log.Warn().Err(err).Str("selector", params.Selector).Msg("Failed to wait for element visibility")
			}
		}
	}

	// Additional wait time
	if params.Wait > 0 {
		log.Debug().Dur("wait", params.Wait).Msg("Additional wait time")
		time.Sleep(params.Wait)
	}

	// Take screenshot
	var screenshotData []byte
	if params.FullPage {
		screenshotData, err = page.Screenshot(true, nil)
	} else {
		screenshotData, err = page.Screenshot(false, nil)
	}

	if err != nil {
		result.Error = err.Error()
		return result, errors.Wrap(err, "failed to take screenshot")
	}

	// Ensure output directory exists
	outputDir := filepath.Dir(params.Output)
	if outputDir != "." && outputDir != "" {
		if err := os.MkdirAll(outputDir, 0755); err != nil {
			result.Error = err.Error()
			return result, errors.Wrap(err, "failed to create output directory")
		}
	}

	// Save screenshot to file
	if err := os.WriteFile(params.Output, screenshotData, 0644); err != nil {
		result.Error = err.Error()
		return result, errors.Wrap(err, "failed to save screenshot")
	}

	// Get file info
	fileInfo, err := os.Stat(params.Output)
	if err != nil {
		log.Warn().Err(err).Msg("Failed to get file info")
	} else {
		result.FileSize = fileInfo.Size()
	}

	result.Duration = time.Since(startTime)
	result.Success = true

	log.Info().
		Str("url", params.URL).
		Str("output", params.Output).
		Int64("file_size", result.FileSize).
		Dur("duration", result.Duration).
		Msg("Screenshot captured successfully")

	return result, nil
}

// ScrapeData extracts data from a web page using CSS selectors
func (c *RodClient) ScrapeData(ctx context.Context, params ScrapingParams) (*ScrapingResult, error) {
	startTime := time.Now()
	result := &ScrapingResult{
		URL:       params.URL,
		Data:      make(map[string]interface{}),
		Timestamp: startTime,
	}

	log.Debug().
		Str("url", params.URL).
		Int("selectors", len(params.Selectors)).
		Msg("Starting data scraping")

	// Create new page
	page, err := c.browser.Page(proto.TargetCreateTarget{URL: ""})
	if err != nil {
		result.Error = err.Error()
		return result, errors.Wrap(err, "failed to create page")
	}
	defer page.Close()

	// Set user agent if configured
	if params.UserAgent != "" {
		if err := page.SetUserAgent(&proto.NetworkSetUserAgentOverride{
			UserAgent: params.UserAgent,
		}); err != nil {
			log.Warn().Err(err).Msg("Failed to set user agent")
		}
	} else if c.config.UserAgent != "" {
		if err := page.SetUserAgent(&proto.NetworkSetUserAgentOverride{
			UserAgent: c.config.UserAgent,
		}); err != nil {
			log.Warn().Err(err).Msg("Failed to set user agent")
		}
	}

	// Navigate to URL
	if err := page.Navigate(params.URL); err != nil {
		result.Error = err.Error()
		return result, errors.Wrap(err, "failed to navigate to URL")
	}

	// Wait for page to load
	if err := page.WaitLoad(); err != nil {
		result.Error = err.Error()
		return result, errors.Wrap(err, "failed to wait for page load")
	}

	// Wait for specific element if provided
	if params.WaitFor != "" {
		timeout := params.Timeout
		if timeout == 0 {
			timeout = c.config.Timeout
		}
		
		log.Debug().Str("selector", params.WaitFor).Msg("Waiting for element")
		element, err := page.Timeout(timeout).Element(params.WaitFor)
		if err != nil {
			log.Warn().Err(err).Str("selector", params.WaitFor).Msg("Failed to wait for element")
		} else {
			if err := element.WaitVisible(); err != nil {
				log.Warn().Err(err).Str("selector", params.WaitFor).Msg("Failed to wait for element visibility")
			}
		}
	}

	// Extract data using selectors
	for key, selector := range params.Selectors {
		log.Debug().Str("key", key).Str("selector", selector).Msg("Extracting data")
		
		elements, err := page.Elements(selector)
		if err != nil {
			log.Warn().Err(err).Str("selector", selector).Msg("Failed to find elements")
			result.Data[key] = nil
			continue
		}

		if len(elements) == 0 {
			log.Debug().Str("selector", selector).Msg("No elements found")
			result.Data[key] = nil
			continue
		}

		// Extract text from all matching elements
		var values []string
		for _, element := range elements {
			text, err := element.Text()
			if err != nil {
				log.Warn().Err(err).Msg("Failed to get element text")
				continue
			}
			if text != "" {
				values = append(values, text)
			}
		}

		// Store single value or array based on count
		if len(values) == 1 {
			result.Data[key] = values[0]
		} else {
			result.Data[key] = values
		}
	}

	result.Duration = time.Since(startTime)
	result.Success = true

	log.Info().
		Str("url", params.URL).
		Int("data_points", len(result.Data)).
		Dur("duration", result.Duration).
		Msg("Data scraping completed successfully")

	return result, nil
}

// NavigateAndWait navigates to a URL and waits for a specific element
func (c *RodClient) NavigateAndWait(url string, selector string, timeout time.Duration) error {
	log.Debug().
		Str("url", url).
		Str("selector", selector).
		Dur("timeout", timeout).
		Msg("Navigating and waiting")

	page, err := c.browser.Page(proto.TargetCreateTarget{URL: ""})
	if err != nil {
		return errors.Wrap(err, "failed to create page")
	}
	defer page.Close()

	if err := page.Navigate(url); err != nil {
		return errors.Wrap(err, "failed to navigate to URL")
	}

	if err := page.WaitLoad(); err != nil {
		return errors.Wrap(err, "failed to wait for page load")
	}

	if selector != "" {
		element, err := page.Timeout(timeout).Element(selector)
		if err != nil {
			return errors.Wrap(err, "failed to find selector")
		}
		if err := element.WaitVisible(); err != nil {
			return errors.Wrap(err, "failed to wait for selector visibility")
		}
	}

	return nil
}

// Close closes the browser and cleans up resources
func (c *RodClient) Close() error {
	if c.browser != nil {
		log.Debug().Msg("Closing browser")
		return c.browser.Close()
	}
	return nil
}

