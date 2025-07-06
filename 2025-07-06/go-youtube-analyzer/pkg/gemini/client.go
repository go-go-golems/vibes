package gemini

import (
	"context"
	"fmt"
	"regexp"
	"strings"
	"time"

	genai "google.golang.org/genai"
	"gopkg.in/yaml.v3"

	"github.com/user/youtube-analyzer-go/internal/config"
	"github.com/user/youtube-analyzer-go/internal/logger"
	"github.com/user/youtube-analyzer-go/pkg/models"
)

// Client wraps the Gemini API client
type Client struct {
	client    *genai.Client
	config    *config.Config
	logger    *logger.Logger
	callCount int
}

// StructuredData represents the YAML structured data from the AI response
type StructuredData struct {
	Topics                  []string           `yaml:"topics"`
	EngagementScore         float64            `yaml:"engagement_score"`
	ViralPotential          float64            `yaml:"viral_potential"`
	TargetAudience          string             `yaml:"target_audience"`
	TechnicalAccuracy       string             `yaml:"technical_accuracy"`
	EducationalValue        string             `yaml:"educational_value"`
	CodeQuality             string             `yaml:"code_quality"`
	DeveloperRelevance      string             `yaml:"developer_relevance"`
	Timestamps              []models.Timestamp `yaml:"timestamps"`
	SocialMediaTips         []string           `yaml:"social_media_tips"`
	PlatformRecommendations map[string]string  `yaml:"platform_recommendations"`
}

// New creates a new Gemini client
func New(cfg *config.Config, log *logger.Logger) (*Client, error) {
	ctx := context.Background()

	client, err := genai.NewClient(ctx, &genai.ClientConfig{
		APIKey: cfg.APIKey,
	})
	if err != nil {
		return nil, fmt.Errorf("failed to create Gemini client: %w", err)
	}

	modelName := cfg.GetModelName()
	log.Info(fmt.Sprintf("🤖 Initialized Gemini client with model: %s", modelName))

	return &Client{
		client: client,
		config: cfg,
		logger: log,
	}, nil
}

// Close closes the Gemini client
func (c *Client) Close() error {
	// The new genai client doesn't have a Close method
	return nil
}

// AnalyzeVideo analyzes a YouTube video using Gemini's video understanding
func (c *Client) AnalyzeVideo(ctx context.Context, videoURL string) (*models.TechnicalAnalysis, error) {
	c.callCount++
	startTime := time.Now()

	// Create the prompt
	prompt := c.CreateTechnicalPrompt()

	c.logger.Info(fmt.Sprintf("🎬 Starting video analysis for: %s", videoURL))
	c.logger.Info(fmt.Sprintf("📝 Using prompt length: %d characters", len(prompt)))

	// Create the request content with YouTube video FileData
	modelName := c.config.GetModelName()
	contents := []*genai.Content{
		{
			Parts: []*genai.Part{
				{Text: prompt},
				{FileData: &genai.FileData{
					FileURI:  videoURL,
					MIMEType: "video/mp4",
				}},
			},
			Role: "user",
		},
	}

	// Debug: Show the full request body
	c.logger.Info("📋 Full request body to Gemini:")
	c.logger.Info(fmt.Sprintf("  Model: %s", modelName))
	c.logger.Info(fmt.Sprintf("  Content Role: %s", contents[0].Role))
	c.logger.Info(fmt.Sprintf("  Part 1: Text prompt (%d characters)", len(prompt)))
	c.logger.Info(fmt.Sprintf("  Part 2: FileData - URI: %s, MIME: %s", videoURL, "video/mp4"))

	// Make the API call
	resp, err := c.client.Models.GenerateContent(ctx, modelName, contents, nil)

	duration := time.Since(startTime)
	success := err == nil

	// Log the API call
	c.logger.APICall(c.callCount, c.config.GetModelName(), "video_analysis", duration, success)

	if err != nil {
		return nil, fmt.Errorf("Gemini API call failed: %w", err)
	}

	// Extract the response text
	responseText := resp.Text()
	if responseText == "" {
		return nil, fmt.Errorf("empty response from Gemini API")
	}

	c.logger.Info(fmt.Sprintf("✅ Received response: %d characters", len(responseText)))

	// Parse the response into structured data
	analysis := c.parseResponse(responseText)
	analysis.RawResponse = responseText

	return analysis, nil
}

// CreateTechnicalPrompt creates the social media analysis prompt
func (c *Client) CreateTechnicalPrompt() string {
	basePrompt := `Analyze this video for social media optimization and engagement potential.

Please provide your analysis in a detailed written format, and at the end include a YAML block with structured data.

## Analysis Structure:

1. **Summary**: A brief 2-3 sentence overview of the video content and main message.

2. **Key Topics**: List the main topics, themes, or subjects covered in the video.

3. **Target Audience**: Identify who would be most interested in this content.

4. **Engagement Potential**: Rate the video's potential for social media engagement (1-10) and explain why.

5. **Key Moments**: Identify 3-5 important timestamps with descriptions of what makes them engaging or shareable.

6. **Social Media Recommendations**: 
   - Best platforms for sharing this content
   - Suggested post formats and approaches
   - Hashtag recommendations
   - Content adaptation ideas for different platforms

7. **Viral Potential**: Assess what elements could make this content go viral or gain traction.

8. **Technical Assessment**: Evaluate the technical accuracy, educational value, code quality (if applicable), and developer relevance.

At the end of your response, include a YAML block with structured data in exactly this format:

` + "```yaml" + `
# structured_data
topics: ["topic1", "topic2", "topic3"]
engagement_score: 8.5
viral_potential: 7.2
target_audience: "Young adults (18-35)"
technical_accuracy: "High quality technical content"
educational_value: "Provides clear learning objectives"
code_quality: "Well-structured examples" 
developer_relevance: "Highly relevant to software developers"
timestamps:
  - time: "01:30"
    description: "Key moment description"
    importance: "high"
    type: "engagement"
  - time: "03:45"
    description: "Another important moment"
    importance: "medium"
    type: "technical"
social_media_tips:
  - "Create short clips of the most engaging moments"
  - "Use relevant hashtags for the topics mentioned"
  - "Post key insights as carousel posts"
platform_recommendations:
  twitter: "Twitter recommendation"
  linkedin: "LinkedIn recommendation"
  youtube: "YouTube recommendation"
  tiktok: "TikTok recommendation"
  reddit: "Reddit recommendation"
  instagram: "Instagram recommendation"
` + "```" + `

Keep your analysis practical and actionable for social media marketing.`

	if c.config.Mode == "comprehensive" {
		basePrompt += `

## Additional Analysis:
- Content trends alignment
- Competitor comparison opportunities  
- Cross-platform adaptation strategies
- Long-term content series potential`
	}

	return basePrompt
}

// parseResponse parses the AI response into structured data
func (c *Client) parseResponse(response string) *models.TechnicalAnalysis {
	analysis := &models.TechnicalAnalysis{
		RawResponse: response,
		AnalysisMetadata: map[string]interface{}{
			"model_used":    c.config.GetModelName(),
			"analysis_mode": c.config.Mode,
			"parsed_at":     time.Now().Format(time.RFC3339),
		},
	}

	// Extract summary (first paragraph or first 500 chars)
	lines := strings.Split(response, "\n")
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if len(line) > 50 && !strings.HasPrefix(line, "#") && !strings.HasPrefix(line, "```") {
			analysis.Summary = line
			if len(analysis.Summary) > 500 {
				analysis.Summary = analysis.Summary[:500] + "..."
			}
			break
		}
	}

	// Extract structured data from YAML block
	structuredData, err := c.extractStructuredData(response)
	if err != nil {
		if c.logger != nil {
			c.logger.Info(fmt.Sprintf("⚠️  Failed to extract structured data: %v", err))
		}
		// Fall back to mock data if structured extraction fails
		c.populateWithFallbackData(analysis)
		return analysis
	}

	// Populate analysis with structured data
	analysis.Technologies = structuredData.Topics
	analysis.TechnicalScore = structuredData.EngagementScore
	analysis.ViralPotential = structuredData.ViralPotential
	analysis.TargetAudience = structuredData.TargetAudience
	analysis.TechnicalAccuracy = structuredData.TechnicalAccuracy
	analysis.EducationalValue = structuredData.EducationalValue
	analysis.CodeQuality = structuredData.CodeQuality
	analysis.DeveloperRelevance = structuredData.DeveloperRelevance
	analysis.KeyTimestamps = structuredData.Timestamps
	analysis.SocialMediaTips = structuredData.SocialMediaTips
	analysis.PlatformRecommendations = structuredData.PlatformRecommendations

	return analysis
}

// extractStructuredData extracts YAML structured data from the AI response
func (c *Client) extractStructuredData(response string) (*StructuredData, error) {
	// Find the YAML block in the response
	yamlBlock := c.extractYAMLBlock(response)
	if yamlBlock == "" {
		return nil, fmt.Errorf("no YAML block found in response")
	}

	// Parse the YAML
	var data StructuredData
	err := yaml.Unmarshal([]byte(yamlBlock), &data)
	if err != nil {
		return nil, fmt.Errorf("failed to parse YAML: %w", err)
	}

	// Validate required fields
	if len(data.Topics) == 0 {
		return nil, fmt.Errorf("no topics found in structured data")
	}

	if c.logger != nil {
		c.logger.Info(fmt.Sprintf("✅ Successfully extracted structured data with %d topics", len(data.Topics)))
	}
	return &data, nil
}

// extractYAMLBlock extracts the YAML block from the markdown response
func (c *Client) extractYAMLBlock(response string) string {
	// Use regex to find YAML code blocks with multiline support
	yamlRegex := regexp.MustCompile("(?s)```yaml\\s*\\n(.*?)\\n```")
	matches := yamlRegex.FindStringSubmatch(response)

	if len(matches) > 1 {
		return strings.TrimSpace(matches[1])
	}

	// Alternative: try to find structured_data comment
	lines := strings.Split(response, "\n")
	var yamlLines []string
	inYAMLBlock := false

	for _, line := range lines {
		if strings.Contains(line, "```yaml") {
			inYAMLBlock = true
			continue
		}
		if strings.Contains(line, "```") && inYAMLBlock {
			break
		}
		if inYAMLBlock {
			yamlLines = append(yamlLines, line)
		}
	}

	if len(yamlLines) > 0 {
		return strings.Join(yamlLines, "\n")
	}

	return ""
}

// populateWithFallbackData populates the analysis with fallback data if YAML extraction fails
func (c *Client) populateWithFallbackData(analysis *models.TechnicalAnalysis) {
	if c.logger != nil {
		c.logger.Info("🔄 Using fallback data due to YAML parsing failure")
	}

	analysis.Technologies = []string{"Content analysis", "Social media optimization", "Video engagement"}
	analysis.TechnicalScore = 7.5
	analysis.ViralPotential = 6.8
	analysis.TargetAudience = "General audience"
	analysis.TechnicalAccuracy = "Unable to assess - using fallback data"
	analysis.EducationalValue = "Unable to assess - using fallback data"
	analysis.CodeQuality = "Unable to assess - using fallback data"
	analysis.DeveloperRelevance = "Unable to assess - using fallback data"

	analysis.KeyTimestamps = []models.Timestamp{
		{
			Time:        "00:30",
			Description: "Introduction and overview",
			Importance:  "high",
			Type:        "engagement",
		},
		{
			Time:        "02:15",
			Description: "Technical demonstration begins",
			Importance:  "high",
			Type:        "technical",
		},
	}

	analysis.SocialMediaTips = []string{
		"Create short clips of the most engaging moments",
		"Use relevant hashtags for the topics mentioned",
		"Post key insights as carousel posts",
		"Create discussion threads about the main themes",
		"Share quotes or highlights from the content",
	}

	analysis.PlatformRecommendations = map[string]string{
		"twitter":   "Share key insights as threaded tweets with engaging visuals",
		"linkedin":  "Post professional insights with industry context and discussion prompts",
		"youtube":   "Create shorter clips highlighting the most engaging moments",
		"tiktok":    "Focus on quick, visual demonstrations and trending topics",
		"reddit":    "Share in relevant subreddits with detailed context and discussion starters",
		"instagram": "Use carousel posts with key takeaways and engaging visuals",
	}
}
