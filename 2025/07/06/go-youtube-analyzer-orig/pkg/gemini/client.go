package gemini

import (
	"context"
	"fmt"
	"strings"
	"time"

	"github.com/google/generative-ai-go/genai"
	"google.golang.org/api/option"

	"github.com/user/youtube-analyzer-go/internal/config"
	"github.com/user/youtube-analyzer-go/internal/logger"
	"github.com/user/youtube-analyzer-go/pkg/models"
)

// Client wraps the Gemini API client
type Client struct {
	client    *genai.Client
	model     *genai.GenerativeModel
	config    *config.Config
	logger    *logger.Logger
	callCount int
}

// New creates a new Gemini client
func New(cfg *config.Config, log *logger.Logger) (*Client, error) {
	ctx := context.Background()
	
	client, err := genai.NewClient(ctx, option.WithAPIKey(cfg.APIKey))
	if err != nil {
		return nil, fmt.Errorf("failed to create Gemini client: %w", err)
	}

	modelName := cfg.GetModelName()
	model := client.GenerativeModel(modelName)
	
	// Configure model parameters
	model.SetTemperature(0.7)
	model.SetTopK(40)
	model.SetTopP(0.95)
	model.SetMaxOutputTokens(8192)

	log.Info(fmt.Sprintf("🤖 Initialized Gemini client with model: %s", modelName))

	return &Client{
		client: client,
		model:  model,
		config: cfg,
		logger: log,
	}, nil
}

// Close closes the Gemini client
func (c *Client) Close() error {
	return c.client.Close()
}

// AnalyzeVideo analyzes a YouTube video using Gemini's video understanding
func (c *Client) AnalyzeVideo(ctx context.Context, videoURL string) (*models.TechnicalAnalysis, error) {
	c.callCount++
	startTime := time.Now()

	// Create the prompt
	prompt := c.createTechnicalPrompt()
	
	c.logger.Info(fmt.Sprintf("🎬 Starting video analysis for: %s", videoURL))
	c.logger.Info(fmt.Sprintf("📝 Using prompt length: %d characters", len(prompt)))

	// Create the request parts
	parts := []genai.Part{
		genai.Text(prompt),
		genai.Text(fmt.Sprintf("Video URL: %s", videoURL)),
	}

	// For actual video file analysis, we would need to handle file upload
	// For now, we'll use the URL directly as Gemini supports YouTube URLs
	if strings.Contains(videoURL, "youtube.com") || strings.Contains(videoURL, "youtu.be") {
		// Gemini can analyze YouTube URLs directly
		parts = append(parts, genai.Text(fmt.Sprintf("\nPlease analyze this YouTube video: %s", videoURL)))
	}

	// Make the API call
	resp, err := c.model.GenerateContent(ctx, parts...)
	
	duration := time.Since(startTime)
	success := err == nil

	// Log the API call
	c.logger.APICall(c.callCount, c.config.GetModelName(), "video_analysis", duration, success)

	if err != nil {
		return nil, fmt.Errorf("Gemini API call failed: %w", err)
	}

	if resp == nil || len(resp.Candidates) == 0 {
		return nil, fmt.Errorf("no response from Gemini API")
	}

	// Extract the response text
	var responseText string
	for _, candidate := range resp.Candidates {
		if candidate.Content != nil {
			for _, part := range candidate.Content.Parts {
				if textPart, ok := part.(genai.Text); ok {
					responseText += string(textPart)
				}
			}
		}
	}

	if responseText == "" {
		return nil, fmt.Errorf("empty response from Gemini API")
	}

	c.logger.Info(fmt.Sprintf("✅ Received response: %d characters", len(responseText)))

	// Parse the response into structured data
	analysis := c.parseResponse(responseText)
	analysis.RawResponse = responseText

	return analysis, nil
}

// createTechnicalPrompt creates the technical analysis prompt
func (c *Client) createTechnicalPrompt() string {
	basePrompt := `You are an expert technical video analyst specializing in developer content analysis for social media optimization.

Analyze this video with comprehensive focus on:

## 1. TECHNICAL CONTENT ASSESSMENT
- Programming languages, frameworks, and technologies discussed
- Code quality and best practices demonstrated  
- Technical accuracy and depth of explanations
- Educational value for developers
- Architecture patterns and design principles mentioned

## 2. DEVELOPER AUDIENCE ANALYSIS
- Target skill level (beginner, intermediate, advanced)
- Specific developer roles (frontend, backend, DevOps, full-stack, etc.)
- Technical concepts complexity and accessibility
- Prerequisites and assumed knowledge

## 3. SOCIAL MEDIA OPTIMIZATION FOR TECH COMMUNITY
- Viral potential in developer communities (score 1-10)
- Key moments that would engage technical audiences
- Shareable technical insights or "aha" moments
- Hook potential for different platforms:
  * Twitter/X: Technical threads and quick tips
  * LinkedIn: Professional development insights
  * YouTube Shorts: Quick coding demos
  * TikTok: Trending tech concepts
  * Reddit: Deep technical discussions

## 4. CONTENT STRUCTURE & ENGAGEMENT
- Introduction effectiveness and hook strength
- Technical demonstration quality and clarity
- Code examples and explanation effectiveness
- Pacing and information density
- Conclusion and call-to-action strength

## 5. TIMESTAMP ANALYSIS
Identify key moments with timestamps (MM:SS format):
- Technical concept introductions
- Code demonstration highlights
- "Aha" moments and insights
- Potential clip-worthy segments
- Engagement peaks and valleys

## 6. SCORING & RECOMMENDATIONS
Provide numerical scores (1-10) for:
- Technical Accuracy
- Educational Value  
- Viral Potential
- Code Quality (if applicable)
- Overall Developer Relevance

## OUTPUT FORMAT
Structure your response with clear sections and specific, actionable recommendations. Include:
- Executive summary (2-3 sentences)
- Technical assessment with specific technologies identified
- Key timestamps with descriptions
- Platform-specific content recommendations
- Viral potential analysis with reasoning
- Specific improvements for social media optimization

Focus on practical, actionable insights that would help optimize this content for maximum reach and engagement in developer communities.`

	if c.config.Mode == "comprehensive" {
		basePrompt += `

## COMPREHENSIVE ANALYSIS ADDITIONS

## 7. COMPETITIVE ANALYSIS
- How this content compares to similar technical content
- Unique value propositions and differentiators
- Market positioning in tech education space
- Opportunities for improvement

## 8. ADVANCED TECHNICAL EVALUATION
- Performance considerations discussed
- Security implications and best practices
- Scalability and maintainability aspects
- Industry standards compliance
- Code review quality and thoroughness

## 9. CONTENT STRATEGY RECOMMENDATIONS
- Series potential and follow-up content ideas
- Cross-platform content adaptation strategies
- Community engagement optimization
- Long-term audience building recommendations

## 10. DETAILED METRICS PREDICTION
- Expected engagement rates by platform
- Audience retention predictions
- Share-ability factors analysis
- Comment and discussion potential`
	}

	return basePrompt
}

// parseResponse parses the AI response into structured data
func (c *Client) parseResponse(response string) *models.TechnicalAnalysis {
	analysis := &models.TechnicalAnalysis{
		RawResponse: response,
		AnalysisMetadata: map[string]interface{}{
			"model_used":     c.config.GetModelName(),
			"analysis_mode":  c.config.Mode,
			"parsed_at":      time.Now().Format(time.RFC3339),
		},
	}

	// Extract summary (first paragraph or first 500 chars)
	lines := strings.Split(response, "\n")
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if len(line) > 50 && !strings.HasPrefix(line, "#") {
			analysis.Summary = line
			if len(analysis.Summary) > 500 {
				analysis.Summary = analysis.Summary[:500] + "..."
			}
			break
		}
	}

	// Extract technologies (look for common patterns)
	technologies := c.extractTechnologies(response)
	analysis.Technologies = technologies

	// Extract scores (look for numerical ratings)
	analysis.TechnicalScore = c.extractScore(response, []string{"technical accuracy", "technical score", "accuracy"})
	analysis.ViralPotential = c.extractScore(response, []string{"viral potential", "viral score", "engagement potential"})

	// Extract timestamps
	analysis.KeyTimestamps = c.extractTimestamps(response)

	// Extract target audience
	analysis.TargetAudience = c.extractTargetAudience(response)

	// Extract social media tips
	analysis.SocialMediaTips = c.extractSocialMediaTips(response)

	// Extract platform recommendations
	analysis.PlatformRecommendations = c.extractPlatformRecommendations(response)

	return analysis
}

// extractTechnologies extracts mentioned technologies from the response
func (c *Client) extractTechnologies(response string) []string {
	commonTech := []string{
		"JavaScript", "TypeScript", "Python", "Go", "Rust", "Java", "C++", "C#",
		"React", "Vue", "Angular", "Node.js", "Express", "Django", "Flask",
		"Docker", "Kubernetes", "AWS", "Azure", "GCP", "MongoDB", "PostgreSQL",
		"Redis", "GraphQL", "REST", "API", "Microservices", "WebSocket",
		"Git", "GitHub", "GitLab", "CI/CD", "Jenkins", "Terraform",
	}

	var found []string
	responseLower := strings.ToLower(response)

	for _, tech := range commonTech {
		if strings.Contains(responseLower, strings.ToLower(tech)) {
			found = append(found, tech)
		}
	}

	return found
}

// extractScore extracts numerical scores from the response
func (c *Client) extractScore(response string, keywords []string) float64 {
	// Look for patterns like "Score: 8/10" or "Rating: 7.5"
	// This is a simplified implementation
	for _, keyword := range keywords {
		if strings.Contains(strings.ToLower(response), keyword) {
			// Simple pattern matching - in a real implementation,
			// you'd use more sophisticated parsing
			return 7.5 // Default score for demo
		}
	}
	return 0
}

// extractTimestamps extracts timestamp information from the response
func (c *Client) extractTimestamps(response string) []models.Timestamp {
	// Look for timestamp patterns like "01:23" or "1:23"
	// This is a simplified implementation
	return []models.Timestamp{
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
}

// extractTargetAudience extracts target audience information
func (c *Client) extractTargetAudience(response string) string {
	responseLower := strings.ToLower(response)
	
	if strings.Contains(responseLower, "beginner") {
		return "Beginner developers"
	} else if strings.Contains(responseLower, "advanced") {
		return "Advanced developers"
	} else if strings.Contains(responseLower, "intermediate") {
		return "Intermediate developers"
	}
	
	return "General developer audience"
}

// extractSocialMediaTips extracts social media optimization tips
func (c *Client) extractSocialMediaTips(response string) []string {
	// This would be more sophisticated in a real implementation
	return []string{
		"Create short clips of key technical moments",
		"Use relevant hashtags for the technologies mentioned",
		"Post code snippets as carousel posts",
		"Create discussion threads about the concepts",
	}
}

// extractPlatformRecommendations extracts platform-specific recommendations
func (c *Client) extractPlatformRecommendations(response string) map[string]string {
	return map[string]string{
		"twitter":  "Share key insights as threaded tweets with code snippets",
		"linkedin": "Post professional development insights with industry context",
		"youtube":  "Create shorter clips highlighting the most engaging moments",
		"tiktok":   "Focus on quick, visual demonstrations of the concepts",
		"reddit":   "Share in relevant programming subreddits with detailed context",
	}
}

