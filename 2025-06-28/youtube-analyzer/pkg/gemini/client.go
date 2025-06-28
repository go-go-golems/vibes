package gemini

import (
	"context"
	"fmt"
	"regexp"
	"strconv"
	"strings"
	"time"

	"github.com/google/generative-ai-go/genai"
	"google.golang.org/api/option"

	"github.com/user/youtube-analyzer-go/internal/config"
	"github.com/user/youtube-analyzer-go/internal/logger"
	"github.com/user/youtube-analyzer-go/pkg/models"
	goyaml "gopkg.in/yaml.v3"
)

// Client wraps the Gemini API client
type Client struct {
	client    *genai.Client
	model     *genai.GenerativeModel
	config    *config.Config
	logger    *logger.Logger
	callCount int
}

// min returns the minimum of two integers
func min(a, b int) int {
	if a < b {
		return a
	}
	return b
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

	// Create the request parts - video must come first, then prompt
	var parts []genai.Part

	// Handle video input properly
	if strings.Contains(videoURL, "youtube.com") || strings.Contains(videoURL, "youtu.be") {
		// Use FileData for YouTube URLs as per Gemini documentation
		parts = append(parts, genai.FileData{
			URI:      videoURL,
			MIMEType: "video/mp4", // YouTube videos are treated as MP4
		})
		c.logger.Info("📹 Added YouTube video as file data for analysis")
	} else {
		// For other video files, we would need to handle file upload to Cloud Storage first
		// For now, return an error suggesting proper usage
		return nil, fmt.Errorf("only YouTube URLs are currently supported. Please provide a YouTube URL (youtube.com or youtu.be)")
	}

	// Add the text prompt after the video
	parts = append(parts, genai.Text(prompt))

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

**CRITICAL INSTRUCTION: You MUST analyze the actual video content provided. If you cannot access or view the actual video content, you MUST respond with an error message stating "ERROR: Cannot access video content for analysis" instead of making up or hallucinating any analysis. Do not provide fake timestamps, made-up technical details, or invented content descriptions.**

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
Structure your response with clear sections and specific, actionable recommendations. 

**For the following key data, output each as a separate XML-style tag, e.g.:**
<summary>...</summary>
<technical_score>...</technical_score>
<viral_potential>...</viral_potential>
<target_audience>...</target_audience>
<technologies>
- ...
</technologies>
<timestamps>
- time: "01:23"
  description: ...
  importance: ...
  type: ...
</timestamps>
<social_media_optimization>
- ...
</social_media_optimization>
<platform_recommendations>
twitter: ...
linkedin: ...
...
</platform_recommendations>
<technical_accuracy_assessment>...</technical_accuracy_assessment>
<educational_value>...</educational_value>
<code_quality_assessment>...</code_quality_assessment>
<developer_relevance>...</developer_relevance>

After these tags, provide your usual freeform summary and recommendations. Do not include any other XML tags except for the above.

---

### EXAMPLES (use these formats exactly):
<summary>This video demonstrates building a real-time collaborative text editor using React and WebSockets.</summary>
<technical_score>8.5</technical_score>
<viral_potential>7.2</viral_potential>
<target_audience>Intermediate developers</target_audience>
<technologies>
- React
- WebSocket
- Node.js
</technologies>
<timestamps>
- time: "01:30"
  description: WebSocket implementation begins
  importance: high
  type: technical
- time: "03:45"
  description: Collaborative editing logic explained
  importance: high
  type: technical
</timestamps>
<social_media_optimization>
- Create short clips of key technical moments
- Use relevant hashtags for technologies mentioned
</social_media_optimization>
<platform_recommendations>
twitter: Share key insights as threaded tweets
linkedin: Post professional development insights
youtube: Create shorter clips highlighting the most engaging moments
</platform_recommendations>
<technical_accuracy_assessment>Accurate implementation of WebSocket-based collaboration.</technical_accuracy_assessment>
<educational_value>High educational value for developers interested in real-time apps.</educational_value>
<code_quality_assessment>Code is modular and well-commented.</code_quality_assessment>
<developer_relevance>Highly relevant for full-stack and frontend developers.</developer_relevance>
---
`

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
	c.logger.Info("--- RAW GEMINI RESPONSE ---\n" + response)
	analysis := &models.TechnicalAnalysis{
		RawResponse: response,
		AnalysisMetadata: map[string]interface{}{
			"model_used":     c.config.GetModelName(),
			"analysis_mode":  c.config.Mode,
			"parsed_at":      time.Now().Format(time.RFC3339),
		},
	}

	extractText := func(tag string) string {
		startTag := "<" + tag + ">"
		endTag := "</" + tag + ">"
		start := strings.Index(response, startTag)
		end := strings.Index(response, endTag)
		if start == -1 || end == -1 || end < start {
			return ""
		}
		val := strings.TrimSpace(response[start+len(startTag):end])
		c.logger.Info(fmt.Sprintf("Extracted <%s>: %s", tag, val))
		return val
	}

	extractYAML := func(tag string, out interface{}) bool {
		content := extractText(tag)
		if content == "" {
			return false
		}
		err := goyaml.Unmarshal([]byte(content), out)
		if err != nil {
			c.logger.Info(fmt.Sprintf("Failed to parse <%s> as YAML: %v", tag, err))
			return false
		}
		c.logger.Info(fmt.Sprintf("Parsed <%s> as YAML: %+v", tag, out))
		return true
	}

	// Scalars
	analysis.Summary = extractText("summary")
	if v := extractText("technical_score"); v != "" {
		fmt.Sscanf(v, "%f", &analysis.TechnicalScore)
		c.logger.Info(fmt.Sprintf("Parsed <technical_score>: %f", analysis.TechnicalScore))
	}
	if v := extractText("viral_potential"); v != "" {
		fmt.Sscanf(v, "%f", &analysis.ViralPotential)
		c.logger.Info(fmt.Sprintf("Parsed <viral_potential>: %f", analysis.ViralPotential))
	}
	analysis.TargetAudience = extractText("target_audience")
	analysis.TechnicalAccuracy = extractText("technical_accuracy_assessment")
	analysis.EducationalValue = extractText("educational_value")
	analysis.CodeQuality = extractText("code_quality_assessment")
	analysis.DeveloperRelevance = extractText("developer_relevance")

	// Lists/objects
	var techs []string
	if extractYAML("technologies", &techs) {
		analysis.Technologies = techs
	}
	var timestamps []models.Timestamp
	if extractYAML("timestamps", &timestamps) {
		analysis.KeyTimestamps = timestamps
	}
	var smTips []string
	if extractYAML("social_media_optimization", &smTips) {
		analysis.SocialMediaTips = smTips
	}
	var platforms map[string]string
	if extractYAML("platform_recommendations", &platforms) {
		analysis.PlatformRecommendations = platforms
	}

	// Only use fallback parsing if XML parsing failed to extract data
	if analysis.Summary == "" {
		c.logger.Info("Summary not found in XML tags, using fallback parsing")
		lines := strings.Split(response, "\n")
		for _, line := range lines {
			line = strings.TrimSpace(line)
			if len(line) > 50 && !strings.HasPrefix(line, "#") {
				analysis.Summary = line
				if len(analysis.Summary) > 500 {
					analysis.Summary = analysis.Summary[:500] + "..."
				}
				c.logger.Info("Fallback summary: " + analysis.Summary)
				break
			}
		}
	}
	if len(analysis.Technologies) == 0 {
		c.logger.Info("Technologies not found in XML tags, using fallback parsing")
		analysis.Technologies = c.extractTechnologies(response)
		c.logger.Info(fmt.Sprintf("Fallback technologies: %+v", analysis.Technologies))
	}
	if len(analysis.KeyTimestamps) == 0 {
		c.logger.Info("Timestamps not found in XML tags, using fallback parsing")
		analysis.KeyTimestamps = c.extractTimestamps(response)
		c.logger.Info(fmt.Sprintf("Fallback timestamps: %+v", analysis.KeyTimestamps))
	}
	if analysis.TechnicalScore == 0 {
		c.logger.Info("Technical score not found in XML tags, using fallback parsing")
		analysis.TechnicalScore = c.extractScore(response, []string{"technical accuracy", "technical score", "accuracy"})
		c.logger.Info(fmt.Sprintf("Fallback technical_score: %f", analysis.TechnicalScore))
	}
	if analysis.ViralPotential == 0 {
		c.logger.Info("Viral potential not found in XML tags, using fallback parsing")
		analysis.ViralPotential = c.extractScore(response, []string{"viral potential", "viral score", "engagement potential"})
		c.logger.Info(fmt.Sprintf("Fallback viral_potential: %f", analysis.ViralPotential))
	}
	if analysis.TargetAudience == "" {
		c.logger.Info("Target audience not found in XML tags, using fallback parsing")
		analysis.TargetAudience = c.extractTargetAudience(response)
		c.logger.Info("Fallback target_audience: " + analysis.TargetAudience)
	}
	if len(analysis.SocialMediaTips) == 0 {
		c.logger.Info("Social media tips not found in XML tags, using fallback parsing")
		analysis.SocialMediaTips = c.extractSocialMediaTips(response)
		c.logger.Info(fmt.Sprintf("Fallback social_media_optimization: %+v", analysis.SocialMediaTips))
	}
	if len(analysis.PlatformRecommendations) == 0 {
		c.logger.Info("Platform recommendations not found in XML tags, using fallback parsing")
		analysis.PlatformRecommendations = c.extractPlatformRecommendations(response)
		c.logger.Info(fmt.Sprintf("Fallback platform_recommendations: %+v", analysis.PlatformRecommendations))
	}

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
	responseLower := strings.ToLower(response)
	
	// Look for patterns like "Score: 8/10", "Rating: 7.5", "8.5/10", "Score of 9"
	for _, keyword := range keywords {
		keywordLower := strings.ToLower(keyword)
		if idx := strings.Index(responseLower, keywordLower); idx != -1 {
			// Look for numbers after the keyword within the next 50 characters
			searchText := responseLower[idx:min(len(responseLower), idx+50)]
			
			// Regex patterns to match various score formats
			patterns := []string{
				`(\d+\.?\d*)/10`,     // "8.5/10"
				`(\d+\.?\d*) ?/ ?10`, // "8.5 / 10"
				`(\d+\.?\d*)`,        // Just a number
			}
			
			for _, pattern := range patterns {
				if matches := regexp.MustCompile(pattern).FindStringSubmatch(searchText); len(matches) > 1 {
					if score, err := strconv.ParseFloat(matches[1], 64); err == nil {
						// If it's a /10 score, use as is; otherwise normalize to 10
						if strings.Contains(searchText, "/10") || score <= 10 {
							return score
						} else if score <= 100 {
							return score / 10 // Convert percentage to 10-point scale
						}
						return score
					}
				}
			}
		}
	}
	return 0
}

// extractTimestamps extracts timestamp information from the response
func (c *Client) extractTimestamps(response string) []models.Timestamp {
	var timestamps []models.Timestamp
	
	// Look for timestamp patterns like "01:23", "1:23", "12:34:56"
	timestampRegex := regexp.MustCompile(`(?m)^.*?(\d{1,2}:\d{2}(?::\d{2})?)\s*[-:]?\s*(.+)$`)
	lines := strings.Split(response, "\n")
	
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if matches := timestampRegex.FindStringSubmatch(line); len(matches) >= 3 {
			timestamp := strings.TrimSpace(matches[1])
			description := strings.TrimSpace(matches[2])
			
			// Skip if description is too short or looks like metadata
			if len(description) < 10 || strings.HasPrefix(description, "##") {
				continue
			}
			
			// Determine importance and type based on keywords
			importance := "medium"
			timestampType := "general"
			
			descLower := strings.ToLower(description)
			if strings.Contains(descLower, "key") || strings.Contains(descLower, "important") || 
			   strings.Contains(descLower, "critical") || strings.Contains(descLower, "highlight") {
				importance = "high"
			}
			
			if strings.Contains(descLower, "code") || strings.Contains(descLower, "technical") ||
			   strings.Contains(descLower, "implementation") || strings.Contains(descLower, "demo") {
				timestampType = "technical"
			} else if strings.Contains(descLower, "introduction") || strings.Contains(descLower, "conclusion") ||
			          strings.Contains(descLower, "summary") {
				timestampType = "structural"
			} else if strings.Contains(descLower, "engagement") || strings.Contains(descLower, "hook") ||
			          strings.Contains(descLower, "viral") {
				timestampType = "engagement"
			}
			
			timestamps = append(timestamps, models.Timestamp{
				Time:        timestamp,
				Description: description,
				Importance:  importance,
				Type:        timestampType,
			})
		}
	}
	
	// If no timestamps found, try alternative parsing
	if len(timestamps) == 0 {
		// Look for bullet points with timestamps
		bulletRegex := regexp.MustCompile(`(?m)^\s*[-*•]\s*(?:time:\s*"?(\d{1,2}:\d{2}(?::\d{2})?)"?|(\d{1,2}:\d{2}(?::\d{2})?))\s*[-:]?\s*(.+)$`)
		for _, line := range lines {
			if matches := bulletRegex.FindStringSubmatch(line); len(matches) >= 4 {
				timestamp := matches[1]
				if timestamp == "" {
					timestamp = matches[2]
				}
				description := strings.TrimSpace(matches[3])
				
				if len(description) > 5 {
					timestamps = append(timestamps, models.Timestamp{
						Time:        timestamp,
						Description: description,
						Importance:  "medium",
						Type:        "general",
					})
				}
			}
		}
	}
	
	return timestamps
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
	var tips []string
	lines := strings.Split(response, "\n")
	
	inSocialMediaSection := false
	for _, line := range lines {
		line = strings.TrimSpace(line)
		
		// Look for social media optimization section
		if strings.Contains(strings.ToLower(line), "social media") && 
		   (strings.Contains(strings.ToLower(line), "optimization") || 
		    strings.Contains(strings.ToLower(line), "tips") ||
		    strings.Contains(strings.ToLower(line), "recommendations")) {
			inSocialMediaSection = true
			continue
		}
		
		// Stop if we hit another major section
		if strings.HasPrefix(line, "##") || strings.HasPrefix(line, "#") {
			if inSocialMediaSection && !strings.Contains(strings.ToLower(line), "social media") {
				inSocialMediaSection = false
			}
		}
		
		// Extract bullet points in the social media section
		if inSocialMediaSection && (strings.HasPrefix(line, "-") || strings.HasPrefix(line, "*") || strings.HasPrefix(line, "•")) {
			tip := strings.TrimSpace(strings.TrimPrefix(strings.TrimPrefix(strings.TrimPrefix(line, "-"), "*"), "•"))
			if len(tip) > 10 {
				tips = append(tips, tip)
			}
		}
	}
	
	// Fallback: look for any bullet points that seem like social media tips
	if len(tips) == 0 {
		for _, line := range lines {
			line = strings.TrimSpace(line)
			if strings.HasPrefix(line, "-") || strings.HasPrefix(line, "*") || strings.HasPrefix(line, "•") {
				tip := strings.TrimSpace(strings.TrimPrefix(strings.TrimPrefix(strings.TrimPrefix(line, "-"), "*"), "•"))
				tipLower := strings.ToLower(tip)
				if (strings.Contains(tipLower, "clip") || strings.Contains(tipLower, "hashtag") ||
				    strings.Contains(tipLower, "post") || strings.Contains(tipLower, "share") ||
				    strings.Contains(tipLower, "platform") || strings.Contains(tipLower, "engagement")) && len(tip) > 10 {
					tips = append(tips, tip)
				}
			}
		}
	}
	
	return tips
}

// extractPlatformRecommendations extracts platform-specific recommendations
func (c *Client) extractPlatformRecommendations(response string) map[string]string {
	recommendations := make(map[string]string)
	lines := strings.Split(response, "\n")
	
	inPlatformSection := false
	for _, line := range lines {
		line = strings.TrimSpace(line)
		
		// Look for platform recommendations section
		if strings.Contains(strings.ToLower(line), "platform") && 
		   (strings.Contains(strings.ToLower(line), "recommendation") || 
		    strings.Contains(strings.ToLower(line), "specific") ||
		    strings.Contains(strings.ToLower(line), "strategy")) {
			inPlatformSection = true
			continue
		}
		
		// Stop if we hit another major section
		if strings.HasPrefix(line, "##") || strings.HasPrefix(line, "#") {
			if inPlatformSection && !strings.Contains(strings.ToLower(line), "platform") {
				inPlatformSection = false
			}
		}
		
		// Extract platform-specific recommendations
		if inPlatformSection || strings.Contains(strings.ToLower(line), ":") {
			platforms := []string{"twitter", "linkedin", "youtube", "tiktok", "reddit", "instagram", "facebook", "discord", "slack"}
			lineLower := strings.ToLower(line)
			
			for _, platform := range platforms {
				if strings.Contains(lineLower, platform) && strings.Contains(line, ":") {
					parts := strings.SplitN(line, ":", 2)
					if len(parts) == 2 {
						recommendation := strings.TrimSpace(parts[1])
						if len(recommendation) > 10 {
							recommendations[platform] = recommendation
						}
					}
				}
			}
		}
	}
	
	// Fallback: look for any lines that mention platforms with recommendations
	if len(recommendations) == 0 {
		platforms := []string{"twitter", "linkedin", "youtube", "tiktok", "reddit", "instagram"}
		for _, line := range lines {
			lineLower := strings.ToLower(line)
			for _, platform := range platforms {
				if strings.Contains(lineLower, platform) && len(line) > 20 {
					// Extract the recommendation part
					if colonIdx := strings.Index(line, ":"); colonIdx != -1 && colonIdx < len(line)-1 {
						recommendation := strings.TrimSpace(line[colonIdx+1:])
						if len(recommendation) > 10 {
							recommendations[platform] = recommendation
						}
					} else {
						recommendations[platform] = strings.TrimSpace(line)
					}
				}
			}
		}
	}
	
	return recommendations
}

