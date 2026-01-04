package gemini

import (
	"github.com/user/youtube-analyzer-go/pkg/models"
	"testing"
)

func TestExtractYAMLBlock(t *testing.T) {
	client := &Client{
		logger: nil, // Use nil logger for testing
	}

	mockResponse := `This is a detailed analysis of the video content.

## Summary
The video provides excellent insights into modern web development practices.

## Key Topics
- React development
- State management
- Performance optimization

## Analysis

Here are the key findings from the video analysis.

` + "```yaml" + `
# structured_data
topics: ["React", "JavaScript", "Web Development"]
engagement_score: 8.5
viral_potential: 7.2
target_audience: "Web developers and React enthusiasts"
technical_accuracy: "High quality technical content"
educational_value: "Clear explanations with practical examples"
code_quality: "Well-structured and modern code examples"
developer_relevance: "Highly relevant to modern web development"
timestamps:
  - time: "01:30"
    description: "Introduction to React hooks"
    importance: "high"
    type: "technical"
  - time: "05:45"
    description: "State management best practices"
    importance: "high"
    type: "educational"
social_media_tips:
  - "Create short clips demonstrating React hooks"
  - "Share code snippets on Twitter with explanations"
  - "Post developer insights on LinkedIn"
platform_recommendations:
  twitter: "Share quick tips and code snippets"
  linkedin: "Post detailed technical insights"
  youtube: "Create tutorial series based on key concepts"
  tiktok: "Quick coding demos and tips"
  reddit: "Share in r/reactjs and r/webdev"
  instagram: "Visual code examples and developer tips"
` + "```" + `

This concludes the analysis.`

	yamlBlock := client.extractYAMLBlock(mockResponse)
	if yamlBlock == "" {
		t.Error("Expected YAML block to be extracted, got empty string")
	}

	// Test the structured data extraction
	structuredData, err := client.extractStructuredData(mockResponse)
	if err != nil {
		t.Errorf("Expected successful structured data extraction, got error: %v", err)
	}

	if structuredData == nil {
		t.Error("Expected structured data, got nil")
		return
	}

	// Test specific fields
	if len(structuredData.Topics) == 0 {
		t.Error("Expected topics to be extracted")
	}

	if structuredData.EngagementScore != 8.5 {
		t.Errorf("Expected engagement score 8.5, got %f", structuredData.EngagementScore)
	}

	if structuredData.ViralPotential != 7.2 {
		t.Errorf("Expected viral potential 7.2, got %f", structuredData.ViralPotential)
	}

	if len(structuredData.Timestamps) == 0 {
		t.Error("Expected timestamps to be extracted")
	}

	if len(structuredData.SocialMediaTips) == 0 {
		t.Error("Expected social media tips to be extracted")
	}

	if len(structuredData.PlatformRecommendations) == 0 {
		t.Error("Expected platform recommendations to be extracted")
	}
}

func TestExtractYAMLBlockNotFound(t *testing.T) {
	client := &Client{
		logger: nil,
	}

	mockResponse := `This is a response without YAML block.

## Summary
The video provides excellent insights.

## Key Topics
- Topic 1
- Topic 2

No structured data here.`

	yamlBlock := client.extractYAMLBlock(mockResponse)
	if yamlBlock != "" {
		t.Error("Expected empty YAML block, got:", yamlBlock)
	}
}

func TestPopulateWithFallbackData(t *testing.T) {
	client := &Client{
		logger: nil,
	}

	analysis := &models.TechnicalAnalysis{}
	client.populateWithFallbackData(analysis)

	if len(analysis.Technologies) == 0 {
		t.Error("Expected fallback technologies to be populated")
	}

	if analysis.TechnicalScore == 0 {
		t.Error("Expected fallback technical score to be populated")
	}

	if analysis.ViralPotential == 0 {
		t.Error("Expected fallback viral potential to be populated")
	}

	if analysis.TargetAudience == "" {
		t.Error("Expected fallback target audience to be populated")
	}

	if len(analysis.KeyTimestamps) == 0 {
		t.Error("Expected fallback timestamps to be populated")
	}

	if len(analysis.SocialMediaTips) == 0 {
		t.Error("Expected fallback social media tips to be populated")
	}

	if len(analysis.PlatformRecommendations) == 0 {
		t.Error("Expected fallback platform recommendations to be populated")
	}
}
