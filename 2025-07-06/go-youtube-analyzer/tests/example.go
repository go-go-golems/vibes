package main

import (
	"context"
	"flag"
	"fmt"
	"os"
	"strings"

	genai "google.golang.org/genai"
)

func main() {
	var (
		apiKey   = flag.String("api-key", "", "Google Gemini API key (or set GEMINI_API_KEY env var)")
		videoURL = flag.String("video", "https://www.youtube.com/watch?v=3KtWfp0UopM", "YouTube video URL")
		model    = flag.String("model", "gemini-2.5-flash", "Gemini model to use")
		prompt   = flag.String("prompt", "Write a short and engaging blog post based on this video.", "Prompt for analysis")
	)
	flag.Parse()

	// Use environment variable as fallback
	if *apiKey == "" {
		*apiKey = os.Getenv("GEMINI_API_KEY")
	}

	// Validate required API key
	if *apiKey == "" {
		fmt.Fprintf(os.Stderr, "Error: API key is required. Provide via --api-key flag or GEMINI_API_KEY environment variable\n")
		flag.Usage()
		os.Exit(1)
	}

	if !strings.HasPrefix(*apiKey, "AIza") {
		fmt.Fprintf(os.Stderr, "Error: Invalid API key format (should start with 'AIza')\n")
		os.Exit(1)
	}

	// Run the video analysis
	if err := generateWithYTVideo(os.Stdout, *apiKey, *videoURL, *model, *prompt); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

// generateWithYTVideo shows how to generate text using a YouTube video as input.
func generateWithYTVideo(w *os.File, apiKey, videoURL, modelName, prompt string) error {
	ctx := context.Background()

	// Create client with API key
	client, err := genai.NewClient(ctx, &genai.ClientConfig{
		APIKey:  apiKey,
		Backend: genai.BackendGeminiAPI,
	})
	if err != nil {
		return fmt.Errorf("failed to create genai client: %w", err)
	}

	// Print request info
	fmt.Fprintf(os.Stderr, "🎬 Analyzing YouTube video with Gemini\n")
	fmt.Fprintf(os.Stderr, "📺 Video: %s\n", videoURL)
	fmt.Fprintf(os.Stderr, "🤖 Model: %s\n", modelName)
	fmt.Fprintf(os.Stderr, "📝 Prompt: %s\n", prompt)
	fmt.Fprintf(os.Stderr, "🚀 Sending request...\n\n")

	// Create request content
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

	// Make API call
	resp, err := client.Models.GenerateContent(ctx, modelName, contents, nil)
	if err != nil {
		return fmt.Errorf("failed to generate content: %w", err)
	}

	// Get response text
	respText := resp.Text()
	if respText == "" {
		return fmt.Errorf("received empty response from Gemini")
	}

	fmt.Fprintf(os.Stderr, "✅ Received response (%d characters)\n", len(respText))
	fmt.Fprintf(os.Stderr, "%s\n", strings.Repeat("=", 50))

	// Print the response
	fmt.Fprintln(w, respText)

	return nil
}
