# 🎬 YouTube Analyzer Go

A high-performance CLI tool written in Go that provides AI-powered YouTube video analysis using Google's Gemini models, specifically designed for technical developer content.

## ✨ Features

- **🚀 High Performance**: Fast, concurrent processing with minimal memory footprint
- **🎯 Technical Focus**: Specialized analysis for developer/programming content
- **📊 Comprehensive Logging**: Detailed step-by-step tracking with colored output
- **🔧 Professional CLI**: Built with Cobra framework for excellent UX
- **📱 Cross-Platform**: Single binary for Linux, macOS, and Windows
- **🎨 Beautiful Output**: Colored terminal output with progress bars
- **📄 JSON Export**: Machine-readable results for integration
- **⚙️ Configurable**: Multiple analysis modes and output options

## 🚀 Quick Start

### Installation

#### Option 1: Build from Source
```bash
git clone <repository>
cd youtube-analyzer-go
make build
```

#### Option 2: Download Binary
```bash
# Download the appropriate binary for your platform
wget https://github.com/user/youtube-analyzer-go/releases/latest/download/youtube-analyzer-linux-amd64
chmod +x youtube-analyzer-linux-amd64
sudo mv youtube-analyzer-linux-amd64 /usr/local/bin/youtube-analyzer
```

#### Option 3: Install via Make
```bash
make install
```

### Basic Usage

```bash
# Quick analysis
youtube-analyzer "https://www.youtube.com/watch?v=J3oJqan2Gv8" --api-key YOUR_API_KEY

# Comprehensive analysis with verbose output
youtube-analyzer "https://youtu.be/VIDEO_ID" \
  --api-key YOUR_API_KEY \
  --mode comprehensive \
  --verbose \
  --output detailed_analysis.json

# Quiet mode with JSON output only
youtube-analyzer "https://www.youtube.com/watch?v=VIDEO_ID" \
  --api-key YOUR_API_KEY \
  --quiet \
  --json \
  --output results.json
```

## 📋 Command Line Options

### Required Arguments
- `VIDEO_URL` - YouTube video URL to analyze

### Flags
```
Analysis Options:
  -k, --api-key string      Google Gemini API key (required)
  -m, --mode string         Analysis mode: quick, comprehensive (default "quick")
  -o, --output string       Output file path (default: auto-generated)
      --output-dir string   Output directory (default "./analysis_results")

Output Options:
  -j, --json               Output results in JSON format only
  -v, --verbose            Verbose output with detailed logging
  -q, --quiet              Quiet mode (minimal output)
      --no-color           Disable colored output

Logging Options:
      --log-level string   Log level: debug, info, warn, error (default "info")
      --config string      Config file (default: $HOME/.youtube-analyzer.yaml)

Global Options:
  -h, --help               Show help information
      --version            Show version information
```

## 🎯 Analysis Modes

### Quick Mode (`--mode quick`)
- **Model**: `gemini-2.5-flash`
- **Speed**: ~30-60 seconds
- **Focus**: Core technical assessment and key insights
- **Best for**: Rapid content evaluation and social media optimization

### Comprehensive Mode (`--mode comprehensive`)
- **Model**: `gemini-2.5-pro`
- **Speed**: ~2-5 minutes
- **Focus**: Deep technical analysis with competitive insights
- **Best for**: Detailed content strategy and educational assessment

## 📊 Output Formats

### Terminal Output (Default)
```
🎬 YouTube Analyzer - AI-Powered Technical Video Analysis
============================================================
📺 Video URL: https://www.youtube.com/watch?v=J3oJqan2Gv8
🆔 Session ID: go_20241224_143022
⚙️  Analysis Mode: comprehensive
🚀 Starting analysis...

🎬 Analyzing video... ████████████████████████████████████████ 100% [5/5]

✅ Analysis Complete!
============================================================
📊 Total Steps: 5
🔗 API Calls: 1
⏱️  Duration: 45.32 seconds
📄 Output File: ./analysis_results/analysis_go_20241224_143022.json

📝 Analysis Summary:
----------------------------------------
This video demonstrates building a real-time collaborative text editor using React and WebSockets...

🎯 Technical Score: 8.5/10
🚀 Viral Potential: 7.2/10
```

### JSON Output (`--json` flag)
```json
{
  "session_id": "go_20241224_143022",
  "video_url": "https://www.youtube.com/watch?v=J3oJqan2Gv8",
  "analysis_mode": "comprehensive",
  "model_used": "gemini-2.5-pro",
  "timestamp": "2024-12-24T14:30:22Z",
  "total_steps": 5,
  "api_calls_made": 1,
  "total_time_seconds": 45.32,
  "analysis": {
    "summary": "Comprehensive technical analysis...",
    "technical_score": 8.5,
    "viral_potential": 7.2,
    "target_audience": "Intermediate developers",
    "technologies_identified": ["React", "WebSocket", "Node.js"],
    "key_timestamps": [
      {
        "time": "01:30",
        "description": "WebSocket implementation begins",
        "importance": "high",
        "type": "technical"
      }
    ],
    "social_media_optimization": [
      "Create short clips of key technical moments",
      "Use relevant hashtags for technologies mentioned"
    ],
    "platform_recommendations": {
      "twitter": "Share key insights as threaded tweets",
      "linkedin": "Post professional development insights"
    }
  },
  "steps": [...],
  "api_call_logs": [...]
}
```

## 🔧 Configuration

### Environment Variables
```bash
export YT_ANALYZER_API_KEY="your_gemini_api_key"
export YT_ANALYZER_MODE="comprehensive"
export YT_ANALYZER_OUTPUT_DIR="./my_analysis"
export YT_ANALYZER_LOG_LEVEL="debug"
```

### Config File (`~/.youtube-analyzer.yaml`)
```yaml
api-key: "your_gemini_api_key"
mode: "comprehensive"
output-dir: "./analysis_results"
log-level: "info"
verbose: false
quiet: false
no-color: false
```

## 📁 Project Structure

```
youtube-analyzer-go/
├── main.go                    # Entry point
├── cmd/
│   └── root.go               # CLI commands and flags
├── internal/
│   ├── analyzer/             # Main analysis orchestration
│   ├── config/               # Configuration management
│   └── logger/               # Logging with colors and levels
├── pkg/
│   ├── gemini/               # Gemini API client
│   └── models/               # Data structures
├── bin/                      # Built binaries
├── analysis_results/         # Analysis output files
├── logs/                     # Application logs
├── Makefile                  # Build automation
└── README.md                 # This file
```

## 🛠️ Development

### Prerequisites
- Go 1.21 or later
- Google Gemini API key
- Make (optional, for build automation)

### Building
```bash
# Install dependencies
make deps

# Build for current platform
make build

# Build for all platforms
make build-all

# Run tests
make test

# Format code
make fmt
```

### Testing
```bash
# Run with your API key
API_KEY=your_key make run

# Run comprehensive analysis
API_KEY=your_key make run-comprehensive

# Manual testing
./bin/youtube-analyzer "https://www.youtube.com/watch?v=VIDEO_ID" \
  --api-key YOUR_KEY \
  --verbose \
  --mode comprehensive
```

## 📊 Performance

### Benchmarks
- **Quick Mode**: ~30-60 seconds per video
- **Comprehensive Mode**: ~2-5 minutes per video
- **Memory Usage**: ~50-100MB peak
- **Binary Size**: ~15-20MB (statically linked)

### Optimization Features
- Concurrent processing where possible
- Efficient JSON parsing and generation
- Minimal memory allocations
- Single binary deployment

## 🔍 Troubleshooting

### Common Issues

#### "API key is required"
```bash
# Set API key via flag
youtube-analyzer URL --api-key YOUR_KEY

# Or via environment variable
export YT_ANALYZER_API_KEY="YOUR_KEY"
youtube-analyzer URL
```

#### "Invalid YouTube URL format"
```bash
# Supported formats:
youtube-analyzer "https://www.youtube.com/watch?v=VIDEO_ID"
youtube-analyzer "https://youtu.be/VIDEO_ID"
youtube-analyzer "https://youtube.com/embed/VIDEO_ID"
```

#### "Gemini API call failed"
- Check your API key validity
- Verify you have quota remaining
- Ensure the video is publicly accessible
- Try with `--verbose` flag for detailed error information

### Debug Mode
```bash
youtube-analyzer URL --api-key KEY --log-level debug --verbose
```

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

### Development Guidelines
- Follow Go best practices and idioms
- Add tests for new functionality
- Update documentation for API changes
- Use conventional commit messages

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🆘 Support

- **Issues**: [GitHub Issues](https://github.com/user/youtube-analyzer-go/issues)
- **Discussions**: [GitHub Discussions](https://github.com/user/youtube-analyzer-go/discussions)
- **Documentation**: [Wiki](https://github.com/user/youtube-analyzer-go/wiki)

## 🎯 Examples

### Analyze a React Tutorial
```bash
youtube-analyzer "https://www.youtube.com/watch?v=REACT_VIDEO" \
  --api-key YOUR_KEY \
  --mode comprehensive \
  --output react_analysis.json \
  --verbose
```

### Quick Analysis for Social Media
```bash
youtube-analyzer "https://youtu.be/QUICK_VIDEO" \
  --api-key YOUR_KEY \
  --mode quick \
  --json \
  --quiet > quick_results.json
```

### Batch Analysis Script
```bash
#!/bin/bash
for url in $(cat video_urls.txt); do
  youtube-analyzer "$url" \
    --api-key "$YT_ANALYZER_API_KEY" \
    --mode quick \
    --output-dir "./batch_results" \
    --quiet
done
```

---

**Built with ❤️ in Go for the developer community**

