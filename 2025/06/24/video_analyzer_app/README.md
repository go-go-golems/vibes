# 🎬 Enhanced AI Video Analyzer Web Application

A professional Flask web application that provides real-time AI-powered video analysis using Google's Gemini models, specifically designed for technical developer content.

## ✨ Features

- **🎯 Technical Content Focus**: Specialized prompts for analyzing developer/programming content
- **⚡ Real-time Step Tracking**: Live progress updates using Server-Sent Events (SSE)
- **🎨 Professional UI**: Bootstrap 5.3 responsive design with custom styling
- **🔧 Comprehensive Logging**: Detailed step-by-step analysis tracking
- **📊 Progress Visualization**: Real-time counters and status updates
- **🔐 Secure API Handling**: Masked API keys and input validation
- **📱 Mobile Responsive**: Works on desktop, tablet, and mobile devices

## 🚀 Quick Start

### Option 1: Automated Installation
```bash
./install.sh
source venv/bin/activate
python app.py
```

### Option 2: Manual Installation
```bash
# Create virtual environment
python3 -m venv venv
source venv/bin/activate

# Install dependencies
pip install -r requirements.txt

# Create directories
mkdir -p logs tracking analysis_results

# Run application
python app.py
```

### Option 3: If you get import errors
```bash
# Use the fallback version
python app_fallback.py
```

## 📋 Requirements

- Python 3.8+
- Google Gemini API key
- Internet connection for API calls

## 🔧 Configuration

1. **Get a Gemini API Key**:
   - Visit [Google AI Studio](https://makersuite.google.com/app/apikey)
   - Create a new API key
   - Copy the key for use in the application

2. **Run the Application**:
   - Navigate to `http://localhost:5001` (or `5002` for fallback)
   - Enter your YouTube video URL
   - Paste your Gemini API key
   - Select analysis mode (Quick/Comprehensive)
   - Click "Start Analysis"

## 📁 Project Structure

```
video_analyzer_app/
├── app.py                    # Main Flask application (google-genai)
├── app_fallback.py          # Fallback version (google-generativeai)
├── requirements.txt         # Python dependencies
├── install.sh              # Installation script
├── README.md               # This file
├── templates/
│   └── index.html          # Bootstrap UI template
├── static/
│   ├── css/style.css       # Custom styling
│   └── js/app.js          # Frontend JavaScript with SSE
├── logs/                   # Application logs
├── tracking/              # Real-time step tracking files
└── analysis_results/      # Analysis output storage
```

## 🎯 How It Works

1. **Video Input**: Enter a YouTube URL for analysis
2. **API Configuration**: Provide your Gemini API key
3. **Analysis Mode**: Choose between Quick or Comprehensive analysis
4. **Real-time Tracking**: Watch live progress in the Step Tracking panel
5. **Results**: Get detailed technical analysis focused on developer content

## 📊 Analysis Features

### Technical Content Assessment
- Programming languages and frameworks identification
- Code quality and best practices evaluation
- Technical accuracy assessment
- Educational value for developers

### Developer Audience Analysis
- Target skill level determination
- Specific developer role targeting
- Technical concept complexity analysis

### Social Media Optimization
- Viral potential in developer communities
- Key engagement moments identification
- Platform-specific recommendations
- Shareable technical insights

## 🔍 Real-time Features

- **Live Step Tracking**: See each analysis step as it happens
- **Progress Counters**: Steps completed, API calls made, elapsed time
- **Status Updates**: Current operation being performed
- **Session Management**: Unique session IDs for each analysis

## 🛠️ Troubleshooting

### ModuleNotFoundError: No module named 'google.genai'

**Solution 1**: Use the fallback version
```bash
python app_fallback.py
```

**Solution 2**: Install alternative package
```bash
pip install google-generativeai
```

**Solution 3**: Reinstall dependencies
```bash
pip uninstall google-genai google-generativeai
pip install google-generativeai
```

### Port Already in Use

If port 5001 is busy:
```bash
# Edit app.py and change the port
app.run(host='0.0.0.0', port=5003, debug=True, threaded=True)
```

### API Key Issues

- Ensure your API key is valid and has quota remaining
- Check the [Google AI Studio](https://makersuite.google.com/) for usage limits
- Verify the key has access to Gemini models

## 📝 API Endpoints

- `GET /` - Main application interface
- `POST /analyze` - Start video analysis
- `GET /stream` - Server-Sent Events for real-time updates
- `GET /status/<session_id>` - Get session status

## 🎨 UI Components

- **Video Analysis Form**: Input fields for URL, API key, and mode selection
- **Live Step Tracking**: Real-time progress visualization
- **Analysis Progress**: Counters and status display
- **Connection Status**: SSE connection indicator

## 🔒 Security Features

- API key masking in the UI
- Input validation and sanitization
- Secure session management
- Error handling and logging

## 📈 Performance

- Asynchronous analysis processing
- Real-time updates without page refresh
- Efficient step tracking and logging
- Responsive UI with smooth animations

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test thoroughly
5. Submit a pull request

## 📄 License

This project is licensed under the MIT License.

## 🆘 Support

If you encounter issues:

1. Check the logs in the `logs/` directory
2. Verify your API key and quota
3. Try the fallback version (`app_fallback.py`)
4. Ensure all dependencies are installed correctly

## 🎯 Example Usage

```bash
# Start the application
python app.py

# Navigate to http://localhost:5001
# Enter: https://www.youtube.com/watch?v=YOUR_VIDEO_ID
# Enter: Your Gemini API key
# Select: Comprehensive Analysis
# Click: Start Analysis
# Watch: Real-time step tracking in action!
```

---

**Built with ❤️ for the developer community**

