# 🎬 AI Video Segmenter

Transform your stream videos into viral social media content with advanced AI analysis and chain-of-thought reasoning.

## 🚀 Live Demo

- **Application Demo**: [https://aeamvntk.manus.space](https://aeamvntk.manus.space)
- **Documentation Website**: [https://eseiopcp.manus.space](https://eseiopcp.manus.space)

## 📋 Overview

AI Video Segmenter is a sophisticated application that uses Google Gemini AI with advanced prompting techniques to analyze stream videos and automatically identify the most engaging segments for social media content creation. The system employs chain-of-thought reasoning to evaluate visual appeal, emotional content, and viral potential.

### Key Features

- 🧠 **Advanced AI Analysis**: Chain-of-thought reasoning with Google Gemini
- 🎯 **Social Media Optimization**: Platform-specific recommendations
- 📊 **Viral Potential Scoring**: Proprietary engagement prediction
- 🎨 **Modern Interface**: React-based responsive design
- ⚡ **Real-Time Processing**: Fast analysis with progress tracking
- 📈 **Strategic Planning**: Comprehensive content strategies

## 🏗️ Architecture

```
ai-video-segmenter/
├── video_segmenter_api/          # Flask Backend
│   ├── src/
│   │   ├── routes/video.py       # Video processing API
│   │   ├── agentic_analyzer.py   # AI analysis engine
│   │   ├── video_processor.py    # Video processing utilities
│   │   └── main.py              # Flask application
│   ├── venv/                    # Python virtual environment
│   └── requirements.txt         # Python dependencies
├── video_segmenter_ui/           # React Frontend
│   ├── src/
│   │   ├── App.jsx              # Main application
│   │   └── components/ui/       # UI components
│   ├── dist/                    # Built application
│   └── package.json             # Node.js dependencies
├── video_segmenter_website/      # Documentation Website
│   ├── index.html               # Main documentation
│   ├── css/style.css           # Website styles
│   └── js/script.js            # Interactive features
└── README.md                    # This file
```

## 🛠️ Technology Stack

### Backend
- **Flask**: Python web framework
- **Google Gemini AI**: Advanced language model
- **OpenCV**: Computer vision library
- **FFmpeg**: Video processing
- **Python 3.11**: Core language

### Frontend
- **React 18**: User interface framework
- **Vite**: Build tool and dev server
- **Tailwind CSS**: Utility-first CSS framework
- **shadcn/ui**: Component library
- **Lucide Icons**: Icon library

### AI & Analysis
- **Chain-of-Thought Prompting**: Advanced reasoning techniques
- **Agentic Architecture**: Self-improving analysis system
- **Multi-step Reasoning**: Complex decision making
- **Strategic Planning**: Comprehensive content strategies

## 🚀 Quick Start

### Prerequisites

- Python 3.11+
- Node.js 20+
- Google Gemini API key
- FFmpeg installed

### Backend Setup

```bash
cd video_segmenter_api
source venv/bin/activate
pip install -r requirements.txt
python src/main.py
```

The backend will start on `http://localhost:5000`

### Frontend Setup

```bash
cd video_segmenter_ui
pnpm install
pnpm run dev
```

The frontend will start on `http://localhost:5173`

### Configuration

1. Get a Google Gemini API key from [Google AI Studio](https://makersuite.google.com/app/apikey)
2. Enter the API key in the frontend interface
3. Upload a video file and start analyzing!

## 🎯 How It Works

### 1. Video Upload
- Supports MP4, AVI, MOV, MKV, WebM formats
- Up to 500MB file size
- Automatic metadata extraction

### 2. AI Analysis
The system uses a sophisticated multi-step analysis process:

#### Frame Analysis
- **Visual Observation**: Scene understanding and content description
- **Emotional Analysis**: Emotion detection and viewer response prediction
- **Engagement Evaluation**: Visual appeal and attention-grabbing elements
- **Technical Assessment**: Quality evaluation for platform optimization
- **Viral Potential**: Comprehensive scoring with reasoning
- **Self-Reflection**: Confidence calibration and analysis validation

#### Sequence Analysis
- **Content Classification**: Narrative structure and flow analysis
- **Platform Optimization**: Specific recommendations for each social platform
- **Viral Mechanics**: Identification of engagement-driving elements
- **Audience Analysis**: Target demographic and interest alignment
- **Competitive Positioning**: Market context and differentiation

#### Strategic Planning
- **Content Strategy**: Priority segments and messaging approach
- **Platform Strategy**: Optimal platform selection and adaptation
- **Performance Framework**: Success metrics and risk assessment
- **Resource Planning**: Editing requirements and team needs

### 3. Results & Recommendations
- Interactive tabbed interface showing analysis results
- Best frames for social media with viral potential scores
- Segment analysis with platform-specific recommendations
- Comprehensive strategic plan for content optimization

## 🔌 API Endpoints

### Video Processing
- `POST /api/video/upload` - Upload video files
- `POST /api/video/analyze` - Perform AI analysis
- `POST /api/video/segments` - Create video segments
- `GET /api/video/frames/{file_id}/{filename}` - Retrieve frames
- `GET /api/video/results/{file_id}` - Get analysis results

### Analysis Modes
- **Quick Mode**: Fast analysis for immediate feedback
- **Full Mode**: Comprehensive analysis with detailed insights

## 🎨 Implementation Highlights

### Advanced AI Integration
- **Chain-of-Thought Reasoning**: Multi-step logical analysis
- **Agentic Architecture**: Self-improving and self-evaluating
- **Progressive Enhancement**: Quick to comprehensive analysis
- **Error Resilience**: Graceful handling of API limitations

### Video Processing Pipeline
- **Intelligent Frame Sampling**: Optimal interval extraction
- **Scene Change Detection**: Histogram-based analysis
- **Parallel Processing**: Efficient multi-task execution
- **Memory Optimization**: Efficient resource utilization

### Modern Frontend
- **Component Architecture**: Modular and maintainable
- **Real-time Updates**: Progress tracking and status display
- **Responsive Design**: Works on all device types
- **Interactive Visualization**: Complex data made accessible

## 🔮 Future Extensions

### High Priority
- **Automated Video Editing**: Generate clips automatically
- **Multi-Language Support**: Global content analysis
- **Advanced Analytics**: Performance tracking dashboard
- **Custom AI Training**: Domain-specific model fine-tuning

### Medium Priority
- **Social Media Integration**: Direct platform publishing
- **Real-Time Analysis**: Live stream processing
- **Batch Processing**: Multiple video handling
- **Team Collaboration**: Multi-user workflows

### Technical Enhancements
- **GPU Acceleration**: Faster processing
- **Kubernetes Deployment**: Auto-scaling infrastructure
- **Advanced AI Models**: Transformer-based analysis
- **Enterprise Features**: Team management and compliance

## 📊 Performance Metrics

### Analysis Capabilities
- **Frame Analysis**: 1-10 scoring across multiple dimensions
- **Viral Potential**: Comprehensive engagement prediction
- **Platform Optimization**: Specific recommendations for TikTok, Instagram, YouTube, Twitter
- **Strategic Planning**: Multi-faceted content strategy generation

### Technical Performance
- **Processing Speed**: Minutes for hours of content
- **Accuracy**: High-quality AI analysis with reasoning
- **Scalability**: Designed for horizontal scaling
- **Reliability**: Robust error handling and recovery

## 🤝 Contributing

This project demonstrates advanced AI integration techniques and modern web development practices. Key areas for contribution:

1. **AI Enhancement**: Improve prompting techniques and analysis depth
2. **Performance Optimization**: Faster processing and better resource usage
3. **Feature Extensions**: New analysis capabilities and integrations
4. **UI/UX Improvements**: Enhanced user experience and accessibility

## 📄 License

This project is provided as a demonstration of advanced AI integration and modern web development techniques. Feel free to use, modify, and extend for your own projects.

## 🙏 Acknowledgments

- **Google Gemini AI**: Advanced language model capabilities
- **OpenCV Community**: Computer vision tools
- **React Team**: Excellent frontend framework
- **Flask Community**: Lightweight and flexible backend framework

## 📞 Support

For questions, issues, or feature requests:
- Check the [documentation website](https://eseiopcp.manus.space)
- Try the [live demo](https://aeamvntk.manus.space)
- Review the comprehensive implementation details

---

**Built with ❤️ using React, Flask, and Google Gemini AI**

*Transform your content creation with artificial intelligence*

