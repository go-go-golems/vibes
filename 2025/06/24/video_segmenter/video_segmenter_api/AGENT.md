# Agent Configuration for Technical Video Segmenter API

## Commands
- **Run app**: `python src/main.py` (Flask dev server on port 5000, logs to video_segmenter.log)
- **Install deps**: `pip install -r requirements.txt` (Gemini 2.5 native video processing)
- **No tests configured** - add pytest/unittest setup if needed

## Architecture
- **Framework**: Flask + SQLAlchemy + OpenCV + Google Gemini 2.5 (native video understanding)
- **Database**: SQLite at `src/database/app.db` with User model
- **API Routes**: `/api/*` (users), `/api/video/*` (technical video analysis)
- **Core modules**: `VideoProcessor` (OpenCV analysis), `GeminiVideoAnalyzer` (Gemini 2.5 native processing)
- **File handling**: Temp directories `/tmp/video_*` for uploads, Gemini Files API for processing
- **Video processing**: Gemini 2.5 native video understanding with audio transcription and visual analysis

## Technical Focus
- **Target audience**: Software developers, not general social media
- **Content analysis**: Code snippets, technical concepts, programming languages, development tools
- **Shorts optimization**: Developer-focused platforms (GitHub, Dev.to, LinkedIn tech, YouTube tech)
- **Educational value**: Step-by-step tutorials, problem-solving, best practices, debugging
- **AI Processing**: Gemini 2.5 Flash for comprehensive video+audio analysis

## Code Style
- **Logging**: Comprehensive logging throughout (file + console), use logger.info/warning/error
- **Imports**: Absolute imports from src/, path manipulation for module access
- **Naming**: snake_case for variables/functions, PascalCase for classes
- **Error handling**: Try-catch with JSON error responses, HTTP status codes, exc_info=True for errors
- **File structure**: Blueprint-based routes, separate models/routes/processors
- **Types**: Type hints in processors, technical content focus
- **Config**: Hardcoded secrets (improve for production), 500MB file limit
- **Video formats**: mp4, avi, mov, mkv, webm supported

## Key Dependencies
Flask, OpenCV, Google Generative AI (Gemini 2.5), SQLAlchemy, ffmpeg-python
