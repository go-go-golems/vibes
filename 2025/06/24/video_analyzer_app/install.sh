#!/bin/bash
# Enhanced Video Analyzer Installation Script

echo "🚀 Installing Enhanced Video Analyzer Web Application..."

# Create virtual environment
echo "📦 Creating virtual environment..."
python3 -m venv venv
source venv/bin/activate

# Upgrade pip
echo "⬆️ Upgrading pip..."
pip install --upgrade pip

# Install requirements
echo "📥 Installing requirements..."
pip install -r requirements.txt

# Create necessary directories
echo "📁 Creating directories..."
mkdir -p logs tracking analysis_results

echo "✅ Installation complete!"
echo ""
echo "🎯 To run the application:"
echo "1. Activate virtual environment: source venv/bin/activate"
echo "2. Run main app: python app.py"
echo "3. Or run fallback app: python app_fallback.py"
echo "4. Navigate to http://localhost:5001 (or 5002 for fallback)"
echo ""
echo "📝 Note: If you get 'ModuleNotFoundError: No module named google.genai':"
echo "   - Try the fallback version: python app_fallback.py"
echo "   - Or install manually: pip install google-generativeai"

