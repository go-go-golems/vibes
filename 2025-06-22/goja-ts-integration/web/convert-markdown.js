const fs = require('fs');
const path = require('path');
const marked = require('marked');
const hljs = require('highlight.js');

// Configure marked with highlight.js for code syntax highlighting
marked.setOptions({
  highlight: function(code, lang) {
    if (lang && hljs.getLanguage(lang)) {
      return hljs.highlight(code, { language: lang }).value;
    }
    return hljs.highlightAuto(code).value;
  },
  langPrefix: 'hljs language-',
  gfm: true,
  breaks: false,
  pedantic: false,
  smartLists: true,
  smartypants: true
});

// HTML template for the implementation guide
const implementationGuideTemplate = (content) => `
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Implementation Guide - TypeScript in Go with Goja</title>
    <link rel="stylesheet" href="styles.css">
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/styles/atom-one-dark.min.css">
    <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/highlight.min.js"></script>
</head>
<body>
    <header>
        <div class="container">
            <h1>TypeScript in Go with Goja</h1>
            <p class="subtitle">Implementation Guide</p>
        </div>
    </header>

    <nav>
        <div class="container">
            <ul>
                <li><a href="index.html">Home</a></li>
                <li><a href="implementation-guide.html">Implementation Guide</a></li>
                <li><a href="developer-walkthrough.html">Developer Walkthrough</a></li>
                <li><a href="index.html#download">Download</a></li>
            </ul>
        </div>
    </nav>

    <main>
        <div class="documentation container">
            <div class="markdown-content">
                ${content}
            </div>
            <a href="index.html" class="back-to-home button">Back to Home</a>
        </div>
    </main>

    <footer>
        <div class="container">
            <p>&copy; 2025 TypeScript-in-Go Integration Project</p>
        </div>
    </footer>
    
    <script>
        document.addEventListener('DOMContentLoaded', (event) => {
            document.querySelectorAll('pre code').forEach((block) => {
                hljs.highlightBlock(block);
            });
        });
    </script>
</body>
</html>
`;

// HTML template for the developer walkthrough
const developerWalkthroughTemplate = (content) => `
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Developer Walkthrough - TypeScript in Go with Goja</title>
    <link rel="stylesheet" href="styles.css">
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/styles/atom-one-dark.min.css">
    <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/highlight.min.js"></script>
</head>
<body>
    <header>
        <div class="container">
            <h1>TypeScript in Go with Goja</h1>
            <p class="subtitle">Developer Walkthrough</p>
        </div>
    </header>

    <nav>
        <div class="container">
            <ul>
                <li><a href="index.html">Home</a></li>
                <li><a href="implementation-guide.html">Implementation Guide</a></li>
                <li><a href="developer-walkthrough.html">Developer Walkthrough</a></li>
                <li><a href="index.html#download">Download</a></li>
            </ul>
        </div>
    </nav>

    <main>
        <div class="documentation container">
            <div class="markdown-content">
                ${content}
            </div>
            <a href="index.html" class="back-to-home button">Back to Home</a>
        </div>
    </main>

    <footer>
        <div class="container">
            <p>&copy; 2025 TypeScript-in-Go Integration Project</p>
        </div>
    </footer>
    
    <script>
        document.addEventListener('DOMContentLoaded', (event) => {
            document.querySelectorAll('pre code').forEach((block) => {
                hljs.highlightBlock(block);
            });
        });
    </script>
</body>
</html>
`;

// Convert implementation guide
const implementationGuidePath = path.join(__dirname, 'implementation-guide.md');
const implementationGuideContent = fs.readFileSync(implementationGuidePath, 'utf8');
const implementationGuideHtml = marked.parse(implementationGuideContent);
const implementationGuideFullHtml = implementationGuideTemplate(implementationGuideHtml);
fs.writeFileSync(path.join(__dirname, 'implementation-guide.html'), implementationGuideFullHtml);
console.log('Implementation guide converted to HTML');

// Convert developer walkthrough
const developerWalkthroughPath = path.join(__dirname, 'developer-walkthrough.md');
const developerWalkthroughContent = fs.readFileSync(developerWalkthroughPath, 'utf8');
const developerWalkthroughHtml = marked.parse(developerWalkthroughContent);
const developerWalkthroughFullHtml = developerWalkthroughTemplate(developerWalkthroughHtml);
fs.writeFileSync(path.join(__dirname, 'developer-walkthrough.html'), developerWalkthroughFullHtml);
console.log('Developer walkthrough converted to HTML');
