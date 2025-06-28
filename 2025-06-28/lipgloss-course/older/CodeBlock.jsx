import { useState } from 'react'

const CodeBlock = ({ code, language = 'go', title }) => {
  const [copied, setCopied] = useState(false)

  const copyToClipboard = async () => {
    try {
      await navigator.clipboard.writeText(code)
      setCopied(true)
      setTimeout(() => setCopied(false), 2000)
    } catch (err) {
      console.error('Failed to copy code:', err)
    }
  }

  return (
    <div className="code-block">
      <div className="code-header">
        <span className="code-title">{title || `${language.toUpperCase()} Code`}</span>
        <button 
          className="copy-button"
          onClick={copyToClipboard}
          title="Copy to clipboard"
        >
          {copied ? '✅ Copied!' : '📋 Copy'}
        </button>
      </div>
      <pre className="code-content">
        <code className={`language-${language}`}>
          {code}
        </code>
      </pre>
    </div>
  )
}

export default CodeBlock

