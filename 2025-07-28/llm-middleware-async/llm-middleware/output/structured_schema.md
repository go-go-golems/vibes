# Prompt Rendering Demo - Turn Analysis

**Generated:** 2025-07-28T09:37:55-04:00

## Messages

### System Message 1

Please respond with valid JSON matching this schema: {
		"analysis": "string",
		"confidence": "number",
		"recommendations": ["string"]
	}

### User Message 2

Analyze this data and provide structured output

## Turn Information

- **Turn Index:** 0
- **Total Messages:** 2
- **Context Keys:** 4
- **Output Keys:** 1

### Context

- **schema_output_key:** structured_output
- **schema_error:** invalid character 'I' looking for beginning of value
- **schema_valid:** false
- **schema_text:** {
		"analysis": "string",
		"confidence": "number",
		"recommendations": ["string"]
	}

### Output

- **raw:** I understand your request. This is a mock response for testing the middleware architecture.
