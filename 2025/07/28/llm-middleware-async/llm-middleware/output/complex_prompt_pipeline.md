# Prompt Rendering Demo - Turn Analysis

**Generated:** 2025-07-28T09:37:55-04:00

## Messages

### System Message 1

Available personas: analyst, advisor. Current persona: analyst. Use '/persona <name>' to switch.

### System Message 2

Task: Quarterly Analysis | Context: Q3 2024 Performance Review

### User Message 3

Analyze: Sales increased 20%

### Assistant Response 4

Positive trend analysis

### System Message 5

Persona: analyst - You are a data analyst. Provide objective, data-driven insights.

### System Message 6

Please respond with valid JSON matching this schema: {"summary": "string", "sentiment": "string", "confidence": "number"}

### User Message 7

Analyze the quarterly performance data

Let's analyze this systematically.

## Turn Information

- **Turn Index:** 0
- **Total Messages:** 7
- **Context Keys:** 13
- **Output Keys:** 1

### Context

- **template_applied:** true
- **cot_used:** true
- **schema_text:** {"summary": "string", "sentiment": "string", "confidence": "number"}
- **template_vars:** map[Context:Q3 2024 Performance Review Task:Quarterly Analysis]
- **template_content:** Task: Quarterly Analysis | Context: Q3 2024 Performance Review
- **selected_examples:** [{Analyze: Sales increased 20% Positive trend analysis}]
- **current_persona:** analyst
- **enable_cot:** true
- **injection_mode:** 0
- **examples_injected:** 1
- **schema_error:** invalid character 'I' looking for beginning of value
- **schema_valid:** false
- **schema_output_key:** analysis_result

### Output

- **raw:** I understand your request. This is a mock response for testing the middleware architecture.
