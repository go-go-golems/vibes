# Quiz DSL Specification

**Version:** 1.0  
**Author:** Manus AI  
**Last Updated:** December 16, 2025

---

## Introduction

The Quiz Domain-Specific Language (DSL) is a YAML-based specification designed to embed interactive quizzes and forms within markdown documents. This specification enables content creators to seamlessly integrate assessments, surveys, and interactive forms into educational materials, documentation, and other text-based content without requiring programming knowledge.

The DSL is embedded within standard markdown documents using HTML-style `<form>` tags. Each form is defined using a simple YAML structure that describes the quiz title, description, and field definitions. The system automatically parses these embedded forms, renders them as interactive components, validates user responses, calculates scores, and stores results in a database.

This document provides a comprehensive reference for the Quiz DSL, including syntax rules, field types, validation mechanisms, scoring logic, and numerous practical examples.

---

## Core Concepts

### Embedding Forms in Markdown

Forms are embedded in markdown documents using the following syntax:

```markdown
<form id="unique-form-id">
YAML content here
</form>
```

The `id` attribute must be unique within the document and serves as the identifier for storing and retrieving quiz submissions. The content between the opening and closing tags must be valid YAML that conforms to the Quiz DSL schema.

### Document Structure

A Quiz DSL document consists of three main components: **metadata**, **field definitions**, and **validation rules**. The metadata provides context about the quiz (title, description), field definitions specify the questions and input types, and validation rules (particularly the `correct` attribute) enable automatic scoring.

### Submission Model

When a user completes a quiz document containing multiple embedded forms, all forms are submitted together with a single action. The system groups responses by form ID, calculates individual scores for each quiz, and presents aggregate results showing both per-quiz performance and overall document score.

---

## Basic Syntax

### Minimal Form Example

The simplest valid form contains at minimum a unique ID and at least one field:

```markdown
<form id="simple-quiz">
fields:
  - name: answer
    label: What is your answer?
    type: text
</form>
```

### Complete Form Structure

A fully-featured form includes metadata and multiple fields:

```markdown
<form id="comprehensive-quiz">
name: Quiz Title
description: A brief description of what this quiz covers
fields:
  - name: question1
    label: Question text
    type: radio
    options:
      - Option A
      - Option B
    correct: Option A
    required: true
  
  - name: question2
    label: Another question
    type: checkbox
    options:
      - Choice 1
      - Choice 2
      - Choice 3
    correct:
      - Choice 1
      - Choice 3
</form>
```

---

## Field Types

The Quiz DSL supports a comprehensive set of field types to accommodate various question formats and input requirements.

### Text Input

Text input fields accept free-form text responses. They are suitable for short-answer questions, names, or any single-line text entry.

**Syntax:**

```yaml
- name: student_name
  label: Enter your full name
  type: text
  placeholder: John Doe
  required: true
```

**Attributes:**
- `type`: Must be `text` or `input`
- `placeholder`: Optional hint text displayed in the empty field
- `required`: Boolean indicating whether the field must be filled
- `correct`: Optional string for auto-grading (exact match)

**Example with Scoring:**

```yaml
- name: capital_france
  label: What is the capital of France?
  type: text
  correct: Paris
```

### Textarea

Textarea fields allow multi-line text input, ideal for essay questions, detailed feedback, or longer responses.

**Syntax:**

```yaml
- name: essay
  label: Explain the concept in your own words
  type: textarea
  rows: 5
  placeholder: Type your answer here...
  required: true
```

**Attributes:**
- `type`: Must be `textarea`
- `rows`: Number of visible text lines (default: 3)
- `placeholder`: Optional hint text
- `required`: Boolean for validation

Textarea fields typically do not include a `correct` attribute as they require manual grading.

### Radio Buttons (Single Choice)

Radio button fields present multiple options where exactly one must be selected. This is the standard format for multiple-choice questions.

**Syntax:**

```yaml
- name: question1
  label: Which planet is closest to the Sun?
  type: radio
  options:
    - Mercury
    - Venus
    - Earth
    - Mars
  correct: Mercury
  required: true
```

**Attributes:**
- `type`: Must be `radio` or `choice`
- `options`: Array of strings or objects (see Options Format section)
- `correct`: Single string matching one of the options
- `required`: Boolean for validation

**Object-Based Options:**

```yaml
- name: question2
  label: Select the correct answer
  type: radio
  options:
    - label: First Option
      value: opt1
    - label: Second Option
      value: opt2
  correct: opt1
```

### Checkboxes (Multiple Choice)

Checkbox fields allow users to select zero or more options from a list. This is ideal for "select all that apply" questions.

**Syntax:**

```yaml
- name: languages
  label: Which of the following are programming languages?
  type: checkbox
  options:
    - Python
    - HTML
    - JavaScript
    - CSS
  correct:
    - Python
    - JavaScript
```

**Attributes:**
- `type`: Must be `checkbox` or `multi`
- `options`: Array of strings or objects
- `correct`: Array of strings representing all correct options
- `required`: Boolean for validation

**Scoring Logic:** For checkbox questions, the user must select **exactly** the correct set of options to receive credit. Partial credit is not awarded.

### Dropdown (Select)

Dropdown fields present options in a compact select menu, useful when space is limited or there are many options.

**Syntax:**

```yaml
- name: country
  label: Select your country
  type: select
  placeholder: Choose a country
  options:
    - United States
    - Canada
    - United Kingdom
    - Australia
    - Other
  required: true
```

**Attributes:**
- `type`: Must be `select` or `dropdown`
- `placeholder`: Text shown when no option is selected
- `options`: Array of strings or objects
- `correct`: Optional single string for scoring

### Confirmation Toggle

Confirmation fields are single checkboxes used for agreements, confirmations, or yes/no questions.

**Syntax:**

```yaml
- name: agree_terms
  label: I agree to the terms and conditions
  type: confirm
  required: true
```

**Attributes:**
- `type`: Must be `confirm`
- `placeholder`: Optional label text (alternative to `label`)
- `required`: Boolean for validation
- `correct`: Optional boolean (true/false) for scoring

---

## Options Format

Options can be specified in two formats: simple string arrays or object arrays with explicit labels and values.

### Simple String Format

```yaml
options:
  - Option A
  - Option B
  - Option C
```

In this format, both the display label and the stored value are the same string.

### Object Format

```yaml
options:
  - label: Display Text for Option A
    value: a
  - label: Display Text for Option B
    value: b
```

The object format allows you to separate the user-facing text from the stored value, which is useful for internationalization or when you need short internal codes.

---

## Scoring and Validation

### Automatic Scoring

When a field includes a `correct` attribute, the system automatically grades the response and includes it in the quiz score calculation.

**Scoring Rules:**
- Each field with a `correct` attribute contributes 1 point to the maximum score
- The user receives 1 point if their answer matches the correct value exactly
- For checkbox fields, all correct options must be selected and no incorrect options selected
- Text fields are case-sensitive by default
- The final score is presented as `score/maxScore` (e.g., 7/10)

### Required Fields

Fields marked with `required: true` must be completed before the form can be submitted. The system validates this on the client side before sending data to the server.

### Manual Grading

Fields without a `correct` attribute (such as essay questions) are stored but not automatically scored. These can be reviewed and graded manually through the admin interface.

---

## Complete Examples

### Example 1: Multiple Choice Quiz

This example demonstrates a standard multiple-choice quiz with radio button questions.

```markdown
# Introduction to Programming

This quiz tests your knowledge of basic programming concepts.

<form id="programming-basics">
name: Programming Fundamentals Quiz
description: Test your understanding of core programming concepts
fields:
  - name: q1
    label: What does HTML stand for?
    type: radio
    options:
      - Hyper Text Markup Language
      - High Tech Modern Language
      - Home Tool Markup Language
      - Hyperlinks and Text Markup Language
    correct: Hyper Text Markup Language
    required: true
  
  - name: q2
    label: Which of the following is a programming language?
    type: radio
    options:
      - HTML
      - CSS
      - Python
      - XML
    correct: Python
    required: true
  
  - name: q3
    label: What is the result of 10 % 3 in most programming languages?
    type: radio
    options:
      - "0"
      - "1"
      - "3"
      - "10"
    correct: "1"
    required: true
</form>
```

### Example 2: Multi-Select Knowledge Test

This example uses checkbox fields for questions with multiple correct answers.

```markdown
# Web Development Assessment

<form id="web-dev-multiselect">
name: Web Technologies Multi-Select Test
description: Select all correct answers for each question
fields:
  - name: languages
    label: Which of the following are programming languages? (Select all that apply)
    type: checkbox
    options:
      - Python
      - HTML
      - JavaScript
      - CSS
      - Ruby
      - Markdown
    correct:
      - Python
      - JavaScript
      - Ruby
  
  - name: http_methods
    label: Which are valid HTTP request methods?
    type: checkbox
    options:
      - GET
      - POST
      - SEND
      - DELETE
      - RECEIVE
      - PUT
    correct:
      - GET
      - POST
      - DELETE
      - PUT
  
  - name: databases
    label: Which of these are relational databases?
    type: checkbox
    options:
      - MySQL
      - MongoDB
      - PostgreSQL
      - Redis
      - SQLite
    correct:
      - MySQL
      - PostgreSQL
      - SQLite
</form>
```

### Example 3: Survey with Mixed Field Types

This example combines different field types for a comprehensive survey.

```markdown
# Course Feedback Survey

<form id="course-feedback">
name: Course Evaluation
description: Help us improve by sharing your feedback
fields:
  - name: student_name
    label: Your Name
    type: text
    required: true
  
  - name: course_rating
    label: How would you rate this course overall?
    type: select
    options:
      - Excellent
      - Good
      - Average
      - Below Average
      - Poor
    required: true
  
  - name: topics_enjoyed
    label: Which topics did you enjoy most? (Select all that apply)
    type: checkbox
    options:
      - Introduction to Variables
      - Control Flow
      - Functions and Methods
      - Object-Oriented Programming
      - Data Structures
      - Algorithms
  
  - name: would_recommend
    label: Would you recommend this course to others?
    type: radio
    options:
      - Definitely Yes
      - Probably Yes
      - Not Sure
      - Probably No
      - Definitely No
    required: true
  
  - name: additional_comments
    label: Additional Comments or Suggestions
    type: textarea
    rows: 4
    placeholder: Share any additional feedback here...
</form>
```

### Example 4: Registration Form

This example shows a practical registration form with validation.

```markdown
# Workshop Registration

<form id="workshop-registration">
name: AI Workshop Registration
description: Register for our upcoming AI and Machine Learning workshop
fields:
  - name: full_name
    label: Full Name
    type: text
    placeholder: John Doe
    required: true
  
  - name: email
    label: Email Address
    type: email
    placeholder: john@example.com
    required: true
  
  - name: experience_level
    label: What is your experience level with AI/ML?
    type: select
    options:
      - Beginner
      - Intermediate
      - Advanced
      - Expert
    required: true
  
  - name: topics_interest
    label: Which topics are you most interested in?
    type: checkbox
    options:
      - Neural Networks
      - Natural Language Processing
      - Computer Vision
      - Reinforcement Learning
      - Generative AI
  
  - name: dietary_restrictions
    label: Do you have any dietary restrictions?
    type: textarea
    rows: 2
    placeholder: Please list any allergies or dietary preferences
  
  - name: agree_terms
    label: I agree to the workshop terms and conditions
    type: confirm
    required: true
</form>
```

### Example 5: Math Quiz with Numerical Answers

This example demonstrates a mathematics quiz using text input with correct answers.

```markdown
# Basic Arithmetic Quiz

<form id="math-quiz">
name: Mathematics Assessment
description: Solve the following arithmetic problems
fields:
  - name: addition
    label: What is 127 + 358?
    type: text
    placeholder: Enter your answer
    correct: "485"
    required: true
  
  - name: multiplication
    label: What is 15 × 12?
    type: text
    correct: "180"
    required: true
  
  - name: division
    label: What is 144 ÷ 12?
    type: text
    correct: "12"
    required: true
  
  - name: order_of_operations
    label: What is 5 + 3 × 2?
    type: text
    correct: "11"
    description: Remember to follow the order of operations (PEMDAS)
    required: true
</form>
```

### Example 6: True/False Quiz

This example shows a quiz using radio buttons for true/false questions.

```markdown
# Science Facts Quiz

<form id="true-false-science">
name: True or False - Science Edition
description: Determine whether each statement is true or false
fields:
  - name: water_boiling
    label: Water boils at 100°C at sea level
    type: radio
    options:
      - "True"
      - "False"
    correct: "True"
    required: true
  
  - name: earth_flat
    label: The Earth is flat
    type: radio
    options:
      - "True"
      - "False"
    correct: "False"
    required: true
  
  - name: speed_of_light
    label: Light travels faster than sound
    type: radio
    options:
      - "True"
      - "False"
    correct: "True"
    required: true
  
  - name: human_senses
    label: Humans have exactly five senses
    type: radio
    options:
      - "True"
      - "False"
    correct: "False"
    description: Humans actually have more than five senses, including balance, temperature, and pain
    required: true
</form>
```

### Example 7: Comprehensive Course Assessment

This example combines multiple quiz sections in a single document.

```markdown
# Final Course Examination

Welcome to the final assessment for Introduction to Computer Science. This exam consists of multiple sections testing different aspects of the course material.

## Section 1: Multiple Choice

<form id="section-1-multiple-choice">
name: Multiple Choice Questions
description: Select the best answer for each question
fields:
  - name: mc1
    label: Which data structure uses LIFO (Last In, First Out)?
    type: radio
    options:
      - Queue
      - Stack
      - Array
      - Linked List
    correct: Stack
    required: true
  
  - name: mc2
    label: What is the time complexity of binary search?
    type: radio
    options:
      - O(n)
      - O(log n)
      - O(n²)
      - O(1)
    correct: O(log n)
    required: true
</form>

## Section 2: Select All That Apply

<form id="section-2-multiselect">
name: Multi-Select Questions
description: Choose all correct answers
fields:
  - name: ms1
    label: Which of the following are object-oriented programming principles?
    type: checkbox
    options:
      - Encapsulation
      - Compilation
      - Inheritance
      - Polymorphism
      - Debugging
      - Abstraction
    correct:
      - Encapsulation
      - Inheritance
      - Polymorphism
      - Abstraction
    required: true
</form>

## Section 3: Short Answer

<form id="section-3-short-answer">
name: Short Answer Questions
description: Provide brief answers to the following questions
fields:
  - name: sa1
    label: Explain the difference between a compiler and an interpreter in 2-3 sentences
    type: textarea
    rows: 3
    required: true
  
  - name: sa2
    label: Describe one advantage and one disadvantage of using recursion
    type: textarea
    rows: 3
    required: true
</form>
```

### Example 8: Customer Satisfaction Survey

This example demonstrates a practical business use case.

```markdown
# Customer Satisfaction Survey

Thank you for your recent purchase. We value your feedback!

<form id="customer-satisfaction">
name: Product Satisfaction Survey
description: Help us improve our products and services
fields:
  - name: order_number
    label: Order Number
    type: text
    placeholder: e.g., ORD-12345
    required: true
  
  - name: product_quality
    label: How would you rate the product quality?
    type: radio
    options:
      - Excellent
      - Good
      - Average
      - Poor
      - Very Poor
    required: true
  
  - name: delivery_speed
    label: How satisfied were you with the delivery speed?
    type: radio
    options:
      - Very Satisfied
      - Satisfied
      - Neutral
      - Dissatisfied
      - Very Dissatisfied
    required: true
  
  - name: features_used
    label: Which product features have you used?
    type: checkbox
    options:
      - Basic Functions
      - Advanced Settings
      - Mobile App
      - Web Interface
      - API Integration
      - Reporting Tools
  
  - name: improvement_suggestions
    label: What could we improve?
    type: textarea
    rows: 4
    placeholder: Your suggestions help us serve you better
  
  - name: recommend
    label: Would you recommend our product to a friend or colleague?
    type: radio
    options:
      - Definitely
      - Probably
      - Not Sure
      - Probably Not
      - Definitely Not
    required: true
</form>
```

---

## Advanced Features

### Field Descriptions

Fields can include a `description` attribute to provide additional context or instructions:

```yaml
- name: complex_question
  label: Solve the equation
  type: text
  description: Show your work and round to two decimal places
  required: true
```

### Placeholder Text

Placeholder text provides hints about the expected input format:

```yaml
- name: phone
  label: Phone Number
  type: text
  placeholder: (555) 123-4567
```

### Alternative Field Names

The DSL supports both `name` and `key` for field identifiers, and both `label` and `title` for display text:

```yaml
# These are equivalent
- name: question1
  label: What is your answer?

- key: question1
  title: What is your answer?
```

### Nested Form Structure

An alternative syntax uses a `form` wrapper:

```yaml
name: Quiz Title
form:
  fields:
    - name: q1
      label: Question
      type: radio
      options: [A, B, C]
```

This is functionally equivalent to the flat structure but may be preferred for organizational clarity in complex forms.

---

## Best Practices

### Form ID Naming

Use descriptive, unique IDs that reflect the content:
- Good: `python-basics-quiz`, `customer-feedback-2024`, `chapter-3-assessment`
- Avoid: `quiz1`, `form`, `test`

### Question Clarity

Write clear, unambiguous questions. Avoid double negatives and ensure that only one answer is objectively correct for scored questions.

### Option Ordering

For multiple-choice questions, consider randomizing option order in the implementation to prevent pattern-based guessing. When using the DSL, list options in a logical order (alphabetical, numerical, or conceptual).

### Required vs. Optional

Mark fields as required only when necessary. For surveys and feedback forms, optional fields often yield more honest responses.

### Validation Feedback

Provide clear descriptions for complex questions to guide users toward the expected answer format.

### Accessibility

Use descriptive labels rather than relying solely on placeholder text, as placeholders disappear when users begin typing.

---

## Common Patterns

### Quiz with Immediate Feedback

For educational quizzes where users should see correct answers after submission, include `correct` attributes on all scored fields. The system will automatically display which answers were right or wrong.

### Anonymous Surveys

For anonymous feedback collection, omit identifying fields like name and email, and don't include `correct` attributes since surveys typically don't have right or wrong answers.

### Conditional Logic (Not Yet Supported)

The current version of the DSL does not support conditional field display (e.g., showing field B only if field A has a specific value). This feature may be added in future versions.

### File Uploads (Not Yet Supported)

File upload fields are not currently supported in the DSL. Consider using text fields for URLs or external references as a workaround.

---

## Troubleshooting

### Form Not Rendering

**Symptom:** The form appears as plain text or shows an error message.

**Causes:**
- Invalid YAML syntax (check indentation, colons, and dashes)
- Missing required attributes (`name` and `type` for fields)
- Mismatched form tags (`<form>` without `</form>`)

**Solution:** Validate your YAML using a YAML parser and ensure all required attributes are present.

### Incorrect Scoring

**Symptom:** Quiz shows unexpected scores or doesn't calculate scores at all.

**Causes:**
- `correct` attribute value doesn't exactly match an option value
- For checkbox questions, the `correct` array doesn't include all correct options
- Case sensitivity issues in text answers

**Solution:** Verify that `correct` values exactly match option values, including case and spacing.

### Required Field Not Enforced

**Symptom:** Form submits even when required fields are empty.

**Causes:**
- `required` attribute set to a string instead of boolean
- Client-side validation disabled or bypassed

**Solution:** Ensure `required: true` (not `required: "true"`).

---

## Migration and Compatibility

### Version 1.0

This is the initial release of the Quiz DSL specification. Future versions will maintain backward compatibility where possible, with clear migration guides for breaking changes.

### Reserved Keywords

The following field names are reserved and should not be used: `id`, `formId`, `userId`, `documentId`, `timestamp`, `score`, `maxScore`.

---

## Appendix: Complete Field Reference

| Field Type | Type Value | Required Attributes | Optional Attributes | Supports Scoring |
|------------|------------|---------------------|---------------------|------------------|
| Text Input | `text`, `input` | `name`, `type` | `label`, `placeholder`, `required`, `correct` | Yes |
| Email Input | `email` | `name`, `type` | `label`, `placeholder`, `required` | No |
| Number Input | `number` | `name`, `type` | `label`, `placeholder`, `required`, `correct` | Yes |
| Textarea | `textarea` | `name`, `type` | `label`, `placeholder`, `rows`, `required` | No |
| Radio Buttons | `radio`, `choice` | `name`, `type`, `options` | `label`, `required`, `correct` | Yes |
| Checkboxes | `checkbox`, `multi` | `name`, `type`, `options` | `label`, `required`, `correct` | Yes |
| Dropdown | `select`, `dropdown` | `name`, `type`, `options` | `label`, `placeholder`, `required`, `correct` | Yes |
| Confirmation | `confirm` | `name`, `type` | `label`, `placeholder`, `required`, `correct` | Yes |

---

## Conclusion

The Quiz DSL provides a powerful yet accessible way to create interactive assessments within markdown documents. By following the patterns and examples in this specification, content creators can build engaging educational materials, surveys, and forms without writing code. The automatic scoring system, combined with flexible field types and validation rules, makes it suitable for a wide range of use cases from simple feedback forms to comprehensive course examinations.

For questions, feature requests, or bug reports, please refer to the project documentation or contact the development team.

---

**Document Version:** 1.0  
**Specification Status:** Stable  
**Last Reviewed:** December 16, 2025
