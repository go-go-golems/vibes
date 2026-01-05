export interface Preset {
  id: string;
  name: string;
  description: string;
  category: string;
  title: string;
  content: string;
}

export const presets: Preset[] = [
  {
    id: 'multiple-choice',
    name: 'Multiple Choice Quiz',
    description: 'A classic quiz with radio button questions and automatic scoring',
    category: 'Quiz',
    title: 'Multiple Choice Quiz',
    content: `# Multiple Choice Quiz

Test your knowledge with this multiple choice quiz. Select the best answer for each question.

<form id="multiple-choice-quiz">
name: Knowledge Check
description: Select the correct answer for each question
fields:
  - name: q1
    label: What is the capital of France?
    type: radio
    options:
      - { label: "London", value: "london" }
      - { label: "Paris", value: "paris" }
      - { label: "Berlin", value: "berlin" }
      - { label: "Madrid", value: "madrid" }
    correct: "paris"
  - name: q2
    label: Which planet is known as the Red Planet?
    type: radio
    options:
      - { label: "Venus", value: "venus" }
      - { label: "Mars", value: "mars" }
      - { label: "Jupiter", value: "jupiter" }
      - { label: "Saturn", value: "saturn" }
    correct: "mars"
  - name: q3
    label: What is 7 × 8?
    type: radio
    options:
      - { label: "54", value: "54" }
      - { label: "56", value: "56" }
      - { label: "58", value: "58" }
      - { label: "62", value: "62" }
    correct: "56"
</form>

Good luck!
`,
  },
  {
    id: 'checkbox-quiz',
    name: 'Multi-Select Quiz',
    description: 'Quiz with checkbox questions where multiple answers can be correct',
    category: 'Quiz',
    title: 'Multi-Select Quiz',
    content: `# Multi-Select Quiz

Select **all** correct answers for each question. Some questions may have multiple correct answers.

<form id="multi-select-quiz">
name: Multi-Select Knowledge Test
description: Check all answers that apply
fields:
  - name: q1
    label: Which of the following are programming languages?
    type: checkbox
    options:
      - { label: "Python", value: "python" }
      - { label: "HTML", value: "html" }
      - { label: "JavaScript", value: "javascript" }
      - { label: "CSS", value: "css" }
    correct: ["python", "javascript"]
  - name: q2
    label: Select all prime numbers
    type: checkbox
    options:
      - { label: "2", value: "2" }
      - { label: "3", value: "3" }
      - { label: "4", value: "4" }
      - { label: "5", value: "5" }
      - { label: "6", value: "6" }
    correct: ["2", "3", "5"]
  - name: q3
    label: Which are valid HTTP methods?
    type: checkbox
    options:
      - { label: "GET", value: "get" }
      - { label: "POST", value: "post" }
      - { label: "SEND", value: "send" }
      - { label: "DELETE", value: "delete" }
    correct: ["get", "post", "delete"]
</form>
`,
  },
  {
    id: 'feedback-form',
    name: 'Feedback Form',
    description: 'Collect user feedback with text inputs and ratings',
    category: 'Form',
    title: 'Feedback Form',
    content: `# We Value Your Feedback

Please take a moment to share your thoughts with us. Your feedback helps us improve.

<form id="feedback-form">
name: Feedback Survey
description: Share your experience with us
fields:
  - name: name
    label: Your Name
    type: text
    placeholder: Enter your name
  - name: email
    label: Email Address
    type: email
    placeholder: your@email.com
  - name: rating
    label: How would you rate your experience?
    type: radio
    options:
      - { label: "⭐ Poor", value: "1" }
      - { label: "⭐⭐ Fair", value: "2" }
      - { label: "⭐⭐⭐ Good", value: "3" }
      - { label: "⭐⭐⭐⭐ Very Good", value: "4" }
      - { label: "⭐⭐⭐⭐⭐ Excellent", value: "5" }
  - name: feedback
    label: What did you like most?
    type: textarea
    placeholder: Tell us what you enjoyed...
    rows: 3
  - name: improvements
    label: What could we improve?
    type: textarea
    placeholder: Share your suggestions...
    rows: 3
  - name: recommend
    label: Would you recommend us to others?
    type: confirm
    placeholder: Yes, I would recommend
</form>

Thank you for your feedback!
`,
  },
  {
    id: 'course-assessment',
    name: 'Course Assessment',
    description: 'Comprehensive course assessment with mixed question types',
    category: 'Education',
    title: 'Course Assessment',
    content: `# Course Assessment

## Module 1: Fundamentals

This assessment covers the key concepts from Module 1. Please answer all questions to the best of your ability.

### Section A: Conceptual Understanding

<form id="section-a">
name: Section A - Concepts
description: Test your understanding of core concepts
fields:
  - name: concept1
    label: Define the main concept in your own words
    type: textarea
    placeholder: Write your definition here...
    rows: 3
  - name: concept2
    label: Which statement best describes the principle?
    type: radio
    options:
      - { label: "It is a process of elimination", value: "a" }
      - { label: "It is a method of organization", value: "b" }
      - { label: "It is a framework for analysis", value: "c" }
      - { label: "It is a tool for measurement", value: "d" }
    correct: "c"
</form>

### Section B: Application

<form id="section-b">
name: Section B - Application
description: Apply what you've learned
fields:
  - name: app1
    label: Select all scenarios where this concept applies
    type: checkbox
    options:
      - { label: "Scenario A: Planning phase", value: "a" }
      - { label: "Scenario B: Execution phase", value: "b" }
      - { label: "Scenario C: Review phase", value: "c" }
      - { label: "Scenario D: None of the above", value: "d" }
    correct: ["a", "c"]
  - name: app2
    label: Describe how you would apply this in practice
    type: textarea
    placeholder: Explain your approach...
    rows: 4
</form>

---

*Remember: This assessment is designed to help you learn. Take your time and think through each question.*
`,
  },
  {
    id: 'survey-template',
    name: 'Survey Template',
    description: 'General survey with dropdown selections and text responses',
    category: 'Form',
    title: 'Survey',
    content: `# Quick Survey

Help us understand your preferences better by completing this short survey.

<form id="survey">
name: Preferences Survey
description: Tell us about your preferences
fields:
  - name: age_group
    label: What is your age group?
    type: select
    options:
      - { label: "Under 18", value: "under18" }
      - { label: "18-24", value: "18-24" }
      - { label: "25-34", value: "25-34" }
      - { label: "35-44", value: "35-44" }
      - { label: "45-54", value: "45-54" }
      - { label: "55+", value: "55plus" }
  - name: experience
    label: How much experience do you have?
    type: select
    options:
      - { label: "Beginner (0-1 years)", value: "beginner" }
      - { label: "Intermediate (2-4 years)", value: "intermediate" }
      - { label: "Advanced (5+ years)", value: "advanced" }
  - name: interests
    label: What topics interest you most?
    type: checkbox
    options:
      - { label: "Technology", value: "tech" }
      - { label: "Science", value: "science" }
      - { label: "Arts", value: "arts" }
      - { label: "Business", value: "business" }
      - { label: "Health", value: "health" }
  - name: frequency
    label: How often do you engage with this topic?
    type: radio
    options:
      - { label: "Daily", value: "daily" }
      - { label: "Weekly", value: "weekly" }
      - { label: "Monthly", value: "monthly" }
      - { label: "Rarely", value: "rarely" }
  - name: comments
    label: Any additional comments?
    type: textarea
    placeholder: Share your thoughts...
    rows: 3
</form>
`,
  },
  {
    id: 'math-quiz',
    name: 'Math Quiz',
    description: 'Mathematics quiz with numerical answers',
    category: 'Education',
    title: 'Math Quiz',
    content: `# Mathematics Quiz

Solve the following problems. Enter your numerical answers.

## Basic Arithmetic

<form id="math-basic">
name: Basic Math
description: Test your arithmetic skills
fields:
  - name: q1
    label: What is 15 + 27?
    type: text
    placeholder: Enter your answer
    correct: "42"
  - name: q2
    label: What is 144 ÷ 12?
    type: text
    placeholder: Enter your answer
    correct: "12"
  - name: q3
    label: What is 8 × 9?
    type: text
    placeholder: Enter your answer
    correct: "72"
</form>

## Word Problems

<form id="math-word">
name: Word Problems
description: Solve these word problems
fields:
  - name: wp1
    label: "A store has 45 apples. If 18 are sold, how many remain?"
    type: text
    placeholder: Enter your answer
    correct: "27"
  - name: wp2
    label: "If a rectangle has length 8 and width 5, what is its area?"
    type: text
    placeholder: Enter your answer
    correct: "40"
</form>

Good luck with your calculations!
`,
  },
  {
    id: 'true-false',
    name: 'True/False Quiz',
    description: 'Simple true or false questions',
    category: 'Quiz',
    title: 'True or False Quiz',
    content: `# True or False Quiz

Determine whether each statement is true or false.

<form id="true-false-quiz">
name: True or False
description: Select True or False for each statement
fields:
  - name: tf1
    label: "The Earth is the third planet from the Sun."
    type: radio
    options:
      - { label: "True", value: "true" }
      - { label: "False", value: "false" }
    correct: "true"
  - name: tf2
    label: "Water boils at 90°C at sea level."
    type: radio
    options:
      - { label: "True", value: "true" }
      - { label: "False", value: "false" }
    correct: "false"
  - name: tf3
    label: "The Great Wall of China is visible from space with the naked eye."
    type: radio
    options:
      - { label: "True", value: "true" }
      - { label: "False", value: "false" }
    correct: "false"
  - name: tf4
    label: "Photosynthesis converts sunlight into chemical energy."
    type: radio
    options:
      - { label: "True", value: "true" }
      - { label: "False", value: "false" }
    correct: "true"
  - name: tf5
    label: "JavaScript and Java are the same programming language."
    type: radio
    options:
      - { label: "True", value: "true" }
      - { label: "False", value: "false" }
    correct: "false"
</form>
`,
  },
  {
    id: 'registration-form',
    name: 'Registration Form',
    description: 'Event or course registration form',
    category: 'Form',
    title: 'Registration Form',
    content: `# Event Registration

Please fill out the form below to register for the event.

<form id="registration">
name: Event Registration
description: Complete your registration
fields:
  - name: fullname
    label: Full Name
    type: text
    placeholder: Enter your full name
    required: true
  - name: email
    label: Email Address
    type: email
    placeholder: your@email.com
    required: true
  - name: phone
    label: Phone Number
    type: text
    placeholder: (123) 456-7890
  - name: session
    label: Preferred Session
    type: select
    options:
      - { label: "Morning (9:00 AM - 12:00 PM)", value: "morning" }
      - { label: "Afternoon (1:00 PM - 4:00 PM)", value: "afternoon" }
      - { label: "Evening (6:00 PM - 9:00 PM)", value: "evening" }
  - name: dietary
    label: Dietary Requirements
    type: checkbox
    options:
      - { label: "Vegetarian", value: "vegetarian" }
      - { label: "Vegan", value: "vegan" }
      - { label: "Gluten-free", value: "gluten-free" }
      - { label: "No special requirements", value: "none" }
  - name: newsletter
    label: Subscribe to our newsletter
    type: confirm
    placeholder: Yes, keep me updated
  - name: notes
    label: Additional Notes
    type: textarea
    placeholder: Any special requests or questions?
    rows: 3
</form>

We look forward to seeing you at the event!
`,
  },
];

export const presetCategories = Array.from(new Set(presets.map(p => p.category)));

export function getPresetsByCategory(category: string): Preset[] {
  return presets.filter(p => p.category === category);
}

export function getPresetById(id: string): Preset | undefined {
  return presets.find(p => p.id === id);
}
