---
Title: Entry Types and Usage
Slug: entry-types
Short: Detailed guide to different diary entry types
Topics:
- entries
- types
- usage
SectionType: reference
---

# Entry Types and Usage

The diary CLI supports several types of entries, each designed for different kinds of information you want to capture.

## TIL (Today I Learned)

Use TIL entries to capture new knowledge, insights, or discoveries.

### Examples

```bash
# Technical learning
diary add til "Go interfaces are satisfied implicitly"
diary add til "CSS Grid can replace most flexbox layouts" --title "CSS Layout"

# General knowledge
diary add til "The word 'serendipity' was coined by Horace Walpole in 1754"
```

### Best Practices

- Keep TIL entries focused on a single concept
- Include context when the learning might not be obvious later
- Use titles to categorize related learnings
- Add subtitles to organize multiple related points

## Thoughts

Capture ideas, reflections, opinions, or mental notes.

### Examples

```bash
# Architecture thoughts
diary add thought "Microservices might be overkill for our current scale"

# Personal reflections
diary add thought "The importance of work-life balance" --title "Career Reflection"

# Project ideas
diary add thought "CLI tool for managing dotfiles" --subtitle "project-ideas"
```

### Best Practices

- Use thoughts for subjective content and opinions
- Include reasoning or context for future reference
- Group related thoughts using subtitles
- Don't worry about being "right" - capture the thinking process

## Did (Activities)

Record what you accomplished, worked on, or experienced.

### Examples

```bash
# Work activities
diary add did "Completed user authentication system"
diary add did "Attended team retrospective meeting"

# Personal activities
diary add did "Finished reading 'Clean Code'" --title "Learning"
diary add did "Went hiking at Blue Ridge Trail" --title "Recreation"
```

### Best Practices

- Focus on completed actions or experiences
- Include outcomes or results when relevant
- Use titles to categorize different life areas
- Be specific enough to be meaningful later

## Links

Save interesting articles, resources, tools, or references.

### Examples

```bash
# Technical resources
diary add link "https://go.dev/blog/interfaces" --title "Go Interfaces Explained"

# Articles
diary add link "https://example.com/article" --title "Microservices Best Practices"

# Tools and resources
diary add link "https://github.com/user/repo" --title "Useful CLI Tool"
```

### Best Practices

- Always include a descriptive title
- Add a brief note about why the link is interesting
- Use subtitles to organize links by topic
- Consider adding your own summary or key takeaways

## Todos

Create actionable tasks that integrate with Obsidian Tasks plugin.

### Examples

```bash
# Simple todos
diary todo add "Review pull requests"
diary todo add "Update documentation"

# Todos with metadata
diary todo add "Prepare presentation" --priority high --due "next friday"
diary todo add "Call dentist" --priority low --tags personal health
```

### Best Practices

- Make todos specific and actionable
- Use priorities to focus on what's important
- Set due dates for time-sensitive tasks
- Use tags to categorize and filter todos
- Review and update todos regularly

## Interactive Mode

For any entry type, you can use interactive mode for a guided experience:

```bash
# Interactive entry creation
diary add

# Interactive todo creation
diary todo
```

Interactive mode provides:
- Type selection with descriptions
- Format options
- Visual editor integration
- Subtitle organization
- Validation and helpful prompts

## Combining Entry Types

You can mix different entry types in your daily files. For example:

1. Start with todos for the day
2. Add TIL entries as you learn
3. Capture thoughts and ideas
4. Record what you accomplished (did)
5. Save interesting links you discover

This creates a comprehensive daily record that's both actionable and reflective.

## Format Considerations

- **Default format**: Best for human reading and simple workflows
- **Markdown format**: Good for rich documentation and metadata
- **Task format**: Required for Obsidian Tasks plugin integration

Choose the format that best fits your workflow and tools. You can mix formats within the same file if needed.

