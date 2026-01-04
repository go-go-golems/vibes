# Quiz System Implementation Walkthrough

**Code Review Knowledge Platform**

*A comprehensive guide to how quizzes work from storage to submission*

---

## Table of Contents

1. [System Overview](#system-overview)
2. [Data Flow Architecture](#data-flow-architecture)
3. [Storage Layer: Git Notes with isomorphic-git](#storage-layer-git-notes-with-isomorphic-git)
4. [YAML Parsing and Validation](#yaml-parsing-and-validation)
5. [tRPC API Layer](#trpc-api-layer)
6. [React Frontend Components](#react-frontend-components)
7. [Quiz Submission and Grading](#quiz-submission-and-grading)
8. [Progress Tracking](#progress-tracking)
9. [Embedded Quizzes in Annotations](#embedded-quizzes-in-annotations)
10. [Testing with CLI Tools](#testing-with-cli-tools)

---

## System Overview

The quiz system in the Code Review Knowledge Platform provides an interactive learning experience that tests understanding of code concepts. This document walks through the complete implementation, from how quiz definitions are stored in git notes to how submissions are processed and progress is tracked.

The system architecture follows a layered approach where each layer has a single responsibility. The storage layer handles reading and writing git notes using isomorphic-git. The parsing layer transforms YAML content into typed TypeScript objects. The API layer exposes quiz operations through tRPC procedures. The frontend layer renders quizzes and handles user interactions. This separation of concerns makes the codebase maintainable and testable.

Understanding this architecture is essential for extending the quiz system or debugging issues. Each section of this document corresponds to a layer in the architecture, with code examples and explanations of key implementation decisions.

---

## Data Flow Architecture

Before diving into implementation details, it's helpful to understand how data flows through the system when a user takes a quiz.

### Quiz Loading Flow

When a user navigates to a quiz, the following sequence occurs:

1. **Frontend Request**: The React component calls `trpc.quizzes.get.useQuery()` with the repository path, commit hash, and quiz ID.

2. **tRPC Procedure**: The server-side procedure receives the request, validates the input using Zod schemas, and calls the storage layer.

3. **Git Notes Read**: The storage layer uses isomorphic-git to read the notes ref for quizzes, retrieves the blob content for the specified commit, and returns the raw YAML.

4. **YAML Parsing**: The YAML content is parsed and validated, producing a typed `QuizDefinition` object.

5. **Response**: The quiz definition is returned to the frontend, where React renders the quiz interface.

### Quiz Submission Flow

When a user submits their answers:

1. **Frontend Submission**: The React component collects answers and calls `trpc.quizzes.submit.useMutation()`.

2. **Grading**: The server compares submitted answers against correct answers, calculating the score.

3. **Submission Storage**: The submission is serialized to YAML and written to a separate git notes namespace for submissions.

4. **Response**: The graded result with score and explanations is returned to the frontend.

This bidirectional flow—reading quiz definitions and writing submissions—both use git notes, maintaining a complete audit trail in the repository.

---

## Storage Layer: Git Notes with isomorphic-git

The storage layer is implemented in `server/lib/git-notes.ts` and provides all git operations needed by the quiz system. The implementation uses isomorphic-git, a pure JavaScript git implementation that works without the git CLI.

### Why isomorphic-git?

The platform initially used simple-git, which wraps the git CLI. However, this approach failed in production environments where git was not installed. Isomorphic-git solves this by implementing git operations entirely in JavaScript, making the platform portable to any Node.js environment.

### Core Storage Functions

The storage layer exports several functions for quiz operations:

```typescript
// Read all quizzes from a repository
export async function getAllQuizzes(repoPath: string): Promise<QuizWithMeta[]>

// Read a specific quiz by ID
export async function getQuiz(
  repoPath: string, 
  commit: string, 
  quizId?: string
): Promise<QuizDefinition | undefined>

// Write a quiz submission
export async function writeQuizSubmission(
  repoPath: string,
  quizId: string,
  userId: string,
  submission: QuizSubmission
): Promise<void>

// Read quiz submissions for a user
export async function getQuizSubmissions(
  repoPath: string,
  userId: string
): Promise<QuizSubmission[]>
```

### Reading Notes with isomorphic-git

Reading a git note involves several steps. First, we resolve the notes ref to find the tree that maps commits to note blobs. Then we look up the blob for our target commit. Finally, we read and decode the blob content.

```typescript
async function readNote(
  repoPath: string, 
  ref: string, 
  targetCommit: string
): Promise<string | null> {
  const fs = await import('fs');
  
  try {
    // Resolve the notes ref to get the tree
    const notesCommit = await git.resolveRef({ fs, dir: repoPath, ref });
    const { tree } = await git.readTree({ 
      fs, 
      dir: repoPath, 
      oid: notesCommit 
    });
    
    // Notes are stored with the commit hash as the filename
    const noteEntry = tree.find(entry => entry.path === targetCommit);
    if (!noteEntry) return null;
    
    // Read the blob content
    const { blob } = await git.readBlob({ 
      fs, 
      dir: repoPath, 
      oid: noteEntry.oid 
    });
    
    return new TextDecoder().decode(blob);
  } catch (error) {
    return null;
  }
}
```

### Writing Notes with isomorphic-git

Writing notes is more complex because we need to create a new blob, update the tree, create a new commit, and update the ref. The implementation handles all these steps atomically:

```typescript
async function writeNote(
  repoPath: string,
  ref: string,
  targetCommit: string,
  content: string
): Promise<void> {
  const fs = await import('fs');
  
  // Create a blob with the note content
  const blobOid = await git.writeBlob({
    fs,
    dir: repoPath,
    blob: new TextEncoder().encode(content)
  });
  
  // Read existing tree or create empty one
  let existingTree: TreeEntry[] = [];
  try {
    const notesCommit = await git.resolveRef({ fs, dir: repoPath, ref });
    const { tree } = await git.readTree({ 
      fs, 
      dir: repoPath, 
      oid: notesCommit 
    });
    existingTree = tree;
  } catch {
    // Ref doesn't exist yet, start with empty tree
  }
  
  // Update or add the note entry
  const newTree = existingTree.filter(e => e.path !== targetCommit);
  newTree.push({
    mode: '100644',
    path: targetCommit,
    oid: blobOid,
    type: 'blob'
  });
  
  // Write the new tree
  const treeOid = await git.writeTree({ fs, dir: repoPath, tree: newTree });
  
  // Create a commit for the notes update
  const commitOid = await git.writeCommit({
    fs,
    dir: repoPath,
    commit: {
      tree: treeOid,
      parent: [], // Notes commits don't need parents
      author: { name: 'System', email: 'system@local', timestamp: Date.now() / 1000 },
      committer: { name: 'System', email: 'system@local', timestamp: Date.now() / 1000 },
      message: `Update note for ${targetCommit}`
    }
  });
  
  // Update the ref to point to the new commit
  await git.writeRef({
    fs,
    dir: repoPath,
    ref,
    value: commitOid,
    force: true
  });
}
```

### Quiz-Specific Storage Functions

The `getAllQuizzes` function reads all quizzes from the repository by iterating through notes and parsing each one:

```typescript
export async function getAllQuizzes(repoPath: string): Promise<QuizWithMeta[]> {
  const fs = await import('fs');
  const quizzes: QuizWithMeta[] = [];
  
  try {
    // Get the notes tree
    const notesCommit = await git.resolveRef({ 
      fs, 
      dir: repoPath, 
      ref: 'refs/notes/quizzes' 
    });
    const { tree } = await git.readTree({ 
      fs, 
      dir: repoPath, 
      oid: notesCommit 
    });
    
    // Read each note and parse as quiz array
    for (const entry of tree) {
      const { blob } = await git.readBlob({ 
        fs, 
        dir: repoPath, 
        oid: entry.oid 
      });
      const content = new TextDecoder().decode(blob);
      const parsed = yaml.parse(content);
      
      // Handle both array and single quiz formats
      const quizArray = Array.isArray(parsed) ? parsed : [parsed];
      
      for (const quiz of quizArray) {
        if (quiz && quiz.title) {
          quizzes.push({
            ...quiz,
            commit: entry.path // The commit hash is the entry path
          });
        }
      }
    }
  } catch (error) {
    // Return empty array if notes don't exist
  }
  
  return quizzes;
}
```

---

## YAML Parsing and Validation

The YAML parsing layer is implemented in `server/lib/yaml-parser.ts`. It transforms raw YAML strings into typed TypeScript objects with validation.

### Parser Architecture

The parser uses the `yaml` npm package for parsing and custom validation logic to ensure data integrity:

```typescript
import yaml from 'yaml';

export function parseQuiz(content: string): QuizDefinition | null {
  try {
    const parsed = yaml.parse(content);
    
    // Validate required fields
    if (!parsed.id || !parsed.title || !parsed.questions) {
      return null;
    }
    
    // Validate and transform questions
    const questions = parsed.questions.map(validateQuestion);
    
    return {
      id: parsed.id,
      title: parsed.title,
      description: parsed.description || '',
      difficulty: validateDifficulty(parsed.difficulty),
      estimated_time: parsed.estimated_time || 10,
      tags: parsed.tags || [],
      prerequisites: parsed.prerequisites || [],
      related_files: parsed.related_files || [],
      questions
    };
  } catch (error) {
    console.error('Failed to parse quiz YAML:', error);
    return null;
  }
}
```

### Question Validation

Each question type has specific validation requirements:

```typescript
function validateQuestion(raw: any): Question {
  const base = {
    id: raw.id || generateId(),
    type: validateQuestionType(raw.type),
    question: raw.question,
    points: raw.points || 10,
    explanation: raw.explanation || ''
  };
  
  switch (base.type) {
    case 'multiple_choice':
      return {
        ...base,
        options: raw.options || [],
        correct_answer: raw.correct_answer
      };
      
    case 'code_completion':
      return {
        ...base,
        correct_answer: raw.correct_answer,
        code_context: raw.code_context
      };
      
    case 'scenario':
      return {
        ...base,
        options: raw.options || [],
        correct_answer: raw.correct_answer
      };
      
    default:
      throw new Error(`Unknown question type: ${raw.type}`);
  }
}

function validateQuestionType(type: string): QuestionType {
  const valid = ['multiple_choice', 'code_completion', 'scenario'];
  if (!valid.includes(type)) {
    throw new Error(`Invalid question type: ${type}`);
  }
  return type as QuestionType;
}

function validateDifficulty(difficulty: string): Difficulty {
  const valid = ['beginner', 'intermediate', 'advanced'];
  if (!valid.includes(difficulty)) {
    return 'intermediate'; // Default
  }
  return difficulty as Difficulty;
}
```

### Quiz Statistics Calculation

The parser also provides utility functions for calculating quiz statistics:

```typescript
export function getQuizStats(quiz: QuizDefinition): QuizStats {
  const questionTypes: Record<string, number> = {};
  let totalPoints = 0;
  
  for (const question of quiz.questions) {
    questionTypes[question.type] = (questionTypes[question.type] || 0) + 1;
    totalPoints += question.points;
  }
  
  return {
    questionCount: quiz.questions.length,
    totalPoints,
    questionTypes,
    estimatedTime: quiz.estimated_time,
    difficulty: quiz.difficulty
  };
}
```

---

## tRPC API Layer

The API layer is implemented in `server/routers/quizzes.ts` and exposes quiz operations as tRPC procedures. This layer handles request validation, calls the storage layer, and formats responses.

### Router Structure

The quizzes router defines several procedures:

```typescript
import { router, publicProcedure, protectedProcedure } from '../_core/trpc';
import { z } from 'zod';
import * as gitNotes from '../lib/git-notes';
import { getQuizStats } from '../lib/yaml-parser';

export const quizzesRouter = router({
  // List all quizzes in a repository
  list: publicProcedure
    .input(z.object({
      repoPath: z.string()
    }))
    .query(async ({ input }) => {
      const quizzes = await gitNotes.getAllQuizzes(input.repoPath);
      
      return quizzes.map(quiz => ({
        ...quiz,
        stats: getQuizStats(quiz)
      }));
    }),
  
  // Get a specific quiz by ID
  get: publicProcedure
    .input(z.object({
      repoPath: z.string(),
      commit: z.string(),
      quizId: z.string().optional()
    }))
    .query(async ({ input }) => {
      const quiz = await gitNotes.getQuiz(
        input.repoPath, 
        input.commit,
        input.quizId
      );
      
      if (!quiz) {
        throw new TRPCError({
          code: 'NOT_FOUND',
          message: 'Quiz not found'
        });
      }
      
      return {
        ...quiz,
        stats: getQuizStats(quiz)
      };
    }),
  
  // Submit quiz answers
  submit: protectedProcedure
    .input(z.object({
      repoPath: z.string(),
      quizId: z.string(),
      answers: z.record(z.string(), z.string())
    }))
    .mutation(async ({ input, ctx }) => {
      // Get the quiz to grade against
      const quiz = await gitNotes.getQuizByIdFromAnyCommit(
        input.repoPath,
        input.quizId
      );
      
      if (!quiz) {
        throw new TRPCError({
          code: 'NOT_FOUND',
          message: 'Quiz not found'
        });
      }
      
      // Grade the submission
      const result = gradeSubmission(quiz, input.answers);
      
      // Store the submission
      await gitNotes.writeQuizSubmission(
        input.repoPath,
        input.quizId,
        ctx.user.id.toString(),
        {
          quizId: input.quizId,
          answers: input.answers,
          score: result.score,
          maxScore: result.maxScore,
          passed: result.passed,
          submittedAt: new Date().toISOString()
        }
      );
      
      return result;
    }),
  
  // Get user's quiz progress
  progress: protectedProcedure
    .input(z.object({
      repoPath: z.string()
    }))
    .query(async ({ input, ctx }) => {
      const submissions = await gitNotes.getQuizSubmissions(
        input.repoPath,
        ctx.user.id.toString()
      );
      
      const completed = new Set(submissions.map(s => s.quizId));
      const passed = submissions.filter(s => s.passed).length;
      
      return {
        completed: completed.size,
        passed,
        submissions
      };
    })
});
```

### Grading Logic

The grading function compares submitted answers against correct answers:

```typescript
interface GradingResult {
  score: number;
  maxScore: number;
  passed: boolean;
  questionResults: QuestionResult[];
}

interface QuestionResult {
  questionId: string;
  correct: boolean;
  userAnswer: string;
  correctAnswer: string;
  explanation: string;
  points: number;
  earnedPoints: number;
}

function gradeSubmission(
  quiz: QuizDefinition, 
  answers: Record<string, string>
): GradingResult {
  let score = 0;
  let maxScore = 0;
  const questionResults: QuestionResult[] = [];
  
  for (const question of quiz.questions) {
    const userAnswer = answers[question.id] || '';
    const correctAnswer = question.correct_answer;
    
    // Normalize answers for comparison
    const normalizedUser = userAnswer.trim().toLowerCase();
    const normalizedCorrect = correctAnswer.trim().toLowerCase();
    
    const correct = normalizedUser === normalizedCorrect;
    const earnedPoints = correct ? question.points : 0;
    
    score += earnedPoints;
    maxScore += question.points;
    
    questionResults.push({
      questionId: question.id,
      correct,
      userAnswer,
      correctAnswer,
      explanation: question.explanation,
      points: question.points,
      earnedPoints
    });
  }
  
  // Pass threshold is 70%
  const passed = (score / maxScore) >= 0.7;
  
  return { score, maxScore, passed, questionResults };
}
```

---

## React Frontend Components

The frontend quiz components are implemented in `client/src/pages/QuizTake.tsx` and related files. The implementation uses React hooks and tRPC for data fetching.

### Quiz Taking Component

The main quiz component manages state for user answers and handles submission:

```typescript
import { useState } from 'react';
import { useSearchParams } from 'wouter';
import { trpc } from '@/lib/trpc';
import { Button } from '@/components/ui/button';
import { Card } from '@/components/ui/card';
import { RadioGroup, RadioGroupItem } from '@/components/ui/radio-group';
import { Input } from '@/components/ui/input';

export default function QuizTake() {
  const [searchParams] = useSearchParams();
  const repoPath = searchParams.get('repo') || '';
  const commit = searchParams.get('commit') || '';
  const quizId = searchParams.get('quizId') || '';
  
  const [answers, setAnswers] = useState<Record<string, string>>({});
  const [submitted, setSubmitted] = useState(false);
  const [result, setResult] = useState<GradingResult | null>(null);
  
  // Fetch quiz data
  const { data: quiz, isLoading } = trpc.quizzes.get.useQuery({
    repoPath,
    commit,
    quizId
  });
  
  // Submission mutation
  const submitMutation = trpc.quizzes.submit.useMutation({
    onSuccess: (data) => {
      setResult(data);
      setSubmitted(true);
    }
  });
  
  const handleAnswerChange = (questionId: string, value: string) => {
    setAnswers(prev => ({
      ...prev,
      [questionId]: value
    }));
  };
  
  const handleSubmit = () => {
    submitMutation.mutate({
      repoPath,
      quizId,
      answers
    });
  };
  
  if (isLoading) return <LoadingSpinner />;
  if (!quiz) return <NotFound />;
  
  return (
    <div className="container py-8">
      <h1 className="text-3xl font-bold mb-4">{quiz.title}</h1>
      <p className="text-muted-foreground mb-8">{quiz.description}</p>
      
      <div className="space-y-8">
        {quiz.questions.map((question, index) => (
          <QuestionCard
            key={question.id}
            question={question}
            index={index}
            answer={answers[question.id]}
            onAnswerChange={(value) => handleAnswerChange(question.id, value)}
            submitted={submitted}
            result={result?.questionResults.find(r => r.questionId === question.id)}
          />
        ))}
      </div>
      
      {!submitted ? (
        <Button 
          onClick={handleSubmit}
          disabled={submitMutation.isPending}
          className="mt-8"
        >
          Submit Quiz
        </Button>
      ) : (
        <ResultSummary result={result} />
      )}
    </div>
  );
}
```

### Question Card Component

Each question type has its own rendering logic:

```typescript
function QuestionCard({ 
  question, 
  index, 
  answer, 
  onAnswerChange,
  submitted,
  result 
}: QuestionCardProps) {
  return (
    <Card className={cn(
      "p-6",
      submitted && result?.correct && "border-green-500",
      submitted && !result?.correct && "border-red-500"
    )}>
      <div className="flex items-start gap-4">
        <span className="text-2xl font-bold text-muted-foreground">
          Q{index + 1}
        </span>
        <div className="flex-1">
          <p className="text-lg mb-4">{question.question}</p>
          
          {question.type === 'multiple_choice' && (
            <MultipleChoiceInput
              options={question.options}
              value={answer}
              onChange={onAnswerChange}
              disabled={submitted}
              correctAnswer={submitted ? question.correct_answer : undefined}
            />
          )}
          
          {question.type === 'code_completion' && (
            <CodeCompletionInput
              codeContext={question.code_context}
              value={answer}
              onChange={onAnswerChange}
              disabled={submitted}
            />
          )}
          
          {question.type === 'scenario' && (
            <ScenarioInput
              options={question.options}
              value={answer}
              onChange={onAnswerChange}
              disabled={submitted}
              correctAnswer={submitted ? question.correct_answer : undefined}
            />
          )}
          
          {submitted && result && (
            <div className="mt-4 p-4 bg-muted rounded-lg">
              <p className={cn(
                "font-medium",
                result.correct ? "text-green-600" : "text-red-600"
              )}>
                {result.correct ? '✓ Correct!' : '✗ Incorrect'}
              </p>
              <p className="mt-2 text-sm">{result.explanation}</p>
            </div>
          )}
        </div>
      </div>
    </Card>
  );
}
```

### Multiple Choice Input

The multiple choice component uses Radix UI's RadioGroup:

```typescript
function MultipleChoiceInput({
  options,
  value,
  onChange,
  disabled,
  correctAnswer
}: MultipleChoiceInputProps) {
  return (
    <RadioGroup
      value={value}
      onValueChange={onChange}
      disabled={disabled}
    >
      {options.map((option, index) => (
        <div 
          key={index}
          className={cn(
            "flex items-center space-x-2 p-3 rounded-lg border",
            disabled && option === correctAnswer && "bg-green-50 border-green-500",
            disabled && value === option && option !== correctAnswer && "bg-red-50 border-red-500"
          )}
        >
          <RadioGroupItem value={option} id={`option-${index}`} />
          <label htmlFor={`option-${index}`} className="flex-1 cursor-pointer">
            {option}
          </label>
        </div>
      ))}
    </RadioGroup>
  );
}
```

### Code Completion Input

Code completion questions show a code snippet with a blank to fill:

```typescript
function CodeCompletionInput({
  codeContext,
  value,
  onChange,
  disabled
}: CodeCompletionInputProps) {
  return (
    <div className="space-y-4">
      {codeContext && (
        <pre className="p-4 bg-zinc-900 text-zinc-100 rounded-lg overflow-x-auto">
          <code>{codeContext.code}</code>
        </pre>
      )}
      <Input
        value={value || ''}
        onChange={(e) => onChange(e.target.value)}
        disabled={disabled}
        placeholder="Type your answer..."
        className="font-mono"
      />
    </div>
  );
}
```

---

## Quiz Submission and Grading

The submission flow involves several steps that ensure answers are properly validated, graded, and stored.

### Client-Side Validation

Before submission, the frontend validates that all questions have been answered:

```typescript
const validateSubmission = (quiz: QuizDefinition, answers: Record<string, string>) => {
  const unanswered = quiz.questions.filter(q => !answers[q.id]?.trim());
  
  if (unanswered.length > 0) {
    return {
      valid: false,
      message: `Please answer all questions. ${unanswered.length} remaining.`
    };
  }
  
  return { valid: true };
};
```

### Server-Side Grading

The server performs the actual grading with normalized string comparison:

```typescript
function normalizeAnswer(answer: string): string {
  return answer
    .trim()
    .toLowerCase()
    .replace(/\s+/g, ' '); // Normalize whitespace
}

function compareAnswers(userAnswer: string, correctAnswer: string): boolean {
  return normalizeAnswer(userAnswer) === normalizeAnswer(correctAnswer);
}
```

### Submission Storage Format

Submissions are stored in git notes with the following YAML structure:

```yaml
quizId: security-fundamentals
userId: "123"
answers:
  q1-bcrypt: "Bcrypt includes built-in salting and adaptive cost"
  q2-timing: "Timing attacks on token comparison"
  q3-code-completion: "timingSafeEqual"
  q4-scenario: "Trailing whitespace is being included in the paste"
score: 40
maxScore: 40
passed: true
submittedAt: "2025-01-03T12:00:00.000Z"
```

---

## Progress Tracking

The platform tracks quiz progress per user, storing submissions in git notes and aggregating statistics.

### Progress Data Model

```typescript
interface UserProgress {
  completed: number;      // Number of quizzes completed
  passed: number;         // Number of quizzes passed
  totalScore: number;     // Sum of all scores
  totalMaxScore: number;  // Sum of all max scores
  submissions: QuizSubmission[];
}
```

### Progress Aggregation

The progress endpoint aggregates submission data:

```typescript
async function getUserProgress(
  repoPath: string, 
  userId: string
): Promise<UserProgress> {
  const submissions = await gitNotes.getQuizSubmissions(repoPath, userId);
  
  // Deduplicate by quiz ID, keeping best attempt
  const bestAttempts = new Map<string, QuizSubmission>();
  for (const submission of submissions) {
    const existing = bestAttempts.get(submission.quizId);
    if (!existing || submission.score > existing.score) {
      bestAttempts.set(submission.quizId, submission);
    }
  }
  
  const best = Array.from(bestAttempts.values());
  
  return {
    completed: best.length,
    passed: best.filter(s => s.passed).length,
    totalScore: best.reduce((sum, s) => sum + s.score, 0),
    totalMaxScore: best.reduce((sum, s) => sum + s.maxScore, 0),
    submissions: best
  };
}
```

### Progress Display Component

The quizzes list page shows progress statistics:

```typescript
function ProgressStats({ progress }: { progress: UserProgress }) {
  return (
    <div className="grid grid-cols-3 gap-4 mb-8">
      <Card className="p-4 text-center">
        <p className="text-3xl font-bold">{progress.completed}</p>
        <p className="text-sm text-muted-foreground">Completed</p>
      </Card>
      <Card className="p-4 text-center">
        <p className="text-3xl font-bold">{progress.passed}</p>
        <p className="text-sm text-muted-foreground">Passed</p>
      </Card>
      <Card className="p-4 text-center">
        <p className="text-3xl font-bold">
          {progress.totalMaxScore > 0 
            ? Math.round((progress.totalScore / progress.totalMaxScore) * 100)
            : 0}%
        </p>
        <p className="text-sm text-muted-foreground">Average Score</p>
      </Card>
    </div>
  );
}
```

---

## Embedded Quizzes in Annotations

Code review annotations can include embedded quizzes to test understanding of specific code sections.

### Embedded Quiz Structure

Embedded quizzes are simpler than standalone quizzes, containing a single question:

```yaml
annotations:
  - id: bcrypt-explanation
    type: educational
    file: src/routes/auth.js
    line_start: 35
    title: Why bcrypt for password hashing?
    content: |
      Bcrypt is specifically designed for password hashing...
    quiz:
      question: Why is bcrypt preferred over SHA256?
      type: multiple_choice
      options:
        - SHA256 is cryptographically broken
        - Bcrypt is faster to compute
        - Bcrypt includes built-in salting and adaptive cost
        - SHA256 produces shorter hashes
      correct_answer: Bcrypt includes built-in salting and adaptive cost
      explanation: Bcrypt's slowness is a feature...
```

### Rendering Embedded Quizzes

The annotation component checks for an embedded quiz and renders it inline:

```typescript
function AnnotationCard({ annotation }: { annotation: Annotation }) {
  const [quizAnswer, setQuizAnswer] = useState('');
  const [quizSubmitted, setQuizSubmitted] = useState(false);
  
  return (
    <Card className="p-4">
      <h3 className="font-semibold">{annotation.title}</h3>
      <div className="prose prose-sm mt-2">
        <Streamdown>{annotation.content}</Streamdown>
      </div>
      
      {annotation.quiz && (
        <div className="mt-4 p-4 bg-muted rounded-lg">
          <p className="font-medium mb-2">Quick Quiz</p>
          <p className="mb-4">{annotation.quiz.question}</p>
          
          <RadioGroup
            value={quizAnswer}
            onValueChange={setQuizAnswer}
            disabled={quizSubmitted}
          >
            {annotation.quiz.options?.map((option, i) => (
              <div key={i} className="flex items-center space-x-2">
                <RadioGroupItem value={option} id={`quiz-${i}`} />
                <label htmlFor={`quiz-${i}`}>{option}</label>
              </div>
            ))}
          </RadioGroup>
          
          {!quizSubmitted ? (
            <Button 
              onClick={() => setQuizSubmitted(true)}
              className="mt-4"
              size="sm"
            >
              Check Answer
            </Button>
          ) : (
            <div className={cn(
              "mt-4 p-3 rounded",
              quizAnswer === annotation.quiz.correct_answer 
                ? "bg-green-100" 
                : "bg-red-100"
            )}>
              <p className="font-medium">
                {quizAnswer === annotation.quiz.correct_answer 
                  ? '✓ Correct!' 
                  : '✗ Incorrect'}
              </p>
              <p className="text-sm mt-1">{annotation.quiz.explanation}</p>
            </div>
          )}
        </div>
      )}
    </Card>
  );
}
```

---

## Testing with CLI Tools

The platform includes CLI tools for testing quiz functionality without the web interface.

### Quiz CLI Commands

The `cli/api-test.ts` file provides commands for quiz operations:

```bash
# List all quizzes
npx tsx cli/api-test.ts list-quizzes /path/to/repo

# Get a specific quiz
npx tsx cli/api-test.ts get-quiz /path/to/repo <commit> <quizId>

# Simulate quiz submission (for testing)
npx tsx cli/api-test.ts submit-quiz /path/to/repo <quizId> '{"q1":"answer1","q2":"answer2"}'
```

### YAML Parser CLI

The `cli/yaml-parser.ts` tool validates quiz YAML:

```bash
# Validate a quiz file
npx tsx cli/yaml-parser.ts validate-quiz /path/to/quiz.yaml

# Parse and display quiz structure
npx tsx cli/yaml-parser.ts parse-quiz /path/to/quiz.yaml
```

### Git Notes CLI

The `cli/git-notes.ts` tool manages quiz storage:

```bash
# List all quizzes in git notes
npx tsx cli/git-notes.ts list-quizzes /path/to/repo

# Write a quiz to git notes
npx tsx cli/git-notes.ts write-quiz /path/to/repo <commit> /path/to/quiz.yaml

# Read quiz submissions
npx tsx cli/git-notes.ts list-submissions /path/to/repo <userId>
```

These CLI tools are invaluable for debugging issues, testing new quiz content, and automating quiz management tasks.

---

## Summary

The quiz system demonstrates a well-architected approach to building educational features on top of git. By using git notes for storage, the platform maintains a complete history of quiz definitions and submissions alongside the code they reference. The layered architecture—storage, parsing, API, and frontend—provides clear separation of concerns and makes the system maintainable and testable.

Key implementation decisions include using isomorphic-git for portability, YAML for human-readable definitions, tRPC for type-safe APIs, and React with shadcn/ui for a polished user interface. The CLI tools enable testing and automation without requiring the full web stack.

This architecture can be extended to support additional features like quiz versioning, collaborative quiz authoring, and integration with external learning management systems.

---

*This document is part of the Code Review Knowledge Platform documentation.*
