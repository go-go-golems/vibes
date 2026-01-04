#!/usr/bin/env npx tsx
/**
 * CLI tool for testing git notes storage operations
 * Usage: npx tsx cli/git-notes.ts <command> [options]
 */

import { createGitNotesStorage, GIT_NOTES_REFS, type ReviewDefinition, type QuizDefinition, type GuideDefinition, type QuizSubmission } from '../server/lib/git-notes';
import * as fs from 'fs';
import * as path from 'path';
import * as YAML from 'yaml';

const args = process.argv.slice(2);
const command = args[0];

async function main() {
  const repoPath = args.find(a => a.startsWith('--repo='))?.split('=')[1] || process.cwd();
  
  if (!fs.existsSync(path.join(repoPath, '.git'))) {
    console.error(`Error: ${repoPath} is not a git repository`);
    process.exit(1);
  }

  const storage = createGitNotesStorage(repoPath);
  await storage.initialize();

  switch (command) {
    case 'init':
      await handleInit(storage);
      break;
    
    case 'write-review':
      await handleWriteReview(storage, args);
      break;
    
    case 'read-review':
      await handleReadReview(storage, args);
      break;
    
    case 'write-quiz':
      await handleWriteQuiz(storage, args);
      break;
    
    case 'read-quiz':
      await handleReadQuiz(storage, args);
      break;
    
    case 'write-guide':
      await handleWriteGuide(storage, args);
      break;
    
    case 'read-guide':
      await handleReadGuide(storage, args);
      break;
    
    case 'submit-quiz':
      await handleSubmitQuiz(storage, args);
      break;
    
    case 'get-submissions':
      await handleGetSubmissions(storage, args);
      break;
    
    case 'list':
      await handleList(storage, args);
      break;
    
    case 'branches':
      await handleBranches(storage);
      break;
    
    case 'tree':
      await handleTree(storage, args);
      break;
    
    case 'show':
      await handleShow(storage, args);
      break;
    
    case 'diff':
      await handleDiff(storage, args);
      break;
    
    default:
      printHelp();
  }
}

async function handleInit(storage: ReturnType<typeof createGitNotesStorage>) {
  console.log('Initializing git notes refs...');
  await storage.initialize();
  console.log('Git notes refs initialized:');
  for (const [name, ref] of Object.entries(GIT_NOTES_REFS)) {
    console.log(`  ${name}: ${ref}`);
  }
}

async function handleWriteReview(storage: ReturnType<typeof createGitNotesStorage>, args: string[]) {
  const yamlFile = args.find(a => a.startsWith('--file='))?.split('=')[1];
  const commit = args.find(a => a.startsWith('--commit='))?.split('=')[1] || await storage.getHeadCommit();
  
  if (!yamlFile) {
    console.error('Usage: write-review --file=<yaml-file> [--commit=<commit>]');
    process.exit(1);
  }
  
  const content = fs.readFileSync(yamlFile, 'utf-8');
  const data = YAML.parse(content);
  
  if (!data.review) {
    console.error('YAML file must contain a "review" key');
    process.exit(1);
  }
  
  await storage.storeReview(commit, data.review as ReviewDefinition);
  console.log(`Review stored at commit ${commit}`);
  console.log(YAML.stringify(data.review));
}

async function handleReadReview(storage: ReturnType<typeof createGitNotesStorage>, args: string[]) {
  const commit = args.find(a => a.startsWith('--commit='))?.split('=')[1] || await storage.getHeadCommit();
  
  const review = await storage.getReview(commit);
  if (review) {
    console.log(`Review at commit ${commit}:`);
    console.log(YAML.stringify({ review }));
  } else {
    console.log(`No review found at commit ${commit}`);
  }
}

async function handleWriteQuiz(storage: ReturnType<typeof createGitNotesStorage>, args: string[]) {
  const yamlFile = args.find(a => a.startsWith('--file='))?.split('=')[1];
  const commit = args.find(a => a.startsWith('--commit='))?.split('=')[1] || await storage.getHeadCommit();
  
  if (!yamlFile) {
    console.error('Usage: write-quiz --file=<yaml-file> [--commit=<commit>]');
    process.exit(1);
  }
  
  const content = fs.readFileSync(yamlFile, 'utf-8');
  const data = YAML.parse(content);
  
  if (!data.quiz) {
    console.error('YAML file must contain a "quiz" key');
    process.exit(1);
  }
  
  await storage.storeQuiz(commit, data.quiz as QuizDefinition);
  console.log(`Quiz stored at commit ${commit}`);
  console.log(YAML.stringify(data.quiz));
}

async function handleReadQuiz(storage: ReturnType<typeof createGitNotesStorage>, args: string[]) {
  const commit = args.find(a => a.startsWith('--commit='))?.split('=')[1] || await storage.getHeadCommit();
  
  const quiz = await storage.getQuiz(commit);
  if (quiz) {
    console.log(`Quiz at commit ${commit}:`);
    console.log(YAML.stringify({ quiz }));
  } else {
    console.log(`No quiz found at commit ${commit}`);
  }
}

async function handleWriteGuide(storage: ReturnType<typeof createGitNotesStorage>, args: string[]) {
  const yamlFile = args.find(a => a.startsWith('--file='))?.split('=')[1];
  const commit = args.find(a => a.startsWith('--commit='))?.split('=')[1] || await storage.getHeadCommit();
  
  if (!yamlFile) {
    console.error('Usage: write-guide --file=<yaml-file> [--commit=<commit>]');
    process.exit(1);
  }
  
  const content = fs.readFileSync(yamlFile, 'utf-8');
  const data = YAML.parse(content);
  
  if (!data.guide) {
    console.error('YAML file must contain a "guide" key');
    process.exit(1);
  }
  
  await storage.storeGuide(commit, data.guide as GuideDefinition);
  console.log(`Guide stored at commit ${commit}`);
  console.log(YAML.stringify(data.guide));
}

async function handleReadGuide(storage: ReturnType<typeof createGitNotesStorage>, args: string[]) {
  const commit = args.find(a => a.startsWith('--commit='))?.split('=')[1] || await storage.getHeadCommit();
  
  const guide = await storage.getGuide(commit);
  if (guide) {
    console.log(`Guide at commit ${commit}:`);
    console.log(YAML.stringify({ guide }));
  } else {
    console.log(`No guide found at commit ${commit}`);
  }
}

async function handleSubmitQuiz(storage: ReturnType<typeof createGitNotesStorage>, args: string[]) {
  const quizId = args.find(a => a.startsWith('--quiz='))?.split('=')[1];
  const userId = args.find(a => a.startsWith('--user='))?.split('=')[1];
  const answersJson = args.find(a => a.startsWith('--answers='))?.split('=')[1];
  const score = parseInt(args.find(a => a.startsWith('--score='))?.split('=')[1] || '0');
  const maxScore = parseInt(args.find(a => a.startsWith('--max='))?.split('=')[1] || '100');
  const commit = args.find(a => a.startsWith('--commit='))?.split('=')[1] || await storage.getHeadCommit();
  
  if (!quizId || !userId) {
    console.error('Usage: submit-quiz --quiz=<quiz-id> --user=<user-id> [--answers=<json>] [--score=<n>] [--max=<n>] [--commit=<commit>]');
    process.exit(1);
  }
  
  const submission: QuizSubmission = {
    quizId,
    userId,
    answers: answersJson ? JSON.parse(answersJson) : {},
    score,
    maxScore,
    submittedAt: new Date().toISOString(),
  };
  
  await storage.storeSubmission(commit, submission);
  console.log(`Quiz submission stored at commit ${commit}`);
  console.log(YAML.stringify(submission));
}

async function handleGetSubmissions(storage: ReturnType<typeof createGitNotesStorage>, args: string[]) {
  const commit = args.find(a => a.startsWith('--commit='))?.split('=')[1] || await storage.getHeadCommit();
  const quizId = args.find(a => a.startsWith('--quiz='))?.split('=')[1];
  const userId = args.find(a => a.startsWith('--user='))?.split('=')[1];
  
  if (quizId && userId) {
    const submission = await storage.getUserSubmission(commit, quizId, userId);
    if (submission) {
      console.log(`Submission for quiz ${quizId} by user ${userId}:`);
      console.log(YAML.stringify(submission));
    } else {
      console.log(`No submission found for quiz ${quizId} by user ${userId}`);
    }
  } else {
    const submissions = await storage.getSubmissions(commit);
    console.log(`All submissions at commit ${commit}:`);
    console.log(YAML.stringify(submissions));
  }
}

async function handleList(storage: ReturnType<typeof createGitNotesStorage>, args: string[]) {
  const type = args.find(a => a.startsWith('--type='))?.split('=')[1] || 'all';
  
  if (type === 'reviews' || type === 'all') {
    console.log('\n=== Reviews ===');
    const reviews = await storage.getAllReviews();
    for (const { commit, review } of reviews) {
      console.log(`[${commit.slice(0, 8)}] ${review.title}`);
    }
    if (reviews.length === 0) console.log('  (none)');
  }
  
  if (type === 'quizzes' || type === 'all') {
    console.log('\n=== Quizzes ===');
    const quizzes = await storage.getAllQuizzes();
    for (const { commit, quiz } of quizzes) {
      console.log(`[${commit.slice(0, 8)}] ${quiz.title} (${quiz.questions.length} questions)`);
    }
    if (quizzes.length === 0) console.log('  (none)');
  }
  
  if (type === 'guides' || type === 'all') {
    console.log('\n=== Guides ===');
    const guides = await storage.getAllGuides();
    for (const { commit, guide } of guides) {
      console.log(`[${commit.slice(0, 8)}] ${guide.title} (${guide.stops.length} stops)`);
    }
    if (guides.length === 0) console.log('  (none)');
  }
}

async function handleBranches(storage: ReturnType<typeof createGitNotesStorage>) {
  const branches = await storage.getBranches();
  console.log('Branches:');
  for (const branch of branches) {
    const marker = branch.current ? '* ' : '  ';
    const commit = await storage.getBranchCommit(branch.name);
    console.log(`${marker}${branch.name} (${commit.slice(0, 8)})`);
  }
}

async function handleTree(storage: ReturnType<typeof createGitNotesStorage>, args: string[]) {
  const commit = args.find(a => a.startsWith('--commit='))?.split('=')[1] || await storage.getHeadCommit();
  const dir = args.find(a => a.startsWith('--dir='))?.split('=')[1] || '';
  
  console.log(`File tree at ${commit.slice(0, 8)}${dir ? ` (${dir})` : ''}:`);
  const tree = await storage.getFileTree(commit, dir);
  
  for (const item of tree) {
    const icon = item.type === 'dir' ? '📁' : '📄';
    console.log(`  ${icon} ${item.name}`);
  }
}

async function handleShow(storage: ReturnType<typeof createGitNotesStorage>, args: string[]) {
  const commit = args.find(a => a.startsWith('--commit='))?.split('=')[1] || await storage.getHeadCommit();
  const file = args.find(a => a.startsWith('--file='))?.split('=')[1];
  
  if (!file) {
    console.error('Usage: show --file=<path> [--commit=<commit>]');
    process.exit(1);
  }
  
  const content = await storage.getFileContent(commit, file);
  if (content) {
    console.log(`Content of ${file} at ${commit.slice(0, 8)}:`);
    console.log('---');
    console.log(content);
  } else {
    console.log(`File ${file} not found at commit ${commit.slice(0, 8)}`);
  }
}

async function handleDiff(storage: ReturnType<typeof createGitNotesStorage>, args: string[]) {
  const base = args.find(a => a.startsWith('--base='))?.split('=')[1];
  const head = args.find(a => a.startsWith('--head='))?.split('=')[1] || await storage.getHeadCommit();
  const file = args.find(a => a.startsWith('--file='))?.split('=')[1];
  
  if (!base) {
    console.error('Usage: diff --base=<commit> [--head=<commit>] [--file=<path>]');
    process.exit(1);
  }
  
  let diff: string;
  if (file) {
    diff = await storage.getFileDiff(base, head, file);
    console.log(`Diff for ${file} between ${base.slice(0, 8)} and ${head.slice(0, 8)}:`);
  } else {
    diff = await storage.getDiff(base, head);
    console.log(`Diff between ${base.slice(0, 8)} and ${head.slice(0, 8)}:`);
  }
  
  console.log('---');
  console.log(diff || '(no changes)');
}

function printHelp() {
  console.log(`
Git Notes CLI - Test git notes storage operations

Usage: npx tsx cli/git-notes.ts <command> [options]

Commands:
  init                          Initialize git notes refs
  write-review --file=<yaml>    Store a code review from YAML file
  read-review [--commit=<c>]    Read a code review
  write-quiz --file=<yaml>      Store a quiz from YAML file
  read-quiz [--commit=<c>]      Read a quiz
  write-guide --file=<yaml>     Store a review guide from YAML file
  read-guide [--commit=<c>]     Read a review guide
  submit-quiz --quiz=<id> --user=<id> [--answers=<json>] [--score=<n>]
                                Submit a quiz answer
  get-submissions [--quiz=<id>] [--user=<id>]
                                Get quiz submissions
  list [--type=<reviews|quizzes|guides|all>]
                                List all stored items
  branches                      List repository branches
  tree [--commit=<c>] [--dir=<path>]
                                Show file tree
  show --file=<path> [--commit=<c>]
                                Show file content
  diff --base=<c> [--head=<c>] [--file=<path>]
                                Show diff between commits

Global Options:
  --repo=<path>                 Repository path (default: current directory)
`);
}

main().catch(err => {
  console.error('Error:', err.message);
  process.exit(1);
});
