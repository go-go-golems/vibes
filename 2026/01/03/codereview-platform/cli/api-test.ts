#!/usr/bin/env npx tsx
/**
 * CLI tool for testing API endpoints directly (without HTTP)
 * Usage: npx tsx cli/api-test.ts <command> [options]
 */

import { createGitNotesStorage } from '../server/lib/git-notes';
import { createYamlParser } from '../server/lib/yaml-parser';
import hljs from 'highlight.js';
import * as YAML from 'yaml';

const args = process.argv.slice(2);
const command = args[0];

// Get language from file extension
function getLanguageFromPath(filePath: string): string {
  const ext = filePath.split('.').pop()?.toLowerCase() || '';
  const langMap: Record<string, string> = {
    'js': 'javascript', 'jsx': 'javascript', 'ts': 'typescript', 'tsx': 'typescript',
    'py': 'python', 'rb': 'ruby', 'go': 'go', 'rs': 'rust', 'java': 'java',
    'c': 'c', 'cpp': 'cpp', 'h': 'c', 'hpp': 'cpp', 'cs': 'csharp',
    'php': 'php', 'swift': 'swift', 'kt': 'kotlin', 'scala': 'scala',
    'sh': 'bash', 'bash': 'bash', 'yml': 'yaml', 'yaml': 'yaml',
    'json': 'json', 'xml': 'xml', 'html': 'html', 'css': 'css',
    'md': 'markdown', 'sql': 'sql',
  };
  return langMap[ext] || 'plaintext';
}

async function main() {
  const repoPath = args.find(a => a.startsWith('--repo='))?.split('=')[1] || '/home/ubuntu/test-repo';

  switch (command) {
    case 'list-reviews':
      await handleListReviews(repoPath);
      break;
    
    case 'get-review':
      await handleGetReview(repoPath, args);
      break;
    
    case 'list-quizzes':
      await handleListQuizzes(repoPath);
      break;
    
    case 'list-guides':
      await handleListGuides(repoPath);
      break;
    
    case 'file-tree':
      await handleFileTree(repoPath, args);
      break;
    
    case 'file-content':
      await handleFileContent(repoPath, args);
      break;
    
    case 'file-annotations':
      await handleFileAnnotations(repoPath, args);
      break;
    
    case 'branches':
      await handleBranches(repoPath);
      break;
    
    case 'diff':
      await handleDiff(repoPath, args);
      break;
    
    case 'test-all':
      await handleTestAll(repoPath);
      break;
    
    default:
      printHelp();
  }
}

async function handleListReviews(repoPath: string) {
  const storage = createGitNotesStorage(repoPath);
  const reviews = await storage.getAllReviews();
  
  console.log('=== Reviews ===\n');
  for (const { commit, review } of reviews) {
    console.log(`[${commit.slice(0, 8)}] ${review.title}`);
    console.log(`  PR: ${review.pr || 'N/A'}`);
    console.log(`  Branches: ${review.baseBranch || 'N/A'} → ${review.headBranch || 'N/A'}`);
    console.log(`  Annotations: ${review.annotations.length}`);
    console.log(`  Files: ${Array.from(new Set(review.annotations.map(a => a.file))).join(', ')}`);
    console.log('');
  }
}

async function handleGetReview(repoPath: string, args: string[]) {
  const commit = args.find(a => a.startsWith('--commit='))?.split('=')[1];
  
  if (!commit) {
    console.error('Usage: get-review --commit=<commit>');
    process.exit(1);
  }
  
  const storage = createGitNotesStorage(repoPath);
  const review = await storage.getReview(commit);
  
  if (!review) {
    console.log(`No review found at commit ${commit}`);
    return;
  }
  
  console.log('=== Review Details ===\n');
  console.log(`Title: ${review.title}`);
  console.log(`PR: ${review.pr || 'N/A'}`);
  console.log(`Description: ${review.description || 'N/A'}`);
  console.log(`Branches: ${review.baseBranch || 'N/A'} → ${review.headBranch || 'N/A'}`);
  console.log(`\nAnnotations (${review.annotations.length}):\n`);
  
  for (const annotation of review.annotations) {
    console.log(`  📍 ${annotation.file}:${annotation.line}`);
    console.log(`     Type: ${annotation.type}`);
    console.log(`     Title: ${annotation.title || 'N/A'}`);
    console.log(`     Content: ${annotation.content.slice(0, 100)}...`);
    if (annotation.tags?.length) {
      console.log(`     Tags: ${annotation.tags.join(', ')}`);
    }
    if (annotation.quiz) {
      console.log(`     Quiz: ${annotation.quiz.title} (${annotation.quiz.questions.length} questions)`);
    }
    console.log('');
  }
}

async function handleListQuizzes(repoPath: string) {
  const storage = createGitNotesStorage(repoPath);
  const parser = createYamlParser();
  const quizzes = await storage.getAllQuizzes();
  
  console.log('=== Quizzes ===\n');
  for (const { commit, quiz } of quizzes) {
    const stats = parser.getQuizStats(quiz as any);
    console.log(`[${commit.slice(0, 8)}] ${quiz.title}`);
    console.log(`  ID: ${quiz.id}`);
    console.log(`  Difficulty: ${quiz.difficulty || 'N/A'}`);
    console.log(`  Time: ${quiz.estimatedTime || 'N/A'}`);
    console.log(`  Questions: ${stats.totalQuestions}`);
    console.log(`  Types: ${Object.entries(stats.byType).map(([t, c]) => `${t}(${c})`).join(', ')}`);
    console.log('');
  }
}

async function handleListGuides(repoPath: string) {
  const storage = createGitNotesStorage(repoPath);
  const parser = createYamlParser();
  const guides = await storage.getAllGuides();
  
  console.log('=== Guides ===\n');
  for (const { commit, guide } of guides) {
    const files = parser.extractGuideFileReferences(guide as any);
    console.log(`[${commit.slice(0, 8)}] ${guide.title}`);
    console.log(`  ID: ${guide.id}`);
    console.log(`  Difficulty: ${guide.difficulty || 'N/A'}`);
    console.log(`  Time: ${guide.estimatedTime || 'N/A'}`);
    console.log(`  Stops: ${guide.stops.length}`);
    console.log(`  Files: ${files.join(', ')}`);
    if (guide.prerequisites?.length) {
      console.log(`  Prerequisites: ${guide.prerequisites.length}`);
    }
    console.log('');
  }
}

async function handleFileTree(repoPath: string, args: string[]) {
  const branch = args.find(a => a.startsWith('--branch='))?.split('=')[1];
  const path = args.find(a => a.startsWith('--path='))?.split('=')[1] || '';
  
  const storage = createGitNotesStorage(repoPath);
  const commit = branch 
    ? await storage.getBranchCommit(branch)
    : await storage.getHeadCommit();
  
  const tree = await storage.getFileTree(commit, path);
  
  console.log(`=== File Tree (${branch || 'HEAD'}) ===\n`);
  console.log(`Path: ${path || '/'}\n`);
  
  for (const item of tree) {
    const icon = item.type === 'dir' ? '📁' : '📄';
    console.log(`${icon} ${item.name}`);
  }
}

async function handleFileContent(repoPath: string, args: string[]) {
  const file = args.find(a => a.startsWith('--file='))?.split('=')[1];
  const branch = args.find(a => a.startsWith('--branch='))?.split('=')[1];
  const highlight = args.includes('--highlight');
  
  if (!file) {
    console.error('Usage: file-content --file=<path> [--branch=<branch>] [--highlight]');
    process.exit(1);
  }
  
  const storage = createGitNotesStorage(repoPath);
  const commit = branch 
    ? await storage.getBranchCommit(branch)
    : await storage.getHeadCommit();
  
  const content = await storage.getFileContent(commit, file);
  
  if (!content) {
    console.log(`File not found: ${file}`);
    return;
  }
  
  const language = getLanguageFromPath(file);
  console.log(`=== ${file} (${language}) ===\n`);
  
  if (highlight) {
    try {
      const highlighted = hljs.highlight(content, { language }).value;
      // Strip HTML for terminal display
      const stripped = highlighted.replace(/<[^>]+>/g, '');
      const lines = stripped.split('\n');
      lines.forEach((line, i) => {
        console.log(`${String(i + 1).padStart(4)} │ ${line}`);
      });
    } catch {
      console.log(content);
    }
  } else {
    const lines = content.split('\n');
    lines.forEach((line, i) => {
      console.log(`${String(i + 1).padStart(4)} │ ${line}`);
    });
  }
}

async function handleFileAnnotations(repoPath: string, args: string[]) {
  const file = args.find(a => a.startsWith('--file='))?.split('=')[1];
  
  if (!file) {
    console.error('Usage: file-annotations --file=<path>');
    process.exit(1);
  }
  
  const storage = createGitNotesStorage(repoPath);
  const reviews = await storage.getAllReviews();
  
  console.log(`=== Annotations for ${file} ===\n`);
  
  let found = false;
  for (const { commit, review } of reviews) {
    const fileAnnotations = review.annotations.filter(a => a.file === file);
    if (fileAnnotations.length > 0) {
      found = true;
      console.log(`From review: ${review.title} [${commit.slice(0, 8)}]`);
      for (const annotation of fileAnnotations) {
        console.log(`\n  Line ${annotation.line}: [${annotation.type}] ${annotation.title || ''}`);
        console.log(`  ${annotation.content.split('\n')[0]}...`);
        if (annotation.quiz) {
          console.log(`  📝 Quiz: ${annotation.quiz.title}`);
        }
      }
      console.log('');
    }
  }
  
  if (!found) {
    console.log('No annotations found for this file.');
  }
}

async function handleBranches(repoPath: string) {
  const storage = createGitNotesStorage(repoPath);
  const branches = await storage.getBranches();
  
  console.log('=== Branches ===\n');
  for (const branch of branches) {
    const commit = await storage.getBranchCommit(branch.name);
    const marker = branch.current ? '* ' : '  ';
    console.log(`${marker}${branch.name} (${commit.slice(0, 8)})`);
  }
}

async function handleDiff(repoPath: string, args: string[]) {
  const base = args.find(a => a.startsWith('--base='))?.split('=')[1];
  const head = args.find(a => a.startsWith('--head='))?.split('=')[1];
  
  if (!base) {
    console.error('Usage: diff --base=<commit|branch> [--head=<commit|branch>]');
    process.exit(1);
  }
  
  const storage = createGitNotesStorage(repoPath);
  const headCommit = head 
    ? await storage.getBranchCommit(head).catch(() => head)
    : await storage.getHeadCommit();
  const baseCommit = await storage.getBranchCommit(base).catch(() => base);
  
  const diff = await storage.getDiff(baseCommit, headCommit);
  
  console.log(`=== Diff: ${base} → ${head || 'HEAD'} ===\n`);
  console.log(diff || '(no changes)');
}

async function handleTestAll(repoPath: string) {
  console.log('=== API Test Suite ===\n');
  
  const storage = createGitNotesStorage(repoPath);
  const parser = createYamlParser();
  let passed = 0;
  let failed = 0;
  
  // Test 1: List branches
  console.log('Test 1: List branches');
  try {
    const branches = await storage.getBranches();
    if (branches.length > 0) {
      console.log(`  ✅ PASSED (${branches.length} branches)\n`);
      passed++;
    } else {
      console.log('  ❌ FAILED: No branches found\n');
      failed++;
    }
  } catch (e) {
    console.log(`  ❌ FAILED: ${e}\n`);
    failed++;
  }
  
  // Test 2: Get file tree
  console.log('Test 2: Get file tree');
  try {
    const tree = await storage.getFileTree(await storage.getHeadCommit());
    if (tree.length > 0) {
      console.log(`  ✅ PASSED (${tree.length} items)\n`);
      passed++;
    } else {
      console.log('  ❌ FAILED: Empty tree\n');
      failed++;
    }
  } catch (e) {
    console.log(`  ❌ FAILED: ${e}\n`);
    failed++;
  }
  
  // Test 3: Get file content
  console.log('Test 3: Get file content');
  try {
    const content = await storage.getFileContent(await storage.getHeadCommit(), 'src/auth.js');
    if (content && content.length > 0) {
      console.log(`  ✅ PASSED (${content.length} bytes)\n`);
      passed++;
    } else {
      console.log('  ❌ FAILED: No content\n');
      failed++;
    }
  } catch (e) {
    console.log(`  ❌ FAILED: ${e}\n`);
    failed++;
  }
  
  // Test 4: List reviews
  console.log('Test 4: List reviews');
  try {
    const reviews = await storage.getAllReviews();
    if (reviews.length > 0) {
      console.log(`  ✅ PASSED (${reviews.length} reviews)\n`);
      passed++;
    } else {
      console.log('  ⚠️  WARNING: No reviews found\n');
      passed++; // Still pass, just no data
    }
  } catch (e) {
    console.log(`  ❌ FAILED: ${e}\n`);
    failed++;
  }
  
  // Test 5: List quizzes
  console.log('Test 5: List quizzes');
  try {
    const quizzes = await storage.getAllQuizzes();
    if (quizzes.length > 0) {
      console.log(`  ✅ PASSED (${quizzes.length} quizzes)\n`);
      passed++;
    } else {
      console.log('  ⚠️  WARNING: No quizzes found\n');
      passed++;
    }
  } catch (e) {
    console.log(`  ❌ FAILED: ${e}\n`);
    failed++;
  }
  
  // Test 6: List guides
  console.log('Test 6: List guides');
  try {
    const guides = await storage.getAllGuides();
    if (guides.length > 0) {
      console.log(`  ✅ PASSED (${guides.length} guides)\n`);
      passed++;
    } else {
      console.log('  ⚠️  WARNING: No guides found\n');
      passed++;
    }
  } catch (e) {
    console.log(`  ❌ FAILED: ${e}\n`);
    failed++;
  }
  
  // Test 7: Syntax highlighting
  console.log('Test 7: Syntax highlighting');
  try {
    const code = 'const x = 1;';
    const result = hljs.highlight(code, { language: 'javascript' });
    if (result.value.includes('hljs')) {
      console.log('  ✅ PASSED\n');
      passed++;
    } else {
      console.log('  ❌ FAILED: No highlighting\n');
      failed++;
    }
  } catch (e) {
    console.log(`  ❌ FAILED: ${e}\n`);
    failed++;
  }
  
  // Test 8: Get commit log
  console.log('Test 8: Get commit log');
  try {
    const log = await storage.getCommitLog(undefined, 10);
    if (log.length > 0) {
      console.log(`  ✅ PASSED (${log.length} commits)\n`);
      passed++;
    } else {
      console.log('  ❌ FAILED: No commits\n');
      failed++;
    }
  } catch (e) {
    console.log(`  ❌ FAILED: ${e}\n`);
    failed++;
  }
  
  // Summary
  console.log('=== Test Summary ===');
  console.log(`Passed: ${passed}/${passed + failed}`);
  console.log(`Failed: ${failed}/${passed + failed}`);
  
  if (failed > 0) {
    process.exit(1);
  }
}

function printHelp() {
  console.log(`
API Test CLI - Test API endpoints directly

Usage: npx tsx cli/api-test.ts <command> [options]

Commands:
  list-reviews                  List all reviews
  get-review --commit=<c>       Get review details
  list-quizzes                  List all quizzes
  list-guides                   List all guides
  file-tree [--branch=<b>] [--path=<p>]
                                Show file tree
  file-content --file=<f> [--branch=<b>] [--highlight]
                                Show file content
  file-annotations --file=<f>   Show annotations for a file
  branches                      List branches
  diff --base=<c> [--head=<c>]  Show diff
  test-all                      Run all API tests

Global Options:
  --repo=<path>                 Repository path (default: /home/ubuntu/test-repo)
`);
}

main().catch(err => {
  console.error('Error:', err.message);
  process.exit(1);
});
