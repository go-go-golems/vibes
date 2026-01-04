#!/usr/bin/env npx tsx
/**
 * CLI tool for testing YAML DSL parsing
 * Usage: npx tsx cli/yaml-parser.ts <command> [options]
 */

import { createYamlParser } from '../server/lib/yaml-parser';
import * as fs from 'fs';
import * as YAML from 'yaml';

const args = process.argv.slice(2);
const command = args[0];

async function main() {
  const parser = createYamlParser();

  switch (command) {
    case 'parse':
      await handleParse(parser, args);
      break;
    
    case 'validate':
      await handleValidate(parser, args);
      break;
    
    case 'stats':
      await handleStats(parser, args);
      break;
    
    case 'extract-files':
      await handleExtractFiles(parser, args);
      break;
    
    case 'test-all':
      await handleTestAll(parser);
      break;
    
    default:
      printHelp();
  }
}

async function handleParse(parser: ReturnType<typeof createYamlParser>, args: string[]) {
  const file = args.find(a => a.startsWith('--file='))?.split('=')[1];
  
  if (!file) {
    console.error('Usage: parse --file=<yaml-file>');
    process.exit(1);
  }
  
  const content = fs.readFileSync(file, 'utf-8');
  const result = parser.parseAuto(content);
  
  if (result.success) {
    console.log(`✅ Successfully parsed as: ${result.data!.type}`);
    console.log('\nParsed data:');
    console.log(YAML.stringify(result.data!.data));
  } else {
    console.log('❌ Parse failed:');
    result.errors?.forEach(e => console.log(`  - ${e}`));
    process.exit(1);
  }
}

async function handleValidate(parser: ReturnType<typeof createYamlParser>, args: string[]) {
  const file = args.find(a => a.startsWith('--file='))?.split('=')[1];
  const type = args.find(a => a.startsWith('--type='))?.split('=')[1];
  
  if (!file) {
    console.error('Usage: validate --file=<yaml-file> [--type=<review|quiz|guide>]');
    process.exit(1);
  }
  
  const content = fs.readFileSync(file, 'utf-8');
  
  let result;
  switch (type) {
    case 'review':
      result = parser.parseReview(content);
      break;
    case 'quiz':
      result = parser.parseQuiz(content);
      break;
    case 'guide':
      result = parser.parseGuide(content);
      // Additional validation for guides
      if (result.success) {
        const linkErrors = parser.validateGuideLinks(result.data!);
        if (linkErrors.length > 0) {
          console.log('⚠️  Guide link validation warnings:');
          linkErrors.forEach(e => console.log(`  - ${e}`));
        }
      }
      break;
    default:
      result = parser.parseAuto(content);
  }
  
  if (result.success) {
    console.log('✅ Validation passed');
  } else {
    console.log('❌ Validation failed:');
    result.errors?.forEach(e => console.log(`  - ${e}`));
    process.exit(1);
  }
}

async function handleStats(parser: ReturnType<typeof createYamlParser>, args: string[]) {
  const file = args.find(a => a.startsWith('--file='))?.split('=')[1];
  
  if (!file) {
    console.error('Usage: stats --file=<yaml-file>');
    process.exit(1);
  }
  
  const content = fs.readFileSync(file, 'utf-8');
  const result = parser.parseAuto(content);
  
  if (!result.success) {
    console.log('❌ Parse failed:');
    result.errors?.forEach(e => console.log(`  - ${e}`));
    process.exit(1);
  }
  
  const { type, data } = result.data!;
  
  console.log(`Type: ${type}\n`);
  
  switch (type) {
    case 'quiz':
      const quizData = data as any;
      const stats = parser.getQuizStats(quizData);
      console.log('Quiz Statistics:');
      console.log(`  Title: ${quizData.title}`);
      console.log(`  Total Questions: ${stats.totalQuestions}`);
      console.log(`  Estimated Points: ${stats.estimatedPoints}`);
      console.log(`  Questions by Type:`);
      Object.entries(stats.byType).forEach(([t, count]) => {
        console.log(`    - ${t}: ${count}`);
      });
      break;
    
    case 'review':
      const reviewData = data as any;
      console.log('Review Statistics:');
      console.log(`  Title: ${reviewData.title}`);
      console.log(`  PR: ${reviewData.pr || 'N/A'}`);
      console.log(`  Annotations: ${reviewData.annotations.length}`);
      const annotationTypes: Record<string, number> = {};
      reviewData.annotations.forEach((a: any) => {
        annotationTypes[a.type] = (annotationTypes[a.type] || 0) + 1;
      });
      console.log(`  Annotation Types:`);
      Object.entries(annotationTypes).forEach(([t, count]) => {
        console.log(`    - ${t}: ${count}`);
      });
      const files = parser.extractFileReferences(reviewData);
      console.log(`  Files Referenced: ${files.length}`);
      files.forEach(f => console.log(`    - ${f}`));
      break;
    
    case 'guide':
      const guideData = data as any;
      console.log('Guide Statistics:');
      console.log(`  Title: ${guideData.title}`);
      console.log(`  Difficulty: ${guideData.difficulty || 'N/A'}`);
      console.log(`  Estimated Time: ${guideData.estimatedTime || 'N/A'}`);
      console.log(`  Prerequisites: ${guideData.prerequisites?.length || 0}`);
      console.log(`  Stops: ${guideData.stops.length}`);
      const guideFiles = parser.extractGuideFileReferences(guideData);
      console.log(`  Files Referenced: ${guideFiles.length}`);
      guideFiles.forEach(f => console.log(`    - ${f}`));
      break;
  }
}

async function handleExtractFiles(parser: ReturnType<typeof createYamlParser>, args: string[]) {
  const file = args.find(a => a.startsWith('--file='))?.split('=')[1];
  
  if (!file) {
    console.error('Usage: extract-files --file=<yaml-file>');
    process.exit(1);
  }
  
  const content = fs.readFileSync(file, 'utf-8');
  const result = parser.parseAuto(content);
  
  if (!result.success) {
    console.log('❌ Parse failed:');
    result.errors?.forEach(e => console.log(`  - ${e}`));
    process.exit(1);
  }
  
  const { type, data } = result.data!;
  let files: string[] = [];
  
  switch (type) {
    case 'review':
      files = parser.extractFileReferences(data as any);
      break;
    case 'guide':
      files = parser.extractGuideFileReferences(data as any);
      break;
    default:
      console.log('File extraction not supported for this type');
      process.exit(1);
  }
  
  console.log('Referenced files:');
  files.forEach(f => console.log(f));
}

async function handleTestAll(parser: ReturnType<typeof createYamlParser>) {
  console.log('=== YAML Parser Test Suite ===\n');
  
  let passed = 0;
  let failed = 0;
  
  // Test 1: Valid review
  console.log('Test 1: Parse valid review');
  const validReview = `
review:
  pr: 123
  title: "Test Review"
  description: "A test review"
  annotations:
    - file: src/test.js
      line: 10
      type: educational
      content: "This is a test annotation"
`;
  const reviewResult = parser.parseReview(validReview);
  if (reviewResult.success) {
    console.log('  ✅ PASSED\n');
    passed++;
  } else {
    console.log('  ❌ FAILED:', reviewResult.errors);
    failed++;
  }
  
  // Test 2: Valid quiz
  console.log('Test 2: Parse valid quiz');
  const validQuiz = `
quiz:
  id: test-quiz
  title: "Test Quiz"
  questions:
    - type: multiple_choice
      question: "What is 2+2?"
      options: ["3", "4", "5"]
      correct: 1
`;
  const quizResult = parser.parseQuiz(validQuiz);
  if (quizResult.success) {
    console.log('  ✅ PASSED\n');
    passed++;
  } else {
    console.log('  ❌ FAILED:', quizResult.errors);
    failed++;
  }
  
  // Test 3: Valid guide
  console.log('Test 3: Parse valid guide');
  const validGuide = `
guide:
  id: test-guide
  title: "Test Guide"
  stops:
    - id: stop1
      file: src/test.js
      line: 1
      title: "First Stop"
      content: "Welcome to the guide"
      next: stop2
    - id: stop2
      file: src/test.js
      line: 10
      title: "Second Stop"
      content: "End of guide"
`;
  const guideResult = parser.parseGuide(validGuide);
  if (guideResult.success) {
    console.log('  ✅ PASSED\n');
    passed++;
  } else {
    console.log('  ❌ FAILED:', guideResult.errors);
    failed++;
  }
  
  // Test 4: Invalid review (missing required fields)
  console.log('Test 4: Reject invalid review (missing title)');
  const invalidReview = `
review:
  pr: 123
  annotations: []
`;
  const invalidReviewResult = parser.parseReview(invalidReview);
  if (!invalidReviewResult.success && invalidReviewResult.errors?.some(e => e.includes('title'))) {
    console.log('  ✅ PASSED (correctly rejected)\n');
    passed++;
  } else {
    console.log('  ❌ FAILED: Should have rejected missing title');
    failed++;
  }
  
  // Test 5: Invalid quiz (no questions)
  console.log('Test 5: Reject invalid quiz (no questions)');
  const invalidQuiz = `
quiz:
  id: test
  title: "Test"
  questions: []
`;
  const invalidQuizResult = parser.parseQuiz(invalidQuiz);
  if (!invalidQuizResult.success) {
    console.log('  ✅ PASSED (correctly rejected)\n');
    passed++;
  } else {
    console.log('  ❌ FAILED: Should have rejected empty questions');
    failed++;
  }
  
  // Test 6: Auto-detect type
  console.log('Test 6: Auto-detect YAML type');
  const autoResult = parser.parseAuto(validReview);
  if (autoResult.success && autoResult.data?.type === 'review') {
    console.log('  ✅ PASSED\n');
    passed++;
  } else {
    console.log('  ❌ FAILED');
    failed++;
  }
  
  // Test 7: Guide link validation
  console.log('Test 7: Validate guide links');
  const brokenGuide = `
guide:
  id: broken-guide
  title: "Broken Guide"
  stops:
    - id: stop1
      file: src/test.js
      line: 1
      title: "First Stop"
      content: "Has broken link"
      next: nonexistent
`;
  const brokenGuideResult = parser.parseGuide(brokenGuide);
  if (brokenGuideResult.success) {
    const linkErrors = parser.validateGuideLinks(brokenGuideResult.data!);
    if (linkErrors.length > 0) {
      console.log('  ✅ PASSED (detected broken link)\n');
      passed++;
    } else {
      console.log('  ❌ FAILED: Should have detected broken link');
      failed++;
    }
  } else {
    console.log('  ❌ FAILED: Parse error');
    failed++;
  }
  
  // Test 8: Code completion question
  console.log('Test 8: Parse code completion question');
  const codeQuiz = `
quiz:
  id: code-quiz
  title: "Code Quiz"
  questions:
    - type: code_completion
      question: "Complete the function"
      codeContext: "function add(a, b) { return ____; }"
      answerPattern: "a + b"
      incorrectPatterns:
        - pattern: "a - b"
          feedback: "That's subtraction!"
`;
  const codeQuizResult = parser.parseQuiz(codeQuiz);
  if (codeQuizResult.success) {
    console.log('  ✅ PASSED\n');
    passed++;
  } else {
    console.log('  ❌ FAILED:', codeQuizResult.errors);
    failed++;
  }
  
  // Test 9: Scenario question
  console.log('Test 9: Parse scenario question');
  const scenarioQuiz = `
quiz:
  id: scenario-quiz
  title: "Scenario Quiz"
  questions:
    - type: scenario
      question: "What would happen if the server crashes?"
      options:
        - "Data is lost"
        - "Data is persisted"
        - "Automatic recovery"
      correct: 1
      explanation: "We use persistent storage"
`;
  const scenarioQuizResult = parser.parseQuiz(scenarioQuiz);
  if (scenarioQuizResult.success) {
    console.log('  ✅ PASSED\n');
    passed++;
  } else {
    console.log('  ❌ FAILED:', scenarioQuizResult.errors);
    failed++;
  }
  
  // Test 10: Quiz stats
  console.log('Test 10: Calculate quiz stats');
  const mixedQuiz = `
quiz:
  id: mixed-quiz
  title: "Mixed Quiz"
  questions:
    - type: multiple_choice
      question: "Q1"
      options: ["A", "B"]
      correct: 0
    - type: multiple_choice
      question: "Q2"
      options: ["A", "B"]
      correct: 1
    - type: code_completion
      question: "Q3"
      answerPattern: "test"
`;
  const mixedQuizResult = parser.parseQuiz(mixedQuiz);
  if (mixedQuizResult.success) {
    const stats = parser.getQuizStats(mixedQuizResult.data!);
    if (stats.totalQuestions === 3 && stats.byType['multiple_choice'] === 2 && stats.byType['code_completion'] === 1) {
      console.log('  ✅ PASSED\n');
      passed++;
    } else {
      console.log('  ❌ FAILED: Incorrect stats');
      failed++;
    }
  } else {
    console.log('  ❌ FAILED:', mixedQuizResult.errors);
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
YAML Parser CLI - Test YAML DSL parsing

Usage: npx tsx cli/yaml-parser.ts <command> [options]

Commands:
  parse --file=<yaml>           Parse and display YAML content
  validate --file=<yaml> [--type=<review|quiz|guide>]
                                Validate YAML against schema
  stats --file=<yaml>           Show statistics for parsed content
  extract-files --file=<yaml>   Extract file references from review/guide
  test-all                      Run all parser tests
`);
}

main().catch(err => {
  console.error('Error:', err.message);
  process.exit(1);
});
