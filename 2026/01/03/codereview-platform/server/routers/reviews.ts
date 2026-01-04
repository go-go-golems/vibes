import { z } from 'zod';
import { router, publicProcedure, protectedProcedure } from '../_core/trpc';
import { createGitNotesStorage, type ReviewDefinition } from '../lib/git-notes';
import { createYamlParser } from '../lib/yaml-parser';
import { getDb } from '../db';
import { codeReviews, annotations } from '../../drizzle/schema';
import { eq, and } from 'drizzle-orm';
import hljs from 'highlight.js';

// Parse diff into structured format
function parseDiff(diffText: string): Array<{
  file: string;
  hunks: Array<{
    header: string;
    oldStart: number;
    oldLines: number;
    newStart: number;
    newLines: number;
    lines: Array<{
      type: 'context' | 'add' | 'remove';
      content: string;
      oldLineNumber?: number;
      newLineNumber?: number;
    }>;
  }>;
}> {
  const files: Array<{
    file: string;
    hunks: Array<{
      header: string;
      oldStart: number;
      oldLines: number;
      newStart: number;
      newLines: number;
      lines: Array<{
        type: 'context' | 'add' | 'remove';
        content: string;
        oldLineNumber?: number;
        newLineNumber?: number;
      }>;
    }>;
  }> = [];
  
  if (!diffText.trim()) return files;
  
  const fileChunks = diffText.split(/^diff --git /m).filter(Boolean);
  
  for (const chunk of fileChunks) {
    const lines = chunk.split('\n');
    const headerMatch = lines[0]?.match(/a\/(.+) b\/(.+)/);
    if (!headerMatch) continue;
    
    const file = headerMatch[2];
    const hunks: typeof files[0]['hunks'] = [];
    
    let currentHunk: typeof hunks[0] | null = null;
    let oldLine = 0;
    let newLine = 0;
    
    for (const line of lines) {
      const hunkMatch = line.match(/^@@ -(\d+),?(\d*) \+(\d+),?(\d*) @@(.*)$/);
      
      if (hunkMatch) {
        if (currentHunk) hunks.push(currentHunk);
        
        oldLine = parseInt(hunkMatch[1], 10);
        newLine = parseInt(hunkMatch[3], 10);
        
        currentHunk = {
          header: line,
          oldStart: oldLine,
          oldLines: parseInt(hunkMatch[2] || '1', 10),
          newStart: newLine,
          newLines: parseInt(hunkMatch[4] || '1', 10),
          lines: [],
        };
        continue;
      }
      
      if (!currentHunk) continue;
      
      if (line.startsWith('+') && !line.startsWith('+++')) {
        currentHunk.lines.push({
          type: 'add',
          content: line.slice(1),
          newLineNumber: newLine++,
        });
      } else if (line.startsWith('-') && !line.startsWith('---')) {
        currentHunk.lines.push({
          type: 'remove',
          content: line.slice(1),
          oldLineNumber: oldLine++,
        });
      } else if (line.startsWith(' ')) {
        currentHunk.lines.push({
          type: 'context',
          content: line.slice(1),
          oldLineNumber: oldLine++,
          newLineNumber: newLine++,
        });
      }
    }
    
    if (currentHunk) hunks.push(currentHunk);
    files.push({ file, hunks });
  }
  
  return files;
}

export const reviewsRouter = router({
  // List all reviews from git notes
  list: publicProcedure
    .input(z.object({ repoPath: z.string() }))
    .query(async ({ input }) => {
      const storage = createGitNotesStorage(input.repoPath);
      const reviews = await storage.getAllReviews();
      
      return reviews.map(({ commit, review }) => ({
        commit,
        id: review.id || review.title.toLowerCase().replace(/\s+/g, '-'),
        pr: review.pr,
        title: review.title,
        description: review.description,
        baseBranch: review.baseBranch,
        headBranch: review.headBranch,
        annotationCount: review.annotations.length,
        files: Array.from(new Set(review.annotations.map(a => a.file))),
      }));
    }),

  // Get a specific review by commit and optional reviewId
  get: publicProcedure
    .input(z.object({
      repoPath: z.string(),
      commit: z.string(),
      reviewId: z.string().optional(),
    }))
    .query(async ({ input }) => {
      const storage = createGitNotesStorage(input.repoPath);
      const review = await storage.getReview(input.commit, input.reviewId);
      
      if (!review) return null;
      
      // Get diff if we have base and head branches
      let diff = null;
      // Note: Diff generation is simplified in isomorphic-git version
      
      return {
        ...review,
        commit: input.commit,
        diff,
      };
    }),

  // Get review with full diff
  getWithDiff: publicProcedure
    .input(z.object({
      repoPath: z.string(),
      commit: z.string(),
      reviewId: z.string().optional(),
      baseCommit: z.string().optional(),
      headCommit: z.string().optional(),
    }))
    .query(async ({ input }) => {
      const storage = createGitNotesStorage(input.repoPath);
      const review = await storage.getReview(input.commit, input.reviewId);
      
      if (!review) return null;
      
      let diff = null;
      // Note: Diff generation is simplified in isomorphic-git version
      
      // Group annotations by file
      const annotationsByFile: Record<string, typeof review.annotations> = {};
      for (const annotation of review.annotations) {
        if (!annotationsByFile[annotation.file]) {
          annotationsByFile[annotation.file] = [];
        }
        annotationsByFile[annotation.file].push(annotation);
      }
      
      return {
        ...review,
        commit: input.commit,
        diff,
        annotationsByFile,
      };
    }),

  // Create a new review from YAML
  create: protectedProcedure
    .input(z.object({
      repoPath: z.string(),
      yaml: z.string(),
      commit: z.string().optional(),
    }))
    .mutation(async ({ input }) => {
      const storage = createGitNotesStorage(input.repoPath);
      const parser = createYamlParser();
      
      const result = parser.parseReview(input.yaml);
      if (!result.success) {
        throw new Error(`Invalid review YAML: ${result.errors?.join(', ')}`);
      }
      
      const commit = input.commit || await storage.getHeadCommit();
      await storage.storeReview(commit, result.data!);
      
      return { success: true, commit };
    }),

  // Get annotations for a specific file in a review
  fileAnnotations: publicProcedure
    .input(z.object({
      repoPath: z.string(),
      commit: z.string(),
      filePath: z.string(),
    }))
    .query(async ({ input }) => {
      const storage = createGitNotesStorage(input.repoPath);
      const review = await storage.getReview(input.commit);
      
      if (!review) return [];
      
      return review.annotations
        .filter(a => a.file === input.filePath)
        .sort((a, b) => a.line - b.line);
    }),

  // Get all annotation types used in reviews
  annotationTypes: publicProcedure
    .input(z.object({ repoPath: z.string() }))
    .query(async ({ input }) => {
      const storage = createGitNotesStorage(input.repoPath);
      const reviews = await storage.getAllReviews();
      
      const types = new Set<string>();
      for (const { review } of reviews) {
        for (const annotation of review.annotations) {
          types.add(annotation.type);
        }
      }
      
      return Array.from(types);
    }),

  // Search annotations by tag
  searchByTag: publicProcedure
    .input(z.object({
      repoPath: z.string(),
      tag: z.string(),
    }))
    .query(async ({ input }) => {
      const storage = createGitNotesStorage(input.repoPath);
      const reviews = await storage.getAllReviews();
      
      const results: Array<{
        commit: string;
        reviewTitle: string;
        annotation: ReviewDefinition['annotations'][0];
      }> = [];
      
      for (const { commit, review } of reviews) {
        for (const annotation of review.annotations) {
          if (annotation.tags?.includes(input.tag)) {
            results.push({
              commit,
              reviewTitle: review.title,
              annotation,
            });
          }
        }
      }
      
      return results;
    }),

  // Get all tags used in reviews
  allTags: publicProcedure
    .input(z.object({ repoPath: z.string() }))
    .query(async ({ input }) => {
      const storage = createGitNotesStorage(input.repoPath);
      const reviews = await storage.getAllReviews();
      
      const tagCounts: Record<string, number> = {};
      for (const { review } of reviews) {
        for (const annotation of review.annotations) {
          for (const tag of annotation.tags || []) {
            tagCounts[tag] = (tagCounts[tag] || 0) + 1;
          }
        }
      }
      
      return Object.entries(tagCounts)
        .map(([tag, count]) => ({ tag, count }))
        .sort((a, b) => b.count - a.count);
    }),
});
