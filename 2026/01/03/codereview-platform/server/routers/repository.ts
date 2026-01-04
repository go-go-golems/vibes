import { z } from 'zod';
import { router, publicProcedure, protectedProcedure } from '../_core/trpc';
import { createGitNotesStorage } from '../lib/git-notes';
import { createYamlParser } from '../lib/yaml-parser';
import { getDb } from '../db';
import { repositories, codeReviews, quizzes, reviewGuides, annotations } from '../../drizzle/schema';
import { eq } from 'drizzle-orm';
import hljs from 'highlight.js';
import { ensureDemoRepo, getDemoRepoPath, isDemoRepoInitialized } from '../lib/demo-init';

// Get language from file extension
function getLanguageFromPath(filePath: string): string {
  const ext = filePath.split('.').pop()?.toLowerCase() || '';
  const langMap: Record<string, string> = {
    'js': 'javascript',
    'jsx': 'javascript',
    'ts': 'typescript',
    'tsx': 'typescript',
    'py': 'python',
    'rb': 'ruby',
    'go': 'go',
    'rs': 'rust',
    'java': 'java',
    'c': 'c',
    'cpp': 'cpp',
    'h': 'c',
    'hpp': 'cpp',
    'cs': 'csharp',
    'php': 'php',
    'swift': 'swift',
    'kt': 'kotlin',
    'scala': 'scala',
    'sh': 'bash',
    'bash': 'bash',
    'zsh': 'bash',
    'yml': 'yaml',
    'yaml': 'yaml',
    'json': 'json',
    'xml': 'xml',
    'html': 'html',
    'css': 'css',
    'scss': 'scss',
    'less': 'less',
    'md': 'markdown',
    'sql': 'sql',
    'dockerfile': 'dockerfile',
    'makefile': 'makefile',
    'toml': 'toml',
    'ini': 'ini',
    'conf': 'ini',
  };
  return langMap[ext] || 'plaintext';
}

// Highlight code with line numbers
function highlightCode(code: string, language: string): { html: string; lines: string[] } {
  let highlighted: string;
  try {
    if (language !== 'plaintext' && hljs.getLanguage(language)) {
      highlighted = hljs.highlight(code, { language }).value;
    } else {
      highlighted = hljs.highlightAuto(code).value;
    }
  } catch {
    highlighted = code.replace(/</g, '&lt;').replace(/>/g, '&gt;');
  }
  
  const lines = highlighted.split('\n');
  return { html: highlighted, lines };
}

export const repositoryRouter = router({
  // Initialize demo repository
  initDemo: publicProcedure.mutation(async () => {
    return ensureDemoRepo();
  }),

  // Get demo repository status and path
  demoStatus: publicProcedure.query(async () => {
    const initialized = isDemoRepoInitialized();
    return {
      initialized,
      path: getDemoRepoPath(),
    };
  }),

  // List all repositories
  list: publicProcedure.query(async () => {
    const db = await getDb();
    if (!db) return [];
    return db.select().from(repositories);
  }),

  // Get repository by ID
  get: publicProcedure
    .input(z.object({ id: z.number() }))
    .query(async ({ input }) => {
      const db = await getDb();
      if (!db) return null;
      const result = await db.select().from(repositories).where(eq(repositories.id, input.id)).limit(1);
      return result[0] || null;
    }),

  // Add a new repository
  add: protectedProcedure
    .input(z.object({
      name: z.string(),
      path: z.string(),
      description: z.string().optional(),
    }))
    .mutation(async ({ input }) => {
      const db = await getDb();
      if (!db) throw new Error('Database not available');
      
      // Verify it's a valid git repo
      const storage = createGitNotesStorage(input.path);
      try {
        await storage.getBranches();
      } catch {
        throw new Error('Invalid git repository path');
      }
      
      await db.insert(repositories).values({
        name: input.name,
        path: input.path,
        description: input.description,
      });
      
      return { success: true };
    }),

  // Get branches for a repository
  branches: publicProcedure
    .input(z.object({ repoPath: z.string() }))
    .query(async ({ input }) => {
      const storage = createGitNotesStorage(input.repoPath);
      return storage.getBranches();
    }),

  // Get file tree for a repository
  fileTree: publicProcedure
    .input(z.object({
      repoPath: z.string(),
      branch: z.string().optional(),
      path: z.string().optional(),
    }))
    .query(async ({ input }) => {
      const storage = createGitNotesStorage(input.repoPath);
      const commit = await storage.getHeadCommit();
      return storage.getFileTree(commit, input.path || '');
    }),

  // Get file content with syntax highlighting
  fileContent: publicProcedure
    .input(z.object({
      repoPath: z.string(),
      filePath: z.string(),
      branch: z.string().optional(),
    }))
    .query(async ({ input }) => {
      const storage = createGitNotesStorage(input.repoPath);
      const commit = await storage.getHeadCommit();
      
      const content = await storage.getFileContent(commit, input.filePath);
      if (!content) return null;
      
      const language = getLanguageFromPath(input.filePath);
      const { html, lines } = highlightCode(content, language);
      
      return {
        content,
        highlighted: html,
        lines,
        language,
        lineCount: lines.length,
      };
    }),

  // Get annotations for a file
  fileAnnotations: publicProcedure
    .input(z.object({
      repoPath: z.string(),
      filePath: z.string(),
      branch: z.string().optional(),
    }))
    .query(async ({ input }) => {
      const storage = createGitNotesStorage(input.repoPath);
      const commit = await storage.getHeadCommit();
      
      // Get all reviews and filter annotations for this file
      const allReviews = await storage.getAllReviews();
      const fileAnnotations: Array<{
        commit: string;
        reviewTitle: string;
        annotation: {
          line: number;
          lineEnd?: number;
          type: string;
          title?: string;
          content: string;
          tags?: string[];
          quiz?: unknown;
        };
      }> = [];
      
      for (const { commit: reviewCommit, review } of allReviews) {
        for (const annotation of review.annotations) {
          if (annotation.file === input.filePath) {
            fileAnnotations.push({
              commit: reviewCommit,
              reviewTitle: review.title,
              annotation: {
                line: annotation.line,
                lineEnd: annotation.lineEnd,
                type: annotation.type,
                title: annotation.title,
                content: annotation.content,
                tags: annotation.tags,
                quiz: annotation.quiz,
              },
            });
          }
        }
      }
      
      return fileAnnotations;
    }),

  // Get quizzes associated with a file
  fileQuizzes: publicProcedure
    .input(z.object({
      repoPath: z.string(),
      filePath: z.string(),
    }))
    .query(async ({ input }) => {
      const storage = createGitNotesStorage(input.repoPath);
      const allQuizzes = await storage.getAllQuizzes();
      
      // Filter quizzes that reference this file in their context
      return allQuizzes.filter(({ quiz }) => 
        quiz.context?.includes(input.filePath)
      );
    }),

  // Get reviews associated with a file
  fileReviews: publicProcedure
    .input(z.object({
      repoPath: z.string(),
      filePath: z.string(),
    }))
    .query(async ({ input }) => {
      const storage = createGitNotesStorage(input.repoPath);
      const allReviews = await storage.getAllReviews();
      
      // Filter reviews that have annotations for this file
      return allReviews.filter(({ review }) =>
        review.annotations.some(a => a.file === input.filePath)
      ).map(({ commit, review }) => ({
        commit,
        title: review.title,
        pr: review.pr,
        annotationCount: review.annotations.filter(a => a.file === input.filePath).length,
      }));
    }),

  // Get commit log
  commits: publicProcedure
    .input(z.object({
      repoPath: z.string(),
      branch: z.string().optional(),
      limit: z.number().optional(),
    }))
    .query(async ({ input }) => {
      const storage = createGitNotesStorage(input.repoPath);
      return storage.getLog(input.branch || 'HEAD', input.limit || 50);
    }),

  // Get diff between branches or commits
  diff: publicProcedure
    .input(z.object({
      repoPath: z.string(),
      base: z.string(),
      head: z.string(),
      filePath: z.string().optional(),
    }))
    .query(async ({ input }) => {
      const storage = createGitNotesStorage(input.repoPath);
      return storage.getDiff(input.base, input.head);
    }),

  // Sync git notes to database
  syncNotes: protectedProcedure
    .input(z.object({
      repoId: z.number(),
      repoPath: z.string(),
    }))
    .mutation(async ({ input }) => {
      const db = await getDb();
      if (!db) throw new Error('Database not available');
      
      const storage = createGitNotesStorage(input.repoPath);
      const parser = createYamlParser();
      
      // Sync reviews
      const allReviews = await storage.getAllReviews();
      for (const { commit, review } of allReviews) {
        // Check if review exists
        const existing = await db.select()
          .from(codeReviews)
          .where(eq(codeReviews.gitNotesRef, commit))
          .limit(1);
        
        if (existing.length === 0) {
          await db.insert(codeReviews).values({
            repositoryId: input.repoId,
            prNumber: review.pr,
            title: review.title,
            description: review.description,
            baseBranch: review.baseBranch,
            headBranch: review.headBranch,
            headCommit: commit,
            gitNotesRef: commit,
          });
        }
      }
      
      // Sync quizzes
      const allQuizzes = await storage.getAllQuizzes();
      for (const { commit, quiz } of allQuizzes) {
        const existing = await db.select()
          .from(quizzes)
          .where(eq(quizzes.gitNotesRef, commit))
          .limit(1);
        
        if (existing.length === 0) {
          await db.insert(quizzes).values({
            repositoryId: input.repoId,
            quizId: quiz.id,
            title: quiz.title,
            description: quiz.description,
            difficulty: quiz.difficulty,
            estimatedTime: quiz.estimatedTime,
            gitNotesRef: commit,
          });
        }
      }
      
      // Sync guides
      const allGuides = await storage.getAllGuides();
      for (const { commit, guide } of allGuides) {
        const existing = await db.select()
          .from(reviewGuides)
          .where(eq(reviewGuides.gitNotesRef, commit))
          .limit(1);
        
        if (existing.length === 0) {
          await db.insert(reviewGuides).values({
            repositoryId: input.repoId,
            guideId: guide.id,
            title: guide.title,
            description: guide.description,
            difficulty: guide.difficulty,
            estimatedTime: guide.estimatedTime,
            prerequisites: guide.prerequisites,
            gitNotesRef: commit,
          });
        }
      }
      
      return { success: true, synced: { reviews: allReviews.length, quizzes: allQuizzes.length, guides: allGuides.length } };
    }),
});
