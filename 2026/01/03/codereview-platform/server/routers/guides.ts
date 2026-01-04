import { z } from 'zod';
import { router, publicProcedure, protectedProcedure } from '../_core/trpc';
import { createGitNotesStorage } from '../lib/git-notes';
import { createYamlParser } from '../lib/yaml-parser';
import { getDb } from '../db';
import { guideProgress } from '../../drizzle/schema';
import { eq, and } from 'drizzle-orm';

export const guidesRouter = router({
  // List all guides from git notes
  list: publicProcedure
    .input(z.object({ repoPath: z.string() }))
    .query(async ({ input }) => {
      const storage = createGitNotesStorage(input.repoPath);
      const guides = await storage.getAllGuides();
      const parser = createYamlParser();
      
      return guides.map(({ commit, guide }) => {
        const files = parser.extractGuideFileReferences(guide as any);
        return {
          commit,
          id: guide.id,
          title: guide.title,
          description: guide.description,
          difficulty: guide.difficulty,
          estimatedTime: guide.estimatedTime,
          prerequisites: guide.prerequisites,
          stopCount: guide.stops.length,
          files,
        };
      });
    }),

  // Get a specific guide by commit and optional guideId
  get: publicProcedure
    .input(z.object({
      repoPath: z.string(),
      commit: z.string(),
      guideId: z.string().optional(),
    }))
    .query(async ({ input }) => {
      const storage = createGitNotesStorage(input.repoPath);
      const guide = await storage.getGuide(input.commit, input.guideId);
      
      if (!guide) return null;
      
      return {
        ...guide,
        commit: input.commit,
      };
    }),

  // Get a specific stop in a guide
  getStop: publicProcedure
    .input(z.object({
      repoPath: z.string(),
      commit: z.string(),
      stopId: z.string(),
    }))
    .query(async ({ input }) => {
      const storage = createGitNotesStorage(input.repoPath);
      const guide = await storage.getGuide(input.commit);
      
      if (!guide) return null;
      
      const stopIndex = guide.stops.findIndex(s => s.id === input.stopId);
      if (stopIndex === -1) return null;
      
      const stop = guide.stops[stopIndex];
      const prevStop = stopIndex > 0 ? guide.stops[stopIndex - 1] : null;
      const nextStop = stopIndex < guide.stops.length - 1 ? guide.stops[stopIndex + 1] : null;
      
      // Get file content for this stop
      const headCommit = await storage.getHeadCommit();
      const fileContent = await storage.getFileContent(headCommit, stop.file);
      
      return {
        ...stop,
        stopIndex,
        totalStops: guide.stops.length,
        prevStopId: prevStop?.id,
        nextStopId: nextStop?.id,
        fileContent,
        guideTitle: guide.title,
      };
    }),

  // Get user's progress through a guide
  getProgress: protectedProcedure
    .input(z.object({
      repoPath: z.string(),
      commit: z.string(),
      guideId: z.string(),
    }))
    .query(async ({ input, ctx }) => {
      const db = await getDb();
      if (!db) return null;
      
      const result = await db.select()
        .from(guideProgress)
        .where(and(
          eq(guideProgress.userId, ctx.user.id),
          eq(guideProgress.guideId, 0), // We'd need to look up the guide ID
        ))
        .limit(1);
      
      return result[0] || null;
    }),

  // Update user's progress through a guide
  updateProgress: protectedProcedure
    .input(z.object({
      repoPath: z.string(),
      commit: z.string(),
      guideId: z.string(),
      currentStopId: z.string(),
      visitedStops: z.array(z.string()),
    }))
    .mutation(async ({ input, ctx }) => {
      const db = await getDb();
      if (!db) throw new Error('Database not available');
      
      const storage = createGitNotesStorage(input.repoPath);
      const guide = await storage.getGuide(input.commit);
      
      if (!guide) throw new Error('Guide not found');
      
      // Check if all stops have been visited
      const allVisited = guide.stops.every(s => input.visitedStops.includes(s.id));
      
      // Upsert progress
      await db.insert(guideProgress).values({
        userId: ctx.user.id,
        guideId: 0, // We'd need to look up the guide ID
        repositoryId: 0, // Same here
        currentStopId: input.currentStopId,
        visitedStops: input.visitedStops,
        completedAt: allVisited ? new Date() : undefined,
      }).onDuplicateKeyUpdate({
        set: {
          currentStopId: input.currentStopId,
          visitedStops: input.visitedStops,
          completedAt: allVisited ? new Date() : undefined,
        },
      });
      
      return {
        success: true,
        completed: allVisited,
        progress: Math.round((input.visitedStops.length / guide.stops.length) * 100),
      };
    }),

  // Create a new guide from YAML
  create: protectedProcedure
    .input(z.object({
      repoPath: z.string(),
      yaml: z.string(),
      commit: z.string().optional(),
    }))
    .mutation(async ({ input }) => {
      const storage = createGitNotesStorage(input.repoPath);
      const parser = createYamlParser();
      
      const result = parser.parseGuide(input.yaml);
      if (!result.success) {
        throw new Error(`Invalid guide YAML: ${result.errors?.join(', ')}`);
      }
      
      // Validate guide links
      const linkErrors = parser.validateGuideLinks(result.data!);
      if (linkErrors.length > 0) {
        throw new Error(`Guide link errors: ${linkErrors.join(', ')}`);
      }
      
      const commit = input.commit || await storage.getHeadCommit();
      await storage.storeGuide(commit, result.data as any);
      
      return { success: true, commit };
    }),

  // Get all guides that reference a specific file
  byFile: publicProcedure
    .input(z.object({
      repoPath: z.string(),
      filePath: z.string(),
    }))
    .query(async ({ input }) => {
      const storage = createGitNotesStorage(input.repoPath);
      const guides = await storage.getAllGuides();
      
      return guides.filter(({ guide }) =>
        guide.stops.some(s => s.file === input.filePath)
      ).map(({ commit, guide }) => ({
        commit,
        id: guide.id,
        title: guide.title,
        stopsInFile: guide.stops.filter(s => s.file === input.filePath).map(s => ({
          id: s.id,
          title: s.title,
          line: s.line,
        })),
      }));
    }),
});
