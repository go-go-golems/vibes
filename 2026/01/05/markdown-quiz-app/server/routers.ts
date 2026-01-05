import { COOKIE_NAME } from "@shared/const";
import { getSessionCookieOptions } from "./_core/cookies";
import { systemRouter } from "./_core/systemRouter";
import { publicProcedure, protectedProcedure, router } from "./_core/trpc";
import { z } from "zod";
import * as db from "./db";
import { TRPCError } from "@trpc/server";

// Helper to generate slug from title
function generateSlug(title: string): string {
  return title
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/^-|-$/g, '')
    + '-' + Date.now().toString(36);
}

// Helper to extract forms from markdown content
function extractFormsFromContent(content: string): Array<{ formId: string; definition: unknown }> {
  const formRegex = /<form\s+id=["']?([^"'\s>]+)["']?\s*>([\s\S]*?)<\/form>/gi;
  const forms: Array<{ formId: string; definition: unknown }> = [];
  
  let match;
  while ((match = formRegex.exec(content)) !== null) {
    const formId = match[1];
    const yamlContent = match[2].trim();
    
    try {
      // Dynamic import yaml at runtime
      const yaml = require('yaml');
      const definition = yaml.parse(yamlContent);
      forms.push({ formId, definition });
    } catch (e) {
      console.error(`Failed to parse YAML for form ${formId}:`, e);
    }
  }
  
  return forms;
}

// Helper to calculate score based on form definition and responses
function calculateScore(definition: any, responses: Record<string, any>): { score: number; maxScore: number } {
  let score = 0;
  let maxScore = 0;
  
  const fields = definition.fields || definition.form?.fields || [];
  
  for (const field of fields) {
    if (field.correct !== undefined) {
      maxScore += 1;
      const userAnswer = responses[field.name || field.key];
      
      if (Array.isArray(field.correct)) {
        // Multiple correct answers (checkbox)
        if (Array.isArray(userAnswer) && 
            field.correct.length === userAnswer.length &&
            field.correct.every((c: any) => userAnswer.includes(c))) {
          score += 1;
        }
      } else if (userAnswer === field.correct) {
        score += 1;
      }
    }
  }
  
  return { score, maxScore };
}

export const appRouter = router({
  system: systemRouter,
  
  auth: router({
    me: publicProcedure.query(opts => opts.ctx.user),
    logout: publicProcedure.mutation(({ ctx }) => {
      const cookieOptions = getSessionCookieOptions(ctx.req);
      ctx.res.clearCookie(COOKIE_NAME, { ...cookieOptions, maxAge: -1 });
      return { success: true } as const;
    }),
  }),

  documents: router({
    // List all documents (admin sees all, users see published only)
    list: publicProcedure.query(async ({ ctx }) => {
      if (ctx.user?.role === 'admin') {
        return db.listAllDocuments();
      }
      return db.listDocuments(undefined, true);
    }),

    // List user's own documents
    myDocuments: protectedProcedure.query(async ({ ctx }) => {
      return db.listDocuments(ctx.user.id);
    }),

    // Get single document by slug
    getBySlug: publicProcedure
      .input(z.object({ slug: z.string() }))
      .query(async ({ input, ctx }) => {
        const doc = await db.getDocumentBySlug(input.slug);
        if (!doc) {
          throw new TRPCError({ code: 'NOT_FOUND', message: 'Document not found' });
        }
        // Check access
        if (!doc.isPublished && ctx.user?.role !== 'admin' && ctx.user?.id !== doc.authorId) {
          throw new TRPCError({ code: 'FORBIDDEN', message: 'Access denied' });
        }
        
        // Get quiz forms for this document
        const forms = await db.getQuizFormsByDocument(doc.id);
        
        return { ...doc, forms };
      }),

    // Get single document by ID
    getById: protectedProcedure
      .input(z.object({ id: z.number() }))
      .query(async ({ input, ctx }) => {
        const doc = await db.getDocumentById(input.id);
        if (!doc) {
          throw new TRPCError({ code: 'NOT_FOUND', message: 'Document not found' });
        }
        // Only admin or author can access by ID
        if (ctx.user.role !== 'admin' && ctx.user.id !== doc.authorId) {
          throw new TRPCError({ code: 'FORBIDDEN', message: 'Access denied' });
        }
        return doc;
      }),

    // Create new document
    create: protectedProcedure
      .input(z.object({
        title: z.string().min(1),
        content: z.string(),
        description: z.string().optional(),
        category: z.string().optional(),
        isPublished: z.boolean().default(false),
      }))
      .mutation(async ({ input, ctx }) => {
        const slug = generateSlug(input.title);
        const result = await db.createDocument({
          ...input,
          slug,
          authorId: ctx.user.id,
        });
        
        // Extract and save quiz forms
        const forms = extractFormsFromContent(input.content);
        for (const form of forms) {
          await db.upsertQuizForm({
            documentId: result.id,
            formId: form.formId,
            definition: form.definition,
          });
        }
        
        return { id: result.id, slug };
      }),

    // Update document
    update: protectedProcedure
      .input(z.object({
        id: z.number(),
        title: z.string().min(1).optional(),
        content: z.string().optional(),
        description: z.string().optional(),
        category: z.string().optional(),
        isPublished: z.boolean().optional(),
      }))
      .mutation(async ({ input, ctx }) => {
        const doc = await db.getDocumentById(input.id);
        if (!doc) {
          throw new TRPCError({ code: 'NOT_FOUND', message: 'Document not found' });
        }
        if (ctx.user.role !== 'admin' && ctx.user.id !== doc.authorId) {
          throw new TRPCError({ code: 'FORBIDDEN', message: 'Access denied' });
        }
        
        const { id, ...updates } = input;
        await db.updateDocument(id, updates);
        
        // Re-extract forms if content changed
        if (input.content) {
          await db.deleteQuizFormsByDocument(id);
          const forms = extractFormsFromContent(input.content);
          for (const form of forms) {
            await db.upsertQuizForm({
              documentId: id,
              formId: form.formId,
              definition: form.definition,
            });
          }
        }
        
        return { success: true };
      }),

    // Delete document
    delete: protectedProcedure
      .input(z.object({ id: z.number() }))
      .mutation(async ({ input, ctx }) => {
        const doc = await db.getDocumentById(input.id);
        if (!doc) {
          throw new TRPCError({ code: 'NOT_FOUND', message: 'Document not found' });
        }
        if (ctx.user.role !== 'admin' && ctx.user.id !== doc.authorId) {
          throw new TRPCError({ code: 'FORBIDDEN', message: 'Access denied' });
        }
        
        await db.deleteDocument(input.id);
        return { success: true };
      }),

    // Get analytics for a document
    analytics: protectedProcedure
      .input(z.object({ id: z.number() }))
      .query(async ({ input, ctx }) => {
        const doc = await db.getDocumentById(input.id);
        if (!doc) {
          throw new TRPCError({ code: 'NOT_FOUND', message: 'Document not found' });
        }
        if (ctx.user.role !== 'admin' && ctx.user.id !== doc.authorId) {
          throw new TRPCError({ code: 'FORBIDDEN', message: 'Access denied' });
        }
        
        return db.getQuizAnalytics(input.id);
      }),

    // Get submissions for a document
    submissions: protectedProcedure
      .input(z.object({ id: z.number() }))
      .query(async ({ input, ctx }) => {
        const doc = await db.getDocumentById(input.id);
        if (!doc) {
          throw new TRPCError({ code: 'NOT_FOUND', message: 'Document not found' });
        }
        if (ctx.user.role !== 'admin' && ctx.user.id !== doc.authorId) {
          throw new TRPCError({ code: 'FORBIDDEN', message: 'Access denied' });
        }
        
        return db.getSubmissionsByDocument(input.id);
      }),
  }),

  quiz: router({
    // Submit multiple quizzes at once
    submitMultiple: protectedProcedure
      .input(z.object({
        documentId: z.number(),
        submissions: z.array(z.object({
          formId: z.string(),
          responses: z.record(z.string(), z.any()),
        })),
      }))
      .mutation(async ({ input, ctx }) => {
        const forms = await db.getQuizFormsByDocument(input.documentId);
        const results: Array<{ formId: string; score: number; maxScore: number }> = [];
        
        for (const sub of input.submissions) {
          const form = forms.find(f => f.formId === sub.formId);
          
          let score = 0;
          let maxScore = 0;
          
          if (form) {
            const result = calculateScore(form.definition, sub.responses);
            score = result.score;
            maxScore = result.maxScore;
          }
          
          await db.createQuizSubmission({
            userId: ctx.user.id,
            documentId: input.documentId,
            formId: sub.formId,
            responses: sub.responses,
            score,
            maxScore,
          });
          
          results.push({ formId: sub.formId, score, maxScore });
        }
        
        return { results };
      }),

    // Submit quiz responses
    submit: protectedProcedure
      .input(z.object({
        documentId: z.number(),
        formId: z.string(),
        responses: z.record(z.string(), z.any()),
      }))
      .mutation(async ({ input, ctx }) => {
        // Get the form definition to calculate score
        const forms = await db.getQuizFormsByDocument(input.documentId);
        const form = forms.find(f => f.formId === input.formId);
        
        let score: number | null = null;
        let maxScore: number | null = null;
        
        if (form) {
          const result = calculateScore(form.definition, input.responses);
          score = result.score;
          maxScore = result.maxScore;
        }
        
        const submission = await db.createQuizSubmission({
          userId: ctx.user.id,
          documentId: input.documentId,
          formId: input.formId,
          responses: input.responses,
          score,
          maxScore,
        });
        
        return { id: submission.id, score, maxScore };
      }),

    // Get user's submissions
    mySubmissions: protectedProcedure.query(async ({ ctx }) => {
      return db.getSubmissionsByUser(ctx.user.id);
    }),

    // Get single submission with details
    getSubmission: protectedProcedure
      .input(z.object({ id: z.number() }))
      .query(async ({ input, ctx }) => {
        const result = await db.getSubmissionById(input.id);
        if (!result) {
          throw new TRPCError({ code: 'NOT_FOUND', message: 'Submission not found' });
        }
        
        // Check access - user can see their own, admin can see all
        if (ctx.user.role !== 'admin' && result.submission.userId !== ctx.user.id) {
          throw new TRPCError({ code: 'FORBIDDEN', message: 'Access denied' });
        }
        
        // Get form definition for showing correct answers
        const forms = await db.getQuizFormsByDocument(result.submission.documentId);
        const form = forms.find(f => f.formId === result.submission.formId);
        
        return {
          ...result,
          formDefinition: form?.definition,
        };
      }),
  }),
});

export type AppRouter = typeof appRouter;
