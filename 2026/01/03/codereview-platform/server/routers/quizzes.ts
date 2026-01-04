import { z } from 'zod';
import { router, publicProcedure, protectedProcedure } from '../_core/trpc';
import { createGitNotesStorage, type QuizSubmission } from '../lib/git-notes';
import { createYamlParser } from '../lib/yaml-parser';
import { getDb } from '../db';
import { quizSubmissions } from '../../drizzle/schema';
import { eq, and } from 'drizzle-orm';

export const quizzesRouter = router({
  // List all quizzes from git notes
  list: publicProcedure
    .input(z.object({ repoPath: z.string() }))
    .query(async ({ input }) => {
      const storage = createGitNotesStorage(input.repoPath);
      const quizzes = await storage.getAllQuizzes();
      const parser = createYamlParser();
      
      return quizzes.map(({ commit, quiz }) => {
        const stats = parser.getQuizStats(quiz as any);
        return {
          commit,
          id: quiz.id,
          title: quiz.title,
          description: quiz.description,
          context: quiz.context,
          difficulty: quiz.difficulty,
          estimatedTime: quiz.estimatedTime,
          questionCount: stats.totalQuestions,
          questionTypes: stats.byType,
          estimatedPoints: stats.estimatedPoints,
        };
      });
    }),

  // Get a specific quiz by commit
  get: publicProcedure
    .input(z.object({
      repoPath: z.string(),
      commit: z.string(),
      quizId: z.string().optional(),
    }))
    .query(async ({ input }) => {
      const storage = createGitNotesStorage(input.repoPath);
      const quiz = await storage.getQuiz(input.commit, input.quizId);
      
      if (!quiz) return null;
      
      // Don't include correct answers in the response for taking the quiz
      const questionsWithoutAnswers = quiz.questions.map(q => {
        const { correct, explanation, answerPattern, incorrectPatterns, ...rest } = q as any;
        return rest;
      });
      
      return {
        ...quiz,
        commit: input.commit,
        questionsForTaking: questionsWithoutAnswers,
      };
    }),

  // Get quiz with answers (for review after submission)
  getWithAnswers: publicProcedure
    .input(z.object({
      repoPath: z.string(),
      commit: z.string(),
    }))
    .query(async ({ input }) => {
      const storage = createGitNotesStorage(input.repoPath);
      return storage.getQuiz(input.commit);
    }),

  // Submit quiz answers
  submit: protectedProcedure
    .input(z.object({
      repoPath: z.string(),
      commit: z.string(),
      quizId: z.string(),
      answers: z.record(z.string(), z.unknown()),
    }))
    .mutation(async ({ input, ctx }) => {
      const storage = createGitNotesStorage(input.repoPath);
      const quiz = await storage.getQuiz(input.commit);
      
      if (!quiz) {
        throw new Error('Quiz not found');
      }
      
      // Grade the quiz
      let score = 0;
      const maxScore = quiz.questions.length * 10;
      const grading: Array<{
        questionIndex: number;
        correct: boolean;
        userAnswer: unknown;
        correctAnswer?: unknown;
        explanation?: string;
      }> = [];
      
      for (let i = 0; i < quiz.questions.length; i++) {
        const question = quiz.questions[i];
        const userAnswer = input.answers[`q${i}`];
        let isCorrect = false;
        
        if (question.type === 'multiple_choice' || question.type === 'scenario') {
          const correctAnswer = (question as any).correct;
          if (Array.isArray(correctAnswer)) {
            isCorrect = Array.isArray(userAnswer) && 
              correctAnswer.length === userAnswer.length &&
              correctAnswer.every((a: number) => (userAnswer as number[]).includes(a));
          } else {
            isCorrect = userAnswer === correctAnswer;
          }
        } else if (question.type === 'code_completion') {
          const pattern = (question as any).answerPattern;
          isCorrect = typeof userAnswer === 'string' && 
            userAnswer.trim().toLowerCase() === pattern.trim().toLowerCase();
        }
        
        if (isCorrect) score += 10;
        
        grading.push({
          questionIndex: i,
          correct: isCorrect,
          userAnswer,
          correctAnswer: (question as any).correct || (question as any).answerPattern,
          explanation: (question as any).explanation,
        });
      }
      
      // Store submission in git notes
      const submission: QuizSubmission = {
        quizId: input.quizId,
        userId: ctx.user.openId,
        answers: input.answers,
        score,
        maxScore,
        submittedAt: new Date().toISOString(),
      };
      
      await storage.storeSubmission(input.commit, submission);
      
      // Also store in database for quick access
      const db = await getDb();
      if (db) {
        await db.insert(quizSubmissions).values({
          userId: ctx.user.id,
          quizId: 0, // We'd need to look this up from the quizzes table
          repositoryId: 0, // Same here
          answers: input.answers,
          score,
          maxScore,
          completed: true,
          gitNotesRef: input.commit,
        });
      }
      
      return {
        score,
        maxScore,
        percentage: Math.round((score / maxScore) * 100),
        grading,
        passed: score >= maxScore * 0.7, // 70% to pass
      };
    }),

  // Get user's submission for a quiz
  getSubmission: protectedProcedure
    .input(z.object({
      repoPath: z.string(),
      commit: z.string(),
      quizId: z.string(),
    }))
    .query(async ({ input, ctx }) => {
      const storage = createGitNotesStorage(input.repoPath);
      return storage.getUserSubmission(input.commit, input.quizId, ctx.user.openId);
    }),

  // Get all submissions for a commit
  allSubmissions: publicProcedure
    .input(z.object({
      repoPath: z.string(),
      commit: z.string(),
    }))
    .query(async ({ input }) => {
      const storage = createGitNotesStorage(input.repoPath);
      return storage.getSubmissions(input.commit);
    }),

  // Get user's quiz progress across all quizzes
  userProgress: protectedProcedure
    .input(z.object({ repoPath: z.string() }))
    .query(async ({ input, ctx }) => {
      const storage = createGitNotesStorage(input.repoPath);
      const allQuizzes = await storage.getAllQuizzes();
      
      const progress: Array<{
        quizId: string;
        quizTitle: string;
        commit: string;
        submitted: boolean;
        score?: number;
        maxScore?: number;
        submittedAt?: string;
      }> = [];
      
      for (const { commit, quiz } of allQuizzes) {
        const submission = await storage.getUserSubmission(commit, quiz.id, ctx.user.openId);
        
        progress.push({
          quizId: quiz.id,
          quizTitle: quiz.title,
          commit,
          submitted: !!submission,
          score: submission?.score,
          maxScore: submission?.maxScore,
          submittedAt: submission?.submittedAt,
        });
      }
      
      return progress;
    }),

  // Create a new quiz from YAML
  create: protectedProcedure
    .input(z.object({
      repoPath: z.string(),
      yaml: z.string(),
      commit: z.string().optional(),
    }))
    .mutation(async ({ input }) => {
      const storage = createGitNotesStorage(input.repoPath);
      const parser = createYamlParser();
      
      const result = parser.parseQuiz(input.yaml);
      if (!result.success) {
        throw new Error(`Invalid quiz YAML: ${result.errors?.join(', ')}`);
      }
      
      const commit = input.commit || await storage.getHeadCommit();
      await storage.storeQuiz(commit, result.data!);
      
      return { success: true, commit };
    }),
});
