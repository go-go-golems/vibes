import { z } from "zod";
import { protectedProcedure, router } from "./_core/trpc";
import { createPdfJob, getUserPdfJobs, getPdfJob } from "./db";

export const pdfRouter = router({
  /**
   * Create a new PDF generation job
   */
  createJob: protectedProcedure
    .input(z.object({
      photoIds: z.array(z.number()),
    }))
    .mutation(async ({ ctx, input }) => {
      const userId = ctx.user.id;
      
      const result = await createPdfJob({
        userId,
        photoIds: JSON.stringify(input.photoIds),
        status: "pending",
      });
      
      // Get the inserted job ID from the result
      const jobId = Number((result as any).insertId || 0);
      
      return {
        jobId,
        message: "PDF generation job created. Processing will begin shortly.",
      };
    }),

  /**
   * Get all PDF jobs for the current user
   */
  listJobs: protectedProcedure
    .query(async ({ ctx }) => {
      return await getUserPdfJobs(ctx.user.id);
    }),

  /**
   * Get a specific PDF job by ID
   */
  getJob: protectedProcedure
    .input(z.object({
      jobId: z.number(),
    }))
    .query(async ({ input }) => {
      const job = await getPdfJob(input.jobId);
      if (!job) {
        throw new Error("Job not found");
      }
      return job;
    }),
});
