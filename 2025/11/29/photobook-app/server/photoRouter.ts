import { z } from "zod";
import { protectedProcedure, router } from "./_core/trpc";
import { storagePut } from "./storage";
import { createPhoto, getUserPhotos, updatePhotoPosition, deletePhoto, deleteUserPhotos } from "./db";
import { nanoid } from "nanoid";

export const photoRouter = router({
  /**
   * Upload a photo to S3 and save metadata to database
   */
  upload: protectedProcedure
    .input(z.object({
      filename: z.string(),
      mimeType: z.string(),
      data: z.string(), // base64 encoded
      position: z.number(),
    }))
    .mutation(async ({ ctx, input }) => {
      const userId = ctx.user.id;
      
      // Decode base64 data
      const buffer = Buffer.from(input.data, 'base64');
      const size = buffer.length;
      
      // Generate unique file key
      const fileKey = `user-${userId}/photos/${nanoid()}-${input.filename}`;
      
      // Upload to S3
      const { url } = await storagePut(fileKey, buffer, input.mimeType);
      
      // Save to database
      await createPhoto({
        userId,
        fileKey,
        url,
        filename: input.filename,
        mimeType: input.mimeType,
        size,
        position: input.position,
      });
      
      return { url, fileKey };
    }),

  /**
   * Get all photos for the current user
   */
  list: protectedProcedure
    .query(async ({ ctx }) => {
      return await getUserPhotos(ctx.user.id);
    }),

  /**
   * Update photo positions (for reordering)
   */
  updatePositions: protectedProcedure
    .input(z.object({
      updates: z.array(z.object({
        id: z.number(),
        position: z.number(),
      })),
    }))
    .mutation(async ({ input }) => {
      for (const update of input.updates) {
        await updatePhotoPosition(update.id, update.position);
      }
      return { success: true };
    }),

  /**
   * Delete a single photo
   */
  delete: protectedProcedure
    .input(z.object({
      id: z.number(),
    }))
    .mutation(async ({ input }) => {
      await deletePhoto(input.id);
      return { success: true };
    }),

  /**
   * Delete all photos for the current user
   */
  deleteAll: protectedProcedure
    .mutation(async ({ ctx }) => {
      await deleteUserPhotos(ctx.user.id);
      return { success: true };
    }),
});
