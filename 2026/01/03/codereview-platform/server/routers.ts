import { COOKIE_NAME } from "@shared/const";
import { getSessionCookieOptions } from "./_core/cookies";
import { systemRouter } from "./_core/systemRouter";
import { publicProcedure, router } from "./_core/trpc";
import { repositoryRouter } from "./routers/repository";
import { reviewsRouter } from "./routers/reviews";
import { quizzesRouter } from "./routers/quizzes";
import { guidesRouter } from "./routers/guides";

export const appRouter = router({
  system: systemRouter,
  auth: router({
    me: publicProcedure.query(opts => opts.ctx.user),
    logout: publicProcedure.mutation(({ ctx }) => {
      const cookieOptions = getSessionCookieOptions(ctx.req);
      ctx.res.clearCookie(COOKIE_NAME, { ...cookieOptions, maxAge: -1 });
      return {
        success: true,
      } as const;
    }),
  }),

  // Feature routers
  repository: repositoryRouter,
  reviews: reviewsRouter,
  quizzes: quizzesRouter,
  guides: guidesRouter,
});

export type AppRouter = typeof appRouter;
