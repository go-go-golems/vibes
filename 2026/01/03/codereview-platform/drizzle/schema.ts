import { int, mysqlEnum, mysqlTable, text, timestamp, varchar, json, boolean } from "drizzle-orm/mysql-core";

// Core user table backing auth flow
export const users = mysqlTable("users", {
  id: int("id").autoincrement().primaryKey(),
  openId: varchar("openId", { length: 64 }).notNull().unique(),
  name: text("name"),
  email: varchar("email", { length: 320 }),
  loginMethod: varchar("loginMethod", { length: 64 }),
  role: mysqlEnum("role", ["user", "admin"]).default("user").notNull(),
  createdAt: timestamp("createdAt").defaultNow().notNull(),
  updatedAt: timestamp("updatedAt").defaultNow().onUpdateNow().notNull(),
  lastSignedIn: timestamp("lastSignedIn").defaultNow().notNull(),
});

export type User = typeof users.$inferSelect;
export type InsertUser = typeof users.$inferInsert;

// Git repositories managed by the platform
export const repositories = mysqlTable("repositories", {
  id: int("id").autoincrement().primaryKey(),
  name: varchar("name", { length: 255 }).notNull(),
  path: varchar("path", { length: 512 }).notNull().unique(),
  description: text("description"),
  defaultBranch: varchar("defaultBranch", { length: 128 }).default("main"),
  createdAt: timestamp("createdAt").defaultNow().notNull(),
  updatedAt: timestamp("updatedAt").defaultNow().onUpdateNow().notNull(),
});

export type Repository = typeof repositories.$inferSelect;
export type InsertRepository = typeof repositories.$inferInsert;

// Code reviews stored as metadata (actual content in git notes)
export const codeReviews = mysqlTable("code_reviews", {
  id: int("id").autoincrement().primaryKey(),
  repositoryId: int("repositoryId").notNull(),
  prNumber: int("prNumber"),
  title: varchar("title", { length: 512 }).notNull(),
  description: text("description"),
  status: mysqlEnum("status", ["draft", "open", "merged", "closed"]).default("open").notNull(),
  baseBranch: varchar("baseBranch", { length: 128 }),
  headBranch: varchar("headBranch", { length: 128 }),
  baseCommit: varchar("baseCommit", { length: 64 }),
  headCommit: varchar("headCommit", { length: 64 }),
  authorId: int("authorId"),
  gitNotesRef: varchar("gitNotesRef", { length: 256 }), // Reference to git notes
  createdAt: timestamp("createdAt").defaultNow().notNull(),
  updatedAt: timestamp("updatedAt").defaultNow().onUpdateNow().notNull(),
});

export type CodeReview = typeof codeReviews.$inferSelect;
export type InsertCodeReview = typeof codeReviews.$inferInsert;

// Quizzes associated with code reviews or files
export const quizzes = mysqlTable("quizzes", {
  id: int("id").autoincrement().primaryKey(),
  repositoryId: int("repositoryId").notNull(),
  codeReviewId: int("codeReviewId"),
  quizId: varchar("quizId", { length: 128 }).notNull(), // YAML-defined ID
  title: varchar("title", { length: 512 }).notNull(),
  description: text("description"),
  filePath: varchar("filePath", { length: 512 }), // Associated file
  lineStart: int("lineStart"),
  lineEnd: int("lineEnd"),
  difficulty: mysqlEnum("difficulty", ["beginner", "intermediate", "advanced"]).default("intermediate"),
  estimatedTime: varchar("estimatedTime", { length: 32 }),
  gitNotesRef: varchar("gitNotesRef", { length: 256 }), // Reference to git notes with quiz def
  createdAt: timestamp("createdAt").defaultNow().notNull(),
  updatedAt: timestamp("updatedAt").defaultNow().onUpdateNow().notNull(),
});

export type Quiz = typeof quizzes.$inferSelect;
export type InsertQuiz = typeof quizzes.$inferInsert;

// User quiz submissions (stored in git notes, cached here)
export const quizSubmissions = mysqlTable("quiz_submissions", {
  id: int("id").autoincrement().primaryKey(),
  userId: int("userId").notNull(),
  quizId: int("quizId").notNull(),
  repositoryId: int("repositoryId").notNull(),
  answers: json("answers").$type<Record<string, unknown>>(),
  score: int("score"),
  maxScore: int("maxScore"),
  completed: boolean("completed").default(false),
  gitNotesRef: varchar("gitNotesRef", { length: 256 }), // Reference to git notes with submission
  submittedAt: timestamp("submittedAt").defaultNow().notNull(),
});

export type QuizSubmission = typeof quizSubmissions.$inferSelect;
export type InsertQuizSubmission = typeof quizSubmissions.$inferInsert;

// Review guides (narrative walkthroughs)
export const reviewGuides = mysqlTable("review_guides", {
  id: int("id").autoincrement().primaryKey(),
  repositoryId: int("repositoryId").notNull(),
  codeReviewId: int("codeReviewId"),
  guideId: varchar("guideId", { length: 128 }).notNull(), // YAML-defined ID
  title: varchar("title", { length: 512 }).notNull(),
  description: text("description"),
  difficulty: mysqlEnum("difficulty", ["beginner", "intermediate", "advanced"]).default("intermediate"),
  estimatedTime: varchar("estimatedTime", { length: 32 }),
  prerequisites: json("prerequisites").$type<string[]>(),
  gitNotesRef: varchar("gitNotesRef", { length: 256 }),
  createdAt: timestamp("createdAt").defaultNow().notNull(),
  updatedAt: timestamp("updatedAt").defaultNow().onUpdateNow().notNull(),
});

export type ReviewGuide = typeof reviewGuides.$inferSelect;
export type InsertReviewGuide = typeof reviewGuides.$inferInsert;

// User progress through review guides
export const guideProgress = mysqlTable("guide_progress", {
  id: int("id").autoincrement().primaryKey(),
  userId: int("userId").notNull(),
  guideId: int("guideId").notNull(),
  repositoryId: int("repositoryId").notNull(),
  currentStopId: varchar("currentStopId", { length: 128 }),
  visitedStops: json("visitedStops").$type<string[]>(),
  completedAt: timestamp("completedAt"),
  startedAt: timestamp("startedAt").defaultNow().notNull(),
  updatedAt: timestamp("updatedAt").defaultNow().onUpdateNow().notNull(),
});

export type GuideProgress = typeof guideProgress.$inferSelect;
export type InsertGuideProgress = typeof guideProgress.$inferInsert;

// Annotations on code (from YAML DSL)
export const annotations = mysqlTable("annotations", {
  id: int("id").autoincrement().primaryKey(),
  repositoryId: int("repositoryId").notNull(),
  codeReviewId: int("codeReviewId"),
  filePath: varchar("filePath", { length: 512 }).notNull(),
  lineNumber: int("lineNumber"),
  lineEnd: int("lineEnd"),
  annotationType: mysqlEnum("annotationType", [
    "educational", "knowledge_share", "pattern_highlight", "gotcha", "evolution", "question"
  ]).default("educational"),
  title: varchar("title", { length: 256 }),
  content: text("content"),
  tags: json("tags").$type<string[]>(),
  relatedQuizId: int("relatedQuizId"),
  metadata: json("metadata").$type<Record<string, unknown>>(),
  gitNotesRef: varchar("gitNotesRef", { length: 256 }),
  createdAt: timestamp("createdAt").defaultNow().notNull(),
});

export type Annotation = typeof annotations.$inferSelect;
export type InsertAnnotation = typeof annotations.$inferInsert;
