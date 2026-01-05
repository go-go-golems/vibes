import { int, mysqlEnum, mysqlTable, text, timestamp, varchar, json, boolean } from "drizzle-orm/mysql-core";

/**
 * Core user table backing auth flow.
 */
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

/**
 * Markdown documents that can contain embedded quizzes
 */
export const documents = mysqlTable("documents", {
  id: int("id").autoincrement().primaryKey(),
  title: varchar("title", { length: 255 }).notNull(),
  slug: varchar("slug", { length: 255 }).notNull().unique(),
  content: text("content").notNull(),
  description: text("description"),
  category: varchar("category", { length: 100 }),
  isPublished: boolean("isPublished").default(false).notNull(),
  authorId: int("authorId").notNull(),
  createdAt: timestamp("createdAt").defaultNow().notNull(),
  updatedAt: timestamp("updatedAt").defaultNow().onUpdateNow().notNull(),
});

export type Document = typeof documents.$inferSelect;
export type InsertDocument = typeof documents.$inferInsert;

/**
 * Quiz forms extracted from documents
 * Stores the YAML DSL definition for each form
 */
export const quizForms = mysqlTable("quiz_forms", {
  id: int("id").autoincrement().primaryKey(),
  documentId: int("documentId").notNull(),
  formId: varchar("formId", { length: 100 }).notNull(), // The id from <form id=xxx>
  definition: json("definition").notNull(), // Parsed YAML DSL as JSON
  createdAt: timestamp("createdAt").defaultNow().notNull(),
  updatedAt: timestamp("updatedAt").defaultNow().onUpdateNow().notNull(),
});

export type QuizForm = typeof quizForms.$inferSelect;
export type InsertQuizForm = typeof quizForms.$inferInsert;

/**
 * Quiz submissions - one per user per form attempt
 */
export const quizSubmissions = mysqlTable("quiz_submissions", {
  id: int("id").autoincrement().primaryKey(),
  userId: int("userId").notNull(),
  documentId: int("documentId").notNull(),
  formId: varchar("formId", { length: 100 }).notNull(),
  responses: json("responses").notNull(), // User's answers as JSON
  score: int("score"), // Calculated score if applicable
  maxScore: int("maxScore"), // Maximum possible score
  submittedAt: timestamp("submittedAt").defaultNow().notNull(),
});

export type QuizSubmission = typeof quizSubmissions.$inferSelect;
export type InsertQuizSubmission = typeof quizSubmissions.$inferInsert;
