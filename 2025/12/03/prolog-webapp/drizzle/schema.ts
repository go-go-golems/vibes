import { int, mysqlEnum, mysqlTable, text, timestamp, varchar } from "drizzle-orm/mysql-core";

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
 * Prolog sessions - stores user's current facts and queries
 */
export const prologSessions = mysqlTable("prolog_sessions", {
  id: int("id").autoincrement().primaryKey(),
  userId: int("userId").references(() => users.id),
  name: varchar("name", { length: 255 }).notNull(),
  facts: text("facts").notNull(), // JSON array of fact strings
  description: text("description"),
  createdAt: timestamp("createdAt").defaultNow().notNull(),
  updatedAt: timestamp("updatedAt").defaultNow().onUpdateNow().notNull(),
});

export type PrologSession = typeof prologSessions.$inferSelect;
export type InsertPrologSession = typeof prologSessions.$inferInsert;

/**
 * Prolog presets - predefined example programs
 */
export const prologPresets = mysqlTable("prolog_presets", {
  id: int("id").autoincrement().primaryKey(),
  name: varchar("name", { length: 255 }).notNull(),
  description: text("description"),
  category: varchar("category", { length: 100 }).notNull(),
  facts: text("facts").notNull(), // JSON array of fact strings
  exampleQueries: text("exampleQueries").notNull(), // JSON array of example query strings
  createdAt: timestamp("createdAt").defaultNow().notNull(),
});

export type PrologPreset = typeof prologPresets.$inferSelect;
export type InsertPrologPreset = typeof prologPresets.$inferInsert;
