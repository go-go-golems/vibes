import { eq, desc, and, sql } from "drizzle-orm";
import { drizzle } from "drizzle-orm/mysql2";
import { InsertUser, users, documents, quizForms, quizSubmissions, InsertDocument, InsertQuizForm, InsertQuizSubmission } from "../drizzle/schema";
import { ENV } from './_core/env';

let _db: ReturnType<typeof drizzle> | null = null;

export async function getDb() {
  if (!_db && process.env.DATABASE_URL) {
    try {
      _db = drizzle(process.env.DATABASE_URL);
    } catch (error) {
      console.warn("[Database] Failed to connect:", error);
      _db = null;
    }
  }
  return _db;
}

// ============ USER QUERIES ============

export async function upsertUser(user: InsertUser): Promise<void> {
  if (!user.openId) {
    throw new Error("User openId is required for upsert");
  }

  const db = await getDb();
  if (!db) {
    console.warn("[Database] Cannot upsert user: database not available");
    return;
  }

  try {
    const values: InsertUser = {
      openId: user.openId,
    };
    const updateSet: Record<string, unknown> = {};

    const textFields = ["name", "email", "loginMethod"] as const;
    type TextField = (typeof textFields)[number];

    const assignNullable = (field: TextField) => {
      const value = user[field];
      if (value === undefined) return;
      const normalized = value ?? null;
      values[field] = normalized;
      updateSet[field] = normalized;
    };

    textFields.forEach(assignNullable);

    if (user.lastSignedIn !== undefined) {
      values.lastSignedIn = user.lastSignedIn;
      updateSet.lastSignedIn = user.lastSignedIn;
    }
    if (user.role !== undefined) {
      values.role = user.role;
      updateSet.role = user.role;
    } else if (user.openId === ENV.ownerOpenId) {
      values.role = 'admin';
      updateSet.role = 'admin';
    }

    if (!values.lastSignedIn) {
      values.lastSignedIn = new Date();
    }

    if (Object.keys(updateSet).length === 0) {
      updateSet.lastSignedIn = new Date();
    }

    await db.insert(users).values(values).onDuplicateKeyUpdate({
      set: updateSet,
    });
  } catch (error) {
    console.error("[Database] Failed to upsert user:", error);
    throw error;
  }
}

export async function getUserByOpenId(openId: string) {
  const db = await getDb();
  if (!db) {
    console.warn("[Database] Cannot get user: database not available");
    return undefined;
  }

  const result = await db.select().from(users).where(eq(users.openId, openId)).limit(1);
  return result.length > 0 ? result[0] : undefined;
}

// ============ DOCUMENT QUERIES ============

export async function createDocument(doc: InsertDocument) {
  const db = await getDb();
  if (!db) throw new Error("Database not available");
  
  const result = await db.insert(documents).values(doc);
  return { id: Number(result[0].insertId) };
}

export async function updateDocument(id: number, doc: Partial<InsertDocument>) {
  const db = await getDb();
  if (!db) throw new Error("Database not available");
  
  await db.update(documents).set(doc).where(eq(documents.id, id));
}

export async function deleteDocument(id: number) {
  const db = await getDb();
  if (!db) throw new Error("Database not available");
  
  // Delete related quiz forms and submissions first
  await db.delete(quizSubmissions).where(eq(quizSubmissions.documentId, id));
  await db.delete(quizForms).where(eq(quizForms.documentId, id));
  await db.delete(documents).where(eq(documents.id, id));
}

export async function getDocumentById(id: number) {
  const db = await getDb();
  if (!db) throw new Error("Database not available");
  
  const result = await db.select().from(documents).where(eq(documents.id, id)).limit(1);
  return result[0] || null;
}

export async function getDocumentBySlug(slug: string) {
  const db = await getDb();
  if (!db) throw new Error("Database not available");
  
  const result = await db.select().from(documents).where(eq(documents.slug, slug)).limit(1);
  return result[0] || null;
}

export async function listDocuments(authorId?: number, publishedOnly = false) {
  const db = await getDb();
  if (!db) throw new Error("Database not available");
  
  let query = db.select().from(documents);
  
  if (authorId && publishedOnly) {
    query = query.where(and(eq(documents.authorId, authorId), eq(documents.isPublished, true))) as typeof query;
  } else if (authorId) {
    query = query.where(eq(documents.authorId, authorId)) as typeof query;
  } else if (publishedOnly) {
    query = query.where(eq(documents.isPublished, true)) as typeof query;
  }
  
  return query.orderBy(desc(documents.updatedAt));
}

export async function listAllDocuments() {
  const db = await getDb();
  if (!db) throw new Error("Database not available");
  
  return db.select().from(documents).orderBy(desc(documents.updatedAt));
}

// ============ QUIZ FORM QUERIES ============

export async function upsertQuizForm(form: InsertQuizForm) {
  const db = await getDb();
  if (!db) throw new Error("Database not available");
  
  // Check if form exists
  const existing = await db.select()
    .from(quizForms)
    .where(and(
      eq(quizForms.documentId, form.documentId),
      eq(quizForms.formId, form.formId)
    ))
    .limit(1);
  
  if (existing.length > 0) {
    await db.update(quizForms)
      .set({ definition: form.definition })
      .where(eq(quizForms.id, existing[0].id));
    return { id: existing[0].id };
  } else {
    const result = await db.insert(quizForms).values(form);
    return { id: Number(result[0].insertId) };
  }
}

export async function getQuizFormsByDocument(documentId: number) {
  const db = await getDb();
  if (!db) throw new Error("Database not available");
  
  return db.select().from(quizForms).where(eq(quizForms.documentId, documentId));
}

export async function deleteQuizFormsByDocument(documentId: number) {
  const db = await getDb();
  if (!db) throw new Error("Database not available");
  
  await db.delete(quizForms).where(eq(quizForms.documentId, documentId));
}

// ============ QUIZ SUBMISSION QUERIES ============

export async function createQuizSubmission(submission: InsertQuizSubmission) {
  const db = await getDb();
  if (!db) throw new Error("Database not available");
  
  const result = await db.insert(quizSubmissions).values(submission);
  return { id: Number(result[0].insertId) };
}

export async function getSubmissionsByUser(userId: number) {
  const db = await getDb();
  if (!db) throw new Error("Database not available");
  
  return db.select({
    submission: quizSubmissions,
    documentTitle: documents.title,
    documentSlug: documents.slug,
  })
    .from(quizSubmissions)
    .leftJoin(documents, eq(quizSubmissions.documentId, documents.id))
    .where(eq(quizSubmissions.userId, userId))
    .orderBy(desc(quizSubmissions.submittedAt));
}

export async function getSubmissionsByDocument(documentId: number) {
  const db = await getDb();
  if (!db) throw new Error("Database not available");
  
  return db.select({
    submission: quizSubmissions,
    userName: users.name,
  })
    .from(quizSubmissions)
    .leftJoin(users, eq(quizSubmissions.userId, users.id))
    .where(eq(quizSubmissions.documentId, documentId))
    .orderBy(desc(quizSubmissions.submittedAt));
}

export async function getSubmissionById(id: number) {
  const db = await getDb();
  if (!db) throw new Error("Database not available");
  
  const result = await db.select({
    submission: quizSubmissions,
    documentTitle: documents.title,
    documentSlug: documents.slug,
  })
    .from(quizSubmissions)
    .leftJoin(documents, eq(quizSubmissions.documentId, documents.id))
    .where(eq(quizSubmissions.id, id))
    .limit(1);
  
  return result[0] || null;
}

export async function getQuizAnalytics(documentId: number) {
  const db = await getDb();
  if (!db) throw new Error("Database not available");
  
  const submissions = await db.select()
    .from(quizSubmissions)
    .where(eq(quizSubmissions.documentId, documentId));
  
  const totalSubmissions = submissions.length;
  const scoredSubmissions = submissions.filter(s => s.score !== null && s.maxScore !== null);
  
  let averageScore = 0;
  let highestScore = 0;
  let lowestScore = 100;
  
  if (scoredSubmissions.length > 0) {
    const percentages = scoredSubmissions.map(s => (s.score! / s.maxScore!) * 100);
    averageScore = percentages.reduce((a, b) => a + b, 0) / percentages.length;
    highestScore = Math.max(...percentages);
    lowestScore = Math.min(...percentages);
  }
  
  return {
    totalSubmissions,
    averageScore: Math.round(averageScore * 10) / 10,
    highestScore: Math.round(highestScore * 10) / 10,
    lowestScore: scoredSubmissions.length > 0 ? Math.round(lowestScore * 10) / 10 : 0,
  };
}
