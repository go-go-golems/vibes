import { eq } from "drizzle-orm";
import { drizzle } from "drizzle-orm/mysql2";
import { InsertUser, users, photos, pdfJobs, InsertPhoto, Photo, InsertPdfJob, PdfJob } from "../drizzle/schema";
import { ENV } from './_core/env';

let _db: ReturnType<typeof drizzle> | null = null;

// Lazily create the drizzle instance so local tooling can run without a DB.
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

/**
 * Photo management functions
 */
export async function createPhoto(photo: InsertPhoto) {
  const db = await getDb();
  if (!db) throw new Error("Database not available");
  
  const result = await db.insert(photos).values(photo);
  return result;
}

export async function getUserPhotos(userId: number) {
  const db = await getDb();
  if (!db) return [];
  
  return await db.select().from(photos).where(eq(photos.userId, userId)).orderBy(photos.position);
}

export async function updatePhotoPosition(photoId: number, position: number) {
  const db = await getDb();
  if (!db) throw new Error("Database not available");
  
  await db.update(photos).set({ position }).where(eq(photos.id, photoId));
}

export async function deletePhoto(photoId: number) {
  const db = await getDb();
  if (!db) throw new Error("Database not available");
  
  await db.delete(photos).where(eq(photos.id, photoId));
}

export async function deleteUserPhotos(userId: number) {
  const db = await getDb();
  if (!db) throw new Error("Database not available");
  
  await db.delete(photos).where(eq(photos.userId, userId));
}

/**
 * PDF job management functions
 */
export async function createPdfJob(job: InsertPdfJob) {
  const db = await getDb();
  if (!db) throw new Error("Database not available");
  
  const result = await db.insert(pdfJobs).values(job);
  return result;
}

export async function getPdfJob(jobId: number) {
  const db = await getDb();
  if (!db) return undefined;
  
  const result = await db.select().from(pdfJobs).where(eq(pdfJobs.id, jobId)).limit(1);
  return result.length > 0 ? result[0] : undefined;
}

export async function getUserPdfJobs(userId: number) {
  const db = await getDb();
  if (!db) return [];
  
  return await db.select().from(pdfJobs).where(eq(pdfJobs.userId, userId)).orderBy(pdfJobs.createdAt);
}

export async function updatePdfJob(jobId: number, updates: Partial<Omit<PdfJob, 'id' | 'userId' | 'createdAt'>>) {
  const db = await getDb();
  if (!db) throw new Error("Database not available");
  
  await db.update(pdfJobs).set(updates).where(eq(pdfJobs.id, jobId));
}

export async function getPendingPdfJobs() {
  const db = await getDb();
  if (!db) return [];
  
  return await db.select().from(pdfJobs).where(eq(pdfJobs.status, "pending")).orderBy(pdfJobs.createdAt);
}
