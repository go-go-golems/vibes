import { describe, expect, it, vi, beforeEach } from "vitest";
import { appRouter } from "./routers";
import type { TrpcContext } from "./_core/context";

// Mock the database module
vi.mock("./db", () => ({
  createDocument: vi.fn().mockResolvedValue({ id: 1 }),
  updateDocument: vi.fn().mockResolvedValue(undefined),
  deleteDocument: vi.fn().mockResolvedValue(undefined),
  getDocumentById: vi.fn().mockResolvedValue({
    id: 1,
    title: "Test Document",
    slug: "test-document-abc123",
    content: "# Hello\n\n<form id=\"quiz1\">\nname: Test Quiz\nfields:\n  - name: q1\n    label: Question 1\n    type: text\n</form>",
    description: "A test document",
    category: "Test",
    isPublished: true,
    authorId: 1,
    createdAt: new Date(),
    updatedAt: new Date(),
  }),
  getDocumentBySlug: vi.fn().mockResolvedValue({
    id: 1,
    title: "Test Document",
    slug: "test-document-abc123",
    content: "# Hello",
    description: "A test document",
    category: "Test",
    isPublished: true,
    authorId: 1,
    createdAt: new Date(),
    updatedAt: new Date(),
  }),
  listDocuments: vi.fn().mockResolvedValue([]),
  listAllDocuments: vi.fn().mockResolvedValue([]),
  upsertQuizForm: vi.fn().mockResolvedValue({ id: 1 }),
  getQuizFormsByDocument: vi.fn().mockResolvedValue([
    {
      id: 1,
      documentId: 1,
      formId: "quiz1",
      definition: {
        name: "Test Quiz",
        fields: [
          { name: "q1", label: "Question 1", type: "text", correct: "answer" }
        ]
      },
      createdAt: new Date(),
      updatedAt: new Date(),
    }
  ]),
  deleteQuizFormsByDocument: vi.fn().mockResolvedValue(undefined),
  createQuizSubmission: vi.fn().mockResolvedValue({ id: 1 }),
  getSubmissionsByUser: vi.fn().mockResolvedValue([]),
  getSubmissionsByDocument: vi.fn().mockResolvedValue([]),
  getSubmissionById: vi.fn().mockResolvedValue({
    submission: {
      id: 1,
      userId: 1,
      documentId: 1,
      formId: "quiz1",
      responses: { q1: "answer" },
      score: 1,
      maxScore: 1,
      submittedAt: new Date(),
    },
    documentTitle: "Test Document",
    documentSlug: "test-document-abc123",
  }),
  getQuizAnalytics: vi.fn().mockResolvedValue({
    totalSubmissions: 5,
    averageScore: 80,
    highestScore: 100,
    lowestScore: 60,
  }),
}));

type AuthenticatedUser = NonNullable<TrpcContext["user"]>;

function createAuthContext(role: "user" | "admin" = "user"): TrpcContext {
  const user: AuthenticatedUser = {
    id: 1,
    openId: "test-user",
    email: "test@example.com",
    name: "Test User",
    loginMethod: "manus",
    role,
    createdAt: new Date(),
    updatedAt: new Date(),
    lastSignedIn: new Date(),
  };

  return {
    user,
    req: {
      protocol: "https",
      headers: {},
    } as TrpcContext["req"],
    res: {
      clearCookie: vi.fn(),
    } as unknown as TrpcContext["res"],
  };
}

function createPublicContext(): TrpcContext {
  return {
    user: null,
    req: {
      protocol: "https",
      headers: {},
    } as TrpcContext["req"],
    res: {
      clearCookie: vi.fn(),
    } as unknown as TrpcContext["res"],
  };
}

describe("documents router", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  describe("documents.create", () => {
    it("creates a document with extracted quiz forms", async () => {
      const ctx = createAuthContext();
      const caller = appRouter.createCaller(ctx);

      const result = await caller.documents.create({
        title: "Test Document",
        content: "# Hello\n\n<form id=\"quiz1\">\nname: Test Quiz\nfields:\n  - name: q1\n    label: Question 1\n    type: text\n</form>",
        description: "A test document",
        category: "Test",
        isPublished: true,
      });

      expect(result).toHaveProperty("id");
      expect(result).toHaveProperty("slug");
      expect(result.slug).toContain("test-document");
    });
  });

  describe("documents.getBySlug", () => {
    it("returns document with forms for public access when published", async () => {
      const ctx = createPublicContext();
      const caller = appRouter.createCaller(ctx);

      const result = await caller.documents.getBySlug({ slug: "test-document-abc123" });

      expect(result).toHaveProperty("title", "Test Document");
      expect(result).toHaveProperty("forms");
    });
  });

  describe("documents.update", () => {
    it("updates document and re-extracts forms when content changes", async () => {
      const ctx = createAuthContext();
      const caller = appRouter.createCaller(ctx);

      const result = await caller.documents.update({
        id: 1,
        title: "Updated Title",
        content: "# Updated\n\n<form id=\"quiz2\">\nname: New Quiz\nfields:\n  - name: q2\n    label: Question 2\n    type: radio\n</form>",
      });

      expect(result).toEqual({ success: true });
    });
  });
});

describe("quiz router", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  describe("quiz.submit", () => {
    it("submits quiz responses and calculates score", async () => {
      const ctx = createAuthContext();
      const caller = appRouter.createCaller(ctx);

      const result = await caller.quiz.submit({
        documentId: 1,
        formId: "quiz1",
        responses: { q1: "answer" },
      });

      expect(result).toHaveProperty("id");
      expect(result).toHaveProperty("score");
      expect(result).toHaveProperty("maxScore");
    });
  });

  describe("quiz.mySubmissions", () => {
    it("returns user submissions", async () => {
      const ctx = createAuthContext();
      const caller = appRouter.createCaller(ctx);

      const result = await caller.quiz.mySubmissions();

      expect(Array.isArray(result)).toBe(true);
    });
  });

  describe("quiz.getSubmission", () => {
    it("returns submission with form definition", async () => {
      const ctx = createAuthContext();
      const caller = appRouter.createCaller(ctx);

      const result = await caller.quiz.getSubmission({ id: 1 });

      expect(result).toHaveProperty("submission");
      expect(result).toHaveProperty("documentTitle");
      expect(result).toHaveProperty("formDefinition");
    });
  });
});
