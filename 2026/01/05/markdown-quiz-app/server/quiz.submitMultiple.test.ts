import { describe, expect, it } from "vitest";
import { appRouter } from "./routers";
import type { TrpcContext } from "./_core/context";

type AuthenticatedUser = NonNullable<TrpcContext["user"]>;

function createAuthContext(): { ctx: TrpcContext } {
  const user: AuthenticatedUser = {
    id: 1,
    openId: "test-user",
    email: "test@example.com",
    name: "Test User",
    loginMethod: "manus",
    role: "user",
    createdAt: new Date(),
    updatedAt: new Date(),
    lastSignedIn: new Date(),
  };

  const ctx: TrpcContext = {
    user,
    req: {
      protocol: "https",
      headers: {},
    } as TrpcContext["req"],
    res: {} as TrpcContext["res"],
  };

  return { ctx };
}

describe("quiz.submitMultiple", () => {
  it("submits multiple quizzes and returns results with scores", async () => {
    const { ctx } = createAuthContext();
    const caller = appRouter.createCaller(ctx);

    // First create a document with multiple forms
    const doc = await caller.documents.create({
      title: "Test Multi-Quiz Document",
      content: `# Test Document

<form id="quiz1">
name: Quiz 1
fields:
  - name: q1
    label: What is 2+2?
    type: radio
    options:
      - "3"
      - "4"
      - "5"
    correct: "4"
</form>

<form id="quiz2">
name: Quiz 2
fields:
  - name: q1
    label: Select programming languages
    type: checkbox
    options:
      - Python
      - HTML
      - JavaScript
    correct:
      - Python
      - JavaScript
</form>`,
      description: "Test document",
      category: "Test",
      published: true,
    });

    // Submit multiple quizzes
    const result = await caller.quiz.submitMultiple({
      documentId: doc.id,
      submissions: [
        {
          formId: "quiz1",
          responses: { q1: "4" }, // Correct answer
        },
        {
          formId: "quiz2",
          responses: { q1: ["Python", "JavaScript"] }, // Correct answers
        },
      ],
    });

    expect(result.results).toHaveLength(2);
    expect(result.results[0]).toMatchObject({
      formId: "quiz1",
      score: 1,
      maxScore: 1,
    });
    expect(result.results[1]).toMatchObject({
      formId: "quiz2",
      score: 1,
      maxScore: 1,
    });
  });

  it("handles partial correct answers", async () => {
    const { ctx } = createAuthContext();
    const caller = appRouter.createCaller(ctx);

    const doc = await caller.documents.create({
      title: "Test Partial Scores",
      content: `# Test

<form id="quiz1">
name: Quiz
fields:
  - name: q1
    label: Question 1
    type: radio
    options: ["A", "B"]
    correct: "A"
  - name: q2
    label: Question 2
    type: radio
    options: ["X", "Y"]
    correct: "Y"
</form>`,
      description: "Test",
      category: "Test",
      published: true,
    });

    const result = await caller.quiz.submitMultiple({
      documentId: doc.id,
      submissions: [
        {
          formId: "quiz1",
          responses: { q1: "A", q2: "X" }, // 1 correct, 1 wrong
        },
      ],
    });

    expect(result.results[0]).toMatchObject({
      formId: "quiz1",
      score: 1, // Only q1 is correct
      maxScore: 2,
    });
  });
});
