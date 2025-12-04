import { COOKIE_NAME } from "@shared/const";
import { getSessionCookieOptions } from "./_core/cookies";
import { systemRouter } from "./_core/systemRouter";
import { publicProcedure, protectedProcedure, router } from "./_core/trpc";
import { z } from "zod";
import * as db from "./db";
import { executeQuery, getPredicateSignatures, extractAtoms } from "./prolog-executor-ts";
import { invokeLLM } from "./_core/llm";

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

  prolog: router({
    // Execute a Prolog query
    query: publicProcedure
      .input(z.object({
        facts: z.array(z.string()),
        query: z.string(),
      }))
      .mutation(async ({ input }) => {
        const result = await executeQuery(input.query, input.facts);
        return result;
      }),

    // Get all presets
    presets: publicProcedure.query(async () => {
      const presets = await db.getAllPresets();
      return presets.map(p => ({
        ...p,
        facts: JSON.parse(p.facts),
        exampleQueries: JSON.parse(p.exampleQueries),
      }));
    }),

    // Get a specific preset
    preset: publicProcedure
      .input(z.object({ id: z.number() }))
      .query(async ({ input }) => {
        const preset = await db.getPresetById(input.id);
        if (!preset) return null;
        return {
          ...preset,
          facts: JSON.parse(preset.facts),
          exampleQueries: JSON.parse(preset.exampleQueries),
        };
      }),

    // User sessions (protected)
    sessions: protectedProcedure.query(async ({ ctx }) => {
      const sessions = await db.getUserSessions(ctx.user.id);
      return sessions.map(s => ({
        ...s,
        facts: JSON.parse(s.facts),
      }));
    }),

    // Create session
    createSession: protectedProcedure
      .input(z.object({
        name: z.string(),
        facts: z.array(z.string()),
        description: z.string().optional(),
      }))
      .mutation(async ({ ctx, input }) => {
        await db.createSession({
          userId: ctx.user.id,
          name: input.name,
          facts: JSON.stringify(input.facts),
          description: input.description,
        });
        return { success: true };
      }),

    // Update session
    updateSession: protectedProcedure
      .input(z.object({
        id: z.number(),
        name: z.string().optional(),
        facts: z.array(z.string()).optional(),
        description: z.string().optional(),
      }))
      .mutation(async ({ ctx, input }) => {
        const updates: any = {};
        if (input.name) updates.name = input.name;
        if (input.facts) updates.facts = JSON.stringify(input.facts);
        if (input.description !== undefined) updates.description = input.description;
        
        await db.updateSession(input.id, ctx.user.id, updates);
        return { success: true };
      }),

    // Delete session
    deleteSession: protectedProcedure
      .input(z.object({ id: z.number() }))
      .mutation(async ({ ctx, input }) => {
        await db.deleteSession(input.id, ctx.user.id);
        return { success: true };
      }),

    // AI assistant to convert natural language to Prolog facts/rules
    generateFactsFromNL: publicProcedure
      .input(z.object({
        description: z.string(),
        existingFacts: z.array(z.string()).optional(),
        model: z.string().optional(),
      }))
      .mutation(async ({ input }) => {
        const systemPrompt = `You are a Prolog expert. Convert natural language descriptions into Prolog facts and rules.

Prolog syntax rules:
- Facts: (predicate arg1 arg2 ...) e.g., (parent tom bob)
- Rules: (head args) :- (goal1 args) (goal2 args) e.g., (grandparent ?gp ?gc) :- (parent ?gp ?p) (parent ?p ?gc)
- Variables start with ?
- Use lowercase for atoms
- Use CamelCase or snake_case for multi-word atoms

IMPORTANT: For EVERY rule you generate (any fact containing :-), you MUST provide a docstring explaining what the rule means in plain English.

Example output format:
{
  "reasoning": "Need parent facts and a grandparent rule...",
  "facts": [
    { "fact": "(parent tom bob)" },
    { "fact": "(grandparent ?gp ?gc) :- (parent ?gp ?p) (parent ?p ?gc)", "docstring": "Defines that a grandparent is someone who is a parent of an intermediate parent" }
  ]
}

First, think through what facts and rules are needed. Then provide the Prolog code with docstrings for ALL rules.`;

        const userPrompt = input.existingFacts && input.existingFacts.length > 0
          ? `Existing facts:\n${input.existingFacts.join('\n')}\n\nAdd facts/rules for: ${input.description}`
          : `Create Prolog facts/rules for: ${input.description}`;

        const response = await invokeLLM({
          model: input.model,
          messages: [
            { role: "system", content: systemPrompt },
            { role: "user", content: userPrompt },
          ],
          response_format: {
            type: "json_schema",
            json_schema: {
              name: "prolog_facts_generation",
              strict: false,
              schema: {
                type: "object",
                properties: {
                  reasoning: {
                    type: "string",
                    description: "Step-by-step reasoning about what facts/rules are needed and why"
                  },
                  facts: {
                    type: "array",
                    items: {
                      type: "object",
                      properties: {
                        fact: {
                          type: "string",
                          description: "The Prolog fact or rule"
                        },
                        docstring: {
                          type: "string",
                          description: "Human-readable explanation (required for rules with :-, optional for simple facts)"
                        }
                      },
                      required: ["fact"]
                    },
                    description: "Array of Prolog facts/rules with optional docstrings"
                  }
                },
                required: ["reasoning", "facts"],
                additionalProperties: false
              }
            }
          }
        });

        const content = response.choices[0]?.message?.content || "{}";
        const contentStr = typeof content === 'string' ? content : '{}';
        let parsed;
        try {
          parsed = JSON.parse(contentStr);
          console.log('[generateFactsFromNL] Parsed response:', JSON.stringify(parsed, null, 2));
        } catch (e) {
          console.error('[generateFactsFromNL] Failed to parse content:', e);
          parsed = { reasoning: "", facts: [] };
        }

        // Extract facts and docstrings from new schema format
        const factsArray = parsed.facts || [];
        const facts: string[] = [];
        const docstrings: Record<string, string> = {};
        
        for (const item of factsArray) {
          if (typeof item === 'string') {
            // Backward compatibility: if it's a string, just add it
            facts.push(item);
          } else if (item && typeof item === 'object') {
            // New format: extract fact and docstring
            const fact = item.fact || '';
            if (fact) {
              facts.push(fact);
              if (item.docstring) {
                docstrings[fact] = item.docstring;
              }
            }
          }
        }
        
        console.log('[generateFactsFromNL] Extracted facts:', facts);
        console.log('[generateFactsFromNL] Extracted docstrings:', docstrings);

        return { 
          facts,
          reasoning: parsed.reasoning || "",
          docstrings,
          rawResponse: content,
          debug: {
            systemPrompt,
            userPrompt,
            fullResponse: response,
          }
        };
      }),

    // AI assistant to convert natural language to Prolog queries
    generateQueryFromNL: publicProcedure
      .input(z.object({
        question: z.string(),
        facts: z.array(z.string()),
        docstrings: z.record(z.string(), z.string()).optional(),
        model: z.string().optional(),
      }))
      .mutation(async ({ input }) => {
        // First, load the facts to extract predicate signatures and atoms
        await executeQuery("(dummy)", input.facts); // Load facts into DB
        const signatures = getPredicateSignatures();
        const atomsMap = extractAtoms();
        
        // Format atoms for display
        const atomsList: string[] = [];
        for (const [atom, predicates] of Array.from(atomsMap.entries()).sort()) {
          atomsList.push(`  ${atom} (used in: ${Array.from(predicates).join(', ')})`);
        }
        
        const systemPrompt = `You are a Prolog expert. Convert natural language questions into Prolog queries.

Prolog query syntax:
- Single goal queries: (predicate arg1 arg2 ...) e.g., (parent tom ?child)
- Conjunctive queries (multiple goals): (goal1 args) (goal2 args) e.g., (parent ?p ?c) (age ?c ?age)
- Variables start with ? e.g., ?x, ?person, ?result
- Variables can be shared across goals to connect them
- Use ONLY the predicates and atoms available in the knowledge base
- Do NOT use (and ...) wrapper - just write goals separated by spaces

Examples:
- Single: (leads ?leader TeamA)
- Conjunction: (meeting-for ?m ?proj) (attends ?person ?m)
- Complex: (project-lead ?lead ?proj) (reports-to ?member ?lead) (attends ?member ?meeting)

First, think through which predicates and atoms to use. Then provide the query.`;

        const availablePredicates = signatures.length > 0 
          ? `\n\nAvailable predicates:\n${signatures.join('\n')}`
          : '';
        
        const availableAtoms = atomsList.length > 0
          ? `\n\nAvailable atoms (constants):\n${atomsList.join('\n')}`
          : '';
        
        // Include docstrings for rules if available
        let docstringsSection = '';
        if (input.docstrings && Object.keys(input.docstrings).length > 0) {
          const docstringsList = Object.entries(input.docstrings)
            .map(([rule, doc]) => `  ${rule}: ${doc}`)
            .join('\n');
          docstringsSection = `\n\nRule explanations:\n${docstringsList}`;
        }
        
        const userPrompt = `Question: ${input.question}${availablePredicates}${availableAtoms}${docstringsSection}\n\nProlog query:`;

        const response = await invokeLLM({
          model: input.model,
          messages: [
            { role: "system", content: systemPrompt },
            { role: "user", content: userPrompt },
          ],
          response_format: {
            type: "json_schema",
            json_schema: {
              name: "prolog_query_generation",
              strict: true,
              schema: {
                type: "object",
                properties: {
                  reasoning: {
                    type: "string",
                    description: "Step-by-step reasoning about which predicates and atoms to use and why"
                  },
                  query: {
                    type: "string",
                    description: "The Prolog query (single or conjunctive)"
                  }
                },
                required: ["reasoning", "query"],
                additionalProperties: false
              }
            }
          }
        });

        const content = response.choices[0]?.message?.content || "{}";
        const contentStr = typeof content === 'string' ? content : '{}';
        let parsed;
        try {
          parsed = JSON.parse(contentStr);
        } catch (e) {
          parsed = { reasoning: "", query: "" };
        }

        return { 
          query: parsed.query || "",
          reasoning: parsed.reasoning || "",
          rawResponse: content,
          debug: {
            systemPrompt,
            userPrompt,
            availablePredicates: signatures,
            availableAtoms: atomsList,
            fullResponse: response,
          }
        };
      }),
  }),
});

export type AppRouter = typeof appRouter;
