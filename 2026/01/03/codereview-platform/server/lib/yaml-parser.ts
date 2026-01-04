import * as YAML from 'yaml';
import { z } from 'zod';

// Zod schemas for YAML DSL validation

// Quiz question schemas
const MultipleChoiceQuestionSchema = z.object({
  type: z.literal('multiple_choice'),
  question: z.string(),
  options: z.array(z.string()).min(2),
  correct: z.union([z.number(), z.array(z.number())]),
  explanation: z.string().optional(),
});

const CodeCompletionQuestionSchema = z.object({
  type: z.literal('code_completion'),
  question: z.string(),
  codeContext: z.string().optional(),
  answerPattern: z.string(),
  incorrectPatterns: z.array(z.object({
    pattern: z.string(),
    feedback: z.string(),
  })).optional(),
  explanation: z.string().optional(),
});

const ScenarioQuestionSchema = z.object({
  type: z.literal('scenario'),
  question: z.string(),
  options: z.array(z.string()).optional(),
  correct: z.union([z.number(), z.array(z.number())]).optional(),
  explanation: z.string().optional(),
  freeResponse: z.boolean().optional(),
});

const QuizQuestionSchema = z.discriminatedUnion('type', [
  MultipleChoiceQuestionSchema,
  CodeCompletionQuestionSchema,
  ScenarioQuestionSchema,
]);

// Quiz definition schema
export const QuizDefinitionSchema = z.object({
  id: z.string(),
  title: z.string(),
  description: z.string().optional(),
  context: z.string().optional(),
  difficulty: z.enum(['beginner', 'intermediate', 'advanced']).optional(),
  estimatedTime: z.string().optional(),
  questions: z.array(QuizQuestionSchema).min(1),
});

// Inline quiz schema (embedded in annotations)
const InlineQuizSchema = z.object({
  id: z.string(),
  title: z.string(),
  questions: z.array(QuizQuestionSchema).min(1),
});

// Annotation schema
const AnnotationSchema = z.object({
  file: z.string(),
  line: z.number(),
  lineEnd: z.number().optional(),
  type: z.enum(['educational', 'knowledge_share', 'pattern_highlight', 'gotcha', 'evolution', 'question']),
  title: z.string().optional(),
  content: z.string(),
  tags: z.array(z.string()).optional(),
  quiz: InlineQuizSchema.optional(),
});

// Review definition schema
export const ReviewDefinitionSchema = z.object({
  pr: z.number().optional(),
  title: z.string(),
  description: z.string().optional(),
  baseBranch: z.string().optional(),
  headBranch: z.string().optional(),
  annotations: z.array(AnnotationSchema),
});

// Guide stop schema
const GuideStopSchema = z.object({
  id: z.string(),
  file: z.string(),
  line: z.number(),
  title: z.string(),
  content: z.string(),
  questions: z.array(z.string()).optional(),
  next: z.string().optional(),
});

// Guide definition schema
export const GuideDefinitionSchema = z.object({
  id: z.string(),
  title: z.string(),
  description: z.string().optional(),
  difficulty: z.enum(['beginner', 'intermediate', 'advanced']).optional(),
  estimatedTime: z.string().optional(),
  prerequisites: z.array(z.string()).optional(),
  stops: z.array(GuideStopSchema).min(1),
});

// Pattern highlight annotation (for highlighting patterns across files)
const PatternHighlightSchema = z.object({
  type: z.literal('pattern_highlight'),
  name: z.string(),
  occurrences: z.array(z.object({
    file: z.string(),
    line: z.number(),
  })),
  explanation: z.string().optional(),
});

// Evolution annotation (showing before/after)
const EvolutionAnnotationSchema = z.object({
  type: z.literal('evolution'),
  title: z.string(),
  before: z.object({
    commit: z.string().optional(),
    file: z.string(),
    line: z.number(),
  }),
  after: z.object({
    file: z.string(),
    line: z.number(),
  }),
  narrative: z.string(),
});

// Combined annotations schema for bulk import
export const AnnotationsFileSchema = z.object({
  annotations: z.array(z.union([
    AnnotationSchema,
    PatternHighlightSchema,
    EvolutionAnnotationSchema,
  ])),
});

// Types derived from schemas
export type QuizDefinition = z.infer<typeof QuizDefinitionSchema>;
export type ReviewDefinition = z.infer<typeof ReviewDefinitionSchema>;
export type GuideDefinition = z.infer<typeof GuideDefinitionSchema>;
export type QuizQuestion = z.infer<typeof QuizQuestionSchema>;
export type Annotation = z.infer<typeof AnnotationSchema>;

// Parser result types
export interface ParseResult<T> {
  success: boolean;
  data?: T;
  errors?: string[];
}

// YAML DSL Parser class
export class YamlDslParser {
  // Parse a YAML string and validate against schema
  private parseAndValidate<T>(
    yamlContent: string,
    schema: z.ZodSchema<T>,
    rootKey?: string
  ): ParseResult<T> {
    try {
      const parsed = YAML.parse(yamlContent);
      const data = rootKey ? parsed[rootKey] : parsed;
      
      if (rootKey && !data) {
        return {
          success: false,
          errors: [`Missing required root key: ${rootKey}`],
        };
      }
      
      const result = schema.safeParse(data);
      
      if (result.success) {
        return { success: true, data: result.data };
      } else {
        const errors = result.error.issues.map(e => 
          `${e.path.join('.')}: ${e.message}`
        );
        return { success: false, errors };
      }
    } catch (error) {
      return {
        success: false,
        errors: [`YAML parse error: ${error instanceof Error ? error.message : 'Unknown error'}`],
      };
    }
  }

  // Parse a review definition
  parseReview(yamlContent: string): ParseResult<ReviewDefinition> {
    return this.parseAndValidate(yamlContent, ReviewDefinitionSchema, 'review');
  }

  // Parse a quiz definition
  parseQuiz(yamlContent: string): ParseResult<QuizDefinition> {
    return this.parseAndValidate(yamlContent, QuizDefinitionSchema, 'quiz');
  }

  // Parse a guide definition
  parseGuide(yamlContent: string): ParseResult<GuideDefinition> {
    return this.parseAndValidate(yamlContent, GuideDefinitionSchema, 'guide');
  }

  // Parse annotations file
  parseAnnotations(yamlContent: string): ParseResult<z.infer<typeof AnnotationsFileSchema>> {
    return this.parseAndValidate(yamlContent, AnnotationsFileSchema);
  }

  // Auto-detect and parse YAML content
  parseAuto(yamlContent: string): ParseResult<{
    type: 'review' | 'quiz' | 'guide' | 'annotations';
    data: ReviewDefinition | QuizDefinition | GuideDefinition | z.infer<typeof AnnotationsFileSchema>;
  }> {
    try {
      const parsed = YAML.parse(yamlContent);
      
      if (parsed.review) {
        const result = this.parseReview(yamlContent);
        if (result.success) {
          return { success: true, data: { type: 'review', data: result.data! } };
        }
        return { success: false, errors: result.errors };
      }
      
      if (parsed.quiz) {
        const result = this.parseQuiz(yamlContent);
        if (result.success) {
          return { success: true, data: { type: 'quiz', data: result.data! } };
        }
        return { success: false, errors: result.errors };
      }
      
      if (parsed.guide) {
        const result = this.parseGuide(yamlContent);
        if (result.success) {
          return { success: true, data: { type: 'guide', data: result.data! } };
        }
        return { success: false, errors: result.errors };
      }
      
      if (parsed.annotations) {
        const result = this.parseAnnotations(yamlContent);
        if (result.success) {
          return { success: true, data: { type: 'annotations', data: result.data! } };
        }
        return { success: false, errors: result.errors };
      }
      
      return {
        success: false,
        errors: ['Unknown YAML format. Expected root key: review, quiz, guide, or annotations'],
      };
    } catch (error) {
      return {
        success: false,
        errors: [`YAML parse error: ${error instanceof Error ? error.message : 'Unknown error'}`],
      };
    }
  }

  // Validate guide stop links (ensure all next references are valid)
  validateGuideLinks(guide: GuideDefinition): string[] {
    const errors: string[] = [];
    const stopIds = new Set(guide.stops.map(s => s.id));
    
    for (const stop of guide.stops) {
      if (stop.next && !stopIds.has(stop.next)) {
        errors.push(`Stop "${stop.id}" references non-existent next stop: "${stop.next}"`);
      }
    }
    
    return errors;
  }

  // Extract all file references from a review
  extractFileReferences(review: ReviewDefinition): string[] {
    const files = new Set<string>();
    for (const annotation of review.annotations) {
      files.add(annotation.file);
    }
    return Array.from(files);
  }

  // Extract all file references from a guide
  extractGuideFileReferences(guide: GuideDefinition): string[] {
    const files = new Set<string>();
    for (const stop of guide.stops) {
      files.add(stop.file);
    }
    return Array.from(files);
  }

  // Get quiz statistics
  getQuizStats(quiz: QuizDefinition): {
    totalQuestions: number;
    byType: Record<string, number>;
    estimatedPoints: number;
  } {
    const byType: Record<string, number> = {};
    
    for (const question of quiz.questions) {
      byType[question.type] = (byType[question.type] || 0) + 1;
    }
    
    return {
      totalQuestions: quiz.questions.length,
      byType,
      estimatedPoints: quiz.questions.length * 10, // 10 points per question
    };
  }

  // Serialize to YAML
  toYaml<T>(data: T, rootKey?: string): string {
    const obj = rootKey ? { [rootKey]: data } : data;
    return YAML.stringify(obj);
  }
}

// Factory function
export function createYamlParser(): YamlDslParser {
  return new YamlDslParser();
}
