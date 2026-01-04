import * as git from 'isomorphic-git';
import * as YAML from 'yaml';
import * as path from 'path';
import * as fs from 'fs';

// Git notes namespaces for different data types
export const GIT_NOTES_REFS = {
  REVIEWS: 'refs/notes/reviews',
  QUIZZES: 'refs/notes/quizzes',
  SUBMISSIONS: 'refs/notes/submissions',
  GUIDES: 'refs/notes/guides',
  ANNOTATIONS: 'refs/notes/annotations',
} as const;

export type GitNotesRef = typeof GIT_NOTES_REFS[keyof typeof GIT_NOTES_REFS];

export interface GitNotesOptions {
  repoPath: string;
}

export interface ReviewAnnotation {
  file: string;
  line: number;
  lineEnd?: number;
  type: 'educational' | 'knowledge_share' | 'pattern_highlight' | 'gotcha' | 'evolution' | 'question' | 'best-practice';
  title?: string;
  content: string;
  tags?: string[];
  quiz?: QuizDefinition;
}

export interface QuizQuestion {
  type: 'multiple_choice' | 'code_completion' | 'scenario';
  question: string;
  options?: string[];
  correct?: number | number[];
  explanation?: string;
  codeContext?: string;
  answerPattern?: string;
  incorrectPatterns?: { pattern: string; feedback: string }[];
}

export interface QuizDefinition {
  id: string;
  title: string;
  description?: string;
  context?: string;
  difficulty?: 'beginner' | 'intermediate' | 'advanced';
  estimatedTime?: string;
  questions: QuizQuestion[];
}

export interface ReviewDefinition {
  id?: string;
  pr?: number;
  title: string;
  description?: string;
  baseBranch?: string;
  headBranch?: string;
  annotations: ReviewAnnotation[];
}

export interface GuideStop {
  id: string;
  file: string;
  line: number;
  lineEnd?: number;
  title: string;
  content: string;
  questions?: string[];
  next?: string;
}

export interface GuideDefinition {
  id: string;
  title: string;
  description?: string;
  difficulty?: 'beginner' | 'intermediate' | 'advanced';
  estimatedTime?: string;
  prerequisites?: string[];
  stops: GuideStop[];
}

export interface QuizSubmission {
  quizId: string;
  userId: string;
  answers: Record<string, unknown>;
  score: number;
  maxScore: number;
  submittedAt: string;
}

export class GitNotesStorage {
  private repoPath: string;

  constructor(options: GitNotesOptions) {
    this.repoPath = options.repoPath;
  }

  // Initialize git notes refs if they don't exist
  async initialize(): Promise<void> {
    // No-op with isomorphic-git, refs are created on first write
  }

  // Get the current HEAD commit
  async getHeadCommit(): Promise<string> {
    const oid = await git.resolveRef({
      fs,
      dir: this.repoPath,
      ref: 'HEAD',
    });
    return oid;
  }

  // Read a note from a specific commit
  async readNote(ref: GitNotesRef, commit: string): Promise<string | null> {
    try {
      // Git notes use the first 2 chars as directory, rest as filename
      const noteDir = commit.substring(0, 2);
      const noteFile = commit.substring(2);
      const notePath = `${noteDir}/${noteFile}`;
      
      // Resolve the notes ref
      const notesOid = await git.resolveRef({
        fs,
        dir: this.repoPath,
        ref,
      });
      
      // Read the notes tree
      const { tree } = await git.readTree({
        fs,
        dir: this.repoPath,
        oid: notesOid,
      });
      
      // Find the directory entry
      const dirEntry = tree.find(e => e.path === noteDir);
      if (!dirEntry) return null;
      
      // Read the subdirectory tree
      const { tree: subTree } = await git.readTree({
        fs,
        dir: this.repoPath,
        oid: dirEntry.oid,
      });
      
      // Find the note file
      const noteEntry = subTree.find(e => e.path === noteFile);
      if (!noteEntry) return null;
      
      // Read the blob content
      const { blob } = await git.readBlob({
        fs,
        dir: this.repoPath,
        oid: noteEntry.oid,
      });
      
      return Buffer.from(blob).toString('utf8');
    } catch {
      return null;
    }
  }

  // List all notes in a ref
  async listNotes(ref: GitNotesRef): Promise<{ commit: string; noteCommit: string }[]> {
    try {
      // Resolve the notes ref
      const notesOid = await git.resolveRef({
        fs,
        dir: this.repoPath,
        ref,
      });
      
      // Read the notes tree
      const { tree } = await git.readTree({
        fs,
        dir: this.repoPath,
        oid: notesOid,
      });
      
      const notes: { commit: string; noteCommit: string }[] = [];
      
      // Iterate through directory entries
      for (const dirEntry of tree) {
        if (dirEntry.type === 'tree') {
          // Read subdirectory
          const { tree: subTree } = await git.readTree({
            fs,
            dir: this.repoPath,
            oid: dirEntry.oid,
          });
          
          for (const noteEntry of subTree) {
            // Reconstruct the commit SHA
            const commit = dirEntry.path + noteEntry.path;
            notes.push({ commit, noteCommit: noteEntry.oid });
          }
        }
      }
      
      return notes;
    } catch {
      return [];
    }
  }

  // Write a note to a specific commit
  async writeNote(ref: GitNotesRef, commit: string, content: string): Promise<void> {
    // Create a blob with the note content
    const noteOid = await git.writeBlob({
      fs,
      dir: this.repoPath,
      blob: Buffer.from(content, 'utf8'),
    });
    
    // Get or create the notes tree
    let parentCommit: string | undefined;
    let existingTree: { [path: string]: { mode: string; oid: string; type: string } } = {};
    
    try {
      const notesRef = await git.resolveRef({ fs, dir: this.repoPath, ref });
      parentCommit = notesRef;
      
      const { tree } = await git.readTree({ fs, dir: this.repoPath, oid: notesRef });
      for (const entry of tree) {
        existingTree[entry.path] = { mode: entry.mode, oid: entry.oid, type: entry.type };
      }
    } catch {
      // Notes ref doesn't exist yet
    }
    
    // Add/update the note entry
    const noteDir = commit.substring(0, 2);
    const noteFile = commit.substring(2);
    
    // Build tree entries
    const dirs: { [dir: string]: { mode: string; path: string; oid: string }[] } = {};
    
    // Process existing entries
    for (const [entryPath, entry] of Object.entries(existingTree)) {
      if (entry.type === 'tree') {
        // Read existing subtree
        const { tree: subTree } = await git.readTree({
          fs,
          dir: this.repoPath,
          oid: entry.oid,
        });
        dirs[entryPath] = subTree.map(e => ({ mode: e.mode, path: e.path, oid: e.oid }));
      }
    }
    
    // Add/update the new note
    if (!dirs[noteDir]) dirs[noteDir] = [];
    dirs[noteDir] = dirs[noteDir].filter(e => e.path !== noteFile);
    dirs[noteDir].push({ mode: '100644', path: noteFile, oid: noteOid });
    
    // Create subtrees for each directory
    const treeEntries: { mode: string; path: string; oid: string; type: 'blob' | 'tree' }[] = [];
    for (const [dirName, entries] of Object.entries(dirs)) {
      const subTreeOid = await git.writeTree({
        fs,
        dir: this.repoPath,
        tree: entries.map(e => ({ mode: e.mode, path: e.path, oid: e.oid, type: 'blob' as const })),
      });
      treeEntries.push({ mode: '040000', path: dirName, oid: subTreeOid, type: 'tree' });
    }
    
    // Create the notes tree
    const treeOid = await git.writeTree({
      fs,
      dir: this.repoPath,
      tree: treeEntries,
    });
    
    // Create a commit for the notes
    await git.commit({
      fs,
      dir: this.repoPath,
      ref,
      tree: treeOid,
      parent: parentCommit ? [parentCommit] : [],
      message: `Notes added by isomorphic-git`,
      author: {
        name: 'System',
        email: 'system@example.com',
      },
    });
  }

  // Remove a note from a specific commit
  async removeNote(ref: GitNotesRef, commit: string): Promise<void> {
    // Not implemented for isomorphic-git yet
  }

  // Write YAML content as a note
  async writeYamlNote<T>(ref: GitNotesRef, commit: string, data: T): Promise<void> {
    const yamlContent = YAML.stringify(data);
    await this.writeNote(ref, commit, yamlContent);
  }

  // Read and parse YAML note
  async readYamlNote<T>(ref: GitNotesRef, commit: string): Promise<T | null> {
    const content = await this.readNote(ref, commit);
    if (!content) return null;
    
    try {
      return YAML.parse(content) as T;
    } catch {
      return null;
    }
  }

  // Store a code review definition
  async storeReview(commit: string, review: ReviewDefinition): Promise<void> {
    await this.writeYamlNote(GIT_NOTES_REFS.REVIEWS, commit, { review });
  }

  // Get a code review definition by ID from a commit
  async getReview(commit: string, reviewId?: string): Promise<ReviewDefinition | null> {
    const data = await this.readYamlNote<ReviewDefinition | { review: ReviewDefinition } | { reviews: ReviewDefinition[] }>(GIT_NOTES_REFS.REVIEWS, commit);
    if (!data) return null;
    
    if ('reviews' in data && Array.isArray(data.reviews)) {
      if (reviewId) {
        return data.reviews.find(r => r.id === reviewId) || null;
      }
      return data.reviews[0] || null;
    }
    if ('review' in data && data.review) {
      return data.review;
    }
    if ('title' in data && 'annotations' in data) {
      return data as ReviewDefinition;
    }
    return null;
  }

  // Store a quiz definition
  async storeQuiz(commit: string, quiz: QuizDefinition): Promise<void> {
    await this.writeYamlNote(GIT_NOTES_REFS.QUIZZES, commit, { quiz });
  }

  // Get a quiz definition by ID from a commit
  async getQuiz(commit: string, quizId?: string): Promise<QuizDefinition | null> {
    const data = await this.readYamlNote<QuizDefinition | { quiz: QuizDefinition } | { quizzes: QuizDefinition[] }>(GIT_NOTES_REFS.QUIZZES, commit);
    if (!data) return null;
    
    if ('quizzes' in data && Array.isArray(data.quizzes)) {
      if (quizId) {
        return data.quizzes.find(q => q.id === quizId) || null;
      }
      return data.quizzes[0] || null;
    }
    if ('quiz' in data && data.quiz) {
      return data.quiz;
    }
    if ('id' in data && 'questions' in data) {
      return data as QuizDefinition;
    }
    return null;
  }

  // Store a quiz submission
  async storeSubmission(commit: string, submission: QuizSubmission): Promise<void> {
    const key = `${submission.quizId}:${submission.userId}`;
    
    const existing = await this.readYamlNote<{ submissions: Record<string, QuizSubmission> }>(
      GIT_NOTES_REFS.SUBMISSIONS, commit
    ) || { submissions: {} };
    
    existing.submissions[key] = submission;
    await this.writeYamlNote(GIT_NOTES_REFS.SUBMISSIONS, commit, existing);
  }

  // Get quiz submissions for a commit
  async getSubmissions(commit: string): Promise<Record<string, QuizSubmission>> {
    const data = await this.readYamlNote<{ submissions: Record<string, QuizSubmission> }>(
      GIT_NOTES_REFS.SUBMISSIONS, commit
    );
    return data?.submissions || {};
  }

  // Get a specific user's submission for a quiz
  async getUserSubmission(commit: string, quizId: string, userId: string): Promise<QuizSubmission | null> {
    const submissions = await this.getSubmissions(commit);
    return submissions[`${quizId}:${userId}`] || null;
  }

  // Store a review guide
  async storeGuide(commit: string, guide: GuideDefinition): Promise<void> {
    await this.writeYamlNote(GIT_NOTES_REFS.GUIDES, commit, { guide });
  }

  // Get a review guide by ID from a commit
  async getGuide(commit: string, guideId?: string): Promise<GuideDefinition | null> {
    const data = await this.readYamlNote<GuideDefinition | { guide: GuideDefinition } | { guides: GuideDefinition[] }>(GIT_NOTES_REFS.GUIDES, commit);
    if (!data) return null;
    
    if ('guides' in data && Array.isArray(data.guides)) {
      if (guideId) {
        return data.guides.find(g => g.id === guideId) || null;
      }
      return data.guides[0] || null;
    }
    if ('guide' in data && data.guide) {
      return data.guide;
    }
    if ('id' in data && 'stops' in data) {
      return data as GuideDefinition;
    }
    return null;
  }

  // Store annotations for a commit
  async storeAnnotations(commit: string, annotations: ReviewAnnotation[]): Promise<void> {
    await this.writeYamlNote(GIT_NOTES_REFS.ANNOTATIONS, commit, { annotations });
  }

  // Get annotations for a commit
  async getAnnotations(commit: string): Promise<ReviewAnnotation[]> {
    const data = await this.readYamlNote<{ annotations: ReviewAnnotation[] }>(GIT_NOTES_REFS.ANNOTATIONS, commit);
    return data?.annotations || [];
  }

  // Get all reviews in the repository
  async getAllReviews(): Promise<{ commit: string; review: ReviewDefinition }[]> {
    const notes = await this.listNotes(GIT_NOTES_REFS.REVIEWS);
    const reviews: { commit: string; review: ReviewDefinition }[] = [];
    
    for (const { commit } of notes) {
      const data = await this.readYamlNote<ReviewDefinition | { review: ReviewDefinition } | { reviews: ReviewDefinition[] }>(GIT_NOTES_REFS.REVIEWS, commit);
      if (!data) continue;
      
      if ('reviews' in data && Array.isArray(data.reviews)) {
        for (const review of data.reviews) {
          reviews.push({ commit, review });
        }
      } else if ('review' in data && data.review) {
        reviews.push({ commit, review: data.review });
      } else if ('title' in data && 'annotations' in data) {
        reviews.push({ commit, review: data as ReviewDefinition });
      }
    }
    
    return reviews;
  }

  // Get all quizzes in the repository
  async getAllQuizzes(): Promise<{ commit: string; quiz: QuizDefinition }[]> {
    const notes = await this.listNotes(GIT_NOTES_REFS.QUIZZES);
    const quizzes: { commit: string; quiz: QuizDefinition }[] = [];
    
    for (const { commit } of notes) {
      const data = await this.readYamlNote<QuizDefinition | { quiz: QuizDefinition } | { quizzes: QuizDefinition[] }>(GIT_NOTES_REFS.QUIZZES, commit);
      if (!data) continue;
      
      if ('quizzes' in data && Array.isArray(data.quizzes)) {
        for (const quiz of data.quizzes) {
          quizzes.push({ commit, quiz });
        }
      } else if ('quiz' in data && data.quiz) {
        quizzes.push({ commit, quiz: data.quiz });
      } else if ('id' in data && 'questions' in data) {
        quizzes.push({ commit, quiz: data as QuizDefinition });
      }
    }
    
    return quizzes;
  }

  // Get all guides in the repository
  async getAllGuides(): Promise<{ commit: string; guide: GuideDefinition }[]> {
    const notes = await this.listNotes(GIT_NOTES_REFS.GUIDES);
    const guides: { commit: string; guide: GuideDefinition }[] = [];
    
    for (const { commit } of notes) {
      const data = await this.readYamlNote<GuideDefinition | { guide: GuideDefinition } | { guides: GuideDefinition[] }>(GIT_NOTES_REFS.GUIDES, commit);
      if (!data) continue;
      
      if ('guides' in data && Array.isArray(data.guides)) {
        for (const guide of data.guides) {
          guides.push({ commit, guide });
        }
      } else if ('guide' in data && data.guide) {
        guides.push({ commit, guide: data.guide });
      } else if ('id' in data && 'stops' in data) {
        guides.push({ commit, guide: data as GuideDefinition });
      }
    }
    
    return guides;
  }

  // Get file content at a specific commit
  async getFileContent(commit: string, filePath: string): Promise<string | null> {
    try {
      const { blob } = await git.readBlob({
        fs,
        dir: this.repoPath,
        oid: commit,
        filepath: filePath,
      });
      return Buffer.from(blob).toString('utf8');
    } catch {
      return null;
    }
  }

  // Get file tree at a specific commit/branch
  async getFileTree(ref: string, treePath: string = ''): Promise<{ name: string; path: string; type: 'file' | 'directory' }[]> {
    try {
      // Resolve ref to commit
      let oid: string;
      try {
        oid = await git.resolveRef({ fs, dir: this.repoPath, ref });
      } catch {
        // Try as raw commit SHA
        oid = ref;
      }
      
      // Read commit to get tree
      const { commit } = await git.readCommit({ fs, dir: this.repoPath, oid });
      let treeOid = commit.tree;
      
      // Navigate to subpath if specified
      if (treePath) {
        const parts = treePath.split('/').filter(Boolean);
        for (const part of parts) {
          const { tree } = await git.readTree({ fs, dir: this.repoPath, oid: treeOid });
          const entry = tree.find(e => e.path === part);
          if (!entry || entry.type !== 'tree') {
            return [];
          }
          treeOid = entry.oid;
        }
      }
      
      // Read tree
      const { tree } = await git.readTree({ fs, dir: this.repoPath, oid: treeOid });
      
      return tree.map(entry => ({
        name: entry.path,
        path: treePath ? `${treePath}/${entry.path}` : entry.path,
        type: entry.type === 'tree' ? 'directory' as const : 'file' as const,
      }));
    } catch (e) {
      console.error('getFileTree error:', e);
      return [];
    }
  }

  // Get list of branches
  async getBranches(): Promise<string[]> {
    try {
      const branches = await git.listBranches({ fs, dir: this.repoPath });
      return branches;
    } catch {
      return [];
    }
  }

  // Get current branch
  async getCurrentBranch(): Promise<string | null> {
    try {
      const branch = await git.currentBranch({ fs, dir: this.repoPath });
      return branch || null;
    } catch {
      return null;
    }
  }

  // Get commit log
  async getLog(ref: string = 'HEAD', depth: number = 10): Promise<{ oid: string; message: string; author: string; date: Date }[]> {
    try {
      const commits = await git.log({
        fs,
        dir: this.repoPath,
        ref,
        depth,
      });
      
      return commits.map(c => ({
        oid: c.oid,
        message: c.commit.message,
        author: c.commit.author.name,
        date: new Date(c.commit.author.timestamp * 1000),
      }));
    } catch {
      return [];
    }
  }

  // Get diff between two commits (simplified)
  async getDiff(baseRef: string, headRef: string): Promise<{ file: string; status: string }[]> {
    try {
      // This is a simplified diff - just list changed files
      const baseOid = await git.resolveRef({ fs, dir: this.repoPath, ref: baseRef }).catch(() => baseRef);
      const headOid = await git.resolveRef({ fs, dir: this.repoPath, ref: headRef }).catch(() => headRef);
      
      // For now, return empty - full diff implementation is complex
      return [];
    } catch {
      return [];
    }
  }
}

// Export a helper to create storage instance
export function createGitNotesStorage(repoPath: string): GitNotesStorage {
  return new GitNotesStorage({ repoPath });
}
