import * as git from 'isomorphic-git';
import * as fs from 'fs';
import * as path from 'path';
import { demoSourceFiles, demoReviews, demoQuizzes, demoGuides, DEMO_REPO_NAME } from './demo-data';
import * as yaml from 'yaml';

// Demo repo location - use /tmp for persistence across requests but not deployments
const DEMO_REPO_BASE = process.env.DEMO_REPO_PATH || '/tmp/code-review-demo';
const DEMO_REPO_PATH = path.join(DEMO_REPO_BASE, DEMO_REPO_NAME);

export function getDemoRepoPath(): string {
  return DEMO_REPO_PATH;
}

export function isDemoRepoInitialized(): boolean {
  return fs.existsSync(path.join(DEMO_REPO_PATH, '.git'));
}

// Helper to write git notes using isomorphic-git
async function writeGitNote(dir: string, ref: string, commitOid: string, content: string): Promise<void> {
  // Git notes are stored as blob objects referenced by a special notes tree
  // The tree maps commit SHA -> note blob SHA
  
  // Create a blob with the note content
  const noteOid = await git.writeBlob({
    fs,
    dir,
    blob: Buffer.from(content, 'utf8'),
  });
  
  // Get or create the notes tree
  let parentCommit: string | undefined;
  let existingTree: { [path: string]: { mode: string; oid: string } } = {};
  
  try {
    // Try to resolve the notes ref
    const notesRef = await git.resolveRef({ fs, dir, ref });
    parentCommit = notesRef;
    
    // Read existing tree
    const { tree } = await git.readTree({ fs, dir, oid: notesRef });
    for (const entry of tree) {
      existingTree[entry.path] = { mode: entry.mode, oid: entry.oid };
    }
  } catch {
    // Notes ref doesn't exist yet
  }
  
  // Add/update the note entry
  // Git notes use the first 2 chars as directory, rest as filename
  const noteDir = commitOid.substring(0, 2);
  const noteFile = commitOid.substring(2);
  const notePath = `${noteDir}/${noteFile}`;
  
  // Build tree entries
  const treeEntries: { mode: string; path: string; oid: string; type: 'blob' | 'tree' }[] = [];
  
  // Group entries by directory
  const dirs: { [dir: string]: { mode: string; path: string; oid: string }[] } = {};
  
  // Add existing entries
  for (const [entryPath, entry] of Object.entries(existingTree)) {
    if (entryPath.includes('/')) {
      const [d, f] = entryPath.split('/');
      if (!dirs[d]) dirs[d] = [];
      dirs[d].push({ mode: entry.mode, path: f, oid: entry.oid });
    } else {
      treeEntries.push({ mode: entry.mode, path: entryPath, oid: entry.oid, type: 'blob' });
    }
  }
  
  // Add/update the new note
  if (!dirs[noteDir]) dirs[noteDir] = [];
  // Remove existing entry for this note if any
  dirs[noteDir] = dirs[noteDir].filter(e => e.path !== noteFile);
  dirs[noteDir].push({ mode: '100644', path: noteFile, oid: noteOid });
  
  // Create subtrees for each directory
  for (const [dirName, entries] of Object.entries(dirs)) {
    const subTreeOid = await git.writeTree({
      fs,
      dir,
      tree: entries.map(e => ({ mode: e.mode, path: e.path, oid: e.oid, type: 'blob' as const })),
    });
    treeEntries.push({ mode: '040000', path: dirName, oid: subTreeOid, type: 'tree' });
  }
  
  // Create the notes tree
  const treeOid = await git.writeTree({
    fs,
    dir,
    tree: treeEntries,
  });
  
  // Create a commit for the notes
  const commitOidNew = await git.commit({
    fs,
    dir,
    ref,
    tree: treeOid,
    parent: parentCommit ? [parentCommit] : [],
    message: `Notes added by isomorphic-git`,
    author: {
      name: 'Demo User',
      email: 'demo@example.com',
    },
  });
}

export async function initializeDemoRepo(): Promise<{ success: boolean; path: string; message: string }> {
  try {
    // Check if already initialized
    if (isDemoRepoInitialized()) {
      return { success: true, path: DEMO_REPO_PATH, message: 'Demo repository already initialized' };
    }

    // Create base directory
    fs.mkdirSync(DEMO_REPO_BASE, { recursive: true });
    fs.mkdirSync(DEMO_REPO_PATH, { recursive: true });

    // Initialize git repo using isomorphic-git
    await git.init({
      fs,
      dir: DEMO_REPO_PATH,
      defaultBranch: 'main',
    });

    // Create all source files
    for (const [filePath, content] of Object.entries(demoSourceFiles)) {
      const fullPath = path.join(DEMO_REPO_PATH, filePath);
      const dir = path.dirname(fullPath);
      fs.mkdirSync(dir, { recursive: true });
      fs.writeFileSync(fullPath, content);
    }

    // Stage all files
    for (const filePath of Object.keys(demoSourceFiles)) {
      await git.add({
        fs,
        dir: DEMO_REPO_PATH,
        filepath: filePath,
      });
    }

    // Initial commit
    const commitOid = await git.commit({
      fs,
      dir: DEMO_REPO_PATH,
      message: 'Initial commit: Demo codebase',
      author: {
        name: 'Demo User',
        email: 'demo@example.com',
      },
    });

    // Create feature branches
    await git.branch({
      fs,
      dir: DEMO_REPO_PATH,
      ref: 'feature/auth',
    });
    await git.branch({
      fs,
      dir: DEMO_REPO_PATH,
      ref: 'feature/database',
    });
    await git.branch({
      fs,
      dir: DEMO_REPO_PATH,
      ref: 'feature/error-handling',
    });

    // Store ALL reviews as a single YAML array in one note
    const reviewsYaml = yaml.stringify({ reviews: demoReviews });
    await writeGitNote(DEMO_REPO_PATH, 'refs/notes/reviews', commitOid, reviewsYaml);

    // Store ALL quizzes as a single YAML array in one note
    const quizzesYaml = yaml.stringify({ quizzes: demoQuizzes });
    await writeGitNote(DEMO_REPO_PATH, 'refs/notes/quizzes', commitOid, quizzesYaml);

    // Store ALL guides as a single YAML array in one note
    const guidesYaml = yaml.stringify({ guides: demoGuides });
    await writeGitNote(DEMO_REPO_PATH, 'refs/notes/guides', commitOid, guidesYaml);

    return { success: true, path: DEMO_REPO_PATH, message: 'Demo repository initialized successfully' };
  } catch (error) {
    console.error('Failed to initialize demo repo:', error);
    return { success: false, path: DEMO_REPO_PATH, message: `Failed to initialize: ${error}` };
  }
}

// Auto-initialize on import if not already done
let initPromise: Promise<{ success: boolean; path: string; message: string }> | null = null;

export function ensureDemoRepo(): Promise<{ success: boolean; path: string; message: string }> {
  if (!initPromise) {
    initPromise = initializeDemoRepo();
  }
  return initPromise;
}
