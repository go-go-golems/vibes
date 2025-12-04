import { spawn } from 'child_process';
import path from 'path';
import { fileURLToPath } from 'url';
import { dirname } from 'path';
import fs from 'fs/promises';
import os from 'os';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

export interface PrologQueryResult {
  success: boolean;
  solutions: Array<Record<string, string>>;
  error?: string;
}

/**
 * Execute a Prolog query using SBCL
 */
export async function executePrologQuery(
  facts: string[],
  query: string
): Promise<PrologQueryResult> {
  const lispCode = generateLispCode(facts, query);
  
  // Write to a temporary file
  const tmpFile = path.join(os.tmpdir(), `prolog-query-${Date.now()}-${Math.random().toString(36).substr(2, 9)}.lisp`);
  
  try {
    await fs.writeFile(tmpFile, lispCode, 'utf-8');
    
    return new Promise((resolve) => {
      const sbcl = spawn('sbcl', ['--script', tmpFile], {
        cwd: __dirname,
      });

      let stdout = '';
      let stderr = '';

      sbcl.stdout.on('data', (data) => {
        stdout += data.toString();
      });

      sbcl.stderr.on('data', (data) => {
        stderr += data.toString();
      });

      sbcl.on('close', async (code) => {
        // Clean up temp file
        try {
          await fs.unlink(tmpFile);
        } catch (e) {
          // Ignore cleanup errors
        }
        
        if (code !== 0 && stderr) {
          resolve({
            success: false,
            solutions: [],
            error: stderr,
          });
          return;
        }

        try {
          const result = parsePrologOutput(stdout);
          resolve(result);
        } catch (error) {
          resolve({
            success: false,
            solutions: [],
            error: error instanceof Error ? error.message : 'Unknown error',
          });
        }
      });
    });
  } catch (error) {
    // Clean up temp file on error
    try {
      await fs.unlink(tmpFile);
    } catch (e) {
      // Ignore cleanup errors
    }
    
    return {
      success: false,
      solutions: [],
      error: error instanceof Error ? error.message : 'Unknown error',
    };
  }
}

/**
 * Generate Lisp code that loads the Prolog interpreter and executes a query
 */
function generateLispCode(facts: string[], query: string): string {
  const prologPath = path.join(__dirname, 'prolog-standalone.lisp');
  
  let code = `(load "${prologPath}")\n\n`;
  
  // Add facts
  for (const fact of facts) {
    code += `(<- ${fact})\n`;
  }
  
  // Execute query and capture output
  code += `\n(defvar *query-results* nil)\n`;
  code += `(defvar *query-vars* nil)\n\n`;
  
  code += `(let* ((goals (read-from-string "(${query})"))
       (vars (variables-in goals))
       (solutions (prove-all goals no-bindings)))
  (setf *query-vars* vars)
  (setf *query-results* solutions)
  (format t "~%PROLOG-RESULTS-START~%")
  (if (null solutions)
      (format t "NO-SOLUTIONS~%")
      (dolist (solution solutions)
        (format t "SOLUTION-START~%")
        (dolist (var vars)
          (let ((val (subst-bindings solution var)))
            (format t "~A=~S~%" var val)))
        (format t "SOLUTION-END~%")))
  (format t "PROLOG-RESULTS-END~%"))\n`;
  
  return code;
}

/**
 * Parse the output from the Prolog interpreter
 */
function parsePrologOutput(output: string): PrologQueryResult {
  const startMarker = 'PROLOG-RESULTS-START';
  const endMarker = 'PROLOG-RESULTS-END';
  
  const startIdx = output.indexOf(startMarker);
  const endIdx = output.indexOf(endMarker);
  
  if (startIdx === -1 || endIdx === -1) {
    return {
      success: false,
      solutions: [],
      error: 'Could not parse Prolog output',
    };
  }
  
  const resultsSection = output.substring(startIdx + startMarker.length, endIdx).trim();
  
  if (resultsSection.startsWith('NO-SOLUTIONS')) {
    return {
      success: true,
      solutions: [],
    };
  }
  
  const solutions: Array<Record<string, string>> = [];
  const solutionBlocks = resultsSection.split('SOLUTION-START').slice(1);
  
  for (const block of solutionBlocks) {
    const endIdx = block.indexOf('SOLUTION-END');
    if (endIdx === -1) continue;
    
    const solutionText = block.substring(0, endIdx).trim();
    const lines = solutionText.split('\n').filter(l => l.trim());
    
    const solution: Record<string, string> = {};
    for (const line of lines) {
      const match = line.match(/^(\?[A-Z0-9-]+)=(.+)$/);
      if (match) {
        solution[match[1]] = match[2];
      }
    }
    
    if (Object.keys(solution).length > 0) {
      solutions.push(solution);
    }
  }
  
  return {
    success: true,
    solutions,
  };
}

/**
 * Clear the Prolog database
 */
export async function clearPrologDatabase(): Promise<void> {
  const prologPath = path.join(__dirname, 'prolog-standalone.lisp');
  const lispCode = `(load "${prologPath}")\n(clear-db)\n(format t "Database cleared")`;
  
  const tmpFile = path.join(os.tmpdir(), `prolog-clear-${Date.now()}.lisp`);
  
  try {
    await fs.writeFile(tmpFile, lispCode, 'utf-8');
    
    return new Promise((resolve) => {
      const sbcl = spawn('sbcl', ['--script', tmpFile]);
      
      sbcl.on('close', async () => {
        try {
          await fs.unlink(tmpFile);
        } catch (e) {
          // Ignore cleanup errors
        }
        resolve();
      });
    });
  } catch (error) {
    try {
      await fs.unlink(tmpFile);
    } catch (e) {
      // Ignore cleanup errors
    }
    throw error;
  }
}
