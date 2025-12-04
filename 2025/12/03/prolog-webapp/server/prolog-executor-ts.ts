/**
 * TypeScript Prolog Executor
 * Replaces the SBCL-based executor with a pure TypeScript implementation
 */

import {
  PrologDB,
  parseClause,
  formatTerm,
  substBindings,
  variablesIn,
  tokenize,
  parseTokens,
  type Term,
  type Bindings,
} from './prolog-ts';

export interface QueryResult {
  success: boolean;
  solutions: Array<Record<string, string>>;
  error?: string;
}

/**
 * Global Prolog database instance
 */
const db = new PrologDB();

/**
 * Execute a Prolog query
 */
export async function executeQuery(query: string, facts: string[]): Promise<QueryResult> {
  try {
    // Clear and reload the database
    db.clear();
    
    // Parse and add all facts/rules
    for (const factOrRule of facts) {
      const trimmed = factOrRule.trim();
      if (trimmed.length === 0) {
        continue;
      }
      
      try {
        const clause = parseClause(trimmed);
        db.addClause(clause.head, clause.body);
      } catch (err) {
        console.error(`Failed to parse clause: ${trimmed}`, err);
        return {
          success: false,
          solutions: [],
          error: `Failed to parse clause: ${trimmed}`,
        };
      }
    }
    
    // Parse the query - handle both single goals and conjunctions
    const trimmedQuery = query.trim();
    let queryGoals: Term[];
    
    // Check if it's a conjunction (multiple goals)
    if (trimmedQuery.includes(') (')) {
      // Parse as multiple goals
      queryGoals = [];
      const tokens = tokenize(trimmedQuery);
      let i = 0;
      while (i < tokens.length) {
        const [goal, consumed] = parseTokens(tokens.slice(i));
        queryGoals.push(goal);
        i += consumed;
      }
    } else {
      // Single goal
      const queryClause = parseClause(trimmedQuery);
      queryGoals = queryClause.body.length > 0 
        ? queryClause.body 
        : [queryClause.head];
    }
    
    // Find all variables in the query
    const queryVars = new Set<string>();
    for (const goal of queryGoals) {
      for (const v of variablesIn(goal)) {
        queryVars.add(v.name);
      }
    }
    
    // Execute the query
    const solutions = db.proveAll(queryGoals);
    
    // Format solutions
    const formattedSolutions = solutions.map((bindings: Bindings) => {
      const solution: Record<string, string> = {};
      
      for (const varName of Array.from(queryVars)) {
        const varTerm: Term = { type: 'variable', name: varName };
        const value = substBindings(varTerm, bindings);
        solution[varName] = formatTerm(value);
      }
      
      return solution;
    });
    
    return {
      success: true,
      solutions: formattedSolutions,
    };
  } catch (error) {
    console.error('Query execution error:', error);
    return {
      success: false,
      solutions: [],
      error: error instanceof Error ? error.message : String(error),
    };
  }
}

/**
 * Assert a new fact
 */
export async function assertFact(fact: string): Promise<{ success: boolean; error?: string }> {
  try {
    const clause = parseClause(fact.trim());
    db.addClause(clause.head, clause.body);
    return { success: true };
  } catch (error) {
    return {
      success: false,
      error: error instanceof Error ? error.message : String(error),
    };
  }
}

/**
 * Clear all facts
 */
export async function clearFacts(): Promise<void> {
  db.clear();
}

/**
 * Get all current facts
 */
export function getFacts(): string[] {
  return db.getAllClauses().map(clause => {
    return formatTerm(clause.head);
  });
}

/**
 * Extract all atoms (constants) and track which predicates they appear in
 * Returns a map from atom name to set of predicate names
 */
export function extractAtoms(): Map<string, Set<string>> {
  const atomToPredicates = new Map<string, Set<string>>();

  const extractAtomsFromTerm = (term: Term, predicateName: string) => {
    if (term.type === 'atom') {
      if (!atomToPredicates.has(term.name)) {
        atomToPredicates.set(term.name, new Set());
      }
      atomToPredicates.get(term.name)!.add(predicateName);
    } else if (term.type === 'compound') {
      term.args.forEach(arg => extractAtomsFromTerm(arg, predicateName));
    }
  };

  for (const clause of db.getAllClauses()) {
    const head = clause.head;
    
    if (head.type === 'compound') {
      const predicateName = head.functor;
      
      // Extract atoms from head arguments
      head.args.forEach(arg => extractAtomsFromTerm(arg, predicateName));
      
      // Extract atoms from body goals
      for (const goal of clause.body) {
        if (goal.type === 'compound') {
          goal.args.forEach(arg => extractAtomsFromTerm(arg, goal.functor));
        }
      }
    }
  }

  return atomToPredicates;
}

/**
 * Extract predicate signatures from current facts and rules
 * Returns a list of unique predicate signatures like "(parent ?x ?y)" or "(color ?obj ?c)"
 */
export function getPredicateSignatures(): string[] {
  const signatureMap = new Map<string, string>();
  
  for (const clause of db.getAllClauses()) {
    const head = clause.head;
    
    if (head.type !== 'compound') {
      continue;
    }
    
    const functor = head.functor;
    const key = `${functor}/${head.args.length}`;
    
    // Only add if we haven't seen this predicate/arity combo, or if this is a rule (has body)
    // Rules have better variable names than facts
    if (!signatureMap.has(key) || clause.body.length > 0) {
      // Preserve original variable names from the head
      const signature = formatTerm(head);
      signatureMap.set(key, signature);
    }
  }
  
  return Array.from(signatureMap.values()).sort();
}
