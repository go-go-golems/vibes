/**
 * TypeScript Prolog Interpreter
 * Based on PAIP Chapter 11 (prolog1.lisp)
 * Supports facts and rules with proper unification and backtracking
 */

// ============================================================================
// Types
// ============================================================================

export type Term = Atom | Variable | Compound | ListTerm;

export interface Atom {
  type: 'atom';
  name: string;
}

export interface Variable {
  type: 'variable';
  name: string;
}

export interface Compound {
  type: 'compound';
  functor: string;
  args: Term[];
}

export interface ListTerm {
  type: 'list';
  elements: Term[];
}

export type Bindings = Map<string, Term>;

export interface Clause {
  head: Term;
  body: Term[]; // Empty array for facts, non-empty for rules
}

const FAIL: unique symbol = Symbol('fail');
type Fail = typeof FAIL;

// ============================================================================
// Prolog Database
// ============================================================================

export class PrologDB {
  private predicates: Map<string, Clause[]> = new Map();
  private varCounter = 0;

  clear(): void {
    this.predicates.clear();
    this.varCounter = 0;
  }

  /**
   * Add a clause (fact or rule) to the database
   * @param head The head of the clause
   * @param body The body goals (empty for facts)
   */
  addClause(head: Term, body: Term[] = []): void {
    const predicate = this.getPredicate(head);
    if (!predicate) {
      throw new Error(`Invalid clause head: ${formatTerm(head)}`);
    }

    const clauses = this.predicates.get(predicate) || [];
    clauses.push({ head, body });
    this.predicates.set(predicate, clauses);
  }

  /**
   * Get all clauses for a given predicate
   */
  getClauses(predicate: string): Clause[] {
    return this.predicates.get(predicate) || [];
  }

  /**
   * Get all clauses in the database
   */
  getAllClauses(): Clause[] {
    const all: Clause[] = [];
    for (const clauses of Array.from(this.predicates.values())) {
      all.push(...clauses);
    }
    return all;
  }

  /**
   * Extract predicate name from a term
   */
  private getPredicate(term: Term): string | null {
    if (term.type === 'compound') {
      return term.functor;
    } else if (term.type === 'atom') {
      return term.name;
    }
    return null;
  }

  /**
   * Prove a single goal with given bindings
   * Returns a list of possible binding solutions
   */
  prove(goal: Term, bindings: Bindings): Bindings[] {
    const predicate = this.getPredicate(goal);
    if (!predicate) {
      return [];
    }

    const clauses = this.getClauses(predicate);
    const solutions: Bindings[] = [];

    for (const clause of clauses) {
      // Rename variables in the clause to avoid conflicts
      const renamedClause = this.renameVariables(clause);
      
      // Try to unify the goal with the clause head
      const newBindings = unify(goal, renamedClause.head, bindings);
      
      if (newBindings !== FAIL) {
        // If unification succeeds, prove all goals in the body
        const bodySolutions = this.proveAll(renamedClause.body, newBindings);
        solutions.push(...bodySolutions);
      }
    }

    return solutions;
  }

  /**
   * Prove a conjunction of goals
   * Returns a list of possible binding solutions
   */
  proveAll(goals: Term[], bindings: Bindings = new Map()): Bindings[] {
    // Base case: no more goals, return the current bindings
    if (goals.length === 0) {
      return [bindings];
    }

    // Prove the first goal
    const firstGoal = goals[0];
    const restGoals = goals.slice(1);
    const firstSolutions = this.prove(firstGoal, bindings);

    // For each solution of the first goal, prove the rest
    const allSolutions: Bindings[] = [];
    for (const solution of firstSolutions) {
      const restSolutions = this.proveAll(restGoals, solution);
      allSolutions.push(...restSolutions);
    }

    return allSolutions;
  }

  /**
   * Rename all variables in a clause to avoid conflicts
   */
  private renameVariables(clause: Clause): Clause {
    const vars = this.variablesIn([clause.head, ...clause.body]);
    const renaming = new Map<string, string>();
    
    for (const varName of Array.from(vars)) {
      renaming.set(varName, `?_${this.varCounter++}`);
    }

    return {
      head: this.renameInTerm(clause.head, renaming),
      body: clause.body.map(goal => this.renameInTerm(goal, renaming)),
    };
  }

  private renameInTerm(term: Term, renaming: Map<string, string>): Term {
    if (term.type === 'variable') {
      const newName = renaming.get(term.name);
      return newName ? { type: 'variable', name: newName } : term;
    } else if (term.type === 'compound') {
      return {
        type: 'compound',
        functor: term.functor,
        args: term.args.map(arg => this.renameInTerm(arg, renaming)),
      };
    } else if (term.type === 'list') {
      return {
        type: 'list',
        elements: term.elements.map(el => this.renameInTerm(el, renaming)),
      };
    }
    return term;
  }

  private variablesIn(terms: Term[]): Set<string> {
    const vars = new Set<string>();
    
    const collect = (term: Term) => {
      if (term.type === 'variable') {
        vars.add(term.name);
      } else if (term.type === 'compound') {
        term.args.forEach(collect);
      } else if (term.type === 'list') {
        term.elements.forEach(collect);
      }
    };

    terms.forEach(collect);
    return vars;
  }
}

// ============================================================================
// Unification
// ============================================================================

/**
 * Unify two terms with given bindings
 * Returns new bindings if successful, FAIL otherwise
 */
export function unify(x: Term, y: Term, bindings: Bindings | Fail): Bindings | Fail {
  if (bindings === FAIL) {
    return FAIL;
  }

  // Dereference variables
  x = deref(x, bindings);
  y = deref(y, bindings);

  // Same term
  if (termsEqual(x, y)) {
    return bindings;
  }

  // Variable unification
  if (x.type === 'variable') {
    return bindVariable(x.name, y, bindings);
  }
  if (y.type === 'variable') {
    return bindVariable(y.name, x, bindings);
  }

  // Compound unification
  if (x.type === 'compound' && y.type === 'compound') {
    if (x.functor !== y.functor || x.args.length !== y.args.length) {
      return FAIL;
    }
    
    let newBindings: Bindings | Fail = bindings;
    for (let i = 0; i < x.args.length; i++) {
      newBindings = unify(x.args[i], y.args[i], newBindings);
      if (newBindings === FAIL) {
        return FAIL;
      }
    }
    return newBindings;
  }

  // List unification
  if (x.type === 'list' && y.type === 'list') {
    if (x.elements.length !== y.elements.length) {
      return FAIL;
    }
    
    let newBindings: Bindings | Fail = bindings;
    for (let i = 0; i < x.elements.length; i++) {
      newBindings = unify(x.elements[i], y.elements[i], newBindings);
      if (newBindings === FAIL) {
        return FAIL;
      }
    }
    return newBindings;
  }

  return FAIL;
}

function bindVariable(varName: string, value: Term, bindings: Bindings | Fail): Bindings | Fail {
  if (bindings === FAIL) {
    return FAIL;
  }
  
  // Occur check: prevent infinite structures
  if (occursIn(varName, value, bindings)) {
    return FAIL;
  }

  const newBindings = new Map(bindings);
  newBindings.set(varName, value);
  return newBindings;
}

function occursIn(varName: string, term: Term, bindings: Bindings | Fail): boolean {
  if (bindings === FAIL) {
    return false;
  }
  
  term = deref(term, bindings);
  
  if (term.type === 'variable') {
    return term.name === varName;
  } else if (term.type === 'compound') {
    return term.args.some(arg => occursIn(varName, arg, bindings));
  } else if (term.type === 'list') {
    return term.elements.some(el => occursIn(varName, el, bindings));
  }
  
  return false;
}

function deref(term: Term, bindings: Bindings | Fail): Term {
  if (bindings === FAIL) {
    return term;
  }
  
  if (term.type === 'variable') {
    const binding = bindings.get(term.name);
    if (binding) {
      return deref(binding, bindings);
    }
  }
  return term;
}

function termsEqual(x: Term, y: Term): boolean {
  if (x.type !== y.type) {
    return false;
  }

  if (x.type === 'atom' && y.type === 'atom') {
    return x.name === y.name;
  }

  if (x.type === 'variable' && y.type === 'variable') {
    return x.name === y.name;
  }

  if (x.type === 'compound' && y.type === 'compound') {
    return (
      x.functor === y.functor &&
      x.args.length === y.args.length &&
      x.args.every((arg, i) => termsEqual(arg, y.args[i]))
    );
  }

  if (x.type === 'list' && y.type === 'list') {
    return (
      x.elements.length === y.elements.length &&
      x.elements.every((el, i) => termsEqual(el, y.elements[i]))
    );
  }

  return false;
}

// ============================================================================
// Parsing
// ============================================================================

/**
 * Parse a Prolog term from a string
 * Supports:
 * - Atoms: foo, bar123
 * - Variables: ?x, ?Var
 * - Compounds: (foo bar baz)
 * - Lists: (a b c)
 * - Rules: (head) :- (goal1) (goal2)
 */
export function parseTerm(input: string): Term {
  const tokens = tokenize(input);
  const [term] = parseTokens(tokens);
  return term;
}

/**
 * Parse a clause (fact or rule)
 * Returns { head, body }
 */
export function parseClause(input: string): Clause {
  const tokens = tokenize(input);
  
  // Check for rule syntax: (head) :- (body1) (body2) ...
  const colonDashIndex = tokens.indexOf(':-');
  
  if (colonDashIndex === -1) {
    // It's a fact
    const [head] = parseTokens(tokens);
    return { head, body: [] };
  }

  // It's a rule
  const headTokens = tokens.slice(0, colonDashIndex);
  const bodyTokens = tokens.slice(colonDashIndex + 1);
  
  const [head] = parseTokens(headTokens);
  const body: Term[] = [];
  
  let i = 0;
  while (i < bodyTokens.length) {
    const [goal, consumed] = parseTokens(bodyTokens.slice(i));
    body.push(goal);
    i += consumed;
  }
  
  return { head, body };
}

export function tokenize(input: string): string[] {
  // Add spaces around parentheses and :- for easier tokenization
  const spaced = input
    .replace(/\(/g, ' ( ')
    .replace(/\)/g, ' ) ')
    .replace(/:-/g, ' :- ');
  
  return spaced.trim().split(/\s+/).filter(t => t.length > 0);
}

export function parseTokens(tokens: string[]): [Term, number] {
  if (tokens.length === 0) {
    throw new Error('Unexpected end of input');
  }

  const first = tokens[0];

  // Compound term or list
  if (first === '(') {
    tokens.shift(); // Remove '('
    
    if (tokens.length === 0) {
      throw new Error('Unexpected end of input after (');
    }

    // Check for empty list
    if (tokens[0] === ')') {
      tokens.shift();
      return [{ type: 'list', elements: [] }, 2];
    }

    // Parse elements
    const elements: Term[] = [];
    let consumed = 1; // For the opening '('

    while (tokens.length > 0 && tokens[0] !== ')') {
      const [term, c] = parseTokens(tokens);
      elements.push(term);
      consumed += c;
    }

    if (tokens.length === 0 || tokens[0] !== ')') {
      throw new Error('Missing closing )');
    }

    tokens.shift(); // Remove ')'
    consumed++;

    // Determine if it's a compound or list
    // If first element is an atom and there are more elements, it's a compound
    if (elements.length > 0 && elements[0].type === 'atom') {
      const functor = elements[0].name;
      const args = elements.slice(1);
      return [{ type: 'compound', functor, args }, consumed];
    }

    // Otherwise it's a list
    return [{ type: 'list', elements }, consumed];
  }

  // Variable
  if (first.startsWith('?')) {
    tokens.shift();
    return [{ type: 'variable', name: first }, 1];
  }

  // Atom or number
  tokens.shift();
  return [{ type: 'atom', name: first }, 1];
}

// ============================================================================
// Formatting
// ============================================================================

export function formatTerm(term: Term): string {
  if (term.type === 'atom') {
    return term.name;
  }

  if (term.type === 'variable') {
    return term.name;
  }

  if (term.type === 'compound') {
    const args = term.args.map(formatTerm).join(' ');
    return `(${term.functor}${args ? ' ' + args : ''})`;
  }

  if (term.type === 'list') {
    const elements = term.elements.map(formatTerm).join(' ');
    return `(${elements})`;
  }

  return '?';
}

// ============================================================================
// Utility Functions
// ============================================================================

export function substBindings(term: Term, bindings: Bindings): Term {
  term = deref(term, bindings);
  
  if (term.type === 'compound') {
    return {
      type: 'compound',
      functor: term.functor,
      args: term.args.map(arg => substBindings(arg, bindings)),
    };
  }

  if (term.type === 'list') {
    return {
      type: 'list',
      elements: term.elements.map(el => substBindings(el, bindings)),
    };
  }

  return term;
}

export function variablesIn(term: Term): Variable[] {
  const vars: Variable[] = [];
  const seen = new Set<string>();

  const collect = (t: Term) => {
    if (t.type === 'variable' && !seen.has(t.name)) {
      seen.add(t.name);
      vars.push(t);
    } else if (t.type === 'compound') {
      t.args.forEach(collect);
    } else if (t.type === 'list') {
      t.elements.forEach(collect);
    }
  };

  collect(term);
  return vars;
}
