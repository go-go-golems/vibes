// web/app.ts - Entry point wrapper for Prolog interpreter
// This file imports and re-exports the Prolog interpreter for Goja access

import {
  PrologDB,
  parseTerm,
  parseClause,
  formatTerm,
  unify,
  substBindings,
  variablesIn,
  type Term,
  type Bindings,
  type Clause
} from './prolog-ts';

// Export everything for Go access
export {
  PrologDB,
  parseTerm,
  parseClause,
  formatTerm,
  unify,
  substBindings,
  variablesIn
};

export type { Term, Bindings, Clause };

// Factory function for creating new database instances
export function createPrologDB(): PrologDB {
  return new PrologDB();
}

