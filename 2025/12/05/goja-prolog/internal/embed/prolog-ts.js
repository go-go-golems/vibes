"use strict";
var __defProp = Object.defineProperty;
var __getOwnPropDesc = Object.getOwnPropertyDescriptor;
var __getOwnPropNames = Object.getOwnPropertyNames;
var __hasOwnProp = Object.prototype.hasOwnProperty;
var __export = (target, all) => {
  for (var name in all)
    __defProp(target, name, { get: all[name], enumerable: true });
};
var __copyProps = (to, from, except, desc) => {
  if (from && typeof from === "object" || typeof from === "function") {
    for (let key of __getOwnPropNames(from))
      if (!__hasOwnProp.call(to, key) && key !== except)
        __defProp(to, key, { get: () => from[key], enumerable: !(desc = __getOwnPropDesc(from, key)) || desc.enumerable });
  }
  return to;
};
var __toCommonJS = (mod) => __copyProps(__defProp({}, "__esModule", { value: true }), mod);

// app.ts
var app_exports = {};
__export(app_exports, {
  PrologDB: () => PrologDB,
  createPrologDB: () => createPrologDB,
  formatTerm: () => formatTerm,
  parseClause: () => parseClause,
  parseTerm: () => parseTerm,
  substBindings: () => substBindings,
  unify: () => unify,
  variablesIn: () => variablesIn
});
module.exports = __toCommonJS(app_exports);

// prolog-ts.ts
var FAIL = Symbol("fail");
var PrologDB = class {
  constructor() {
    this.predicates = /* @__PURE__ */ new Map();
    this.varCounter = 0;
  }
  clear() {
    this.predicates.clear();
    this.varCounter = 0;
  }
  /**
   * Add a clause (fact or rule) to the database
   * @param head The head of the clause
   * @param body The body goals (empty for facts)
   */
  addClause(head, body = []) {
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
  getClauses(predicate) {
    return this.predicates.get(predicate) || [];
  }
  /**
   * Get all clauses in the database
   */
  getAllClauses() {
    const all = [];
    for (const clauses of Array.from(this.predicates.values())) {
      all.push(...clauses);
    }
    return all;
  }
  /**
   * Extract predicate name from a term
   */
  getPredicate(term) {
    if (term.type === "compound") {
      return term.functor;
    } else if (term.type === "atom") {
      return term.name;
    }
    return null;
  }
  /**
   * Prove a single goal with given bindings
   * Returns a list of possible binding solutions
   */
  prove(goal, bindings) {
    const predicate = this.getPredicate(goal);
    if (!predicate) {
      return [];
    }
    const clauses = this.getClauses(predicate);
    const solutions = [];
    for (const clause of clauses) {
      const renamedClause = this.renameVariables(clause);
      const newBindings = unify(goal, renamedClause.head, bindings);
      if (newBindings !== FAIL) {
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
  proveAll(goals, bindings = /* @__PURE__ */ new Map()) {
    if (goals.length === 0) {
      return [bindings];
    }
    const firstGoal = goals[0];
    const restGoals = goals.slice(1);
    const firstSolutions = this.prove(firstGoal, bindings);
    const allSolutions = [];
    for (const solution of firstSolutions) {
      const restSolutions = this.proveAll(restGoals, solution);
      allSolutions.push(...restSolutions);
    }
    return allSolutions;
  }
  /**
   * Rename all variables in a clause to avoid conflicts
   */
  renameVariables(clause) {
    const vars = this.variablesIn([clause.head, ...clause.body]);
    const renaming = /* @__PURE__ */ new Map();
    for (const varName of Array.from(vars)) {
      renaming.set(varName, `?_${this.varCounter++}`);
    }
    return {
      head: this.renameInTerm(clause.head, renaming),
      body: clause.body.map((goal) => this.renameInTerm(goal, renaming))
    };
  }
  renameInTerm(term, renaming) {
    if (term.type === "variable") {
      const newName = renaming.get(term.name);
      return newName ? { type: "variable", name: newName } : term;
    } else if (term.type === "compound") {
      return {
        type: "compound",
        functor: term.functor,
        args: term.args.map((arg) => this.renameInTerm(arg, renaming))
      };
    } else if (term.type === "list") {
      return {
        type: "list",
        elements: term.elements.map((el) => this.renameInTerm(el, renaming))
      };
    }
    return term;
  }
  variablesIn(terms) {
    const vars = /* @__PURE__ */ new Set();
    const collect = (term) => {
      if (term.type === "variable") {
        vars.add(term.name);
      } else if (term.type === "compound") {
        term.args.forEach(collect);
      } else if (term.type === "list") {
        term.elements.forEach(collect);
      }
    };
    terms.forEach(collect);
    return vars;
  }
};
function unify(x, y, bindings) {
  if (bindings === FAIL) {
    return FAIL;
  }
  x = deref(x, bindings);
  y = deref(y, bindings);
  if (termsEqual(x, y)) {
    return bindings;
  }
  if (x.type === "variable") {
    return bindVariable(x.name, y, bindings);
  }
  if (y.type === "variable") {
    return bindVariable(y.name, x, bindings);
  }
  if (x.type === "compound" && y.type === "compound") {
    if (x.functor !== y.functor || x.args.length !== y.args.length) {
      return FAIL;
    }
    let newBindings = bindings;
    for (let i = 0; i < x.args.length; i++) {
      newBindings = unify(x.args[i], y.args[i], newBindings);
      if (newBindings === FAIL) {
        return FAIL;
      }
    }
    return newBindings;
  }
  if (x.type === "list" && y.type === "list") {
    if (x.elements.length !== y.elements.length) {
      return FAIL;
    }
    let newBindings = bindings;
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
function bindVariable(varName, value, bindings) {
  if (bindings === FAIL) {
    return FAIL;
  }
  if (occursIn(varName, value, bindings)) {
    return FAIL;
  }
  const newBindings = new Map(bindings);
  newBindings.set(varName, value);
  return newBindings;
}
function occursIn(varName, term, bindings) {
  if (bindings === FAIL) {
    return false;
  }
  term = deref(term, bindings);
  if (term.type === "variable") {
    return term.name === varName;
  } else if (term.type === "compound") {
    return term.args.some((arg) => occursIn(varName, arg, bindings));
  } else if (term.type === "list") {
    return term.elements.some((el) => occursIn(varName, el, bindings));
  }
  return false;
}
function deref(term, bindings) {
  if (bindings === FAIL) {
    return term;
  }
  if (term.type === "variable") {
    const binding = bindings.get(term.name);
    if (binding) {
      return deref(binding, bindings);
    }
  }
  return term;
}
function termsEqual(x, y) {
  if (x.type !== y.type) {
    return false;
  }
  if (x.type === "atom" && y.type === "atom") {
    return x.name === y.name;
  }
  if (x.type === "variable" && y.type === "variable") {
    return x.name === y.name;
  }
  if (x.type === "compound" && y.type === "compound") {
    return x.functor === y.functor && x.args.length === y.args.length && x.args.every((arg, i) => termsEqual(arg, y.args[i]));
  }
  if (x.type === "list" && y.type === "list") {
    return x.elements.length === y.elements.length && x.elements.every((el, i) => termsEqual(el, y.elements[i]));
  }
  return false;
}
function parseTerm(input) {
  const tokens = tokenize(input);
  const [term] = parseTokens(tokens);
  return term;
}
function parseClause(input) {
  const tokens = tokenize(input);
  const colonDashIndex = tokens.indexOf(":-");
  if (colonDashIndex === -1) {
    const [head2] = parseTokens(tokens);
    return { head: head2, body: [] };
  }
  const headTokens = tokens.slice(0, colonDashIndex);
  const bodyTokens = tokens.slice(colonDashIndex + 1);
  const [head] = parseTokens(headTokens);
  const body = [];
  let i = 0;
  while (i < bodyTokens.length) {
    const [goal, consumed] = parseTokens(bodyTokens.slice(i));
    body.push(goal);
    i += consumed;
  }
  return { head, body };
}
function tokenize(input) {
  const spaced = input.replace(/\(/g, " ( ").replace(/\)/g, " ) ").replace(/:-/g, " :- ");
  return spaced.trim().split(/\s+/).filter((t) => t.length > 0);
}
function parseTokens(tokens) {
  if (tokens.length === 0) {
    throw new Error("Unexpected end of input");
  }
  const first = tokens[0];
  if (first === "(") {
    tokens.shift();
    if (tokens.length === 0) {
      throw new Error("Unexpected end of input after (");
    }
    if (tokens[0] === ")") {
      tokens.shift();
      return [{ type: "list", elements: [] }, 2];
    }
    const elements = [];
    let consumed = 1;
    while (tokens.length > 0 && tokens[0] !== ")") {
      const [term, c] = parseTokens(tokens);
      elements.push(term);
      consumed += c;
    }
    if (tokens.length === 0 || tokens[0] !== ")") {
      throw new Error("Missing closing )");
    }
    tokens.shift();
    consumed++;
    if (elements.length > 0 && elements[0].type === "atom") {
      const functor = elements[0].name;
      const args = elements.slice(1);
      return [{ type: "compound", functor, args }, consumed];
    }
    return [{ type: "list", elements }, consumed];
  }
  if (first.startsWith("?")) {
    tokens.shift();
    return [{ type: "variable", name: first }, 1];
  }
  tokens.shift();
  return [{ type: "atom", name: first }, 1];
}
function formatTerm(term) {
  if (term.type === "atom") {
    return term.name;
  }
  if (term.type === "variable") {
    return term.name;
  }
  if (term.type === "compound") {
    const args = term.args.map(formatTerm).join(" ");
    return `(${term.functor}${args ? " " + args : ""})`;
  }
  if (term.type === "list") {
    const elements = term.elements.map(formatTerm).join(" ");
    return `(${elements})`;
  }
  return "?";
}
function substBindings(term, bindings) {
  term = deref(term, bindings);
  if (term.type === "compound") {
    return {
      type: "compound",
      functor: term.functor,
      args: term.args.map((arg) => substBindings(arg, bindings))
    };
  }
  if (term.type === "list") {
    return {
      type: "list",
      elements: term.elements.map((el) => substBindings(el, bindings))
    };
  }
  return term;
}
function variablesIn(term) {
  const vars = [];
  const seen = /* @__PURE__ */ new Set();
  const collect = (t) => {
    if (t.type === "variable" && !seen.has(t.name)) {
      seen.add(t.name);
      vars.push(t);
    } else if (t.type === "compound") {
      t.args.forEach(collect);
    } else if (t.type === "list") {
      t.elements.forEach(collect);
    }
  };
  collect(term);
  return vars;
}

// app.ts
function createPrologDB() {
  return new PrologDB();
}
// Annotate the CommonJS export names for ESM import in node:
0 && (module.exports = {
  PrologDB,
  createPrologDB,
  formatTerm,
  parseClause,
  parseTerm,
  substBindings,
  unify,
  variablesIn
});
//# sourceMappingURL=data:application/json;base64,ewogICJ2ZXJzaW9uIjogMywKICAic291cmNlcyI6IFsiLi4vd2ViL2FwcC50cyIsICIuLi93ZWIvcHJvbG9nLXRzLnRzIl0sCiAgInNvdXJjZXNDb250ZW50IjogWyIvLyB3ZWIvYXBwLnRzIC0gRW50cnkgcG9pbnQgd3JhcHBlciBmb3IgUHJvbG9nIGludGVycHJldGVyXG4vLyBUaGlzIGZpbGUgaW1wb3J0cyBhbmQgcmUtZXhwb3J0cyB0aGUgUHJvbG9nIGludGVycHJldGVyIGZvciBHb2phIGFjY2Vzc1xuXG5pbXBvcnQge1xuICBQcm9sb2dEQixcbiAgcGFyc2VUZXJtLFxuICBwYXJzZUNsYXVzZSxcbiAgZm9ybWF0VGVybSxcbiAgdW5pZnksXG4gIHN1YnN0QmluZGluZ3MsXG4gIHZhcmlhYmxlc0luLFxuICB0eXBlIFRlcm0sXG4gIHR5cGUgQmluZGluZ3MsXG4gIHR5cGUgQ2xhdXNlXG59IGZyb20gJy4vcHJvbG9nLXRzJztcblxuLy8gRXhwb3J0IGV2ZXJ5dGhpbmcgZm9yIEdvIGFjY2Vzc1xuZXhwb3J0IHtcbiAgUHJvbG9nREIsXG4gIHBhcnNlVGVybSxcbiAgcGFyc2VDbGF1c2UsXG4gIGZvcm1hdFRlcm0sXG4gIHVuaWZ5LFxuICBzdWJzdEJpbmRpbmdzLFxuICB2YXJpYWJsZXNJblxufTtcblxuZXhwb3J0IHR5cGUgeyBUZXJtLCBCaW5kaW5ncywgQ2xhdXNlIH07XG5cbi8vIEZhY3RvcnkgZnVuY3Rpb24gZm9yIGNyZWF0aW5nIG5ldyBkYXRhYmFzZSBpbnN0YW5jZXNcbmV4cG9ydCBmdW5jdGlvbiBjcmVhdGVQcm9sb2dEQigpOiBQcm9sb2dEQiB7XG4gIHJldHVybiBuZXcgUHJvbG9nREIoKTtcbn1cblxuIiwgIi8qKlxuICogVHlwZVNjcmlwdCBQcm9sb2cgSW50ZXJwcmV0ZXJcbiAqIEJhc2VkIG9uIFBBSVAgQ2hhcHRlciAxMSAocHJvbG9nMS5saXNwKVxuICogU3VwcG9ydHMgZmFjdHMgYW5kIHJ1bGVzIHdpdGggcHJvcGVyIHVuaWZpY2F0aW9uIGFuZCBiYWNrdHJhY2tpbmdcbiAqL1xuXG4vLyA9PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09XG4vLyBUeXBlc1xuLy8gPT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PVxuXG5leHBvcnQgdHlwZSBUZXJtID0gQXRvbSB8IFZhcmlhYmxlIHwgQ29tcG91bmQgfCBMaXN0VGVybTtcblxuZXhwb3J0IGludGVyZmFjZSBBdG9tIHtcbiAgdHlwZTogJ2F0b20nO1xuICBuYW1lOiBzdHJpbmc7XG59XG5cbmV4cG9ydCBpbnRlcmZhY2UgVmFyaWFibGUge1xuICB0eXBlOiAndmFyaWFibGUnO1xuICBuYW1lOiBzdHJpbmc7XG59XG5cbmV4cG9ydCBpbnRlcmZhY2UgQ29tcG91bmQge1xuICB0eXBlOiAnY29tcG91bmQnO1xuICBmdW5jdG9yOiBzdHJpbmc7XG4gIGFyZ3M6IFRlcm1bXTtcbn1cblxuZXhwb3J0IGludGVyZmFjZSBMaXN0VGVybSB7XG4gIHR5cGU6ICdsaXN0JztcbiAgZWxlbWVudHM6IFRlcm1bXTtcbn1cblxuZXhwb3J0IHR5cGUgQmluZGluZ3MgPSBNYXA8c3RyaW5nLCBUZXJtPjtcblxuZXhwb3J0IGludGVyZmFjZSBDbGF1c2Uge1xuICBoZWFkOiBUZXJtO1xuICBib2R5OiBUZXJtW107IC8vIEVtcHR5IGFycmF5IGZvciBmYWN0cywgbm9uLWVtcHR5IGZvciBydWxlc1xufVxuXG5jb25zdCBGQUlMOiB1bmlxdWUgc3ltYm9sID0gU3ltYm9sKCdmYWlsJyk7XG50eXBlIEZhaWwgPSB0eXBlb2YgRkFJTDtcblxuLy8gPT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PVxuLy8gUHJvbG9nIERhdGFiYXNlXG4vLyA9PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09XG5cbmV4cG9ydCBjbGFzcyBQcm9sb2dEQiB7XG4gIHByaXZhdGUgcHJlZGljYXRlczogTWFwPHN0cmluZywgQ2xhdXNlW10+ID0gbmV3IE1hcCgpO1xuICBwcml2YXRlIHZhckNvdW50ZXIgPSAwO1xuXG4gIGNsZWFyKCk6IHZvaWQge1xuICAgIHRoaXMucHJlZGljYXRlcy5jbGVhcigpO1xuICAgIHRoaXMudmFyQ291bnRlciA9IDA7XG4gIH1cblxuICAvKipcbiAgICogQWRkIGEgY2xhdXNlIChmYWN0IG9yIHJ1bGUpIHRvIHRoZSBkYXRhYmFzZVxuICAgKiBAcGFyYW0gaGVhZCBUaGUgaGVhZCBvZiB0aGUgY2xhdXNlXG4gICAqIEBwYXJhbSBib2R5IFRoZSBib2R5IGdvYWxzIChlbXB0eSBmb3IgZmFjdHMpXG4gICAqL1xuICBhZGRDbGF1c2UoaGVhZDogVGVybSwgYm9keTogVGVybVtdID0gW10pOiB2b2lkIHtcbiAgICBjb25zdCBwcmVkaWNhdGUgPSB0aGlzLmdldFByZWRpY2F0ZShoZWFkKTtcbiAgICBpZiAoIXByZWRpY2F0ZSkge1xuICAgICAgdGhyb3cgbmV3IEVycm9yKGBJbnZhbGlkIGNsYXVzZSBoZWFkOiAke2Zvcm1hdFRlcm0oaGVhZCl9YCk7XG4gICAgfVxuXG4gICAgY29uc3QgY2xhdXNlcyA9IHRoaXMucHJlZGljYXRlcy5nZXQocHJlZGljYXRlKSB8fCBbXTtcbiAgICBjbGF1c2VzLnB1c2goeyBoZWFkLCBib2R5IH0pO1xuICAgIHRoaXMucHJlZGljYXRlcy5zZXQocHJlZGljYXRlLCBjbGF1c2VzKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBHZXQgYWxsIGNsYXVzZXMgZm9yIGEgZ2l2ZW4gcHJlZGljYXRlXG4gICAqL1xuICBnZXRDbGF1c2VzKHByZWRpY2F0ZTogc3RyaW5nKTogQ2xhdXNlW10ge1xuICAgIHJldHVybiB0aGlzLnByZWRpY2F0ZXMuZ2V0KHByZWRpY2F0ZSkgfHwgW107XG4gIH1cblxuICAvKipcbiAgICogR2V0IGFsbCBjbGF1c2VzIGluIHRoZSBkYXRhYmFzZVxuICAgKi9cbiAgZ2V0QWxsQ2xhdXNlcygpOiBDbGF1c2VbXSB7XG4gICAgY29uc3QgYWxsOiBDbGF1c2VbXSA9IFtdO1xuICAgIGZvciAoY29uc3QgY2xhdXNlcyBvZiBBcnJheS5mcm9tKHRoaXMucHJlZGljYXRlcy52YWx1ZXMoKSkpIHtcbiAgICAgIGFsbC5wdXNoKC4uLmNsYXVzZXMpO1xuICAgIH1cbiAgICByZXR1cm4gYWxsO1xuICB9XG5cbiAgLyoqXG4gICAqIEV4dHJhY3QgcHJlZGljYXRlIG5hbWUgZnJvbSBhIHRlcm1cbiAgICovXG4gIHByaXZhdGUgZ2V0UHJlZGljYXRlKHRlcm06IFRlcm0pOiBzdHJpbmcgfCBudWxsIHtcbiAgICBpZiAodGVybS50eXBlID09PSAnY29tcG91bmQnKSB7XG4gICAgICByZXR1cm4gdGVybS5mdW5jdG9yO1xuICAgIH0gZWxzZSBpZiAodGVybS50eXBlID09PSAnYXRvbScpIHtcbiAgICAgIHJldHVybiB0ZXJtLm5hbWU7XG4gICAgfVxuICAgIHJldHVybiBudWxsO1xuICB9XG5cbiAgLyoqXG4gICAqIFByb3ZlIGEgc2luZ2xlIGdvYWwgd2l0aCBnaXZlbiBiaW5kaW5nc1xuICAgKiBSZXR1cm5zIGEgbGlzdCBvZiBwb3NzaWJsZSBiaW5kaW5nIHNvbHV0aW9uc1xuICAgKi9cbiAgcHJvdmUoZ29hbDogVGVybSwgYmluZGluZ3M6IEJpbmRpbmdzKTogQmluZGluZ3NbXSB7XG4gICAgY29uc3QgcHJlZGljYXRlID0gdGhpcy5nZXRQcmVkaWNhdGUoZ29hbCk7XG4gICAgaWYgKCFwcmVkaWNhdGUpIHtcbiAgICAgIHJldHVybiBbXTtcbiAgICB9XG5cbiAgICBjb25zdCBjbGF1c2VzID0gdGhpcy5nZXRDbGF1c2VzKHByZWRpY2F0ZSk7XG4gICAgY29uc3Qgc29sdXRpb25zOiBCaW5kaW5nc1tdID0gW107XG5cbiAgICBmb3IgKGNvbnN0IGNsYXVzZSBvZiBjbGF1c2VzKSB7XG4gICAgICAvLyBSZW5hbWUgdmFyaWFibGVzIGluIHRoZSBjbGF1c2UgdG8gYXZvaWQgY29uZmxpY3RzXG4gICAgICBjb25zdCByZW5hbWVkQ2xhdXNlID0gdGhpcy5yZW5hbWVWYXJpYWJsZXMoY2xhdXNlKTtcbiAgICAgIFxuICAgICAgLy8gVHJ5IHRvIHVuaWZ5IHRoZSBnb2FsIHdpdGggdGhlIGNsYXVzZSBoZWFkXG4gICAgICBjb25zdCBuZXdCaW5kaW5ncyA9IHVuaWZ5KGdvYWwsIHJlbmFtZWRDbGF1c2UuaGVhZCwgYmluZGluZ3MpO1xuICAgICAgXG4gICAgICBpZiAobmV3QmluZGluZ3MgIT09IEZBSUwpIHtcbiAgICAgICAgLy8gSWYgdW5pZmljYXRpb24gc3VjY2VlZHMsIHByb3ZlIGFsbCBnb2FscyBpbiB0aGUgYm9keVxuICAgICAgICBjb25zdCBib2R5U29sdXRpb25zID0gdGhpcy5wcm92ZUFsbChyZW5hbWVkQ2xhdXNlLmJvZHksIG5ld0JpbmRpbmdzKTtcbiAgICAgICAgc29sdXRpb25zLnB1c2goLi4uYm9keVNvbHV0aW9ucyk7XG4gICAgICB9XG4gICAgfVxuXG4gICAgcmV0dXJuIHNvbHV0aW9ucztcbiAgfVxuXG4gIC8qKlxuICAgKiBQcm92ZSBhIGNvbmp1bmN0aW9uIG9mIGdvYWxzXG4gICAqIFJldHVybnMgYSBsaXN0IG9mIHBvc3NpYmxlIGJpbmRpbmcgc29sdXRpb25zXG4gICAqL1xuICBwcm92ZUFsbChnb2FsczogVGVybVtdLCBiaW5kaW5nczogQmluZGluZ3MgPSBuZXcgTWFwKCkpOiBCaW5kaW5nc1tdIHtcbiAgICAvLyBCYXNlIGNhc2U6IG5vIG1vcmUgZ29hbHMsIHJldHVybiB0aGUgY3VycmVudCBiaW5kaW5nc1xuICAgIGlmIChnb2Fscy5sZW5ndGggPT09IDApIHtcbiAgICAgIHJldHVybiBbYmluZGluZ3NdO1xuICAgIH1cblxuICAgIC8vIFByb3ZlIHRoZSBmaXJzdCBnb2FsXG4gICAgY29uc3QgZmlyc3RHb2FsID0gZ29hbHNbMF07XG4gICAgY29uc3QgcmVzdEdvYWxzID0gZ29hbHMuc2xpY2UoMSk7XG4gICAgY29uc3QgZmlyc3RTb2x1dGlvbnMgPSB0aGlzLnByb3ZlKGZpcnN0R29hbCwgYmluZGluZ3MpO1xuXG4gICAgLy8gRm9yIGVhY2ggc29sdXRpb24gb2YgdGhlIGZpcnN0IGdvYWwsIHByb3ZlIHRoZSByZXN0XG4gICAgY29uc3QgYWxsU29sdXRpb25zOiBCaW5kaW5nc1tdID0gW107XG4gICAgZm9yIChjb25zdCBzb2x1dGlvbiBvZiBmaXJzdFNvbHV0aW9ucykge1xuICAgICAgY29uc3QgcmVzdFNvbHV0aW9ucyA9IHRoaXMucHJvdmVBbGwocmVzdEdvYWxzLCBzb2x1dGlvbik7XG4gICAgICBhbGxTb2x1dGlvbnMucHVzaCguLi5yZXN0U29sdXRpb25zKTtcbiAgICB9XG5cbiAgICByZXR1cm4gYWxsU29sdXRpb25zO1xuICB9XG5cbiAgLyoqXG4gICAqIFJlbmFtZSBhbGwgdmFyaWFibGVzIGluIGEgY2xhdXNlIHRvIGF2b2lkIGNvbmZsaWN0c1xuICAgKi9cbiAgcHJpdmF0ZSByZW5hbWVWYXJpYWJsZXMoY2xhdXNlOiBDbGF1c2UpOiBDbGF1c2Uge1xuICAgIGNvbnN0IHZhcnMgPSB0aGlzLnZhcmlhYmxlc0luKFtjbGF1c2UuaGVhZCwgLi4uY2xhdXNlLmJvZHldKTtcbiAgICBjb25zdCByZW5hbWluZyA9IG5ldyBNYXA8c3RyaW5nLCBzdHJpbmc+KCk7XG4gICAgXG4gICAgZm9yIChjb25zdCB2YXJOYW1lIG9mIEFycmF5LmZyb20odmFycykpIHtcbiAgICAgIHJlbmFtaW5nLnNldCh2YXJOYW1lLCBgP18ke3RoaXMudmFyQ291bnRlcisrfWApO1xuICAgIH1cblxuICAgIHJldHVybiB7XG4gICAgICBoZWFkOiB0aGlzLnJlbmFtZUluVGVybShjbGF1c2UuaGVhZCwgcmVuYW1pbmcpLFxuICAgICAgYm9keTogY2xhdXNlLmJvZHkubWFwKGdvYWwgPT4gdGhpcy5yZW5hbWVJblRlcm0oZ29hbCwgcmVuYW1pbmcpKSxcbiAgICB9O1xuICB9XG5cbiAgcHJpdmF0ZSByZW5hbWVJblRlcm0odGVybTogVGVybSwgcmVuYW1pbmc6IE1hcDxzdHJpbmcsIHN0cmluZz4pOiBUZXJtIHtcbiAgICBpZiAodGVybS50eXBlID09PSAndmFyaWFibGUnKSB7XG4gICAgICBjb25zdCBuZXdOYW1lID0gcmVuYW1pbmcuZ2V0KHRlcm0ubmFtZSk7XG4gICAgICByZXR1cm4gbmV3TmFtZSA/IHsgdHlwZTogJ3ZhcmlhYmxlJywgbmFtZTogbmV3TmFtZSB9IDogdGVybTtcbiAgICB9IGVsc2UgaWYgKHRlcm0udHlwZSA9PT0gJ2NvbXBvdW5kJykge1xuICAgICAgcmV0dXJuIHtcbiAgICAgICAgdHlwZTogJ2NvbXBvdW5kJyxcbiAgICAgICAgZnVuY3RvcjogdGVybS5mdW5jdG9yLFxuICAgICAgICBhcmdzOiB0ZXJtLmFyZ3MubWFwKGFyZyA9PiB0aGlzLnJlbmFtZUluVGVybShhcmcsIHJlbmFtaW5nKSksXG4gICAgICB9O1xuICAgIH0gZWxzZSBpZiAodGVybS50eXBlID09PSAnbGlzdCcpIHtcbiAgICAgIHJldHVybiB7XG4gICAgICAgIHR5cGU6ICdsaXN0JyxcbiAgICAgICAgZWxlbWVudHM6IHRlcm0uZWxlbWVudHMubWFwKGVsID0+IHRoaXMucmVuYW1lSW5UZXJtKGVsLCByZW5hbWluZykpLFxuICAgICAgfTtcbiAgICB9XG4gICAgcmV0dXJuIHRlcm07XG4gIH1cblxuICBwcml2YXRlIHZhcmlhYmxlc0luKHRlcm1zOiBUZXJtW10pOiBTZXQ8c3RyaW5nPiB7XG4gICAgY29uc3QgdmFycyA9IG5ldyBTZXQ8c3RyaW5nPigpO1xuICAgIFxuICAgIGNvbnN0IGNvbGxlY3QgPSAodGVybTogVGVybSkgPT4ge1xuICAgICAgaWYgKHRlcm0udHlwZSA9PT0gJ3ZhcmlhYmxlJykge1xuICAgICAgICB2YXJzLmFkZCh0ZXJtLm5hbWUpO1xuICAgICAgfSBlbHNlIGlmICh0ZXJtLnR5cGUgPT09ICdjb21wb3VuZCcpIHtcbiAgICAgICAgdGVybS5hcmdzLmZvckVhY2goY29sbGVjdCk7XG4gICAgICB9IGVsc2UgaWYgKHRlcm0udHlwZSA9PT0gJ2xpc3QnKSB7XG4gICAgICAgIHRlcm0uZWxlbWVudHMuZm9yRWFjaChjb2xsZWN0KTtcbiAgICAgIH1cbiAgICB9O1xuXG4gICAgdGVybXMuZm9yRWFjaChjb2xsZWN0KTtcbiAgICByZXR1cm4gdmFycztcbiAgfVxufVxuXG4vLyA9PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09XG4vLyBVbmlmaWNhdGlvblxuLy8gPT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PVxuXG4vKipcbiAqIFVuaWZ5IHR3byB0ZXJtcyB3aXRoIGdpdmVuIGJpbmRpbmdzXG4gKiBSZXR1cm5zIG5ldyBiaW5kaW5ncyBpZiBzdWNjZXNzZnVsLCBGQUlMIG90aGVyd2lzZVxuICovXG5leHBvcnQgZnVuY3Rpb24gdW5pZnkoeDogVGVybSwgeTogVGVybSwgYmluZGluZ3M6IEJpbmRpbmdzIHwgRmFpbCk6IEJpbmRpbmdzIHwgRmFpbCB7XG4gIGlmIChiaW5kaW5ncyA9PT0gRkFJTCkge1xuICAgIHJldHVybiBGQUlMO1xuICB9XG5cbiAgLy8gRGVyZWZlcmVuY2UgdmFyaWFibGVzXG4gIHggPSBkZXJlZih4LCBiaW5kaW5ncyk7XG4gIHkgPSBkZXJlZih5LCBiaW5kaW5ncyk7XG5cbiAgLy8gU2FtZSB0ZXJtXG4gIGlmICh0ZXJtc0VxdWFsKHgsIHkpKSB7XG4gICAgcmV0dXJuIGJpbmRpbmdzO1xuICB9XG5cbiAgLy8gVmFyaWFibGUgdW5pZmljYXRpb25cbiAgaWYgKHgudHlwZSA9PT0gJ3ZhcmlhYmxlJykge1xuICAgIHJldHVybiBiaW5kVmFyaWFibGUoeC5uYW1lLCB5LCBiaW5kaW5ncyk7XG4gIH1cbiAgaWYgKHkudHlwZSA9PT0gJ3ZhcmlhYmxlJykge1xuICAgIHJldHVybiBiaW5kVmFyaWFibGUoeS5uYW1lLCB4LCBiaW5kaW5ncyk7XG4gIH1cblxuICAvLyBDb21wb3VuZCB1bmlmaWNhdGlvblxuICBpZiAoeC50eXBlID09PSAnY29tcG91bmQnICYmIHkudHlwZSA9PT0gJ2NvbXBvdW5kJykge1xuICAgIGlmICh4LmZ1bmN0b3IgIT09IHkuZnVuY3RvciB8fCB4LmFyZ3MubGVuZ3RoICE9PSB5LmFyZ3MubGVuZ3RoKSB7XG4gICAgICByZXR1cm4gRkFJTDtcbiAgICB9XG4gICAgXG4gICAgbGV0IG5ld0JpbmRpbmdzOiBCaW5kaW5ncyB8IEZhaWwgPSBiaW5kaW5ncztcbiAgICBmb3IgKGxldCBpID0gMDsgaSA8IHguYXJncy5sZW5ndGg7IGkrKykge1xuICAgICAgbmV3QmluZGluZ3MgPSB1bmlmeSh4LmFyZ3NbaV0sIHkuYXJnc1tpXSwgbmV3QmluZGluZ3MpO1xuICAgICAgaWYgKG5ld0JpbmRpbmdzID09PSBGQUlMKSB7XG4gICAgICAgIHJldHVybiBGQUlMO1xuICAgICAgfVxuICAgIH1cbiAgICByZXR1cm4gbmV3QmluZGluZ3M7XG4gIH1cblxuICAvLyBMaXN0IHVuaWZpY2F0aW9uXG4gIGlmICh4LnR5cGUgPT09ICdsaXN0JyAmJiB5LnR5cGUgPT09ICdsaXN0Jykge1xuICAgIGlmICh4LmVsZW1lbnRzLmxlbmd0aCAhPT0geS5lbGVtZW50cy5sZW5ndGgpIHtcbiAgICAgIHJldHVybiBGQUlMO1xuICAgIH1cbiAgICBcbiAgICBsZXQgbmV3QmluZGluZ3M6IEJpbmRpbmdzIHwgRmFpbCA9IGJpbmRpbmdzO1xuICAgIGZvciAobGV0IGkgPSAwOyBpIDwgeC5lbGVtZW50cy5sZW5ndGg7IGkrKykge1xuICAgICAgbmV3QmluZGluZ3MgPSB1bmlmeSh4LmVsZW1lbnRzW2ldLCB5LmVsZW1lbnRzW2ldLCBuZXdCaW5kaW5ncyk7XG4gICAgICBpZiAobmV3QmluZGluZ3MgPT09IEZBSUwpIHtcbiAgICAgICAgcmV0dXJuIEZBSUw7XG4gICAgICB9XG4gICAgfVxuICAgIHJldHVybiBuZXdCaW5kaW5ncztcbiAgfVxuXG4gIHJldHVybiBGQUlMO1xufVxuXG5mdW5jdGlvbiBiaW5kVmFyaWFibGUodmFyTmFtZTogc3RyaW5nLCB2YWx1ZTogVGVybSwgYmluZGluZ3M6IEJpbmRpbmdzIHwgRmFpbCk6IEJpbmRpbmdzIHwgRmFpbCB7XG4gIGlmIChiaW5kaW5ncyA9PT0gRkFJTCkge1xuICAgIHJldHVybiBGQUlMO1xuICB9XG4gIFxuICAvLyBPY2N1ciBjaGVjazogcHJldmVudCBpbmZpbml0ZSBzdHJ1Y3R1cmVzXG4gIGlmIChvY2N1cnNJbih2YXJOYW1lLCB2YWx1ZSwgYmluZGluZ3MpKSB7XG4gICAgcmV0dXJuIEZBSUw7XG4gIH1cblxuICBjb25zdCBuZXdCaW5kaW5ncyA9IG5ldyBNYXAoYmluZGluZ3MpO1xuICBuZXdCaW5kaW5ncy5zZXQodmFyTmFtZSwgdmFsdWUpO1xuICByZXR1cm4gbmV3QmluZGluZ3M7XG59XG5cbmZ1bmN0aW9uIG9jY3Vyc0luKHZhck5hbWU6IHN0cmluZywgdGVybTogVGVybSwgYmluZGluZ3M6IEJpbmRpbmdzIHwgRmFpbCk6IGJvb2xlYW4ge1xuICBpZiAoYmluZGluZ3MgPT09IEZBSUwpIHtcbiAgICByZXR1cm4gZmFsc2U7XG4gIH1cbiAgXG4gIHRlcm0gPSBkZXJlZih0ZXJtLCBiaW5kaW5ncyk7XG4gIFxuICBpZiAodGVybS50eXBlID09PSAndmFyaWFibGUnKSB7XG4gICAgcmV0dXJuIHRlcm0ubmFtZSA9PT0gdmFyTmFtZTtcbiAgfSBlbHNlIGlmICh0ZXJtLnR5cGUgPT09ICdjb21wb3VuZCcpIHtcbiAgICByZXR1cm4gdGVybS5hcmdzLnNvbWUoYXJnID0+IG9jY3Vyc0luKHZhck5hbWUsIGFyZywgYmluZGluZ3MpKTtcbiAgfSBlbHNlIGlmICh0ZXJtLnR5cGUgPT09ICdsaXN0Jykge1xuICAgIHJldHVybiB0ZXJtLmVsZW1lbnRzLnNvbWUoZWwgPT4gb2NjdXJzSW4odmFyTmFtZSwgZWwsIGJpbmRpbmdzKSk7XG4gIH1cbiAgXG4gIHJldHVybiBmYWxzZTtcbn1cblxuZnVuY3Rpb24gZGVyZWYodGVybTogVGVybSwgYmluZGluZ3M6IEJpbmRpbmdzIHwgRmFpbCk6IFRlcm0ge1xuICBpZiAoYmluZGluZ3MgPT09IEZBSUwpIHtcbiAgICByZXR1cm4gdGVybTtcbiAgfVxuICBcbiAgaWYgKHRlcm0udHlwZSA9PT0gJ3ZhcmlhYmxlJykge1xuICAgIGNvbnN0IGJpbmRpbmcgPSBiaW5kaW5ncy5nZXQodGVybS5uYW1lKTtcbiAgICBpZiAoYmluZGluZykge1xuICAgICAgcmV0dXJuIGRlcmVmKGJpbmRpbmcsIGJpbmRpbmdzKTtcbiAgICB9XG4gIH1cbiAgcmV0dXJuIHRlcm07XG59XG5cbmZ1bmN0aW9uIHRlcm1zRXF1YWwoeDogVGVybSwgeTogVGVybSk6IGJvb2xlYW4ge1xuICBpZiAoeC50eXBlICE9PSB5LnR5cGUpIHtcbiAgICByZXR1cm4gZmFsc2U7XG4gIH1cblxuICBpZiAoeC50eXBlID09PSAnYXRvbScgJiYgeS50eXBlID09PSAnYXRvbScpIHtcbiAgICByZXR1cm4geC5uYW1lID09PSB5Lm5hbWU7XG4gIH1cblxuICBpZiAoeC50eXBlID09PSAndmFyaWFibGUnICYmIHkudHlwZSA9PT0gJ3ZhcmlhYmxlJykge1xuICAgIHJldHVybiB4Lm5hbWUgPT09IHkubmFtZTtcbiAgfVxuXG4gIGlmICh4LnR5cGUgPT09ICdjb21wb3VuZCcgJiYgeS50eXBlID09PSAnY29tcG91bmQnKSB7XG4gICAgcmV0dXJuIChcbiAgICAgIHguZnVuY3RvciA9PT0geS5mdW5jdG9yICYmXG4gICAgICB4LmFyZ3MubGVuZ3RoID09PSB5LmFyZ3MubGVuZ3RoICYmXG4gICAgICB4LmFyZ3MuZXZlcnkoKGFyZywgaSkgPT4gdGVybXNFcXVhbChhcmcsIHkuYXJnc1tpXSkpXG4gICAgKTtcbiAgfVxuXG4gIGlmICh4LnR5cGUgPT09ICdsaXN0JyAmJiB5LnR5cGUgPT09ICdsaXN0Jykge1xuICAgIHJldHVybiAoXG4gICAgICB4LmVsZW1lbnRzLmxlbmd0aCA9PT0geS5lbGVtZW50cy5sZW5ndGggJiZcbiAgICAgIHguZWxlbWVudHMuZXZlcnkoKGVsLCBpKSA9PiB0ZXJtc0VxdWFsKGVsLCB5LmVsZW1lbnRzW2ldKSlcbiAgICApO1xuICB9XG5cbiAgcmV0dXJuIGZhbHNlO1xufVxuXG4vLyA9PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09XG4vLyBQYXJzaW5nXG4vLyA9PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09XG5cbi8qKlxuICogUGFyc2UgYSBQcm9sb2cgdGVybSBmcm9tIGEgc3RyaW5nXG4gKiBTdXBwb3J0czpcbiAqIC0gQXRvbXM6IGZvbywgYmFyMTIzXG4gKiAtIFZhcmlhYmxlczogP3gsID9WYXJcbiAqIC0gQ29tcG91bmRzOiAoZm9vIGJhciBiYXopXG4gKiAtIExpc3RzOiAoYSBiIGMpXG4gKiAtIFJ1bGVzOiAoaGVhZCkgOi0gKGdvYWwxKSAoZ29hbDIpXG4gKi9cbmV4cG9ydCBmdW5jdGlvbiBwYXJzZVRlcm0oaW5wdXQ6IHN0cmluZyk6IFRlcm0ge1xuICBjb25zdCB0b2tlbnMgPSB0b2tlbml6ZShpbnB1dCk7XG4gIGNvbnN0IFt0ZXJtXSA9IHBhcnNlVG9rZW5zKHRva2Vucyk7XG4gIHJldHVybiB0ZXJtO1xufVxuXG4vKipcbiAqIFBhcnNlIGEgY2xhdXNlIChmYWN0IG9yIHJ1bGUpXG4gKiBSZXR1cm5zIHsgaGVhZCwgYm9keSB9XG4gKi9cbmV4cG9ydCBmdW5jdGlvbiBwYXJzZUNsYXVzZShpbnB1dDogc3RyaW5nKTogQ2xhdXNlIHtcbiAgY29uc3QgdG9rZW5zID0gdG9rZW5pemUoaW5wdXQpO1xuICBcbiAgLy8gQ2hlY2sgZm9yIHJ1bGUgc3ludGF4OiAoaGVhZCkgOi0gKGJvZHkxKSAoYm9keTIpIC4uLlxuICBjb25zdCBjb2xvbkRhc2hJbmRleCA9IHRva2Vucy5pbmRleE9mKCc6LScpO1xuICBcbiAgaWYgKGNvbG9uRGFzaEluZGV4ID09PSAtMSkge1xuICAgIC8vIEl0J3MgYSBmYWN0XG4gICAgY29uc3QgW2hlYWRdID0gcGFyc2VUb2tlbnModG9rZW5zKTtcbiAgICByZXR1cm4geyBoZWFkLCBib2R5OiBbXSB9O1xuICB9XG5cbiAgLy8gSXQncyBhIHJ1bGVcbiAgY29uc3QgaGVhZFRva2VucyA9IHRva2Vucy5zbGljZSgwLCBjb2xvbkRhc2hJbmRleCk7XG4gIGNvbnN0IGJvZHlUb2tlbnMgPSB0b2tlbnMuc2xpY2UoY29sb25EYXNoSW5kZXggKyAxKTtcbiAgXG4gIGNvbnN0IFtoZWFkXSA9IHBhcnNlVG9rZW5zKGhlYWRUb2tlbnMpO1xuICBjb25zdCBib2R5OiBUZXJtW10gPSBbXTtcbiAgXG4gIGxldCBpID0gMDtcbiAgd2hpbGUgKGkgPCBib2R5VG9rZW5zLmxlbmd0aCkge1xuICAgIGNvbnN0IFtnb2FsLCBjb25zdW1lZF0gPSBwYXJzZVRva2Vucyhib2R5VG9rZW5zLnNsaWNlKGkpKTtcbiAgICBib2R5LnB1c2goZ29hbCk7XG4gICAgaSArPSBjb25zdW1lZDtcbiAgfVxuICBcbiAgcmV0dXJuIHsgaGVhZCwgYm9keSB9O1xufVxuXG5leHBvcnQgZnVuY3Rpb24gdG9rZW5pemUoaW5wdXQ6IHN0cmluZyk6IHN0cmluZ1tdIHtcbiAgLy8gQWRkIHNwYWNlcyBhcm91bmQgcGFyZW50aGVzZXMgYW5kIDotIGZvciBlYXNpZXIgdG9rZW5pemF0aW9uXG4gIGNvbnN0IHNwYWNlZCA9IGlucHV0XG4gICAgLnJlcGxhY2UoL1xcKC9nLCAnICggJylcbiAgICAucmVwbGFjZSgvXFwpL2csICcgKSAnKVxuICAgIC5yZXBsYWNlKC86LS9nLCAnIDotICcpO1xuICBcbiAgcmV0dXJuIHNwYWNlZC50cmltKCkuc3BsaXQoL1xccysvKS5maWx0ZXIodCA9PiB0Lmxlbmd0aCA+IDApO1xufVxuXG5leHBvcnQgZnVuY3Rpb24gcGFyc2VUb2tlbnModG9rZW5zOiBzdHJpbmdbXSk6IFtUZXJtLCBudW1iZXJdIHtcbiAgaWYgKHRva2Vucy5sZW5ndGggPT09IDApIHtcbiAgICB0aHJvdyBuZXcgRXJyb3IoJ1VuZXhwZWN0ZWQgZW5kIG9mIGlucHV0Jyk7XG4gIH1cblxuICBjb25zdCBmaXJzdCA9IHRva2Vuc1swXTtcblxuICAvLyBDb21wb3VuZCB0ZXJtIG9yIGxpc3RcbiAgaWYgKGZpcnN0ID09PSAnKCcpIHtcbiAgICB0b2tlbnMuc2hpZnQoKTsgLy8gUmVtb3ZlICcoJ1xuICAgIFxuICAgIGlmICh0b2tlbnMubGVuZ3RoID09PSAwKSB7XG4gICAgICB0aHJvdyBuZXcgRXJyb3IoJ1VuZXhwZWN0ZWQgZW5kIG9mIGlucHV0IGFmdGVyICgnKTtcbiAgICB9XG5cbiAgICAvLyBDaGVjayBmb3IgZW1wdHkgbGlzdFxuICAgIGlmICh0b2tlbnNbMF0gPT09ICcpJykge1xuICAgICAgdG9rZW5zLnNoaWZ0KCk7XG4gICAgICByZXR1cm4gW3sgdHlwZTogJ2xpc3QnLCBlbGVtZW50czogW10gfSwgMl07XG4gICAgfVxuXG4gICAgLy8gUGFyc2UgZWxlbWVudHNcbiAgICBjb25zdCBlbGVtZW50czogVGVybVtdID0gW107XG4gICAgbGV0IGNvbnN1bWVkID0gMTsgLy8gRm9yIHRoZSBvcGVuaW5nICcoJ1xuXG4gICAgd2hpbGUgKHRva2Vucy5sZW5ndGggPiAwICYmIHRva2Vuc1swXSAhPT0gJyknKSB7XG4gICAgICBjb25zdCBbdGVybSwgY10gPSBwYXJzZVRva2Vucyh0b2tlbnMpO1xuICAgICAgZWxlbWVudHMucHVzaCh0ZXJtKTtcbiAgICAgIGNvbnN1bWVkICs9IGM7XG4gICAgfVxuXG4gICAgaWYgKHRva2Vucy5sZW5ndGggPT09IDAgfHwgdG9rZW5zWzBdICE9PSAnKScpIHtcbiAgICAgIHRocm93IG5ldyBFcnJvcignTWlzc2luZyBjbG9zaW5nICknKTtcbiAgICB9XG5cbiAgICB0b2tlbnMuc2hpZnQoKTsgLy8gUmVtb3ZlICcpJ1xuICAgIGNvbnN1bWVkKys7XG5cbiAgICAvLyBEZXRlcm1pbmUgaWYgaXQncyBhIGNvbXBvdW5kIG9yIGxpc3RcbiAgICAvLyBJZiBmaXJzdCBlbGVtZW50IGlzIGFuIGF0b20gYW5kIHRoZXJlIGFyZSBtb3JlIGVsZW1lbnRzLCBpdCdzIGEgY29tcG91bmRcbiAgICBpZiAoZWxlbWVudHMubGVuZ3RoID4gMCAmJiBlbGVtZW50c1swXS50eXBlID09PSAnYXRvbScpIHtcbiAgICAgIGNvbnN0IGZ1bmN0b3IgPSBlbGVtZW50c1swXS5uYW1lO1xuICAgICAgY29uc3QgYXJncyA9IGVsZW1lbnRzLnNsaWNlKDEpO1xuICAgICAgcmV0dXJuIFt7IHR5cGU6ICdjb21wb3VuZCcsIGZ1bmN0b3IsIGFyZ3MgfSwgY29uc3VtZWRdO1xuICAgIH1cblxuICAgIC8vIE90aGVyd2lzZSBpdCdzIGEgbGlzdFxuICAgIHJldHVybiBbeyB0eXBlOiAnbGlzdCcsIGVsZW1lbnRzIH0sIGNvbnN1bWVkXTtcbiAgfVxuXG4gIC8vIFZhcmlhYmxlXG4gIGlmIChmaXJzdC5zdGFydHNXaXRoKCc/JykpIHtcbiAgICB0b2tlbnMuc2hpZnQoKTtcbiAgICByZXR1cm4gW3sgdHlwZTogJ3ZhcmlhYmxlJywgbmFtZTogZmlyc3QgfSwgMV07XG4gIH1cblxuICAvLyBBdG9tIG9yIG51bWJlclxuICB0b2tlbnMuc2hpZnQoKTtcbiAgcmV0dXJuIFt7IHR5cGU6ICdhdG9tJywgbmFtZTogZmlyc3QgfSwgMV07XG59XG5cbi8vID09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT1cbi8vIEZvcm1hdHRpbmdcbi8vID09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT1cblxuZXhwb3J0IGZ1bmN0aW9uIGZvcm1hdFRlcm0odGVybTogVGVybSk6IHN0cmluZyB7XG4gIGlmICh0ZXJtLnR5cGUgPT09ICdhdG9tJykge1xuICAgIHJldHVybiB0ZXJtLm5hbWU7XG4gIH1cblxuICBpZiAodGVybS50eXBlID09PSAndmFyaWFibGUnKSB7XG4gICAgcmV0dXJuIHRlcm0ubmFtZTtcbiAgfVxuXG4gIGlmICh0ZXJtLnR5cGUgPT09ICdjb21wb3VuZCcpIHtcbiAgICBjb25zdCBhcmdzID0gdGVybS5hcmdzLm1hcChmb3JtYXRUZXJtKS5qb2luKCcgJyk7XG4gICAgcmV0dXJuIGAoJHt0ZXJtLmZ1bmN0b3J9JHthcmdzID8gJyAnICsgYXJncyA6ICcnfSlgO1xuICB9XG5cbiAgaWYgKHRlcm0udHlwZSA9PT0gJ2xpc3QnKSB7XG4gICAgY29uc3QgZWxlbWVudHMgPSB0ZXJtLmVsZW1lbnRzLm1hcChmb3JtYXRUZXJtKS5qb2luKCcgJyk7XG4gICAgcmV0dXJuIGAoJHtlbGVtZW50c30pYDtcbiAgfVxuXG4gIHJldHVybiAnPyc7XG59XG5cbi8vID09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT1cbi8vIFV0aWxpdHkgRnVuY3Rpb25zXG4vLyA9PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09XG5cbmV4cG9ydCBmdW5jdGlvbiBzdWJzdEJpbmRpbmdzKHRlcm06IFRlcm0sIGJpbmRpbmdzOiBCaW5kaW5ncyk6IFRlcm0ge1xuICB0ZXJtID0gZGVyZWYodGVybSwgYmluZGluZ3MpO1xuICBcbiAgaWYgKHRlcm0udHlwZSA9PT0gJ2NvbXBvdW5kJykge1xuICAgIHJldHVybiB7XG4gICAgICB0eXBlOiAnY29tcG91bmQnLFxuICAgICAgZnVuY3RvcjogdGVybS5mdW5jdG9yLFxuICAgICAgYXJnczogdGVybS5hcmdzLm1hcChhcmcgPT4gc3Vic3RCaW5kaW5ncyhhcmcsIGJpbmRpbmdzKSksXG4gICAgfTtcbiAgfVxuXG4gIGlmICh0ZXJtLnR5cGUgPT09ICdsaXN0Jykge1xuICAgIHJldHVybiB7XG4gICAgICB0eXBlOiAnbGlzdCcsXG4gICAgICBlbGVtZW50czogdGVybS5lbGVtZW50cy5tYXAoZWwgPT4gc3Vic3RCaW5kaW5ncyhlbCwgYmluZGluZ3MpKSxcbiAgICB9O1xuICB9XG5cbiAgcmV0dXJuIHRlcm07XG59XG5cbmV4cG9ydCBmdW5jdGlvbiB2YXJpYWJsZXNJbih0ZXJtOiBUZXJtKTogVmFyaWFibGVbXSB7XG4gIGNvbnN0IHZhcnM6IFZhcmlhYmxlW10gPSBbXTtcbiAgY29uc3Qgc2VlbiA9IG5ldyBTZXQ8c3RyaW5nPigpO1xuXG4gIGNvbnN0IGNvbGxlY3QgPSAodDogVGVybSkgPT4ge1xuICAgIGlmICh0LnR5cGUgPT09ICd2YXJpYWJsZScgJiYgIXNlZW4uaGFzKHQubmFtZSkpIHtcbiAgICAgIHNlZW4uYWRkKHQubmFtZSk7XG4gICAgICB2YXJzLnB1c2godCk7XG4gICAgfSBlbHNlIGlmICh0LnR5cGUgPT09ICdjb21wb3VuZCcpIHtcbiAgICAgIHQuYXJncy5mb3JFYWNoKGNvbGxlY3QpO1xuICAgIH0gZWxzZSBpZiAodC50eXBlID09PSAnbGlzdCcpIHtcbiAgICAgIHQuZWxlbWVudHMuZm9yRWFjaChjb2xsZWN0KTtcbiAgICB9XG4gIH07XG5cbiAgY29sbGVjdCh0ZXJtKTtcbiAgcmV0dXJuIHZhcnM7XG59XG4iXSwKICAibWFwcGluZ3MiOiAiOzs7Ozs7Ozs7Ozs7Ozs7Ozs7OztBQUFBO0FBQUE7QUFBQTtBQUFBO0FBQUE7QUFBQTtBQUFBO0FBQUE7QUFBQTtBQUFBO0FBQUE7QUFBQTs7O0FDd0NBLElBQU0sT0FBc0IsT0FBTyxNQUFNO0FBT2xDLElBQU0sV0FBTixNQUFlO0FBQUEsRUFBZjtBQUNMLFNBQVEsYUFBb0Msb0JBQUksSUFBSTtBQUNwRCxTQUFRLGFBQWE7QUFBQTtBQUFBLEVBRXJCLFFBQWM7QUFD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
