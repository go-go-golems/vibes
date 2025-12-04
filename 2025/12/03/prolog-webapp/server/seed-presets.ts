import { drizzle } from 'drizzle-orm/mysql2';
import { prologPresets } from '../drizzle/schema';

const db = drizzle(process.env.DATABASE_URL!);

const presets = [
  {
    name: 'Family Relationships',
    category: 'Logic & Relations',
    description: 'Basic family relationships with parent, grandparent, and sibling rules',
    facts: JSON.stringify([
      '(parent tom bob)',
      '(parent tom liz)',
      '(parent bob ann)',
      '(parent bob pat)',
      '(parent pat jim)',
      '(grandparent ?gp ?gc) (parent ?gp ?p) (parent ?p ?gc)',
      '(sibling ?x ?y) (parent ?p ?x) (parent ?p ?y)',
      '(ancestor ?x ?y) (parent ?x ?y)',
      '(ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y)',
    ]),
    exampleQueries: JSON.stringify([
      '(parent tom ?child)',
      '(grandparent tom ?gc)',
      '(sibling ann ?sib)',
      '(ancestor tom ?desc)',
    ]),
  },
  {
    name: 'List Operations',
    category: 'Data Structures',
    description: 'Common list operations: member, append, reverse, and length',
    facts: JSON.stringify([
      '(member ?x (?x . ?rest))',
      '(member ?x (?y . ?rest)) (member ?x ?rest)',
      '(append nil ?l ?l)',
      '(append (?x . ?l1) ?l2 (?x . ?l3)) (append ?l1 ?l2 ?l3)',
      '(last (?x) ?x)',
      '(last (? . ?t) ?x) (last ?t ?x)',
      '(reverse nil nil)',
      '(reverse (?h . ?t) ?r) (reverse ?t ?rt) (append ?rt (?h) ?r)',
    ]),
    exampleQueries: JSON.stringify([
      '(member 3 (1 2 3 4))',
      '(member ?x (a b c))',
      '(append (1 2) (3 4) ?result)',
      '(append ?x ?y (1 2 3))',
      '(last (1 2 3 4) ?x)',
      '(reverse (1 2 3) ?r)',
    ]),
  },
  {
    name: 'Graph Traversal',
    category: 'Algorithms',
    description: 'Graph edges and path finding with transitive closure',
    facts: JSON.stringify([
      '(edge a b)',
      '(edge b c)',
      '(edge c d)',
      '(edge b e)',
      '(edge e f)',
      '(edge a g)',
      '(path ?x ?y) (edge ?x ?y)',
      '(path ?x ?y) (edge ?x ?z) (path ?z ?y)',
    ]),
    exampleQueries: JSON.stringify([
      '(path a d)',
      '(path a ?node)',
      '(path ?from ?to)',
      '(edge ?x ?y)',
    ]),
  },
  {
    name: 'Likes Relations',
    category: 'Logic & Relations',
    description: 'Who likes whom, with transitive rules',
    facts: JSON.stringify([
      '(likes kim robin)',
      '(likes sandy lee)',
      '(likes sandy kim)',
      '(likes robin cats)',
      '(likes sandy ?x) (likes ?x cats)',
    ]),
    exampleQueries: JSON.stringify([
      '(likes sandy ?who)',
      '(likes ?who kim)',
      '(likes ?x ?y)',
    ]),
  },
  {
    name: 'Arithmetic (Peano)',
    category: 'Mathematics',
    description: 'Natural numbers using successor notation (Peano arithmetic)',
    facts: JSON.stringify([
      '(plus 0 ?x ?x)',
      '(plus (s ?x) ?y (s ?z)) (plus ?x ?y ?z)',
      '(times 0 ?x 0)',
      '(times (s ?x) ?y ?z) (times ?x ?y ?w) (plus ?w ?y ?z)',
    ]),
    exampleQueries: JSON.stringify([
      '(plus (s (s 0)) (s (s (s 0))) ?sum)',
      '(plus (s 0) (s 0) ?result)',
      '(times (s (s 0)) (s (s (s 0))) ?product)',
    ]),
  },
  {
    name: 'Extended Family',
    category: 'Logic & Relations',
    description: 'Complex family relations including gender, uncles, aunts, and cousins',
    facts: JSON.stringify([
      '(male john)',
      '(male bob)',
      '(male jim)',
      '(male tom)',
      '(female mary)',
      '(female sue)',
      '(female ann)',
      '(parent john mary)',
      '(parent john bob)',
      '(parent sue mary)',
      '(parent sue bob)',
      '(parent mary ann)',
      '(parent mary jim)',
      '(parent bob tom)',
      '(father ?f ?c) (parent ?f ?c) (male ?f)',
      '(mother ?m ?c) (parent ?m ?c) (female ?m)',
      '(grandfather ?gf ?gc) (father ?gf ?p) (parent ?p ?gc)',
      '(grandmother ?gm ?gc) (mother ?gm ?p) (parent ?p ?gc)',
      '(sibling ?x ?y) (parent ?p ?x) (parent ?p ?y)',
      '(cousin ?x ?y) (parent ?px ?x) (parent ?py ?y) (sibling ?px ?py)',
    ]),
    exampleQueries: JSON.stringify([
      '(father ?f mary)',
      '(mother ?m ann)',
      '(grandfather ?gf tom)',
      '(grandmother ?gm ann)',
      '(sibling mary ?sib)',
      '(cousin tom ?cousin)',
    ]),
  },
  {
    name: 'Simple Facts',
    category: 'Getting Started',
    description: 'Simple facts to get started with Prolog',
    facts: JSON.stringify([
      '(color sky blue)',
      '(color grass green)',
      '(color sun yellow)',
      '(animal dog)',
      '(animal cat)',
      '(animal bird)',
      '(has-legs dog 4)',
      '(has-legs cat 4)',
      '(has-legs bird 2)',
    ]),
    exampleQueries: JSON.stringify([
      '(color sky ?c)',
      '(color ?thing green)',
      '(animal ?x)',
      '(has-legs ?animal 4)',
    ]),
  },
];

async function seed() {
  console.log('Seeding presets...');
  
  for (const preset of presets) {
    try {
      await db.insert(prologPresets).values(preset);
      console.log(`✓ Added preset: ${preset.name}`);
    } catch (error) {
      console.log(`  Preset "${preset.name}" may already exist, skipping...`);
    }
  }
  
  console.log('Seeding complete!');
  process.exit(0);
}

seed().catch((error) => {
  console.error('Error seeding presets:', error);
  process.exit(1);
});
