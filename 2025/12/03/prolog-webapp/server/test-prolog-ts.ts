/**
 * Test the TypeScript Prolog interpreter
 */

import { executeQuery } from './prolog-executor-ts';

async function runTests() {
  console.log('Testing TypeScript Prolog Interpreter\n');
  console.log('=' .repeat(60));

  // Test 1: Simple facts
  console.log('\n\nTest 1: Simple Facts');
  console.log('-'.repeat(60));
  const facts1 = [
    '(color sky blue)',
    '(color grass green)',
    '(color sun yellow)',
  ];
  const query1 = '(color sky ?c)';
  console.log('Facts:', facts1);
  console.log('Query:', query1);
  const result1 = await executeQuery(query1, facts1);
  console.log('Result:', JSON.stringify(result1, null, 2));

  // Test 2: Variable query
  console.log('\n\nTest 2: Variable Query');
  console.log('-'.repeat(60));
  const query2 = '(color ?thing blue)';
  console.log('Query:', query2);
  const result2 = await executeQuery(query2, facts1);
  console.log('Result:', JSON.stringify(result2, null, 2));

  // Test 3: Family relationships with rules
  console.log('\n\nTest 3: Family Relationships');
  console.log('-'.repeat(60));
  const facts3 = [
    '(parent tom bob)',
    '(parent tom liz)',
    '(parent bob ann)',
    '(parent bob pat)',
    '(parent pat jim)',
  ];
  console.log('Facts:', facts3);
  const query3 = '(parent tom ?child)';
  console.log('Query:', query3);
  const result3 = await executeQuery(query3, facts3);
  console.log('Result:', JSON.stringify(result3, null, 2));

  // Test 4: All parents
  console.log('\n\nTest 4: Find All Parents');
  console.log('-'.repeat(60));
  const query4 = '(parent ?p ?c)';
  console.log('Query:', query4);
  const result4 = await executeQuery(query4, facts3);
  console.log('Result:', JSON.stringify(result4, null, 2));

  // Test 5: List operations
  console.log('\n\nTest 5: List Operations');
  console.log('-'.repeat(60));
  const facts5 = [
    '(member ?x (?x . ?rest))',
    '(member ?x (?y . ?rest))',
  ];
  console.log('Facts:', facts5);
  const query5 = '(member 2 (1 2 3))';
  console.log('Query:', query5);
  const result5 = await executeQuery(query5, facts5);
  console.log('Result:', JSON.stringify(result5, null, 2));

  console.log('\n\n' + '='.repeat(60));
  console.log('Tests completed!');
}

runTests().catch(console.error);
