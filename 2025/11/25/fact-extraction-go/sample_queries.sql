-- Sample SQL Queries for Fact Extraction Database

-- 1. All Jeffrey Epstein relationships
SELECT 
    actor, 
    action, 
    target, 
    location,
    explicit_topic,
    implicit_topic
FROM rdf_triples 
WHERE actor LIKE '%Epstein%' OR target LIKE '%Epstein%'
ORDER BY sequence_order;

-- 2. Relationships involving Donald Trump
SELECT 
    actor, 
    action, 
    target, 
    location,
    timestamp
FROM rdf_triples 
WHERE actor LIKE '%Trump%' OR target LIKE '%Trump%';

-- 3. Sexual abuse related triples
SELECT 
    actor, 
    action, 
    target,
    explicit_topic
FROM rdf_triples 
WHERE triple_tags LIKE '%sexual_abuse%' OR triple_tags LIKE '%sexual abuse%';

-- 4. Document summaries by category
SELECT 
    category,
    doc_id,
    one_sentence_summary
FROM documents
ORDER BY category, doc_id;

-- 5. Most connected people (as actors)
SELECT 
    actor,
    COUNT(*) as relationship_count
FROM rdf_triples
GROUP BY actor
ORDER BY relationship_count DESC
LIMIT 20;

-- 6. Most targeted people
SELECT 
    target,
    COUNT(*) as times_targeted
FROM rdf_triples
GROUP BY target
ORDER BY times_targeted DESC
LIMIT 20;

-- 7. Relationships with timestamps
SELECT 
    timestamp,
    actor,
    action,
    target,
    location
FROM rdf_triples
WHERE timestamp IS NOT NULL
ORDER BY timestamp;

-- 8. Relationships at Mar-a-Lago
SELECT 
    actor,
    action,
    target,
    timestamp
FROM rdf_triples
WHERE location LIKE '%Mar-a-Lago%';

-- 9. Legal/court related relationships
SELECT 
    actor,
    action,
    target,
    explicit_topic
FROM rdf_triples
WHERE triple_tags LIKE '%court%' 
   OR triple_tags LIKE '%legal%'
   OR triple_tags LIKE '%deposition%';

-- 10. Implicit topics analysis
SELECT 
    implicit_topic,
    COUNT(*) as frequency
FROM rdf_triples
WHERE implicit_topic IS NOT NULL
GROUP BY implicit_topic
ORDER BY frequency DESC
LIMIT 15;
