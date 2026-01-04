-- Document Management System Query Examples

-- 1. Documents by type
SELECT 'Documents by type:' as query_description;
SELECT d.doc_kind, COUNT(*) as count 
FROM documents d 
GROUP BY d.doc_kind;

-- 2. Documents by status
SELECT 'Documents by status:' as query_description;
SELECT d.doc_status, COUNT(*) as count 
FROM documents d 
GROUP BY d.doc_status;

-- 3. Long-lived documents requiring review
SELECT 'Long-lived documents with review intervals:' as query_description;
SELECT n.title, d.doc_kind, d.review_interval_days, p.handle as owner
FROM nodes n
JOIN documents d ON n.id = d.node_id
LEFT JOIN doc_owners do ON n.id = do.doc_id
LEFT JOIN people p ON do.person_id = p.node_id
WHERE d.long_lived = 1 AND d.review_interval_days IS NOT NULL
ORDER BY d.review_interval_days;

-- 4. Documents by owner
SELECT 'Documents by owner:' as query_description;
SELECT p.handle, p.display_name, COUNT(*) as document_count
FROM people p
JOIN doc_owners do ON p.node_id = do.person_id
GROUP BY p.node_id, p.handle, p.display_name
ORDER BY document_count DESC;

-- 5. Draft documents that need attention
SELECT 'Draft documents needing attention:' as query_description;
SELECT n.title, d.doc_kind, p.handle as owner, n.created_at
FROM nodes n
JOIN documents d ON n.id = d.node_id
LEFT JOIN doc_owners do ON n.id = do.doc_id
LEFT JOIN people p ON do.person_id = p.node_id
WHERE d.doc_status = 'draft'
ORDER BY n.created_at;

-- 6. All relationships in the system (graph view)
SELECT 'Document relationships (sample):' as query_description;
SELECT subject, predicate, object
FROM cayley_quads
WHERE predicate IN ('ex:ownedBy', 'ex:hasTopic', 'ex:references', 'ex:supersedes')
LIMIT 10;

