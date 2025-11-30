#!/usr/bin/env python3
"""
Analyze the extracted facts from the database
"""

import json
import sqlite3
from collections import Counter, defaultdict

def analyze_database(db_path="fact_extraction.db"):
    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()
    
    print("=" * 80)
    print("FACT EXTRACTION ANALYSIS REPORT")
    print("=" * 80)
    print()
    
    # Document statistics
    cursor.execute("SELECT COUNT(*), SUM(input_tokens), SUM(output_tokens), SUM(cost_usd) FROM documents")
    doc_count, total_input, total_output, total_cost = cursor.fetchone()
    
    print(f"📄 DOCUMENT STATISTICS")
    print(f"   Total documents analyzed: {doc_count}")
    print(f"   Total input tokens: {total_input:,}")
    print(f"   Total output tokens: {total_output:,}")
    print(f"   Total cost: ${total_cost:.4f}")
    print()
    
    # Category breakdown
    cursor.execute("SELECT category, COUNT(*) FROM documents GROUP BY category ORDER BY COUNT(*) DESC")
    print(f"📊 DOCUMENT CATEGORIES")
    for category, count in cursor.fetchall():
        print(f"   {category}: {count}")
    print()
    
    # Triple statistics
    cursor.execute("SELECT COUNT(*) FROM rdf_triples")
    triple_count = cursor.fetchone()[0]
    print(f"🔗 RDF TRIPLE STATISTICS")
    print(f"   Total triples extracted: {triple_count}")
    print(f"   Average triples per document: {triple_count/doc_count:.1f}")
    print()
    
    # Top actors
    cursor.execute("""
        SELECT actor, COUNT(*) as count 
        FROM rdf_triples 
        GROUP BY actor 
        ORDER BY count DESC 
        LIMIT 15
    """)
    print(f"👤 TOP 15 ACTORS (by number of relationships)")
    for i, (actor, count) in enumerate(cursor.fetchall(), 1):
        print(f"   {i:2d}. {actor}: {count} relationships")
    print()
    
    # Top targets
    cursor.execute("""
        SELECT target, COUNT(*) as count 
        FROM rdf_triples 
        GROUP BY target 
        ORDER BY count DESC 
        LIMIT 15
    """)
    print(f"🎯 TOP 15 TARGETS (by number of relationships)")
    for i, (target, count) in enumerate(cursor.fetchall(), 1):
        print(f"   {i:2d}. {target}: {count} relationships")
    print()
    
    # Common actions
    cursor.execute("""
        SELECT action, COUNT(*) as count 
        FROM rdf_triples 
        GROUP BY action 
        ORDER BY count DESC 
        LIMIT 10
    """)
    print(f"⚡ TOP 10 ACTIONS")
    for i, (action, count) in enumerate(cursor.fetchall(), 1):
        print(f"   {i:2d}. {action}: {count} occurrences")
    print()
    
    # Common tags
    cursor.execute("SELECT triple_tags FROM rdf_triples WHERE triple_tags IS NOT NULL")
    all_tags = []
    for (tags_json,) in cursor.fetchall():
        if tags_json:
            all_tags.extend(json.loads(tags_json))
    
    tag_counts = Counter(all_tags)
    print(f"🏷️  TOP 15 TAGS")
    for i, (tag, count) in enumerate(tag_counts.most_common(15), 1):
        print(f"   {i:2d}. {tag}: {count} occurrences")
    print()
    
    # Explicit topics
    cursor.execute("""
        SELECT explicit_topic, COUNT(*) as count 
        FROM rdf_triples 
        WHERE explicit_topic IS NOT NULL
        GROUP BY explicit_topic 
        ORDER BY count DESC 
        LIMIT 10
    """)
    print(f"📝 TOP 10 EXPLICIT TOPICS")
    for i, (topic, count) in enumerate(cursor.fetchall(), 1):
        print(f"   {i:2d}. {topic}: {count} occurrences")
    print()
    
    # Sample Jeffrey Epstein relationships
    cursor.execute("""
        SELECT actor, action, target, location, explicit_topic, implicit_topic
        FROM rdf_triples 
        WHERE actor LIKE '%Epstein%' OR target LIKE '%Epstein%'
        LIMIT 10
    """)
    print(f"🔍 SAMPLE JEFFREY EPSTEIN RELATIONSHIPS (first 10)")
    for i, (actor, action, target, location, exp_topic, imp_topic) in enumerate(cursor.fetchall(), 1):
        loc_str = f" at {location}" if location else ""
        print(f"   {i:2d}. {actor} → {action} → {target}{loc_str}")
        print(f"       Explicit: {exp_topic}")
        print(f"       Implicit: {imp_topic}")
        print()
    
    conn.close()
    print("=" * 80)

if __name__ == "__main__":
    analyze_database()
