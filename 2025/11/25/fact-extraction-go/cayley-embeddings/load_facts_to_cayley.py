#!/usr/bin/env python3
"""
Load fact extraction data from SQLite into Cayley graph database
and generate embeddings from entity/relation descriptions
"""

import sqlite3
import json
import subprocess
import sys
from pathlib import Path
from urllib.parse import quote

def load_facts_from_sqlite(db_path):
    """Load triples and descriptions from SQLite"""
    conn = sqlite3.connect(db_path)
    conn.row_factory = sqlite3.Row
    cursor = conn.cursor()
    
    # Load triples
    cursor.execute("""
        SELECT actor, action, target, confidence, reasoning, citations
        FROM rdf_triples_full
    """)
    triples = [dict(row) for row in cursor.fetchall()]
    
    # Load entity descriptions
    cursor.execute("""
        SELECT entity_name, entity_type, description
        FROM entity_descriptions
    """)
    entities = {row['entity_name']: dict(row) for row in cursor.fetchall()}
    
    # Load relation descriptions
    cursor.execute("""
        SELECT relation_name, relation_type, description
        FROM relation_descriptions
    """)
    relations = {row['relation_name']: dict(row) for row in cursor.fetchall()}
    
    conn.close()
    
    return triples, entities, relations
def iri_encode(s):
    """Encode string as IRI (URL encode spaces and special chars)"""
    # Replace spaces with underscores for cleaner IRIs
    return s.replace(' ', '_').replace('"', '').replace('\n', '')

def generate_nquads(triples, entities, relations, output_path):
    """Generate N-Quads file for Cayley"""
    with open(output_path, 'w') as f:
        # Write triples
        for triple in triples:
            actor = iri_encode(triple['actor'])
            action = iri_encode(triple['action'])
            target = iri_encode(triple['target'])
            
            # Main triple
            f.write(f'<{actor}> <{action}> <{target}> .\n')
            
            # Add metadata as separate triples
            if triple.get('confidence'):
                f.write(f'<{actor}_{action}_{target}> <confidence> "{triple["confidence"]}" .\n')
            
            if triple.get('reasoning'):
                reasoning = triple['reasoning'].replace('"', '\\"').replace('\n', ' ')[:500]
                f.write(f'<{actor}_{action}_{target}> <reasoning> "{reasoning}" .\n')
        
        # Write entity types and descriptions
        for name, entity in entities.items():
            name_iri = iri_encode(name)
            if entity.get('entity_type'):
                entity_type_iri = iri_encode(entity['entity_type'])
                f.write(f'<{name_iri}> <rdf:type> <{entity_type_iri}> .\n')
            if entity.get('description'):
                desc = entity['description'].replace('"', '\\"').replace('\n', ' ')[:500]
                f.write(f'<{name_iri}> <description> "{desc}" .\n')
        
        # Write relation types and descriptions
        for name, relation in relations.items():
            name_iri = iri_encode(name)
            if relation.get('relation_type'):
                rel_type_iri = iri_encode(relation['relation_type'])
                f.write(f'<relation:{name_iri}> <rdf:type> <RelationType:{rel_type_iri}> .\n')
            if relation.get('description'):
                desc = relation['description'].replace('"', '\\"').replace('\n', ' ')[:500]
                f.write(f'<relation:{name_iri}> <description> "{desc}" .\n')
    
    print(f"Generated {output_path} with {len(triples)} triples")

def generate_mock_embeddings(entities, relations, output_path):
    """Generate mock embeddings from descriptions"""
    embeddings = {}
    
    # Mock: Use description length and first char as simple features
    # In production, call an embedding API
    for name, entity in entities.items():
        name_iri = iri_encode(name)
        desc = entity.get('description', '')
        # Create 384-dim vector (mock)
        vec = [0.0] * 384
        if desc:
            # Simple hash-based mock embedding
            for i, char in enumerate(desc[:384]):
                vec[i] = ord(char) / 255.0
        embeddings[name_iri] = vec
    
    for name, relation in relations.items():
        name_iri = iri_encode(name)
        desc = relation.get('description', '')
        vec = [0.0] * 384
        if desc:
            for i, char in enumerate(desc[:384]):
                vec[i] = ord(char) / 255.0
        embeddings[f"relation:{name_iri}"] = vec
    
    with open(output_path, 'w') as f:
        json.dump(embeddings, f)
    
    print(f"Generated {len(embeddings)} mock embeddings in {output_path}")

def main():
    if len(sys.argv) < 2:
        print("Usage: python3 load_facts_to_cayley.py <sqlite_db_path>")
        sys.exit(1)
    
    db_path = sys.argv[1]
    if not Path(db_path).exists():
        print(f"Error: Database not found: {db_path}")
        sys.exit(1)
    
    print(f"Loading facts from {db_path}...")
    triples, entities, relations = load_facts_from_sqlite(db_path)
    
    print(f"Loaded:")
    print(f"  - {len(triples)} triples")
    print(f"  - {len(entities)} entities")
    print(f"  - {len(relations)} relations")
    
    # Generate N-Quads
    nquads_path = "facts.nq"
    generate_nquads(triples, entities, relations, nquads_path)
    
    # Generate embeddings
    emb_path = "embeddings.json"
    generate_mock_embeddings(entities, relations, emb_path)
    
    # Initialize Cayley database
    print("\nInitializing Cayley database...")
    cayley_db = "cayley_facts.db"
    
    # Create Cayley config
    config = {
        "database": "bolt",
        "db_path": cayley_db,
        "load": [nquads_path]
    }
    
    with open("cayley_config.json", 'w') as f:
        json.dump(config, f, indent=2)
    
    print(f"\nCayley setup complete!")
    print(f"  - Database: {cayley_db}")
    print(f"  - N-Quads: {nquads_path}")
    print(f"  - Embeddings: {emb_path}")
    print(f"\nTo load data into Cayley, run:")
    print(f"  cayley load -c cayley_config.json")
    print(f"\nTo test search:")
    print(f"  ./cayley-search -db {cayley_db} -emb {emb_path} -query 'Jeffrey Epstein' -k 10")

if __name__ == "__main__":
    main()
