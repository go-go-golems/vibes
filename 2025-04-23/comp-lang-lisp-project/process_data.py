#!/usr/bin/env python3
"""
Script to create a structured dataset from the collected comp.lang.lisp articles
for use in the frontend application
"""

import os
import json
import re
from datetime import datetime

# Configuration
DATA_DIR = "data/naggum"
COMBINED_DIR = "data/combined"
PROCESSED_DIR = "data/processed"
FRONTEND_DATA_DIR = "frontend/public/data"

def ensure_dir(directory):
    """Ensure that a directory exists."""
    if not os.path.exists(directory):
        os.makedirs(directory)

def process_articles():
    """Process the collected articles and create a structured dataset."""
    ensure_dir(PROCESSED_DIR)
    ensure_dir(FRONTEND_DATA_DIR)
    
    # Load all articles
    all_articles = []
    
    if os.path.exists(os.path.join(COMBINED_DIR, "all_articles.json")):
        with open(os.path.join(COMBINED_DIR, "all_articles.json"), 'r', encoding='utf-8') as f:
            all_articles = json.load(f)
    else:
        # If combined file doesn't exist, load from individual files
        for year_dir in os.listdir(DATA_DIR):
            year_path = os.path.join(DATA_DIR, year_dir)
            if os.path.isdir(year_path):
                for file_name in os.listdir(year_path):
                    if file_name.endswith('.json'):
                        file_path = os.path.join(year_path, file_name)
                        with open(file_path, 'r', encoding='utf-8') as f:
                            article = json.load(f)
                            all_articles.append(article)
    
    print(f"Loaded {len(all_articles)} articles")
    
    # Process articles for frontend
    threads = {}
    authors = {}
    
    for article in all_articles:
        # Extract author information
        author_name = "Unknown"
        author_email = ""
        
        if 'author' in article:
            author_match = re.search(r'([^<]+)<([^>]+)>', article['author'])
            if author_match:
                author_name = author_match.group(1).strip()
                author_email = author_match.group(2).strip()
            else:
                author_name = article['author'].strip()
        
        # Create or update author record
        author_id = author_email.lower() if author_email else author_name.lower().replace(' ', '_')
        if author_id not in authors:
            authors[author_id] = {
                'id': author_id,
                'name': author_name,
                'email': author_email,
                'post_count': 0
            }
        authors[author_id]['post_count'] += 1
        
        # Extract thread information from subject
        subject = article.get('title', 'No Subject').strip()
        # Remove Re: and similar prefixes for thread grouping
        thread_subject = re.sub(r'^(Re|Fwd|Fw):\s*', '', subject, flags=re.IGNORECASE)
        thread_id = thread_subject.lower().replace(' ', '_')[:50]  # Limit length for IDs
        
        # Format date for display
        display_date = article.get('date', '')
        if display_date:
            try:
                # Convert YYYY-MM-DD to display format
                date_obj = datetime.strptime(display_date, '%Y-%m-%d')
                display_date = date_obj.strftime('%m/%d/%Y')
            except:
                pass
        
        # Create post object
        post = {
            'id': article.get('id', ''),
            'subject': subject,
            'author_id': author_id,
            'date': article.get('date', ''),
            'display_date': display_date,
            'content': article.get('content', ''),
            'references': article.get('references', []),
            'thread_id': thread_id
        }
        
        # Create or update thread record
        if thread_id not in threads:
            threads[thread_id] = {
                'id': thread_id,
                'title': thread_subject,
                'posts': [],
                'post_count': 0,
                'first_post_date': article.get('date', ''),
                'last_post_date': article.get('date', '')
            }
        
        threads[thread_id]['posts'].append(post)
        threads[thread_id]['post_count'] += 1
        
        # Update thread dates
        if article.get('date', '') < threads[thread_id]['first_post_date'] or not threads[thread_id]['first_post_date']:
            threads[thread_id]['first_post_date'] = article.get('date', '')
        if article.get('date', '') > threads[thread_id]['last_post_date']:
            threads[thread_id]['last_post_date'] = article.get('date', '')
    
    # Convert to lists for frontend
    threads_list = list(threads.values())
    authors_list = list(authors.values())
    
    # Sort threads by last post date (newest first)
    threads_list.sort(key=lambda x: x['last_post_date'], reverse=True)
    
    # Sort authors by post count (most active first)
    authors_list.sort(key=lambda x: x['post_count'], reverse=True)
    
    # Create frontend data files
    frontend_data = {
        'threads': threads_list,
        'authors': authors_list,
        'stats': {
            'total_threads': len(threads_list),
            'total_posts': len(all_articles),
            'total_authors': len(authors_list)
        }
    }
    
    # Save processed data
    with open(os.path.join(PROCESSED_DIR, "processed_data.json"), 'w', encoding='utf-8') as f:
        json.dump(frontend_data, f, ensure_ascii=False, indent=2)
    
    # Save frontend data files
    with open(os.path.join(FRONTEND_DATA_DIR, "threads.json"), 'w', encoding='utf-8') as f:
        json.dump(threads_list, f, ensure_ascii=False, indent=2)
    
    with open(os.path.join(FRONTEND_DATA_DIR, "authors.json"), 'w', encoding='utf-8') as f:
        json.dump(authors_list, f, ensure_ascii=False, indent=2)
    
    with open(os.path.join(FRONTEND_DATA_DIR, "stats.json"), 'w', encoding='utf-8') as f:
        json.dump(frontend_data['stats'], f, ensure_ascii=False, indent=2)
    
    print(f"Processed data saved to {PROCESSED_DIR}")
    print(f"Frontend data saved to {FRONTEND_DATA_DIR}")
    print(f"Total threads: {len(threads_list)}")
    print(f"Total posts: {len(all_articles)}")
    print(f"Total authors: {len(authors_list)}")
    
    return frontend_data

if __name__ == "__main__":
    process_articles()
