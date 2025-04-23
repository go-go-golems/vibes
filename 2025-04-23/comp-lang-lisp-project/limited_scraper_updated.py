#!/usr/bin/env python3
"""
Limited scraper to collect a sample of around 300 posts from Erik Naggum's comp.lang.lisp archive
Updated with correct URL structure
"""

import os
import sys
import json
import time
import re
from datetime import datetime
import requests
from bs4 import BeautifulSoup
from urllib.parse import urljoin

# Configuration
DATA_DIR = "data"
NAGGUM_DIR = "data/naggum"
COMBINED_DIR = "data/combined"
LOG_FILE = "scraping_log.txt"
BASE_URL = "https://www.xach.com/naggum/articles/"
YEARS = ["2000", "2001"]  # Focus on just a couple of years to limit the sample
MAX_ARTICLES = 300  # Maximum number of articles to collect
DELAY = 1  # Delay between requests in seconds

def ensure_dir(directory):
    """Ensure that a directory exists."""
    if not os.path.exists(directory):
        os.makedirs(directory)

def log_message(message):
    """Log a message to both console and log file."""
    timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    log_entry = f"[{timestamp}] {message}"
    print(log_entry)
    
    with open(LOG_FILE, 'a', encoding='utf-8') as f:
        f.write(log_entry + "\n")

def fetch_page(url):
    """Fetch a page and return its content."""
    log_message(f"Fetching {url}")
    response = requests.get(url)
    response.raise_for_status()
    return response.text

def parse_year_page(html, year):
    """Parse the year page and extract article links."""
    soup = BeautifulSoup(html, 'html.parser')
    articles = []
    
    # Find all links that point to articles
    for link in soup.find_all('a'):
        href = link.get('href')
        if href and '@naggum' in href:
            article_url = urljoin(BASE_URL, href)
            articles.append(article_url)
    
    return articles

def parse_article_page(html, article_url):
    """Parse an article page and extract its content."""
    soup = BeautifulSoup(html, 'html.parser')
    
    # Extract article metadata and content
    article_id = os.path.basename(article_url).replace('.html', '')
    
    article = {
        'url': article_url,
        'id': article_id,
        'title': soup.title.string if soup.title else "No title",
        'content': '',
        'date': '',
        'author': 'Erik Naggum',
        'references': [],
        'source': 'naggum'
    }
    
    # Extract content
    pre_tag = soup.find('pre')
    if pre_tag:
        content = pre_tag.get_text()
        article['content'] = content
        
        # Extract date from content
        date_match = re.search(r'Date:\s+(\d{4}/\d{2}/\d{2})', content)
        if date_match:
            date_str = date_match.group(1)
            try:
                # Convert YYYY/MM/DD to YYYY-MM-DD
                date_parts = date_str.split('/')
                article['date'] = f"{date_parts[0]}-{date_parts[1]}-{date_parts[2]}"
            except:
                article['date'] = date_str
        
        # Extract subject from content
        subject_match = re.search(r'Subject:\s+(.*?)$', content, re.MULTILINE)
        if subject_match:
            article['title'] = subject_match.group(1).strip()
        
        # Extract references
        references_match = re.search(r'References:\s+(.*?)$', content, re.MULTILINE)
        if references_match:
            refs = references_match.group(1).strip()
            article['references'] = [ref.strip() for ref in refs.split(',')]
    
    return article

def collect_limited_sample():
    """Collect a limited sample of articles from Erik Naggum's archive."""
    ensure_dir(DATA_DIR)
    ensure_dir(NAGGUM_DIR)
    ensure_dir(COMBINED_DIR)
    
    # Initialize log file
    with open(LOG_FILE, 'w', encoding='utf-8') as f:
        f.write(f"Limited scraping started at {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
    
    all_articles = []
    articles_collected = 0
    
    for year in YEARS:
        if articles_collected >= MAX_ARTICLES:
            break
            
        year_dir = os.path.join(NAGGUM_DIR, year)
        ensure_dir(year_dir)
        
        try:
            log_message(f"Fetching articles from year {year}")
            year_url = f"{BASE_URL}{year}.html"  # Corrected URL format
            year_html = fetch_page(year_url)
            article_urls = parse_year_page(year_html, year)
            
            # Limit the number of articles per year
            articles_per_year = min(len(article_urls), MAX_ARTICLES - articles_collected)
            log_message(f"Found {len(article_urls)} articles, collecting {articles_per_year} from year {year}")
            
            for i, article_url in enumerate(article_urls[:articles_per_year]):
                try:
                    article_html = fetch_page(article_url)
                    article = parse_article_page(article_html, article_url)
                    
                    # Save article to JSON file
                    article_filename = os.path.join(year_dir, f"{article['id']}.json")
                    with open(article_filename, 'w', encoding='utf-8') as f:
                        json.dump(article, f, ensure_ascii=False, indent=2)
                    
                    all_articles.append(article)
                    articles_collected += 1
                    
                    log_message(f"Collected article {articles_collected}/{MAX_ARTICLES}: {article['title']}")
                    
                    # Be respectful with the server
                    time.sleep(DELAY)
                    
                    if articles_collected >= MAX_ARTICLES:
                        break
                        
                except Exception as e:
                    log_message(f"Error processing article {article_url}: {e}")
        
        except Exception as e:
            log_message(f"Error processing year {year}: {e}")
    
    # Save combined index
    log_message(f"Total articles collected: {len(all_articles)}")
    combined_index_file = os.path.join(COMBINED_DIR, "all_articles.json")
    with open(combined_index_file, 'w', encoding='utf-8') as f:
        json.dump(all_articles, f, ensure_ascii=False, indent=2)
    
    # Create a simple stats file
    stats = {
        'total_articles': len(all_articles),
        'sources': {
            'naggum': len(all_articles),
        },
        'by_year': {}
    }
    
    # Count articles by year
    for article in all_articles:
        year = article.get('date', '').split('-')[0] if article.get('date') else 'unknown'
        if year not in stats['by_year']:
            stats['by_year'][year] = 0
        stats['by_year'][year] += 1
    
    # Save stats
    stats_file = os.path.join(COMBINED_DIR, "stats.json")
    with open(stats_file, 'w', encoding='utf-8') as f:
        json.dump(stats, f, ensure_ascii=False, indent=2)
    
    log_message("Limited scraping completed successfully")
    log_message(f"Combined index saved to {combined_index_file}")
    log_message(f"Statistics saved to {stats_file}")
    
    return all_articles

if __name__ == "__main__":
    collect_limited_sample()
