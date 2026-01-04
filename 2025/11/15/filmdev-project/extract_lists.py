#!/usr/bin/env python3
"""
Extract film and developer lists from digitaltruth.com
"""

import requests
from bs4 import BeautifulSoup
import json

def extract_lists():
    """Extract all films and developers from the website"""
    url = "https://www.digitaltruth.com/devchart.php"
    
    print("Fetching page...")
    response = requests.get(url)
    response.raise_for_status()
    
    soup = BeautifulSoup(response.content, 'html.parser')
    
    # Find film dropdown
    film_select = soup.find('select', {'name': 'Film'})
    films = []
    if film_select:
        for option in film_select.find_all('option'):
            value = option.get('value', '')
            if value and value != 'All Films' and not value.startswith('--'):
                films.append(value)
    
    # Find developer dropdown
    dev_select = soup.find('select', {'name': 'Developer'})
    developers = []
    if dev_select:
        for option in dev_select.find_all('option'):
            value = option.get('value', '')
            if value and value != 'All Developers' and not value.startswith('--'):
                developers.append(value)
    
    print(f"Found {len(films)} films")
    print(f"Found {len(developers)} developers")
    
    # Save to JSON files
    with open('/home/ubuntu/filmdev-project/films.json', 'w') as f:
        json.dump(films, f, indent=2)
    
    with open('/home/ubuntu/filmdev-project/developers.json', 'w') as f:
        json.dump(developers, f, indent=2)
    
    print("\nSaved lists to films.json and developers.json")
    
    return films, developers

if __name__ == '__main__':
    films, developers = extract_lists()
    
    print("\n=== Sample Films ===")
    for film in films[:10]:
        print(f"  - {film}")
    
    print("\n=== Sample Developers ===")
    for dev in developers[:10]:
        print(f"  - {dev}")
