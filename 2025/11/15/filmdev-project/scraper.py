#!/usr/bin/env python3
"""
Film Development Chart Scraper
Scrapes development times from digitaltruth.com and stores in SQLite database
"""

import requests
from bs4 import BeautifulSoup
import sqlite3
import json
import time
import sys
from urllib.parse import quote_plus

class FilmDevScraper:
    def __init__(self, db_path='filmdev.db'):
        self.db_path = db_path
        self.base_url = 'https://www.digitaltruth.com/devchart.php'
        self.session = requests.Session()
        self.session.headers.update({
            'User-Agent': 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36'
        })
        self.init_database()
    
    def init_database(self):
        """Initialize SQLite database with schema"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        # Create films table
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS films (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                name TEXT UNIQUE NOT NULL
            )
        ''')
        
        # Create developers table
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS developers (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                name TEXT UNIQUE NOT NULL
            )
        ''')
        
        # Create development_times table
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS development_times (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                film_id INTEGER NOT NULL,
                developer_id INTEGER NOT NULL,
                dilution TEXT,
                iso INTEGER,
                time_35mm TEXT,
                time_120 TEXT,
                time_sheet TEXT,
                temp_c TEXT,
                notes TEXT,
                FOREIGN KEY (film_id) REFERENCES films(id),
                FOREIGN KEY (developer_id) REFERENCES developers(id),
                UNIQUE(film_id, developer_id, dilution, iso, temp_c)
            )
        ''')
        
        # Create indexes for faster queries
        cursor.execute('''
            CREATE INDEX IF NOT EXISTS idx_film_name ON films(name)
        ''')
        cursor.execute('''
            CREATE INDEX IF NOT EXISTS idx_developer_name ON developers(name)
        ''')
        cursor.execute('''
            CREATE INDEX IF NOT EXISTS idx_dev_times_film ON development_times(film_id)
        ''')
        cursor.execute('''
            CREATE INDEX IF NOT EXISTS idx_dev_times_developer ON development_times(developer_id)
        ''')
        
        conn.commit()
        conn.close()
        print(f"Database initialized: {self.db_path}")
    
    def get_or_create_film(self, cursor, film_name):
        """Get or create film ID"""
        cursor.execute('SELECT id FROM films WHERE name = ?', (film_name,))
        result = cursor.fetchone()
        
        if result:
            film_id = result[0]
        else:
            cursor.execute('INSERT INTO films (name) VALUES (?)', (film_name,))
            film_id = cursor.lastrowid
        
        return film_id
    
    def get_or_create_developer(self, cursor, dev_name):
        """Get or create developer ID"""
        cursor.execute('SELECT id FROM developers WHERE name = ?', (dev_name,))
        result = cursor.fetchone()
        
        if result:
            dev_id = result[0]
        else:
            cursor.execute('INSERT INTO developers (name) VALUES (?)', (dev_name,))
            dev_id = cursor.lastrowid
        
        return dev_id
    
    def scrape_film_developer_combo(self, film, developer):
        """Scrape development times for a specific film+developer combination"""
        # URL encode the parameters properly - % becomes %25
        film_encoded = quote_plus(film)
        dev_encoded = quote_plus(developer)
        
        url = f"{self.base_url}?Film={film_encoded}&Developer={dev_encoded}&mdc=Search&TempUnits=C"
        
        try:
            response = self.session.get(url, timeout=10)
            response.raise_for_status()
            
            soup = BeautifulSoup(response.content, 'html.parser')
            
            # Find the data table - look for table with development times
            tables = soup.find_all('table')
            table = None
            for t in tables:
                # Check if this table has the right headers
                headers = t.find_all('th') if t.find('th') else t.find_all('td')
                header_text = ' '.join([h.get_text(strip=True) for h in headers[:5]])
                if 'Film' in header_text and 'Developer' in header_text:
                    table = t
                    break
            
            if not table:
                return []
            
            rows = table.find_all('tr')
            if len(rows) < 2:  # No data rows
                return []
            
            data = []
            # Skip header row
            for row in rows[1:]:
                cols = row.find_all('td')
                if len(cols) < 8:
                    continue
                
                # Extract data from columns
                film_name = cols[0].get_text(strip=True)
                dev_name = cols[1].get_text(strip=True)
                dilution = cols[2].get_text(strip=True)
                iso = cols[3].get_text(strip=True)
                time_35mm = cols[4].get_text(strip=True)
                time_120 = cols[5].get_text(strip=True)
                time_sheet = cols[6].get_text(strip=True)
                temp = cols[7].get_text(strip=True)
                
                # Skip empty rows
                if not film_name or not dev_name:
                    continue
                
                # Get notes if available
                notes = ''
                if len(cols) > 8:
                    notes_link = cols[8].find('a')
                    if notes_link:
                        notes = 'Has notes'
                
                # Parse ISO as integer
                try:
                    iso_int = int(iso)
                except:
                    iso_int = 0
                
                data.append({
                    'film': film_name,
                    'developer': dev_name,
                    'dilution': dilution,
                    'iso': iso_int,
                    'time_35mm': time_35mm,
                    'time_120': time_120,
                    'time_sheet': time_sheet,
                    'temp_c': temp,
                    'notes': notes
                })
            
            return data
        
        except Exception as e:
            print(f"Error: {e}")
            return []
    
    def save_development_times(self, conn, cursor, data_list):
        """Save development times to database"""
        saved_count = 0
        for data in data_list:
            film_id = self.get_or_create_film(cursor, data['film'])
            dev_id = self.get_or_create_developer(cursor, data['developer'])
            
            try:
                cursor.execute('''
                    INSERT OR REPLACE INTO development_times 
                    (film_id, developer_id, dilution, iso, time_35mm, time_120, 
                     time_sheet, temp_c, notes)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
                ''', (
                    film_id, dev_id, data['dilution'], data['iso'],
                    data['time_35mm'], data['time_120'], data['time_sheet'],
                    data['temp_c'], data['notes']
                ))
                saved_count += 1
            except sqlite3.IntegrityError:
                pass  # Already exists
        
        conn.commit()
        return saved_count
    
    def scrape_all(self, films_file, developers_file):
        """Scrape all film+developer combinations"""
        with open(films_file) as f:
            films = json.load(f)
        
        with open(developers_file) as f:
            developers = json.load(f)
        
        total = len(films) * len(developers)
        current = 0
        
        print(f"Starting scrape: {len(films)} films × {len(developers)} developers = {total} combinations")
        
        # Keep database connection open
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        for film in films:
            for developer in developers:
                current += 1
                print(f"[{current}/{total}] {film} + {developer}...", end=' ')
                
                data = self.scrape_film_developer_combo(film, developer)
                if data:
                    saved = self.save_development_times(conn, cursor, data)
                    print(f"✓ {saved} entries")
                else:
                    print("✗")
                
                # Be polite to the server
                time.sleep(0.5)
        
        conn.close()
        print("\nScraping complete!")
        self.print_stats()
    
    def print_stats(self):
        """Print database statistics"""
        conn = sqlite3.connect(self.db_path)
        cursor = conn.cursor()
        
        cursor.execute('SELECT COUNT(*) FROM films')
        film_count = cursor.fetchone()[0]
        
        cursor.execute('SELECT COUNT(*) FROM developers')
        dev_count = cursor.fetchone()[0]
        
        cursor.execute('SELECT COUNT(*) FROM development_times')
        time_count = cursor.fetchone()[0]
        
        conn.close()
        
        print(f"\n=== Database Statistics ===")
        print(f"Films: {film_count}")
        print(f"Developers: {dev_count}")
        print(f"Development times: {time_count}")


def main():
    if len(sys.argv) < 3:
        print("Usage: python3 scraper.py <films.json> <developers.json>")
        sys.exit(1)
    
    films_file = sys.argv[1]
    developers_file = sys.argv[2]
    
    scraper = FilmDevScraper()
    scraper.scrape_all(films_file, developers_file)


if __name__ == '__main__':
    main()
