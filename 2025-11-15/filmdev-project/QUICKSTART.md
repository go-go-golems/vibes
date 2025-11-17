# Quick Start Guide

## Get Started in 3 Steps

### Step 1: Scrape the Data (5-6 minutes)

```bash
cd filmdev-project
python3 scraper.py common_films.json common_developers.json
```

This will create `filmdev.db` with 3,308 development time entries covering 33 films and 37 developers.

### Step 2: Build the CLI Tool

```bash
cd filmdev-cli
CGO_ENABLED=1 go build -o filmdev
```

**Note:** Requires gcc/build-essential. Install with:
```bash
sudo apt-get install build-essential
```

### Step 3: Start Querying!

```bash
# Find Tri-X 400 development times with D-76
./filmdev query --film "Tri-X" --developer "D-76" --iso 400

# List all available films
./filmdev list-films

# Export HP5 data to CSV
./filmdev query --film "HP5" --output csv > hp5.csv
```

## Common Queries

### By Film Name
```bash
./filmdev query --film "Tri-X"
./filmdev query --film "HP5"
./filmdev query --film "Delta 400"
```

### By Developer
```bash
./filmdev query --developer "D-76"
./filmdev query --developer "HC-110"
./filmdev query --developer "Rodinal"
```

### By Film + Developer
```bash
./filmdev query --film "Tri-X" --developer "D-76"
./filmdev query --film "HP5" --developer "ID-11"
```

### By Film + Developer + ISO
```bash
./filmdev query --film "Tri-X" --developer "D-76" --iso 400
./filmdev query --film "TMax 400" --developer "HC-110" --iso 400
```

## Output Formats

### Table (default)
```bash
./filmdev query --film "Tri-X" --developer "D-76"
```

### JSON
```bash
./filmdev query --film "HP5" --output json
```

### CSV
```bash
./filmdev query --film "Delta 400" --output csv
```

### YAML
```bash
./filmdev query --film "TMax 400" --output yaml
```

## Tips

1. **Partial matching works:** `--film "Tri-X"` matches "Kodak Tri-X 400"
2. **Case insensitive:** `--film "tri-x"` works just as well
3. **Use quotes for multi-word names:** `--film "Delta 400"`
4. **Pipe to files:** `./filmdev query --film "HP5" --output csv > hp5.csv`
5. **Select fields:** `--fields film,developer,iso,time_35mm`

## Database Location

By default, the tool looks for the database at:
```
~/filmdev-project/filmdev.db
```

To use a different location:
```bash
./filmdev query --film "Tri-X" --db-path /path/to/filmdev.db
```

## Next Steps

- Read the full [README.md](README.md) for detailed documentation
- Run `./filmdev --help` to see all available commands
- Run `./filmdev query --help` to see all query options
- Check out `examples.sh` for more usage examples
