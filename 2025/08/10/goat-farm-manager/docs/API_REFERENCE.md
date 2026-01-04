# API Reference

This document provides detailed information about all available commands and their parameters.

## Global Options

All commands support the following global options:

- `--output`: Output format (table, json, csv, yaml) - default: table
- `--help`: Show help information
- `--version`: Show version information

## Commands

### goat

Manage goat records and information.

#### Parameters

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `--tag-id` | string | Yes* | Unique identifier for the goat |
| `--name` | string | No | Name of the goat |
| `--breed` | choice | No | Breed (alpine, nubian, boer, saanen, toggenburg, lamancha, other) |
| `--gender` | choice | No | Gender (male, female) |
| `--birth-date` | string | No | Birth date (YYYY-MM-DD) |
| `--sire-tag` | string | No | Father's tag ID |
| `--dam-tag` | string | No | Mother's tag ID |
| `--status` | choice | No | Status (active, lactating, dry, pregnant, sick, sold, deceased) |
| `--weight` | float | No | Current weight in kg |
| `--color` | string | No | Color description |
| `--notes` | string | No | Additional notes |
| `--filter` | string | No | Filter results by breed, status, or name |
| `--limit` | integer | No | Limit number of results (default: 50) |

*Required when creating a new goat

#### Examples

```bash
# Add a new goat
goat-manager goat --tag-id "G001" --name "Bella" --breed "alpine" --gender "female" --birth-date "2023-03-15"

# List all goats
goat-manager goat

# Filter goats by breed
goat-manager goat --filter "alpine"

# Update goat weight
goat-manager goat --tag-id "G001" --weight 67.5
```

### milk

Record and track milk production.

#### Parameters

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `--goat-tag` | string | Yes | Tag ID of the goat |
| `--milking-time` | string | No | Date and time (YYYY-MM-DD HH:MM, defaults to now) |
| `--session` | choice | Yes | Milking session (morning, evening) |
| `--volume` | float | Yes | Volume in liters |
| `--fat-content` | float | No | Fat content percentage |
| `--protein-content` | float | No | Protein content percentage |
| `--somatic-cell-count` | float | No | Somatic cell count (cells/ml) |
| `--quality-grade` | choice | Yes | Quality grade (A, B, C, reject) |
| `--milked-by` | string | No | Person who performed milking |
| `--notes` | string | No | Additional notes |
| `--date-from` | string | No | Filter from date (YYYY-MM-DD) |
| `--date-to` | string | No | Filter to date (YYYY-MM-DD) |
| `--limit` | integer | No | Limit number of results (default: 50) |

#### Examples

```bash
# Record morning milking
goat-manager milk --goat-tag "G001" --volume 3.2 --session "morning" --quality-grade "A" --milked-by "John"

# View milk records for a goat
goat-manager milk --goat-tag "G001" --date-from "2024-01-01"

# List recent milk records
goat-manager milk --limit 20
```

### health

Manage health records and veterinary care.

#### Parameters

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `--goat-tag` | string | Yes | Tag ID of the goat |
| `--record-date` | string | No | Date of record (YYYY-MM-DD, defaults to today) |
| `--record-type` | choice | Yes | Type (vaccination, treatment, checkup, injury, illness, medication, deworming, hoof_trim) |
| `--description` | string | Yes | Description of the health event |
| `--veterinarian` | string | No | Veterinarian name |
| `--medication` | string | No | Medication administered |
| `--dosage` | string | No | Medication dosage |
| `--temperature` | float | No | Body temperature in Celsius |
| `--weight` | float | No | Weight at time of record in kg |
| `--next-due-date` | string | No | Next due date (YYYY-MM-DD) |
| `--cost` | float | No | Cost of treatment |
| `--notes` | string | No | Additional notes |
| `--limit` | integer | No | Limit number of results (default: 50) |

#### Examples

```bash
# Record vaccination
goat-manager health --goat-tag "G001" --record-type "vaccination" --description "Annual CDT vaccination" --veterinarian "Dr. Smith"

# Record illness and treatment
goat-manager health --goat-tag "G002" --record-type "illness" --description "Respiratory infection" --medication "Antibiotics" --dosage "5ml twice daily"

# View health history
goat-manager health --goat-tag "G001"
```

### breeding

Track breeding activities and kidding events.

#### Parameters

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `--doe-tag` | string | Yes | Tag ID of the female goat |
| `--buck-tag` | string | Yes | Tag ID of the male goat |
| `--breeding-date` | string | No | Date of breeding (YYYY-MM-DD, defaults to today) |
| `--expected-kidding-date` | string | No | Expected kidding date (YYYY-MM-DD) |
| `--actual-kidding-date` | string | No | Actual kidding date (YYYY-MM-DD) |
| `--kids-born` | integer | No | Number of kids born |
| `--kids-alive` | integer | No | Number of kids that survived |
| `--breeding-method` | choice | No | Method (natural, artificial_insemination) |
| `--status` | choice | No | Status (bred, confirmed_pregnant, kidded, failed, aborted) |
| `--complications` | string | No | Any complications |
| `--notes` | string | No | Additional notes |
| `--limit` | integer | No | Limit number of results (default: 50) |

#### Examples

```bash
# Record breeding
goat-manager breeding --doe-tag "G001" --buck-tag "G002" --breeding-date "2024-01-15"

# Update with kidding information
goat-manager breeding --doe-tag "G001" --buck-tag "G002" --actual-kidding-date "2024-06-15" --kids-born 2 --kids-alive 2 --status "kidded"

# View breeding records
goat-manager breeding --status "pregnant"
```

### feed

Record feeding activities and track feed consumption.

#### Parameters

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `--feeding-date` | string | No | Date and time (YYYY-MM-DD HH:MM, defaults to now) |
| `--feed-type` | choice | Yes | Type (hay, grain, pellets, pasture, silage, supplements, treats, other) |
| `--feed-name` | string | No | Specific name or brand |
| `--quantity` | float | Yes | Quantity of feed |
| `--unit` | choice | No | Unit (kg, lbs, cups, scoops, bales) - default: kg |
| `--goat-tags` | string | No | Comma-separated goat tags (empty for group feeding) |
| `--feeding-method` | choice | No | Method (individual, group, pasture) - default: group |
| `--fed-by` | string | Yes | Person who performed feeding |
| `--cost-per-unit` | float | No | Cost per unit |
| `--notes` | string | No | Additional notes |
| `--limit` | integer | No | Limit number of results (default: 50) |

#### Examples

```bash
# Record group feeding
goat-manager feed --feed-type "hay" --quantity 25 --unit "kg" --fed-by "John" --feeding-method "group"

# Record individual feeding
goat-manager feed --feed-type "grain" --quantity 2 --unit "kg" --goat-tags "G001,G002" --fed-by "Jane" --feeding-method "individual"

# View feed records
goat-manager feed --feed-type "grain"
```

### farm

Log general farm operations and activities.

#### Parameters

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `--operation-date` | string | No | Date of operation (YYYY-MM-DD, defaults to today) |
| `--operation-type` | choice | Yes | Type (feeding, milking, cleaning, maintenance, vaccination_batch, deworming_batch, hoof_trimming, pasture_rotation, equipment_maintenance, feed_purchase, supply_purchase, milk_sale, goat_sale, other) |
| `--description` | string | Yes | Description of the operation |
| `--performed-by` | string | Yes | Person who performed the operation |
| `--affected-goats` | string | No | Comma-separated goat tags |
| `--quantity` | float | No | Quantity involved |
| `--unit` | string | No | Unit of measurement |
| `--cost` | float | No | Cost associated |
| `--revenue` | float | No | Revenue generated |
| `--supplier-buyer` | string | No | Supplier or buyer involved |
| `--notes` | string | No | Additional notes |
| `--limit` | integer | No | Limit number of results (default: 50) |

#### Examples

```bash
# Log cleaning operation
goat-manager farm --operation-type "cleaning" --description "Deep clean milking parlor" --performed-by "Team"

# Log feed purchase
goat-manager farm --operation-type "feed_purchase" --description "Alfalfa hay purchase" --quantity 100 --unit "bales" --cost 500.00 --supplier-buyer "Local Feed Store" --performed-by "Manager"

# View operations
goat-manager farm --operation-type "maintenance"
```

### analytics

Generate analytics reports and insights.

#### Parameters

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `--report-type` | choice | Yes | Type (farm-summary, milk-production, health-summary, breeding-summary, feed-consumption, goat-performance) |
| `--goat-tag` | string | No | Specific goat for individual reports |
| `--date-from` | string | No | Start date (YYYY-MM-DD) |
| `--date-to` | string | No | End date (YYYY-MM-DD) |
| `--limit` | integer | No | Limit number of results (default: 50) |

#### Examples

```bash
# Farm summary
goat-manager analytics --report-type "farm-summary"

# Milk production analysis
goat-manager analytics --report-type "milk-production" --date-from "2024-01-01" --date-to "2024-01-31"

# Individual goat performance
goat-manager analytics --report-type "goat-performance" --goat-tag "G001"
```

### version

Version control operations for data management.

#### Parameters

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `--action` | choice | Yes | Action (commit, branch, switch, log, diff, branches, status, backup, restore, merge, compare) |
| `--branch` | string | No | Branch name for operations |
| `--message` | string | No | Commit message |
| `--limit` | integer | No | Limit results (default: 10) |
| `--table` | string | No | Table name for diff operations |
| `--from-commit` | string | No | From commit for diff (default: HEAD~1) |
| `--to-commit` | string | No | To commit for diff (default: HEAD) |

#### Examples

```bash
# Commit changes
goat-manager version --action "commit" --message "Updated goat records"

# Create branch
goat-manager version --action "branch" --branch "breeding-season-2024"

# Switch branch
goat-manager version --action "switch" --branch "breeding-season-2024"

# View commit history
goat-manager version --action "log" --limit 10

# Create backup
goat-manager version --action "backup" --message "Before major updates"

# Compare branches
goat-manager version --action "compare" --branch "breeding-season-2024" --from-commit "main"

# View branch status
goat-manager version --action "status" --branch "main"
```

## Output Formats

### Table Format (Default)
Human-readable tabular output suitable for terminal viewing.

### JSON Format
Machine-readable JSON format for integration with other tools:
```bash
goat-manager goat --output json
```

### CSV Format
Comma-separated values format for spreadsheet applications:
```bash
goat-manager milk --output csv > milk_records.csv
```

### YAML Format
YAML format for configuration and data exchange:
```bash
goat-manager analytics --report-type "farm-summary" --output yaml
```

## Error Handling

Commands return appropriate exit codes:
- `0`: Success
- `1`: General error
- `2`: Invalid arguments
- `3`: Database error
- `4`: Version control error

Error messages are written to stderr and include context about the failure.

## Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `GOAT_FARM_DB_PATH` | Database directory path | `~/goat-farm-data` |
| `GOAT_FARM_DB_NAME` | Database name | `goat_farm` |
| `GOAT_FARM_COMMIT_NAME` | Default commit author name | System username |
| `GOAT_FARM_COMMIT_EMAIL` | Default commit author email | `user@localhost` |

## Data Types

### Date Formats
- Date: `YYYY-MM-DD` (e.g., `2024-01-15`)
- DateTime: `YYYY-MM-DD HH:MM` (e.g., `2024-01-15 14:30`)

### Numeric Formats
- Weight: Decimal in kilograms (e.g., `65.5`)
- Volume: Decimal in liters (e.g., `3.2`)
- Temperature: Decimal in Celsius (e.g., `38.5`)
- Cost: Decimal in local currency (e.g., `25.50`)

### String Formats
- Tag IDs: Alphanumeric identifiers (e.g., `G001`, `BELLA2024`)
- Names: UTF-8 text strings
- Notes: Free-form text with no length limit

