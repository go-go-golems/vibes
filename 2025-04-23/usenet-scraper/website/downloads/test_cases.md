# Test Cases for UsenetScraper YAML DSL Implementation

This document outlines test cases for verifying the functionality of the UsenetScraper YAML DSL implementation.

## 1. Configuration Parsing Tests

### 1.1 Basic Configuration Test
**Description**: Test parsing a basic valid configuration file.
**Test File**: `test_basic_config.yaml`
```yaml
# UsenetScraper DSL
version: 1.0

# Connection settings
connection:
  server: news.example.com
  port: 119
  ssl: false
  auth:
    username: testuser
    password: testpass

# Define what to scrape
sources:
  - group: alt.test
    max_posts: 10
    since: 1d

# Output configuration
output:
  format: json
  file: test_output.json
  fields:
    - id
    - author
    - subject
    - body
```

### 1.2 Environment Variable Substitution Test
**Description**: Test environment variable substitution in configuration.
**Test File**: `test_env_vars.yaml`
```yaml
# UsenetScraper DSL
version: 1.0

# Connection settings with environment variables
connection:
  server: news.example.com
  port: 119
  ssl: false
  auth:
    username: ${USENET_USER}
    password: ${USENET_PASS}

# Define what to scrape
sources:
  - group: alt.test
    max_posts: 10
    since: 1d

# Output configuration
output:
  format: json
  file: test_output.json
```

## 2. Transformation Tests

### 2.1 Extraction Transformation Test
**Description**: Test extraction of data using regex patterns.
**Test File**: `test_extraction.yaml`
```yaml
# UsenetScraper DSL
version: 1.0

connection:
  server: news.example.com
  port: 119

sources:
  - group: alt.test
    max_posts: 5

transformations:
  - id: extract_header
    field: subject
    pattern: '\[(.*?)\]'
    store_as: category
    
  - id: extract_urls
    field: body
    pattern: 'https?://[^\s]+'
    store_as: links

output:
  format: json
  file: test_extraction.json
  fields:
    - subject
    - category
    - links
```

## 3. Output Format Tests

### 3.1 JSON Output Test
**Description**: Test JSON output format.
**Test File**: `test_json_output.yaml`
```yaml
# UsenetScraper DSL
version: 1.0

connection:
  server: news.example.com
  port: 119

sources:
  - group: alt.test
    max_posts: 5

output:
  format: json
  file: test_output.json
  pretty: true
  fields:
    - id
    - author
    - subject
    - body
```

### 3.2 CSV Output Test
**Description**: Test CSV output format.
**Test File**: `test_csv_output.yaml`
```yaml
# UsenetScraper DSL
version: 1.0

connection:
  server: news.example.com
  port: 119

sources:
  - group: alt.test
    max_posts: 5

output:
  format: csv
  file: test_output.csv
  fields:
    - id
    - author
    - subject
    - date
```

## 4. Concurrency and Rate Limiting Tests

### 4.1 Concurrent Scraping Test
**Description**: Test concurrent scraping of multiple groups.
**Test File**: `test_concurrency.yaml`
```yaml
# UsenetScraper DSL
version: 1.0

connection:
  server: news.example.com
  port: 119

sources:
  - group: alt.test.one
    max_posts: 10
    concurrent: 3
  - group: alt.test.two
    max_posts: 10
    concurrent: 2

output:
  format: json
  file: test_concurrency.json
```

### 4.2 Rate Limiting Test
**Description**: Test rate limiting for scraping.
**Test File**: `test_rate_limiting.yaml`
```yaml
# UsenetScraper DSL
version: 1.0

connection:
  server: news.example.com
  port: 119

sources:
  - group: alt.test
    max_posts: 20
    rate_limit: 5/minute

output:
  format: json
  file: test_rate_limiting.json
```

## 5. Integration Tests

### 5.1 End-to-End Test
**Description**: Test the entire pipeline from configuration to output.
**Test File**: `test_integration.yaml`
```yaml
# UsenetScraper DSL
version: 1.0

connection:
  server: news.example.com
  port: 119
  ssl: true
  auth:
    username: testuser
    password: testpass

sources:
  - group: alt.test
    max_posts: 10
    since: 1d
    filter: "subject: 'test'"

transformations:
  - id: extract_header
    field: subject
    pattern: '\[(.*?)\]'
    store_as: category
    
  - id: clean_body
    field: body
    operations:
      - strip_quotes
      - remove_signatures
    store_as: clean_body
    
  - id: classify
    field: clean_body
    model: simple_bayes
    categories: [question, announcement, discussion]
    store_as: post_type

output:
  format: json
  file: test_integration.json
  fields:
    - id
    - author
    - date
    - subject
    - category
    - clean_body
    - post_type
  pretty: true
```
