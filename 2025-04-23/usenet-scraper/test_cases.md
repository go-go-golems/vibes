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

### 1.3 Invalid Configuration Test
**Description**: Test handling of invalid configuration files.
**Test File**: `test_invalid_config.yaml`
```yaml
# Invalid configuration (missing required fields)
version: 1.0

# Missing connection section

# Define what to scrape
sources:
  - group: alt.test
    max_posts: 10

# Missing output section
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

### 2.2 Cleaning Transformation Test
**Description**: Test cleaning operations on post content.
**Test File**: `test_cleaning.yaml`
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
  - id: clean_body
    field: body
    operations:
      - strip_quotes
      - remove_signatures
      - normalize_whitespace
    store_as: clean_body

output:
  format: json
  file: test_cleaning.json
  fields:
    - body
    - clean_body
```

### 2.3 Classification Transformation Test
**Description**: Test classification of posts.
**Test File**: `test_classification.yaml`
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
  - id: classify
    field: body
    model: simple_bayes
    categories: [question, announcement, discussion]
    store_as: post_type

output:
  format: json
  file: test_classification.json
  fields:
    - subject
    - body
    - post_type
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

### 3.3 XML Output Test
**Description**: Test XML output format.
**Test File**: `test_xml_output.yaml`
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
  format: xml
  file: test_output.xml
  fields:
    - id
    - author
    - subject
    - body
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

## 5. Web Interface Tests

### 5.1 Web Interface Configuration Test
**Description**: Test web interface configuration.
**Test File**: `test_web_interface.yaml`
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

web_interface:
  enabled: true
  port: 8080
  host: localhost
  auth:
    username: admin
    password: password
```

### 5.2 Web API Tests
**Description**: Test the web API endpoints.
**Test Cases**:
1. GET `/api/status` - Should return server status
2. POST `/api/jobs` - Should create a new job
3. GET `/api/jobs` - Should list all jobs
4. GET `/api/jobs/:id` - Should get a specific job
5. DELETE `/api/jobs/:id` - Should delete a completed job

## 6. Integration Tests

### 6.1 End-to-End Test
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

### 6.2 Real Usenet Server Test
**Description**: Test against a real Usenet server (if available).
**Test File**: `test_real_server.yaml`
```yaml
# UsenetScraper DSL
version: 1.0

connection:
  server: news.eternal-september.org  # Public Usenet server
  port: 119
  ssl: false

sources:
  - group: alt.test
    max_posts: 5
    since: 1d

transformations:
  - id: clean_body
    field: body
    operations:
      - strip_quotes
      - remove_signatures
    store_as: clean_body

output:
  format: json
  file: test_real_server.json
  fields:
    - id
    - author
    - date
    - subject
    - clean_body
```

## 7. Error Handling Tests

### 7.1 Connection Error Test
**Description**: Test handling of connection errors.
**Test File**: `test_connection_error.yaml`
```yaml
# UsenetScraper DSL
version: 1.0

connection:
  server: nonexistent.example.com  # Non-existent server
  port: 119
  timeout: 5
  retries: 2

sources:
  - group: alt.test
    max_posts: 5

output:
  format: json
  file: test_connection_error.json
```

### 7.2 Authentication Error Test
**Description**: Test handling of authentication errors.
**Test File**: `test_auth_error.yaml`
```yaml
# UsenetScraper DSL
version: 1.0

connection:
  server: news.example.com
  port: 119
  auth:
    username: invalid_user
    password: invalid_pass

sources:
  - group: alt.test
    max_posts: 5

output:
  format: json
  file: test_auth_error.json
```

## 8. Performance Tests

### 8.1 Large Dataset Test
**Description**: Test performance with a large number of posts.
**Test File**: `test_large_dataset.yaml`
```yaml
# UsenetScraper DSL
version: 1.0

connection:
  server: news.example.com
  port: 119

sources:
  - group: alt.binaries.test
    max_posts: 1000
    concurrent: 5

output:
  format: json
  file: test_large_dataset.json
  batch_size: 100
```

### 8.2 Multiple Groups Test
**Description**: Test performance when scraping multiple groups.
**Test File**: `test_multiple_groups.yaml`
```yaml
# UsenetScraper DSL
version: 1.0

connection:
  server: news.example.com
  port: 119

sources:
  - group: alt.test.one
    max_posts: 100
  - group: alt.test.two
    max_posts: 100
  - group: alt.test.three
    max_posts: 100
  - group: comp.test.one
    max_posts: 100
  - group: comp.test.two
    max_posts: 100

output:
  format: json
  file: test_multiple_groups.json
```
