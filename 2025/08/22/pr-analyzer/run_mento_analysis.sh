#!/usr/bin/env bash
set -euo pipefail

# Config
REPO_PATH=${REPO_PATH:-/home/manuel/code/mento/go-go-mento}
DB_PATH=${DB_PATH:-$(pwd)/pr-analyzer.sqlite}
BIN=${BIN:-$(pwd)/pr-analyzer}

COMMITS=(
  4f8e1ca1bc662aa40ca36abcff453c48d315a5f6
  34db989bf53675e85f8019602042a02535970b0e
  192dd362aac1dbd4bec535ca4aebba643f21640c
  bb6072251edc9cce4faf23e5072cef14b0b567d0
  735523f2609390482c74b7a01f9079a63c5c0f6b
  1e5193876e4154a9b4bc05de58b36464c3e37e0d
  57e6f0d549da1f57505fa3186aa9303cd539aeb8
  0770097aaae54fe76473cca9a6c1a2be6e0d91db
  cb83c5d82132c451970ee7fe6f4859eca5375cfd
  0174347ad849e46b58bf92ba05fd6c5a99c5204f
  c43b99995d1af331e6354b480a9f343cc4cb42f9
  c8a7ba2bcaa52ab96d53e516e6ea5aed061b797a
  1d0880a080cdd0d85b05aadb5e370752d827991a
  7b4f78b8bb26100c6d12839f319f11b97f93458b
  df3d3c677073d5772797bbd496776ee17936133f
  118bec877e6d5bc1bf0b704a7601f63319e0a456
)

echo "Building pr-analyzer binary..."
go build -o "$BIN" ./

echo "Initializing database at $DB_PATH"
"$BIN" db init --db-path "$DB_PATH"

echo "Analyzing commits for repo $REPO_PATH"
for h in "${COMMITS[@]}"; do
  echo "- Analyzing commit $h"
  "$BIN" analyze --repo-path "$REPO_PATH" --commit "$h" --use-defaults --save-to-db --db-path "$DB_PATH"
done

echo "==== SUMMARY (filtered) ===="
"$BIN" db summary --db-path "$DB_PATH" --repo-contains go-go-mento --since 2025-08-18T00:00:00Z --until 2025-08-22T00:00:00Z

echo "==== LANGUAGES (global) ===="
"$BIN" db languages --db-path "$DB_PATH"

echo "==== SYSTEMS (global) ===="
"$BIN" db systems --db-path "$DB_PATH"

echo "==== PRS (filtered, newest first) ===="
"$BIN" db prs --db-path "$DB_PATH" --repo-contains go-go-mento --since 2025-08-18T00:00:00Z --until 2025-08-22T00:00:00Z --order-by analyzed_at --desc

echo "Done. Database at $DB_PATH"


