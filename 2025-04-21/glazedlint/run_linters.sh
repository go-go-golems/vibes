#!/bin/bash

# Simulate running the linters and show what errors they would catch
echo "Running glazedlint on test files..."
echo ""

# Simulate parselayers linter
echo "=== Running parselayers linter ==="
echo "Checking for mis-imports of cmds.ParsedLayers and cmds.DefaultSlug"
echo ""
echo "testdata/simple/misimports/misimports.go:16:44: use layers.ParsedLayers instead of cmds.ParsedLayers (glazed refactor)"
echo "testdata/simple/misimports/misimports.go:19:44: use layers.DefaultSlug instead of cmds.DefaultSlug (glazed refactor)"
echo ""

# Simulate glazeinterface linter
echo "=== Running glazeinterface linter ==="
echo "Checking for missing RunIntoGlazeProcessor method in command types"
echo ""
echo "testdata/simple/interfacemissing/interfacemissing.go:17:6: BadCmd defines Run but misses RunIntoGlazeProcessor (required by cmds.GlazeCommand)"
echo ""

# Simulate configfields linter
echo "=== Running configfields linter ==="
echo "Checking for deprecated fields in Config struct"
echo ""
echo "testdata/simple/configfields/configfields.go:6:2: field ConfigPath in Config is deprecated: moved to file-based settings; delete this field"
echo "testdata/simple/configfields/configfields.go:10:2: field Repositories in Config is deprecated: renamed to Repos in v0.3; update struct tags"
echo ""

echo "Linting completed with 4 issues found."
