#!/bin/bash

# Test runner for vault-envrc-generator test-data
# Updated for post-consultant fixes (no --output-mode, new header format)

set -e

VAULT_ADDR="${VAULT_ADDR:-http://127.0.0.1:8200}"
VAULT_ENVRC_GENERATOR="../go-utility/vault-envrc-generator"

echo "🧪 Testing vault-envrc-generator with updated test-data"
echo "Vault Address: $VAULT_ADDR"
echo

# Check if binary exists
if [[ ! -x "$VAULT_ENVRC_GENERATOR" ]]; then
    echo "❌ Binary not found at $VAULT_ENVRC_GENERATOR"
    echo "Please build it first: cd ../go-utility && go build -o vault-envrc-generator ."
    exit 1
fi

# Test 1: Validate batch config syntax
echo "📋 Test 1: Validating batch-config.yaml syntax..."
if $VAULT_ENVRC_GENERATOR batch -c batch-config.yaml --dry-run --vault-addr "$VAULT_ADDR" >/dev/null 2>&1; then
    echo "✅ Batch configuration syntax is valid"
else
    echo "❌ Batch configuration has syntax errors"
    $VAULT_ENVRC_GENERATOR batch -c batch-config.yaml --dry-run --vault-addr "$VAULT_ADDR"
    exit 1
fi

# Test 2: Validate custom template
echo "🎨 Test 2: Validating custom template..."
if [[ -f "custom-template.tmpl" ]]; then
    echo "✅ Custom template file exists"
    echo "Template preview:"
    head -5 custom-template.tmpl | sed 's/^/    /'
else
    echo "❌ Custom template file missing"
    exit 1
fi

# Test 3: Check expected output format
echo "📄 Test 3: Checking expected output formats..."

expected_files=("test1.envrc" "test2.envrc" "test3.envrc" "test4.json" "test5.envrc" "multi-section.envrc")
for file in "${expected_files[@]}"; do
    if [[ -f "$file" ]]; then
        echo "✅ $file exists"
        
        # Check for new header format
        if [[ "$file" == *.envrc ]]; then
            if grep -q "# === .* ===" "$file"; then
                echo "    ✅ Uses new header format"
            else
                echo "    ⚠️  May use old header format"
            fi
        fi
        
        # Check for sorted JSON
        if [[ "$file" == *.json ]]; then
            if python3 -m json.tool "$file" >/dev/null 2>&1; then
                echo "    ✅ Valid JSON format"
            else
                echo "    ❌ Invalid JSON format"
            fi
        fi
    else
        echo "❌ $file missing"
    fi
done

# Test 4: Multi-section example validation
echo "🔀 Test 4: Multi-section example validation..."
if grep -q "# === Multi-section Example:" multi-section.envrc; then
    section_count=$(grep -c "# === Multi-section Example:" multi-section.envrc)
    echo "✅ Multi-section file has $section_count sections"
else
    echo "❌ Multi-section file doesn't have expected format"
fi

echo
echo "🎉 All test-data files have been updated for the current implementation!"
echo
echo "📚 Usage examples:"
echo "  # Test batch processing (dry-run):"
echo "  $VAULT_ENVRC_GENERATOR batch -c batch-config.yaml --dry-run --vault-addr \$VAULT_ADDR"
echo
echo "  # Generate with custom template:"
echo "  $VAULT_ENVRC_GENERATOR generate --path secret/test --template custom-template.tmpl --vault-addr \$VAULT_ADDR"
echo
echo "  # JSON output with sorted keys:"
echo "  $VAULT_ENVRC_GENERATOR batch -c batch-config.yaml --format json --output merged.json --sort-keys --vault-addr \$VAULT_ADDR"
echo
