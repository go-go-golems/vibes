#!/bin/bash
set -e

echo "========================================"
echo "TTMP CLI Test Suite"
echo "========================================"
echo ""

TTMP=~/ttmp-cli/ttmp

echo "1. Testing vocabulary commands..."
$TTMP vocab list --category topics --output json > /tmp/topics.json
echo "   ✓ Listed topics (JSON)"
$TTMP vocab list --category docTypes > /dev/null
echo "   ✓ Listed docTypes (table)"
$TTMP vocab list --category intent > /dev/null
echo "   ✓ Listed intent"
echo ""

echo "2. Testing init command..."
$TTMP init MEN-9001 --title "Test ticket one" --topics testing,backend > /dev/null
echo "   ✓ Created MEN-9001"
$TTMP init MEN-9002 --title "Test ticket two" --topics frontend,testing --owners alice > /dev/null
echo "   ✓ Created MEN-9002"
echo ""

echo "3. Testing list commands..."
TICKET_COUNT=$($TTMP list tickets --output json | grep -c '"ticket"')
echo "   ✓ Found $TICKET_COUNT tickets"
DOC_COUNT=$($TTMP list docs --output json | grep -c '"file"')
echo "   ✓ Found $DOC_COUNT documents"
echo ""

echo "4. Testing relate command..."
$TTMP relate --ticket MEN-9001 --files src/test.go,src/main.go > /dev/null
echo "   ✓ Added related files to MEN-9001"
echo ""

echo "5. Testing meta update command..."
$TTMP meta update --doc ttmp/MEN-9001-test-ticket-one/index.md --field Status --value active > /dev/null
echo "   ✓ Updated status to active"
$TTMP meta update --doc ttmp/MEN-9002-test-ticket-two/index.md --field Intent --value long-term > /dev/null
echo "   ✓ Updated intent to long-term"
echo ""

echo "6. Testing doctor command..."
$TTMP doctor --output json > /tmp/doctor-results.json
ISSUE_COUNT=$(cat /tmp/doctor-results.json | grep -c '"severity"' || echo "0")
echo "   ✓ Doctor found $ISSUE_COUNT issues"
echo ""

echo "7. Testing help system..."
$TTMP help introduction > /dev/null
echo "   ✓ Help: introduction"
$TTMP help tutorial-basic-workflow > /dev/null
echo "   ✓ Help: tutorial-basic-workflow"
$TTMP help commands-reference > /dev/null
echo "   ✓ Help: commands-reference"
$TTMP help metadata-schema > /dev/null
echo "   ✓ Help: metadata-schema"
$TTMP help vocabulary-guide > /dev/null
echo "   ✓ Help: vocabulary-guide"
echo ""

echo "========================================"
echo "All tests passed!"
echo "========================================"
