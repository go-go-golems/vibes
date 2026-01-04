#!/bin/bash
# Film Development Database Query Examples

echo "=== Example 1: Query Kodak Tri-X 400 with D-76 at ISO 400 ==="
./filmdev query --film "Tri-X" --developer "D-76" --iso 400

echo ""
echo "=== Example 2: List all films ==="
./filmdev list-films | head -15

echo ""
echo "=== Example 3: Find all HP5 development options ==="
./filmdev query --film "HP5" | head -15

echo ""
echo "=== Example 4: JSON output for TMax 400 with HC-110 ==="
./filmdev query --film "TMax 400" --developer "HC-110" --output json | head -20

echo ""
echo "=== Example 5: CSV output for all Ilford Delta 400 ==="
./filmdev query --film "Delta 400" --output csv | head -10

echo ""
echo "=== Example 6: Select specific fields ==="
./filmdev query --film "Tri-X" --developer "D-76" --fields film,dilution,iso,time_35mm,temp_c | head -10
