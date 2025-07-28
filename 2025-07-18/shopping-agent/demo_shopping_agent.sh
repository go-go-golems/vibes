#!/bin/bash

echo "=== Shopping Agent Demonstration ==="
echo "Built with Go and go-go-golems/glazed library"
echo ""

echo "1. Product Search Demo:"
./shop search --query "laptop" --max-results 3
echo ""

echo "2. Product Comparison Demo:"
./shop compare --products "MacBook Air,Dell XPS 13" --compare-sort-by price --max-results 2
echo ""

echo "3. Screenshot Demo - Amazon Homepage:"
./shop screenshot --url "https://amazon.com" --wait 3
echo ""

echo "4. Screenshot Demo - eBay Homepage:"
./shop screenshot --url "https://ebay.com" --wait 3
echo ""

echo "5. Full Page Screenshot Demo:"
./shop screenshot --url "https://example.com" --full-page true --wait 2
echo ""

echo "=== Demo Complete ==="
echo "Check the generated screenshot files:"
ls -la screenshot_*.png
