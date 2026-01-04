#!/bin/bash
# Run interesting graph queries and save results

echo "Running Cayley Graph Queries..."
echo "================================"
echo ""

echo "1. Graph Statistics" | tee queries_output.txt
./graph-query stats | tee -a queries_output.txt
echo "" | tee -a queries_output.txt

echo "2. Jeffrey Epstein's Relationships (first 20)" | tee -a queries_output.txt
./graph-query query "Jeffrey Epstein" | head -40 | tee -a queries_output.txt
echo "" | tee -a queries_output.txt

echo "3. Alan Dershowitz's Relationships (first 15)" | tee -a queries_output.txt
./graph-query query "Alan M. Dershowitz" | head -30 | tee -a queries_output.txt
echo "" | tee -a queries_output.txt

echo "4. Donald Trump's Network" | tee -a queries_output.txt
./graph-query neighbors "Donald J. Trump" | tee -a queries_output.txt
echo "" | tee -a queries_output.txt

echo "5. Prince Andrew's Connections" | tee -a queries_output.txt
./graph-query neighbors "Prince Andrew" | tee -a queries_output.txt
echo "" | tee -a queries_output.txt

echo "6. Ghislaine Maxwell's Network" | tee -a queries_output.txt
./graph-query neighbors "Ghislaine Maxwell" | tee -a queries_output.txt
echo "" | tee -a queries_output.txt

echo "All queries saved to queries_output.txt"
