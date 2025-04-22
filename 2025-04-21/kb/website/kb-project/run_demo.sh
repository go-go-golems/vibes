#!/bin/bash

# Script to demonstrate the KB system functionality and capture logs

LOG_FILE="kb_demo_logs.txt"

echo "KB System Demonstration" > $LOG_FILE
echo "======================" >> $LOG_FILE
echo "" >> $LOG_FILE

echo "1. Showing help information" >> $LOG_FILE
echo "-------------------------" >> $LOG_FILE
./kb --help >> $LOG_FILE 2>&1
echo "" >> $LOG_FILE

echo "2. Initializing the knowledge base" >> $LOG_FILE
echo "-------------------------------" >> $LOG_FILE
./kb init --path ~/.kb/index --dim 768 >> $LOG_FILE 2>&1
echo "" >> $LOG_FILE

echo "3. Adding a repository to track" >> $LOG_FILE
echo "----------------------------" >> $LOG_FILE
./kb add --path /home/ubuntu/kb-project >> $LOG_FILE 2>&1
echo "" >> $LOG_FILE

echo "4. Indexing the repository" >> $LOG_FILE
echo "-----------------------" >> $LOG_FILE
./kb index >> $LOG_FILE 2>&1
echo "" >> $LOG_FILE

echo "5. Generating embeddings" >> $LOG_FILE
echo "----------------------" >> $LOG_FILE
./kb embed >> $LOG_FILE 2>&1
echo "" >> $LOG_FILE

echo "6. Searching for code by text" >> $LOG_FILE
echo "---------------------------" >> $LOG_FILE
./kb search --query "index repository" >> $LOG_FILE 2>&1
echo "" >> $LOG_FILE

echo "7. Searching for code by similarity" >> $LOG_FILE
echo "--------------------------------" >> $LOG_FILE
./kb sim --query "vector embeddings" >> $LOG_FILE 2>&1
echo "" >> $LOG_FILE

echo "8. Asking a question about the code" >> $LOG_FILE
echo "--------------------------------" >> $LOG_FILE
./kb ask --question "How does the indexing process work?" >> $LOG_FILE 2>&1
echo "" >> $LOG_FILE

echo "9. Showing details of a code chunk" >> $LOG_FILE
echo "-------------------------------" >> $LOG_FILE
./kb show --file /home/ubuntu/kb-project/pkg/index/indexer.go >> $LOG_FILE 2>&1
echo "" >> $LOG_FILE

echo "Demo completed. See $LOG_FILE for detailed logs."
