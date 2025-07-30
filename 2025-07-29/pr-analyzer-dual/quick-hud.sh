#!/bin/bash

# Quick HUD - A simplified version for demos
# Usage: ./quick-hud.sh [owner] [repo] [pr-number]

OWNER=${1:-go-go-golems}
REPO=${2:-glazed}
PR_NUMBER=${3:-483}
SESSION_NAME="quick-hud-${PR_NUMBER}"

echo "🚀 Quick HUD for ${OWNER}/${REPO}#${PR_NUMBER}"

# Kill existing session
tmux has-session -t "$SESSION_NAME" 2>/dev/null && tmux kill-session -t "$SESSION_NAME"

# Create new session with 2x2 grid
tmux new-session -d -s "$SESSION_NAME"

# Create 4-pane layout
tmux split-window -h    # Split horizontally
tmux split-window -v    # Split right pane vertically  
tmux select-pane -t 0
tmux split-window -v    # Split left pane vertically

# Set pane titles
tmux select-pane -t 0 -T "📊 DASHBOARD"
tmux select-pane -t 1 -T "🔧 FUNCTIONS"  
tmux select-pane -t 2 -T "📁 FILES"
tmux select-pane -t 3 -T "📝 COMMITS"

# Pane 0: Dashboard
tmux send-keys -t 0 "
sqlite3 pr_analysis.db << 'SQL'
.mode box
.headers on
SELECT 'METRIC' as Category, 'VALUE' as Value, 'IMPACT' as Impact
UNION ALL
SELECT 
    'Functions Changed',
    CAST(SUM(is_changed) AS TEXT) || '/' || CAST(COUNT(*) AS TEXT) || 
    ' (' || CAST(ROUND(100.0 * SUM(is_changed) / COUNT(*), 1) AS TEXT) || '%)',
    CASE 
        WHEN ROUND(100.0 * SUM(is_changed) / COUNT(*), 1) > 40 THEN 'HIGH RISK'
        WHEN ROUND(100.0 * SUM(is_changed) / COUNT(*), 1) > 20 THEN 'MEDIUM RISK'
        ELSE 'LOW RISK'
    END
FROM functions
UNION ALL
SELECT 
    'Most Impacted File',
    file_name || ' (' || CAST(changed_functions AS TEXT) || '/' || CAST(total_functions AS TEXT) || ')',
    CASE WHEN change_rate > 75 THEN 'CRITICAL' ELSE 'HIGH' END
FROM file_analysis 
WHERE changed_functions = (SELECT MAX(changed_functions) FROM file_analysis)
UNION ALL
SELECT 
    'Entry Points',
    CAST(COUNT(*) AS TEXT) || ' main() functions',
    'INTEGRATION RISK'
FROM critical_changes 
WHERE change_category LIKE 'CRITICAL%';
SQL
" Enter

# Pane 1: Changed Functions
tmux send-keys -t 1 "
sqlite3 pr_analysis.db << 'SQL'
.mode table
.headers on
SELECT 
    SUBSTR(function_name, 1, 25) as Function,
    SUBSTR(change_category, 1, 15) as Category,
    CASE WHEN is_exported = 1 THEN 'PUB' ELSE 'PVT' END as Scope
FROM critical_changes 
ORDER BY 
    CASE change_category
        WHEN 'CRITICAL - Entry Point' THEN 1
        WHEN 'NEW - Dual Mode API' THEN 2
        WHEN 'CORE - Command Builder' THEN 3
        ELSE 4
    END
LIMIT 15;
SQL
" Enter

# Pane 2: File Changes
tmux send-keys -t 2 "
sqlite3 pr_analysis.db << 'SQL'
.mode table
.headers on
SELECT 
    SUBSTR(file_name, 1, 30) as File,
    total_functions as Total,
    changed_functions as Changed,
    CAST(change_rate AS TEXT) || '%' as Rate
FROM file_analysis 
WHERE changed_functions > 0
ORDER BY changed_functions DESC;
SQL
" Enter

# Pane 3: Commits
tmux send-keys -t 3 "
sqlite3 pr_analysis.db << 'SQL'
.mode table
.headers on
SELECT 
    SUBSTR(sha, 1, 8) as SHA,
    SUBSTR(message, 1, 40) as Message
FROM commits 
ORDER BY ROWID DESC
LIMIT 10;
SQL
" Enter

# Set status
tmux set-option -t "$SESSION_NAME" status-right "#[fg=cyan]PR #${PR_NUMBER} Quick HUD | %H:%M"
tmux set-option -t "$SESSION_NAME" status-left "#[fg=green]${OWNER}/${REPO} "

echo "🎬 Quick HUD ready! Use: tmux attach -t $SESSION_NAME"
tmux attach-session -t "$SESSION_NAME"
