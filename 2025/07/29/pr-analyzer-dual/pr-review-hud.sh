#!/bin/bash

# PR Review HUD - A sick tmux dashboard for reviewing PRs
# Usage: ./pr-review-hud.sh <owner> <repo> <pr-number>

set -e

OWNER=${1:-go-go-golems}
REPO=${2:-glazed}
PR_NUMBER=${3:-483}
SESSION_NAME="pr-review-${PR_NUMBER}"

# Colors and styling
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

echo -e "${CYAN}🚀 Setting up PR Review HUD for ${OWNER}/${REPO}#${PR_NUMBER}${NC}"

# Kill existing session if it exists
tmux has-session -t "$SESSION_NAME" 2>/dev/null && tmux kill-session -t "$SESSION_NAME"

# Create new session
tmux new-session -d -s "$SESSION_NAME" -x 180 -y 50

# Set up the layout - we'll create a complex layout with multiple panes
tmux send-keys -t "$SESSION_NAME" "clear" Enter

# Split into main areas:
# Top half: Dashboard and Summary (2 panes side by side)
# Bottom half: Details (3 panes)

# Create initial split - top and bottom
tmux split-window -v -t "$SESSION_NAME"

# Split top half horizontally (Dashboard | Summary)
tmux select-pane -t 0
tmux split-window -h

# Split bottom half into 3 panes
tmux select-pane -t 2
tmux split-window -h
tmux select-pane -t 3
tmux split-window -h

# Now we have 5 panes:
# 0: Top-left (Dashboard)
# 1: Top-right (Summary)
# 2: Bottom-left (Functions)
# 3: Bottom-middle (Files)
# 4: Bottom-right (Commits)

# Resize panes for optimal viewing
tmux resize-pane -t 0 -x 90
tmux resize-pane -t 1 -x 90
tmux resize-pane -t 2 -x 60
tmux resize-pane -t 3 -x 60
tmux resize-pane -t 4 -x 60

# Set pane titles
tmux select-pane -t 0 -T "📊 PR DASHBOARD"
tmux select-pane -t 1 -T "🎯 ANALYSIS SUMMARY"
tmux select-pane -t 2 -T "🔧 CHANGED FUNCTIONS"
tmux select-pane -t 3 -T "📁 FILE CHANGES"
tmux select-pane -t 4 -T "📝 COMMIT TIMELINE"

# Function to create a continuous display script
create_display_script() {
    local pane_id=$1
    local title=$2
    local command=$3
    
    cat > "/tmp/hud_pane_${pane_id}.sh" << EOF
#!/bin/bash
while true; do
    clear
    echo -e "\033[1;36m╔════════════════════════════════════════════════════════════════════════════════════════╗\033[0m"
    echo -e "\033[1;36m║ ${title}\033[0m"
    echo -e "\033[1;36m╚════════════════════════════════════════════════════════════════════════════════════════╝\033[0m"
    echo ""
    ${command}
    echo ""
    echo -e "\033[2m[Auto-refresh every 30s | Press Ctrl+C to stop | $(date)]\033[0m"
    sleep 30
done
EOF
    chmod +x "/tmp/hud_pane_${pane_id}.sh"
}

# Create static display script (no auto-refresh)
create_static_script() {
    local pane_id=$1
    local title=$2
    local command=$3
    
    cat > "/tmp/hud_static_${pane_id}.sh" << EOF
#!/bin/bash
clear
echo -e "\033[1;36m╔════════════════════════════════════════════════════════════════════════════════════════╗\033[0m"
echo -e "\033[1;36m║ ${title}\033[0m"
echo -e "\033[1;36m╚════════════════════════════════════════════════════════════════════════════════════════╝\033[0m"
echo ""
${command}
echo ""
echo -e "\033[2m[Static view | Press 'r' + Enter to refresh | $(date)]\033[0m"

# Wait for user input to refresh
while true; do
    read -p "Press 'r' to refresh, 'q' to quit: " input
    case \$input in
        r|R)
            clear
            echo -e "\033[1;36m╔════════════════════════════════════════════════════════════════════════════════════════╗\033[0m"
            echo -e "\033[1;36m║ ${title}\033[0m"
            echo -e "\033[1;36m╚════════════════════════════════════════════════════════════════════════════════════════╝\033[0m"
            echo ""
            ${command}
            echo ""
            echo -e "\033[2m[Refreshed at $(date)]\033[0m"
            ;;
        q|Q)
            exit 0
            ;;
        *)
            echo "Use 'r' to refresh or 'q' to quit"
            ;;
    esac
done
EOF
    chmod +x "/tmp/hud_static_${pane_id}.sh"
}

echo -e "${YELLOW}📡 Generating fresh data for the HUD...${NC}"

# Generate fresh data
./pr-analyzer analyze functions --owner "$OWNER" --repo "$REPO" --pr-number "$PR_NUMBER" --with-glaze-output --output json > /tmp/functions_hud.json 2>/dev/null || echo "[]" > /tmp/functions_hud.json
./pr-analyzer get commits --owner "$OWNER" --repo "$REPO" --pr-number "$PR_NUMBER" --with-glaze-output --output json > /tmp/commits_hud.json 2>/dev/null || echo "[]" > /tmp/commits_hud.json

# Update SQLite database
if [ -f "pr_analysis.db" ]; then
    echo -e "${GREEN}📊 Updating SQLite database...${NC}"
    # Recreate database with fresh data
    rm -f pr_analysis.db
    
    cat /tmp/functions_hud.json | jq -r '
      ["file_path","function_name","is_changed","is_exported","start_line","end_line","receiver","signature","owner","repo","pr_number"],
      (.[] | [.file_path, .function_name, (if .is_changed then 1 else 0 end), (if .is_exported then 1 else 0 end), .start_line, .end_line, .receiver, .signature, .owner, .repo, .pr_number])
      | @csv' > /tmp/functions_hud.csv

    cat /tmp/commits_hud.json | jq -r '
      ["sha","author","date","message","owner","repo","pr_number"],
      (.[] | [.sha, .author, .date, .message, .owner, .repo, .pr_number])
      | @csv' > /tmp/commits_hud.csv

    sqlite3 pr_analysis.db << 'SQL'
CREATE TABLE functions (
    file_path TEXT, function_name TEXT, is_changed INTEGER, is_exported INTEGER,
    start_line INTEGER, end_line INTEGER, receiver TEXT, signature TEXT,
    owner TEXT, repo TEXT, pr_number INTEGER
);
CREATE TABLE commits (
    sha TEXT, author TEXT, date TEXT, message TEXT,
    owner TEXT, repo TEXT, pr_number INTEGER
);
.mode csv
.import /tmp/functions_hud.csv functions
.import /tmp/commits_hud.csv commits
DELETE FROM functions WHERE file_path = 'file_path';
DELETE FROM commits WHERE sha = 'sha';

CREATE VIEW function_summary AS
SELECT 
    COUNT(*) as total_functions,
    SUM(is_changed) as changed_functions,
    SUM(is_exported) as exported_functions,
    SUM(CASE WHEN is_changed = 1 AND is_exported = 1 THEN 1 ELSE 0 END) as changed_exported_functions,
    ROUND(100.0 * SUM(is_changed) / COUNT(*), 1) as change_rate,
    ROUND(100.0 * SUM(is_exported) / COUNT(*), 1) as export_rate
FROM functions;

CREATE VIEW file_analysis AS
SELECT 
    SUBSTR(file_path, INSTR(file_path, '/') + 1) as file_name,
    file_path, COUNT(*) as total_functions, SUM(is_changed) as changed_functions,
    ROUND(100.0 * SUM(is_changed) / COUNT(*), 1) as change_rate,
    GROUP_CONCAT(CASE WHEN is_changed = 1 THEN function_name END, ', ') as changed_function_names
FROM functions GROUP BY file_path ORDER BY changed_functions DESC, change_rate DESC;

CREATE VIEW critical_changes AS
SELECT function_name, file_path,
    CASE 
        WHEN function_name = 'main' THEN 'CRITICAL - Entry Point'
        WHEN function_name LIKE '%DualMode%' THEN 'NEW - Dual Mode API'
        WHEN function_name LIKE 'Build%' THEN 'CORE - Command Builder'
        WHEN function_name LIKE '%Parser%' THEN 'CORE - Parser Logic'
        WHEN function_name LIKE 'With%' THEN 'API - Configuration'
        ELSE 'STANDARD'
    END as change_category,
    is_exported, start_line, end_line
FROM functions WHERE is_changed = 1;
SQL
fi

# Pane 0: Main Dashboard (SQL-powered)
create_static_script 0 "📊 PR #${PR_NUMBER} LIVE DASHBOARD - ${OWNER}/${REPO}" "
sqlite3 pr_analysis.db << 'SQL'
.headers off
.mode list
.separator ' '
SELECT '║ 📈 OVERVIEW METRICS                                                                  ║';
SELECT '║ ─────────────────────────────────────────────────────────────────────────────────── ║';
SELECT '║   Total Functions: ' || 
       PRINTF('%-12s', total_functions) || 
       '│   Changed: ' || 
       PRINTF('%-12s', changed_functions) || 
       '│   Change Rate: ' || 
       PRINTF('%3.0f%%', change_rate) || '     ║'
FROM function_summary;
SELECT '║   Exported Funcs:  ' || 
       PRINTF('%-12s', exported_functions) || 
       '│   Changed+Exp: ' || 
       PRINTF('%-8s', changed_exported_functions) || 
       '│   Export Rate: ' || 
       PRINTF('%3.0f%%', export_rate) || '     ║'
FROM function_summary;
SELECT '║                                                                                      ║';
SELECT '║ 🔥 HOTSPOT FILES                                                                    ║';
SELECT '║ ─────────────────────────────────────────────────────────────────────────────────── ║';
SELECT '║ ' || 
       PRINTF('%-45s', SUBSTR(file_name, 1, 45)) || 
       ' │ ' || 
       PRINTF('%2d', changed_functions) || '/' || 
       PRINTF('%-2d', total_functions) || 
       ' │ ' || 
       PRINTF('%3.0f%%', change_rate) || 
       ' ║'
FROM file_analysis 
WHERE changed_functions > 0
ORDER BY changed_functions DESC
LIMIT 8;
SELECT '║                                                                                      ║';
SELECT '║ 🎯 RISK ASSESSMENT                                                                  ║';
SELECT '║ ─────────────────────────────────────────────────────────────────────────────────── ║';
SELECT '║ Risk Level: ' ||
       CASE 
         WHEN change_rate > 40 THEN '🔴 HIGH RISK (' || CAST(ROUND(change_rate,1) AS TEXT) || '% changed)'
         WHEN change_rate > 20 THEN '🟡 MEDIUM RISK (' || CAST(ROUND(change_rate,1) AS TEXT) || '% changed)'
         ELSE '🟢 LOW RISK (' || CAST(ROUND(change_rate,1) AS TEXT) || '% changed)'
       END || PRINTF('%30s', ' ') || '║'
FROM function_summary;
SELECT '║ Entry Points: ' || 
       (SELECT COUNT(*) FROM critical_changes WHERE change_category LIKE 'CRITICAL%') ||
       ' main() functions modified                                        ║';
SELECT '║ Core Changes: ' || 
       (SELECT COUNT(*) FROM critical_changes WHERE change_category LIKE 'CORE%') ||
       ' infrastructure functions affected                                ║';
SELECT '║ New Features: ' || 
       (SELECT COUNT(*) FROM critical_changes WHERE change_category LIKE 'NEW%') ||
       ' dual-mode API functions added                                    ║';
SQL
"

# Pane 1: Analysis Summary
create_static_script 1 "🎯 DETAILED ANALYSIS SUMMARY" "
echo '🔍 CHANGE CATEGORIES:'
echo ''
sqlite3 pr_analysis.db << 'SQL'
.headers off
.mode list
SELECT '  ' ||
       CASE 
         WHEN change_category LIKE 'CRITICAL%' THEN '🔧'
         WHEN change_category LIKE 'NEW%' THEN '⚡'
         WHEN change_category LIKE 'CORE%' THEN '🏗️ '
         ELSE '📝'
       END ||
       ' ' || change_category || ': ' || COUNT(*) || ' functions'
FROM critical_changes 
GROUP BY change_category
ORDER BY COUNT(*) DESC;
SQL

echo ''
echo '📊 TOP CHANGED FILES:'
echo ''
sqlite3 pr_analysis.db << 'SQL'
.headers off
.mode list
SELECT '  📁 ' || file_name || ' (' || changed_functions || '/' || total_functions || ' = ' || CAST(change_rate AS TEXT) || '%)'
FROM file_analysis 
WHERE changed_functions > 0
ORDER BY changed_functions DESC
LIMIT 10;
SQL

echo ''
echo '🚨 CRITICAL FUNCTIONS:'
echo ''
sqlite3 pr_analysis.db << 'SQL'
.headers off
.mode list
SELECT '  ' ||
       CASE 
         WHEN change_category LIKE 'CRITICAL%' THEN '🔧'
         WHEN change_category LIKE 'NEW%' THEN '⚡'
         WHEN change_category LIKE 'CORE%' THEN '🏗️ '
         ELSE '📝'
       END ||
       ' ' || function_name || ' (' || SUBSTR(file_path, INSTR(file_path, '/') + 1) || ')'
FROM critical_changes 
WHERE change_category != 'STANDARD'
ORDER BY 
    CASE change_category
        WHEN 'CRITICAL - Entry Point' THEN 1
        WHEN 'NEW - Dual Mode API' THEN 2
        WHEN 'CORE - Command Builder' THEN 3
        WHEN 'CORE - Parser Logic' THEN 4
        ELSE 5
    END
LIMIT 15;
SQL
"

# Pane 2: Changed Functions
create_static_script 2 "🔧 CHANGED FUNCTIONS DETAILS" "
./pr-analyzer analyze functions --owner $OWNER --repo $REPO --pr-number $PR_NUMBER --only-changed 2>/dev/null | head -50
"

# Pane 3: File Changes
create_static_script 3 "📁 FILE MODIFICATION DETAILS" "
echo '📊 Files by Change Impact:'
echo ''
sqlite3 pr_analysis.db << 'SQL'
.headers off
.mode list
SELECT file_name || ':' FROM file_analysis WHERE changed_functions > 0 ORDER BY changed_functions DESC;
SELECT '  • ' || changed_functions || ' functions changed (' || CAST(change_rate AS TEXT) || '%)' 
FROM file_analysis WHERE changed_functions > 0 ORDER BY changed_functions DESC;
SQL

echo ''
echo '🔍 Function Details:'
echo ''
cat /tmp/functions_hud.json | jq -r '
map(select(.is_changed == true)) | 
group_by(.file_path) |
map(\"📁 \" + (.[0].file_path | split(\"/\")[-1]) + \":\n\" + 
    (map(\"  • \" + .function_name + \" (L\" + (.start_line|tostring) + \"-\" + (.end_line|tostring) + \")\") | join(\"\n\"))
) | join(\"\n\n\")
' 2>/dev/null | head -30
"

# Pane 4: Commit Timeline
create_static_script 4 "📝 COMMIT TIMELINE & ANALYSIS" "
echo '🕐 Recent Commits:'
echo ''
sqlite3 pr_analysis.db << 'SQL'
.headers off
.mode list
SELECT '📍 ' || SUBSTR(sha, 1, 8) || ' - ' || SUBSTR(message, 1, 60)
FROM commits 
ORDER BY ROWID DESC
LIMIT 12;
SQL

echo ''
echo '📈 Commit Insights:'
echo ''
echo '  • Total commits in PR: '
sqlite3 pr_analysis.db 'SELECT COUNT(*) FROM commits;'
echo '  • Contains revert: '
sqlite3 pr_analysis.db 'SELECT CASE WHEN COUNT(*) > 0 THEN \"Yes - indicates complexity\" ELSE \"No\" END FROM commits WHERE message LIKE \"%revert%\" OR message LIKE \"%Revert%\";'
echo '  • AI-assisted commits: '
sqlite3 pr_analysis.db 'SELECT COUNT(*) FROM commits WHERE message LIKE \"%sonnet%\" OR message LIKE \"%o4-mini%\";'
echo ''
echo '🏷️  Commit Types:'
sqlite3 pr_analysis.db << 'SQL'
.headers off
.mode list
SELECT '  ' ||
  CASE 
    WHEN message LIKE ':books:%' THEN '📚 Documentation'
    WHEN message LIKE ':art:%' THEN '🎨 Code Style'
    WHEN message LIKE ':tractor:%' THEN '🚜 Refactor'
    WHEN message LIKE ':sparkles:%' THEN '✨ Feature'
    WHEN message LIKE 'Revert%' THEN '⏪ Revert'
    ELSE '📝 Other'
  END || ': ' || COUNT(*)
FROM commits 
GROUP BY 
  CASE 
    WHEN message LIKE ':books:%' THEN 'docs'
    WHEN message LIKE ':art:%' THEN 'style'
    WHEN message LIKE ':tractor:%' THEN 'refactor'
    WHEN message LIKE ':sparkles:%' THEN 'feature'
    WHEN message LIKE 'Revert%' THEN 'revert'
    ELSE 'other'
  END;
SQL
"

echo -e "${GREEN}🎬 Starting PR Review HUD...${NC}"

# Start each pane with its script
tmux send-keys -t "$SESSION_NAME:0.0" "/tmp/hud_static_0.sh" Enter
tmux send-keys -t "$SESSION_NAME:0.1" "/tmp/hud_static_1.sh" Enter
tmux send-keys -t "$SESSION_NAME:0.2" "/tmp/hud_static_2.sh" Enter
tmux send-keys -t "$SESSION_NAME:0.3" "/tmp/hud_static_3.sh" Enter
tmux send-keys -t "$SESSION_NAME:0.4" "/tmp/hud_static_4.sh" Enter

# Add key bindings for easy navigation
tmux bind-key -n F1 select-pane -t 0
tmux bind-key -n F2 select-pane -t 1
tmux bind-key -n F3 select-pane -t 2
tmux bind-key -n F4 select-pane -t 3
tmux bind-key -n F5 select-pane -t 4

# Set status line with helpful info
tmux set-option -t "$SESSION_NAME" status-right "#[fg=cyan]PR #${PR_NUMBER} HUD | F1-F5: Switch Panes | %H:%M:%S"
tmux set-option -t "$SESSION_NAME" status-left "#[fg=green]🚀 ${OWNER}/${REPO} "

echo -e "${PURPLE}════════════════════════════════════════════════════════════════════════════════════════${NC}"
echo -e "${CYAN}🎉 PR Review HUD is now live!${NC}"
echo -e "${YELLOW}📺 Session: ${SESSION_NAME}${NC}"
echo -e "${GREEN}🎮 Controls:${NC}"
echo -e "   • F1-F5: Switch between panes"
echo -e "   • 'r' + Enter: Refresh current pane"
echo -e "   • 'q' + Enter: Quit current pane"
echo -e "   • Ctrl+B, d: Detach from session"
echo -e "   • tmux attach -t ${SESSION_NAME}: Reattach later"
echo -e "${PURPLE}════════════════════════════════════════════════════════════════════════════════════════${NC}"

# Attach to the session
tmux attach-session -t "$SESSION_NAME"

# Cleanup function
cleanup() {
    echo -e "${YELLOW}🧹 Cleaning up HUD files...${NC}"
    rm -f /tmp/hud_*.sh /tmp/functions_hud.* /tmp/commits_hud.*
}

trap cleanup EXIT
