#!/bin/bash

# Agent Manager Script
# Parses LLM output for SET_AGENT(XXX) commands and manages agent state

DB_FILE="${SCRIPT_DIR:-$(dirname "$0")}/agents.db"

# Initialize database if it doesn't exist
init_db() {
    sqlite3 "$DB_FILE" <<EOF
CREATE TABLE IF NOT EXISTS agents (
    id INTEGER PRIMARY KEY,
    name TEXT UNIQUE NOT NULL,
    guidelines TEXT NOT NULL,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS current_agent (
    id INTEGER PRIMARY KEY,
    agent_name TEXT NOT NULL,
    set_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- Insert default agents if table is empty
INSERT OR IGNORE INTO agents (name, guidelines) VALUES 
('CODER', 'Focus on writing clean, efficient code. Prioritize functionality and maintainability. Use best practices and appropriate design patterns.'),
('ANALYST', 'Analyze data and provide insights. Break down complex problems into manageable parts. Focus on accuracy and evidence-based conclusions.'),
('DEBUGGER', 'Systematically identify and fix issues. Use methodical troubleshooting approaches. Verify fixes thoroughly before concluding.'),
('ARCHITECT', 'Design system architecture and high-level solutions. Consider scalability, maintainability, and integration patterns.'),
('REVIEWER', 'Conduct thorough code and design reviews. Identify potential issues, suggest improvements, and ensure quality standards.');
EOF
}

# Extract agent type from SET_AGENT(XXX) pattern
extract_agent_type() {
    local input="$1"
    echo "$input" | grep -o 'SET_AGENT([^)]*)' | sed 's/SET_AGENT(\(.*\))/\1/' | tr '[:lower:]' '[:upper:]'
}

# Store current agent in database
set_current_agent() {
    local agent_name="$1"
    sqlite3 "$DB_FILE" <<EOF
DELETE FROM current_agent;
INSERT INTO current_agent (agent_name) VALUES ('$agent_name');
EOF
}

# Get guidelines for agent type
get_agent_guidelines() {
    local agent_name="$1"
    sqlite3 "$DB_FILE" "SELECT guidelines FROM agents WHERE name = '$agent_name';"
}

# Get list of available agent types
get_available_agents() {
    sqlite3 "$DB_FILE" "SELECT name FROM agents ORDER BY name;"
}

# Get current agent
get_current_agent() {
    sqlite3 "$DB_FILE" "SELECT agent_name FROM current_agent ORDER BY set_at DESC LIMIT 1;"
}

# Main function
main() {
    local input="$1"
    
    if [ -z "$input" ]; then
        echo "Usage: $0 <llm_output_string>"
        exit 1
    fi
    
    # Initialize database
    init_db
    
    # Check for SET_AGENT command
    agent_type=$(extract_agent_type "$input")
    
    if [ -n "$agent_type" ]; then
        # Check if agent type exists
        guidelines=$(get_agent_guidelines "$agent_type")
        
        if [ -n "$guidelines" ]; then
            # Store current agent
            set_current_agent "$agent_type"
            
            echo "=== AGENT MODE: $agent_type ==="
            echo
            echo "Guidelines:"
            echo "$guidelines"
            echo
        else
            echo "=== UNKNOWN AGENT TYPE: $agent_type ==="
            echo "Agent type '$agent_type' not found in database."
            echo
        fi
    else
        # No SET_AGENT command found, show current agent if any
        current_agent=$(get_current_agent)
        if [ -n "$current_agent" ]; then
            guidelines=$(get_agent_guidelines "$current_agent")
            echo "=== CURRENT AGENT MODE: $current_agent ==="
            echo
            echo "Guidelines:"
            echo "$guidelines"
            echo
        fi
    fi
    
    # Always show available agents and switching guide
    echo "Available Agent Types:"
    get_available_agents | sed 's/^/- /'
    echo
    echo "To switch mode, use: SET_AGENT(AGENT_TYPE) in your response"
    echo "Example: SET_AGENT(CODER) or SET_AGENT(DEBUGGER)"
}

# Run main function
main "$@"
