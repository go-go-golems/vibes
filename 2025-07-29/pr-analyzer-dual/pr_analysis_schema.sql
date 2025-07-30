CREATE TABLE functions (
    file_path TEXT,
    function_name TEXT,
    is_changed INTEGER,
    is_exported INTEGER,
    start_line INTEGER,
    end_line INTEGER,
    receiver TEXT,
    signature TEXT,
    owner TEXT,
    repo TEXT,
    pr_number INTEGER
);
CREATE TABLE commits (
    sha TEXT,
    author TEXT,
    date TEXT,
    message TEXT,
    owner TEXT,
    repo TEXT,
    pr_number INTEGER
);
CREATE VIEW function_summary AS
SELECT 
    COUNT(*) as total_functions,
    SUM(is_changed) as changed_functions,
    SUM(is_exported) as exported_functions,
    SUM(CASE WHEN is_changed = 1 AND is_exported = 1 THEN 1 ELSE 0 END) as changed_exported_functions,
    ROUND(100.0 * SUM(is_changed) / COUNT(*), 1) as change_rate,
    ROUND(100.0 * SUM(is_exported) / COUNT(*), 1) as export_rate
FROM functions
/* function_summary(total_functions,changed_functions,exported_functions,changed_exported_functions,change_rate,export_rate) */;
CREATE VIEW file_analysis AS
SELECT 
    SUBSTR(file_path, INSTR(file_path, '/') + 1) as file_name,
    file_path,
    COUNT(*) as total_functions,
    SUM(is_changed) as changed_functions,
    ROUND(100.0 * SUM(is_changed) / COUNT(*), 1) as change_rate,
    GROUP_CONCAT(CASE WHEN is_changed = 1 THEN function_name END, ', ') as changed_function_names
FROM functions 
GROUP BY file_path
ORDER BY changed_functions DESC, change_rate DESC
/* file_analysis(file_name,file_path,total_functions,changed_functions,change_rate,changed_function_names) */;
CREATE VIEW critical_changes AS
SELECT 
    function_name,
    file_path,
    CASE 
        WHEN function_name = 'main' THEN 'CRITICAL - Entry Point'
        WHEN function_name LIKE '%DualMode%' THEN 'NEW - Dual Mode API'
        WHEN function_name LIKE 'Build%' THEN 'CORE - Command Builder'
        WHEN function_name LIKE '%Parser%' THEN 'CORE - Parser Logic'
        WHEN function_name LIKE 'With%' THEN 'API - Configuration'
        ELSE 'STANDARD'
    END as change_category,
    is_exported,
    start_line,
    end_line
FROM functions 
WHERE is_changed = 1
ORDER BY 
    CASE change_category
        WHEN 'CRITICAL - Entry Point' THEN 1
        WHEN 'NEW - Dual Mode API' THEN 2
        WHEN 'CORE - Command Builder' THEN 3
        WHEN 'CORE - Parser Logic' THEN 4
        WHEN 'API - Configuration' THEN 5
        ELSE 6
    END
/* critical_changes(function_name,file_path,change_category,is_exported,start_line,end_line) */;
CREATE VIEW pr_483_summary AS
SELECT 
    'PR Summary' as section,
    'Total Functions: ' || COUNT(*) || 
    ', Changed: ' || SUM(is_changed) ||
    ' (' || ROUND(100.0 * SUM(is_changed) / COUNT(*), 1) || '%)' ||
    ', Risk Level: ' || 
    CASE 
        WHEN ROUND(100.0 * SUM(is_changed) / COUNT(*), 1) > 40 THEN 'HIGH'
        WHEN ROUND(100.0 * SUM(is_changed) / COUNT(*), 1) > 20 THEN 'MEDIUM'
        ELSE 'LOW'
    END as details
FROM functions

UNION ALL

SELECT 
    'Most Impacted File',
    file_name,
    CAST(changed_functions AS TEXT) || '/' || CAST(total_functions AS TEXT) || 
    ' functions (' || CAST(change_rate AS TEXT) || '%)'
FROM file_analysis 
WHERE changed_functions = (SELECT MAX(changed_functions) FROM file_analysis)

UNION ALL

SELECT 
    'Critical Changes',
    change_category,
    CAST(COUNT(*) AS TEXT) || ' functions'
FROM critical_changes 
WHERE change_category LIKE 'CRITICAL%' OR change_category LIKE 'NEW%'
GROUP BY change_category

UNION ALL

SELECT 
    'Commit Info',
    'Total Commits: ' || COUNT(*),
    'Including revert - indicates complexity'
FROM commits;
