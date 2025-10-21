package ticket

import (
    "fmt"
    "os"
    "path/filepath"
    "regexp"
    "sort"
    "strings"
    "time"

    "github.com/ttmp/ttmp-cli/pkg/metadata"
)

// ticketDirPattern matches either conventional JIRA-like tickets (ABC-123)
// or numeric tickets (e.g., 9999) prefixed at the start of a directory name.
// Accept either ABC-123 style, or numeric tickets like 9999 optionally followed by a hyphen and a slug starting with a letter.
// This excludes date-like directories such as 2024-07-12.
var ticketDirPattern = regexp.MustCompile(`^(?:[A-Z]+-\d+|\d{3,}(?:-[A-Za-z].*)?)$`)

// FindTickets finds all ticket directories in the ttmp/ directory
func FindTickets(ttmpRoot string) ([]metadata.TicketInfo, error) {
    entries, err := os.ReadDir(ttmpRoot)
    if err != nil {
        return nil, fmt.Errorf("failed to read ttmp directory: %w", err)
    }

    var tickets []metadata.TicketInfo
    for _, entry := range entries {
        if !entry.IsDir() {
            continue
        }

        name := entry.Name()
        if !ticketDirPattern.MatchString(name) {
            continue
        }

        ticketTop := filepath.Join(ttmpRoot, name)

        // Try to parse the ticket from the top-level directory
        if ti, err := ParseTicket(ticketTop); err == nil && ti != nil && ti.HasIndex {
            tickets = append(tickets, *ti)
            continue
        }

        // If no index at top level, look one level deeper for a slug directory containing index.md
        subEntries, _ := os.ReadDir(ticketTop)
        for _, se := range subEntries {
            if !se.IsDir() {
                continue
            }
            candidate := filepath.Join(ticketTop, se.Name())
            if ti, err := ParseTicket(candidate); err == nil && ti != nil && ti.HasIndex {
                tickets = append(tickets, *ti)
                break
            }
        }
    }

    // Sort by ticket identifier (lexicographic is fine for numeric or ABC-123)
    sort.Slice(tickets, func(i, j int) bool {
        return tickets[i].Ticket < tickets[j].Ticket
    })

    return tickets, nil
}

// ParseTicket parses a single ticket directory
func ParseTicket(ticketPath string) (*metadata.TicketInfo, error) {
    dirName := filepath.Base(ticketPath)
    parts := strings.Split(dirName, "-")

    var ticket, slug string
    if len(parts) >= 2 {
        // If the first part is all digits, treat it as the ticket ID (e.g., 9999)
        isNumeric := true
        for _, r := range parts[0] {
            if r < '0' || r > '9' {
                isNumeric = false
                break
            }
        }
        if isNumeric {
            ticket = parts[0]
            slug = strings.Join(parts[1:], "-")
        } else {
            // Conventional ABC-123 style: ticket is ABC-123, slug is the remainder if present
            ticket = parts[0]
            if len(parts) >= 2 {
                ticket += "-" + parts[1]
            }
            if len(parts) > 2 {
                slug = strings.Join(parts[2:], "-")
            }
        }
    } else if len(parts) == 1 {
        ticket = parts[0]
    }

    ticketInfo := &metadata.TicketInfo{
        Ticket: ticket,
        Slug:   slug,
        Path:   ticketPath,
    }

    // Try to find an index.md at this level or below
    indexPath := filepath.Join(ticketPath, "index.md")
    indexDir := ticketPath
    if _, err := os.Stat(indexPath); err != nil {
        // search one level deeper for index.md
        _ = filepath.WalkDir(ticketPath, func(p string, d os.DirEntry, err error) error {
            if err != nil {
                return nil
            }
            if d.IsDir() {
                // limit depth to 3 to avoid expensive walks
                if strings.Count(strings.TrimPrefix(p, ticketPath), string(os.PathSeparator)) > 3 {
                    return filepath.SkipDir
                }
                return nil
            }
            if filepath.Base(p) == "index.md" {
                indexPath = p
                indexDir = filepath.Dir(p)
                return fmt.Errorf("found") // break walk
            }
            return nil
        })
    }
    if _, err := os.Stat(indexPath); err == nil {
        ticketInfo.HasIndex = true
        ticketInfo.IndexPath = indexPath
        ticketInfo.Path = indexDir

        // Derive slug from relative path inside the ticket directory for numeric tickets
        if ticket != "" {
            rel, _ := filepath.Rel(ticketPath, indexDir)
            rel = filepath.ToSlash(rel)
            if rel != "." && rel != "" {
                ticketInfo.Slug = rel
            }
        }

        // Parse index metadata
        if meta, _, err := metadata.ParseFile(indexPath); err == nil && meta != nil {
            ticketInfo.Status = meta.Status
            ticketInfo.Topics = meta.Topics
            ticketInfo.Owners = meta.Owners
            if meta.LastUpdated != "" {
                if t, err := time.Parse("2006-01-02", meta.LastUpdated); err == nil {
                    ticketInfo.LastUpdated = t
                }
            }
        }
    }

    // Find all documents rooted at the index directory if found, else at ticketPath
    docsRoot := indexDir
    if docsRoot == "" {
        docsRoot = ticketPath
    }
    if docs, err := FindDocuments(docsRoot); err == nil {
        ticketInfo.Documents = docs
    }

    return ticketInfo, nil
}

// FindDocuments finds all markdown documents in a ticket directory
func FindDocuments(ticketPath string) ([]metadata.DocumentInfo, error) {
	var docs []metadata.DocumentInfo

	err := filepath.Walk(ticketPath, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		if info.IsDir() {
			// Skip archive directories
			if info.Name() == "archive" || info.Name() == ".git" {
				return filepath.SkipDir
			}
			return nil
		}

		// Only process markdown files
		if !strings.HasSuffix(path, ".md") {
			return nil
		}

		meta, _, err := metadata.ParseFile(path)
		if err != nil {
			// Skip files that can't be parsed
			return nil
		}

		relPath, _ := filepath.Rel(ticketPath, path)
		doc := metadata.DocumentInfo{
			Path:     path,
			Filename: relPath,
		}
		if meta != nil {
			doc.Metadata = *meta
		}

		docs = append(docs, doc)
		return nil
	})

	if err != nil {
		return nil, err
	}

	return docs, nil
}

// GetTicketFromBranch extracts ticket ID from git branch name
func GetTicketFromBranch() (string, error) {
	// Try to read git branch name
	// This is a simplified version - in real implementation would use exec
	return "", fmt.Errorf("not implemented")
}

// GetCurrentTicket tries to determine the current ticket from context
func GetCurrentTicket(ttmpRoot string) (string, error) {
	// Try to get from git branch
	if ticket, err := GetTicketFromBranch(); err == nil && ticket != "" {
		return ticket, nil
	}

	// Try to get from current directory
	cwd, err := os.Getwd()
	if err != nil {
		return "", err
	}

	// Check if we're inside a ticket directory
	if strings.Contains(cwd, ttmpRoot) {
		relPath, err := filepath.Rel(ttmpRoot, cwd)
		if err == nil {
			parts := strings.Split(relPath, string(os.PathSeparator))
			if len(parts) > 0 && ticketDirPattern.MatchString(parts[0]) {
				ticketParts := strings.SplitN(parts[0], "-", 3)
				if len(ticketParts) >= 2 {
					return ticketParts[0] + "-" + ticketParts[1], nil
				}
			}
		}
	}

	return "", fmt.Errorf("could not determine current ticket")
}

