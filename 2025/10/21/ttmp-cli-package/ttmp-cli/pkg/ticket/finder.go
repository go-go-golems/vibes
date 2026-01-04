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
    // Walk the root and discover any directory containing an index.md file;
    // parse its frontmatter to derive TicketInfo. This supports arbitrary
    // directory naming conventions and date-bucketed layouts.
    var tickets []metadata.TicketInfo

    // Normalize root
    fi, err := os.Stat(ttmpRoot)
    if err != nil || !fi.IsDir() {
        return nil, fmt.Errorf("failed to read ttmp directory: %w", err)
    }

    // Track directories we've already added (by index dir path)
    seen := map[string]bool{}

    // Depth-limited walk: up to 4 levels deep from root
    _ = filepath.WalkDir(ttmpRoot, func(path string, d os.DirEntry, err error) error {
        if err != nil { return nil }
        if d.IsDir() {
            // Avoid deep recursion into heavy dirs
            if strings.Count(strings.TrimPrefix(path, ttmpRoot), string(os.PathSeparator)) > 4 {
                return filepath.SkipDir
            }
            return nil
        }
        if filepath.Base(path) != "index.md" {
            return nil
        }
        indexDir := filepath.Dir(path)
        if seen[indexDir] { return nil }

        // Parse metadata
        meta, _, perr := metadata.ParseFile(path)
        if perr != nil || meta == nil {
            return nil
        }
        ti := metadata.TicketInfo{
            Ticket:    meta.Ticket,
            Path:      indexDir,
            IndexPath: path,
            HasIndex:  true,
            Status:    meta.Status,
            Topics:    meta.Topics,
            Owners:    meta.Owners,
        }
        // Derive slug from directory name, or relative path under root
        ti.Slug, _ = filepath.Rel(ttmpRoot, indexDir)
        // Derive LastUpdated if present
        if meta.LastUpdated != "" {
            if t, e := time.Parse("2006-01-02", meta.LastUpdated); e == nil {
                ti.LastUpdated = t
            }
        }
        // Enumerate documents under the index directory
        if docs, derr := FindDocuments(indexDir); derr == nil {
            ti.Documents = docs
        }
        tickets = append(tickets, ti)
        seen[indexDir] = true
        return nil
    })

    // Prefer tickets that actually have a non-empty Ticket field; but keep others
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

    ticketInfo := &metadata.TicketInfo{ Ticket: ticket, Slug: slug, Path: ticketPath }

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
        // Parse index metadata; prefer metadata.Ticket as canonical ID
        if meta, _, err := metadata.ParseFile(indexPath); err == nil && meta != nil {
            if meta.Ticket != "" { ticketInfo.Ticket = meta.Ticket }
            ticketInfo.Status = meta.Status
            ticketInfo.Topics = meta.Topics
            ticketInfo.Owners = meta.Owners
            if meta.LastUpdated != "" {
                if t, err := time.Parse("2006-01-02", meta.LastUpdated); err == nil { ticketInfo.LastUpdated = t }
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
    // Normalize paths
    absRoot, _ := filepath.Abs(ttmpRoot)
    cwd, err := os.Getwd()
    if err != nil {
        return "", err
    }
    absCwd, _ := filepath.Abs(cwd)
    // Ensure cwd is under root
    rel, err := filepath.Rel(absRoot, absCwd)
    if err != nil || strings.HasPrefix(rel, "..") {
        return "", fmt.Errorf("cwd not under ttmp root")
    }
    // Walk up from cwd to root, search for nearest index.md and read Ticket
    dir := absCwd
    for {
        indexPath := filepath.Join(dir, "index.md")
        if _, err := os.Stat(indexPath); err == nil {
            if meta, _, perr := metadata.ParseFile(indexPath); perr == nil && meta != nil && meta.Ticket != "" {
                return meta.Ticket, nil
            }
        }
        if dir == absRoot { break }
        parent := filepath.Dir(dir)
        if parent == dir { break }
        dir = parent
    }
    return "", fmt.Errorf("could not determine current ticket")
}

