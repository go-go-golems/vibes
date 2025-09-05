package listing

import (
	"fmt"
	"sort"
	"strings"

	"vault-envrc-generator/pkg/vault"
)

// Walk recursively lists keys and subdirectories up to depth
func Walk(client *vault.Client, path string, depth int) ([]string, []error) {
	return walkVault(client, vault.NormalizeListPath(path), depth)
}

func walkVault(client *vault.Client, path string, depth int) ([]string, []error) {
    var results []string
    var errs []error

    keys, err := client.ListSecrets(path)
    if err != nil {
        // Fallback: try to treat path (without trailing '/') as a secret leaf
        trimmed := strings.TrimSuffix(path, "/")
        if trimmed != "" {
            if _, err2 := client.GetSecrets(trimmed); err2 == nil {
                // Return the leaf secret path
                results = append(results, trimmed)
                sort.Strings(results)
                return results, errs
            }
        }
        errs = append(errs, fmt.Errorf("%s: %w", path, err))
        return results, errs
    }

	for _, k := range keys {
		full := path + k
		results = append(results, full)
		if strings.HasSuffix(k, "/") {
			if depth == 1 {
				continue
			}
			nextDepth := depth
			if nextDepth > 0 {
				nextDepth = depth - 1
			}
			subResults, subErrs := walkVault(client, full, nextDepth)
			results = append(results, subResults...)
			errs = append(errs, subErrs...)
		}
	}

	sort.Strings(results)
	return results, errs
}
