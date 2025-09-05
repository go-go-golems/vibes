package vault

import "strings"

func IsAbsoluteVaultPath(p string) bool {
	return strings.HasPrefix(p, "secrets/") || strings.HasPrefix(p, "secret/") || strings.HasPrefix(p, "auth/") || strings.HasPrefix(p, "sys/") || strings.HasPrefix(p, "transit/")
}

func JoinBaseAndPath(basePath, p string) string {
	if basePath == "" || IsAbsoluteVaultPath(p) {
		return p
	}
	bp := strings.TrimSuffix(basePath, "/")
	pp := strings.TrimPrefix(p, "/")
	return bp + "/" + pp
}

func NormalizeListPath(p string) string {
	p = strings.TrimSpace(p)
	if p == "" {
		return p
	}
	if !strings.HasSuffix(p, "/") {
		return p + "/"
	}
	return p
}
