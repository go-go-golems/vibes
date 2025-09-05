package vault

import (
	"fmt"
	"regexp"
	"strings"
)

// TemplateContext used for rendering templated strings such as paths
type TemplateContext struct {
	Token TokenContext
}

type TokenContext struct {
	Accessor    string
	CreationTTL string
	DisplayName string
	EntityID    string
	ExpireTime  string
	ID          string
	IssueTime   string
	Meta        map[string]string
	Policies    []string
	Path        string
	TTL         string
	Type        string
	OIDCUserID  string
}

// BuildTemplateContext populates TemplateContext from the current token info
func BuildTemplateContext(vc *Client) (TemplateContext, error) {
	client := vc.GetClient()
	tInfo, err := client.Auth().Token().LookupSelf()
	if err != nil {
		return TemplateContext{}, fmt.Errorf("token lookup failed: %w", err)
	}
	ctx := TemplateContext{Token: TokenContext{}}
	if tInfo != nil && tInfo.Data != nil {
		getStr := func(key string) string {
			if v, ok := tInfo.Data[key]; ok {
				if s, ok := v.(string); ok {
					return s
				}
			}
			return ""
		}
		ctx.Token.Accessor = getStr("accessor")
		ctx.Token.CreationTTL = getStr("creation_ttl")
		ctx.Token.DisplayName = getStr("display_name")
		ctx.Token.EntityID = getStr("entity_id")
		ctx.Token.ExpireTime = getStr("expire_time")
		ctx.Token.ID = getStr("id")
		ctx.Token.IssueTime = getStr("issue_time")
		ctx.Token.Path = getStr("path")
		ctx.Token.TTL = getStr("ttl")
		ctx.Token.Type = getStr("type")
		// Policies
		if pv, ok := tInfo.Data["policies"]; ok {
			if arr, ok := pv.([]interface{}); ok {
				for _, it := range arr {
					if s, ok := it.(string); ok {
						ctx.Token.Policies = append(ctx.Token.Policies, s)
					}
				}
			}
		}
		// Meta (flatten map[string]string)
		ctx.Token.Meta = map[string]string{}
		if mv, ok := tInfo.Data["meta"]; ok {
			if m, ok := mv.(map[string]interface{}); ok {
				for k, v := range m {
					if s, ok := v.(string); ok {
						ctx.Token.Meta[k] = s
					}
				}
			}
		}
		// Derive OIDCUserID from display_name like "oidc-123456"
		if strings.HasPrefix(ctx.Token.DisplayName, "oidc-") {
			re := regexp.MustCompile(`oidc-([0-9A-Za-z_-]+)`)
			m := re.FindStringSubmatch(ctx.Token.DisplayName)
			if len(m) == 2 {
				ctx.Token.OIDCUserID = m[1]
			}
		}
	}
	return ctx, nil
}
