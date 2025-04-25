package query

import (
	"github.com/ttmp/ttmp-go/pkg/model"
)

// DocumentResult represents a document that matches a query
type DocumentResult struct {
	Type     string             `json:"type"`
	Document *model.TTMPDocument `json:"document"`
}