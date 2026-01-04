package main

import (
	"fmt"
	"strings"
)

// ASTUtilities provides advanced AST manipulation and traversal utilities
type ASTUtilities struct{}

// NewASTUtilities creates a new AST utilities instance
func NewASTUtilities() *ASTUtilities {
	return &ASTUtilities{}
}

// NodeVisitor represents a function that visits nodes during traversal
type NodeVisitor func(node *Node, depth int) bool // return false to stop traversal

// TraversalOrder represents different traversal orders
type TraversalOrder int

const (
	PreOrder TraversalOrder = iota
	PostOrder
	BreadthFirst
)

// TraverseTree traverses the tree with the specified order and visitor function
func (ast *ASTUtilities) TraverseTree(root *Node, order TraversalOrder, visitor NodeVisitor) {
	switch order {
	case PreOrder:
		ast.traversePreOrder(root, visitor, 0)
	case PostOrder:
		ast.traversePostOrder(root, visitor, 0)
	case BreadthFirst:
		ast.traverseBreadthFirst(root, visitor)
	}
}

// traversePreOrder performs pre-order traversal
func (ast *ASTUtilities) traversePreOrder(node *Node, visitor NodeVisitor, depth int) {
	if node == nil {
		return
	}
	
	// Visit current node first
	if !visitor(node, depth) {
		return
	}
	
	// Then visit children
	for _, child := range node.Children_ {
		ast.traversePreOrder(child, visitor, depth+1)
	}
}

// traversePostOrder performs post-order traversal
func (ast *ASTUtilities) traversePostOrder(node *Node, visitor NodeVisitor, depth int) {
	if node == nil {
		return
	}
	
	// Visit children first
	for _, child := range node.Children_ {
		ast.traversePostOrder(child, visitor, depth+1)
	}
	
	// Then visit current node
	visitor(node, depth)
}

// traverseBreadthFirst performs breadth-first traversal
func (ast *ASTUtilities) traverseBreadthFirst(root *Node, visitor NodeVisitor) {
	if root == nil {
		return
	}
	
	queue := []*Node{root}
	depths := []int{0}
	
	for len(queue) > 0 {
		node := queue[0]
		depth := depths[0]
		queue = queue[1:]
		depths = depths[1:]
		
		if !visitor(node, depth) {
			return
		}
		
		for _, child := range node.Children_ {
			queue = append(queue, child)
			depths = append(depths, depth+1)
		}
	}
}

// FindNodes finds all nodes matching the given predicate
func (ast *ASTUtilities) FindNodes(root *Node, predicate func(*Node) bool) []*Node {
	var matches []*Node
	
	ast.TraverseTree(root, PreOrder, func(node *Node, depth int) bool {
		if predicate(node) {
			matches = append(matches, node)
		}
		return true
	})
	
	return matches
}

// FindNodesByType finds all nodes of the specified type
func (ast *ASTUtilities) FindNodesByType(root *Node, nodeType string) []*Node {
	return ast.FindNodes(root, func(node *Node) bool {
		return node.Type_ == nodeType
	})
}

// FindNodesByProperty finds all nodes with the specified property
func (ast *ASTUtilities) FindNodesByProperty(root *Node, property, value string) []*Node {
	return ast.FindNodes(root, func(node *Node) bool {
		return node.Properties[property] == value
	})
}

// GetNodePath returns the path from root to the specified node
func (ast *ASTUtilities) GetNodePath(root, target *Node) []*Node {
	var path []*Node
	found := ast.findNodePath(root, target, &path)
	if found {
		return path
	}
	return nil
}

// findNodePath recursively finds the path to a target node
func (ast *ASTUtilities) findNodePath(current, target *Node, path *[]*Node) bool {
	if current == nil {
		return false
	}
	
	*path = append(*path, current)
	
	if current == target {
		return true
	}
	
	for _, child := range current.Children_ {
		if ast.findNodePath(child, target, path) {
			return true
		}
	}
	
	// Remove current node from path if target not found in this subtree
	*path = (*path)[:len(*path)-1]
	return false
}

// GetNodeDepth returns the depth of a node in the tree
func (ast *ASTUtilities) GetNodeDepth(root, target *Node) int {
	depth := -1
	ast.TraverseTree(root, PreOrder, func(node *Node, d int) bool {
		if node == target {
			depth = d
			return false
		}
		return true
	})
	return depth
}

// GetTreeStatistics returns comprehensive statistics about the tree
func (ast *ASTUtilities) GetTreeStatistics(root *Node) *TreeStatistics {
	stats := &TreeStatistics{
		NodeCounts:    make(map[string]int),
		PropertyCounts: make(map[string]int),
	}
	
	ast.TraverseTree(root, PreOrder, func(node *Node, depth int) bool {
		stats.TotalNodes++
		stats.NodeCounts[node.Type_]++
		
		if depth > stats.MaxDepth {
			stats.MaxDepth = depth
		}
		
		if len(node.Children_) == 0 {
			stats.LeafNodes++
		}
		
		for property := range node.Properties {
			stats.PropertyCounts[property]++
		}
		
		return true
	})
	
	return stats
}

// TreeStatistics represents comprehensive tree statistics
type TreeStatistics struct {
	TotalNodes     int
	LeafNodes      int
	MaxDepth       int
	NodeCounts     map[string]int
	PropertyCounts map[string]int
}

// GetMostCommonNodeType returns the most common node type
func (stats *TreeStatistics) GetMostCommonNodeType() string {
	maxCount := 0
	mostCommon := ""
	
	for nodeType, count := range stats.NodeCounts {
		if count > maxCount {
			maxCount = count
			mostCommon = nodeType
		}
	}
	
	return mostCommon
}

// GetNodeTypeDistribution returns a sorted list of node types by frequency
func (stats *TreeStatistics) GetNodeTypeDistribution() []NodeTypeCount {
	var distribution []NodeTypeCount
	
	for nodeType, count := range stats.NodeCounts {
		distribution = append(distribution, NodeTypeCount{
			Type:  nodeType,
			Count: count,
		})
	}
	
	// Sort by count (descending)
	for i := 0; i < len(distribution)-1; i++ {
		for j := i + 1; j < len(distribution); j++ {
			if distribution[j].Count > distribution[i].Count {
				distribution[i], distribution[j] = distribution[j], distribution[i]
			}
		}
	}
	
	return distribution
}

// NodeTypeCount represents a node type and its count
type NodeTypeCount struct {
	Type  string
	Count int
}

// NodeFilter represents a filter for nodes
type NodeFilter struct {
	Types      []string
	Properties map[string]string
	MinDepth   int
	MaxDepth   int
}

// Matches checks if a node matches the filter criteria
func (filter *NodeFilter) Matches(node *Node, depth int) bool {
	// Check depth constraints
	if depth < filter.MinDepth || (filter.MaxDepth > 0 && depth > filter.MaxDepth) {
		return false
	}
	
	// Check type constraints
	if len(filter.Types) > 0 {
		typeMatch := false
		for _, t := range filter.Types {
			if node.Type_ == t {
				typeMatch = true
				break
			}
		}
		if !typeMatch {
			return false
		}
	}
	
	// Check property constraints
	for key, value := range filter.Properties {
		if node.Properties[key] != value {
			return false
		}
	}
	
	return true
}

// FilterNodes filters nodes based on the given criteria
func (ast *ASTUtilities) FilterNodes(root *Node, filter *NodeFilter) []*Node {
	var matches []*Node
	
	ast.TraverseTree(root, PreOrder, func(node *Node, depth int) bool {
		if filter.Matches(node, depth) {
			matches = append(matches, node)
		}
		return true
	})
	
	return matches
}

// NodeTransformer represents a function that transforms nodes
type NodeTransformer func(node *Node) *Node

// TransformTree applies a transformation to all nodes in the tree
func (ast *ASTUtilities) TransformTree(root *Node, transformer NodeTransformer) *Node {
	if root == nil {
		return nil
	}
	
	// Transform children first
	var newChildren []*Node
	for _, child := range root.Children_ {
		transformedChild := ast.TransformTree(child, transformer)
		if transformedChild != nil {
			newChildren = append(newChildren, transformedChild)
		}
	}
	
	// Create a copy of the current node with transformed children
	newNode := &Node{
		Type_:      root.Type_,
		Text_:      root.Text_,
		StartByte_: root.StartByte_,
		EndByte_:   root.EndByte_,
		Children_:  newChildren,
		Properties: make(map[string]string),
	}
	
	// Copy properties
	for k, v := range root.Properties {
		newNode.Properties[k] = v
	}
	
	// Apply transformation to the current node
	return transformer(newNode)
}

// GetNodeSignature returns a unique signature for a node based on its structure
func (ast *ASTUtilities) GetNodeSignature(node *Node) string {
	if node == nil {
		return ""
	}
	
	var parts []string
	parts = append(parts, node.Type_)
	
	// Add property information
	if len(node.Properties) > 0 {
		var propParts []string
		for k, v := range node.Properties {
			propParts = append(propParts, fmt.Sprintf("%s:%s", k, v))
		}
		parts = append(parts, strings.Join(propParts, ","))
	}
	
	// Add child type information
	if len(node.Children_) > 0 {
		var childTypes []string
		for _, child := range node.Children_ {
			childTypes = append(childTypes, child.Type_)
		}
		parts = append(parts, strings.Join(childTypes, ","))
	}
	
	return strings.Join(parts, "|")
}

// FindSimilarNodes finds nodes with similar structure to the given node
func (ast *ASTUtilities) FindSimilarNodes(root, template *Node) []*Node {
	templateSig := ast.GetNodeSignature(template)
	
	return ast.FindNodes(root, func(node *Node) bool {
		return ast.GetNodeSignature(node) == templateSig
	})
}

// ValidateTree performs basic validation on the tree structure
func (ast *ASTUtilities) ValidateTree(root *Node) []string {
	var errors []string
	
	ast.TraverseTree(root, PreOrder, func(node *Node, depth int) bool {
		// Check for nil node
		if node == nil {
			errors = append(errors, "Found nil node in tree")
			return true
		}
		
		// Check for empty type
		if node.Type_ == "" {
			errors = append(errors, "Found node with empty type")
		}
		
		// Check for invalid byte positions
		if node.StartByte_ > node.EndByte_ {
			errors = append(errors, fmt.Sprintf("Node %s has invalid byte range: %d-%d", 
				node.Type_, node.StartByte_, node.EndByte_))
		}
		
		// Check for nil properties map
		if node.Properties == nil {
			errors = append(errors, fmt.Sprintf("Node %s has nil properties map", node.Type_))
		}
		
		return true
	})
	
	return errors
}

