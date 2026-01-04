package main

// Node represents a simplified AST node exposed to JavaScript
type Node struct {
	Type_      string            `json:"type"`
	Text_      string            `json:"text"`
	StartByte_ int               `json:"startByte"`
	EndByte_   int               `json:"endByte"`
	Children_  []*Node           `json:"children"`
	Properties map[string]string `json:"properties"`
}

// Parser represents a simplified parser exposed to JavaScript
type Parser struct {
	language string
}

// Tree represents a parsed tree exposed to JavaScript
type Tree struct {
	rootNode *Node
	source   string
}

// Query represents a simplified query exposed to JavaScript
type Query struct {
	pattern  string
	language string
}

// Type returns the type of the node
func (n *Node) Type() string {
	return n.Type_
}

// Text returns the text content of the node
func (n *Node) Text() string {
	return n.Text_
}

// StartByte returns the start byte position of the node
func (n *Node) StartByte() int {
	return n.StartByte_
}

// EndByte returns the end byte position of the node
func (n *Node) EndByte() int {
	return n.EndByte_
}

// ChildCount returns the number of children
func (n *Node) ChildCount() int {
	return len(n.Children_)
}

// Child returns the child at the given index
func (n *Node) Child(index int) *Node {
	if index < 0 || index >= len(n.Children_) {
		return nil
	}
	return n.Children_[index]
}

// Children returns all children of the node
func (n *Node) Children() []*Node {
	return n.Children_
}

// NamedChildren returns all named children of the node
func (n *Node) NamedChildren() []*Node {
	var named []*Node
	for _, child := range n.Children_ {
		if child.Type_ != "," && child.Type_ != ";" {
			named = append(named, child)
		}
	}
	return named
}

// GetProperty returns a property of the node
func (n *Node) GetProperty(key string) string {
	return n.Properties[key]
}

// HasProperty checks if a property exists
func (n *Node) HasProperty(key string) bool {
	_, exists := n.Properties[key]
	return exists
}

// RootNode returns the root node of the tree
func (t *Tree) RootNode() *Node {
	return t.rootNode
}

// GetSource returns the source code
func (t *Tree) GetSource() string {
	return t.source
}

