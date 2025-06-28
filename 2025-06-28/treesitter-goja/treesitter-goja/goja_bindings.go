package main

import (
	"github.com/dop251/goja"
)

// GojaTreeSitter provides the main interface for tree-sitter in Goja
type GojaTreeSitter struct {
	runtime *goja.Runtime
	factory *ParserFactory
}

// NewGojaTreeSitter creates a new Goja tree-sitter instance
func NewGojaTreeSitter(runtime *goja.Runtime) *GojaTreeSitter {
	return &GojaTreeSitter{
		runtime: runtime,
		factory: NewParserFactory(),
	}
}

// CreateParser creates a parser for the specified language
func (gts *GojaTreeSitter) CreateParser(language string) map[string]interface{} {
	parser := gts.factory.CreateParser(language)
	
	return map[string]interface{}{
		"language": parser.GetLanguageName(),
		"parse": func(sourceCode string) map[string]interface{} {
			rootNode := parser.Parse(sourceCode)
			tree := &Tree{rootNode: rootNode, source: sourceCode}
			
			return map[string]interface{}{
				"rootNode": func() map[string]interface{} {
					return gts.nodeToGojaObject(rootNode)
				},
				"getSource": func() string {
					return tree.source
				},
			}
		},
	}
}

// CreateQuery creates a query for the specified language
func (gts *GojaTreeSitter) CreateQuery(language, queryString string) map[string]interface{} {
	queryEngine := NewQueryEngine(language)
	
	return map[string]interface{}{
		"language": language,
		"pattern":  queryString,
		"execute": func(treeObj map[string]interface{}) []map[string]interface{} {
			// Extract the tree from the Goja object
			// This is a simplified implementation
			tree := gts.extractTreeFromGojaObject(treeObj)
			if tree == nil {
				return []map[string]interface{}{}
			}
			
			matches, err := queryEngine.ExecuteQuery(queryString, tree)
			if err != nil {
				panic(gts.runtime.NewGoError(err))
			}
			
			// Convert matches to Goja objects
			var result []map[string]interface{}
			for _, match := range matches {
				gojaMatch := make(map[string]interface{})
				for captureName, node := range match.Captures {
					gojaMatch[captureName] = gts.nodeToGojaObject(node)
				}
				result = append(result, gojaMatch)
			}
			
			return result
		},
	}
}

// CreateQueryBuilder creates a query builder
func (gts *GojaTreeSitter) CreateQueryBuilder() map[string]interface{} {
	builder := NewQueryBuilder()
	
	return map[string]interface{}{
		"functionDeclaration": func(captureName string) map[string]interface{} {
			builder.FunctionDeclaration(captureName)
			return gts.queryBuilderToGojaObject(builder)
		},
		"classDeclaration": func(captureName string) map[string]interface{} {
			builder.ClassDeclaration(captureName)
			return gts.queryBuilderToGojaObject(builder)
		},
		"variableDeclaration": func(captureName string) map[string]interface{} {
			builder.VariableDeclaration(captureName)
			return gts.queryBuilderToGojaObject(builder)
		},
		"arrowFunction": func(captureName string) map[string]interface{} {
			builder.ArrowFunction(captureName)
			return gts.queryBuilderToGojaObject(builder)
		},
		"methodDefinition": func(captureName string) map[string]interface{} {
			builder.MethodDefinition(captureName)
			return gts.queryBuilderToGojaObject(builder)
		},
		"callExpression": func(captureName string) map[string]interface{} {
			builder.CallExpression(captureName)
			return gts.queryBuilderToGojaObject(builder)
		},
		"build": func() string {
			return builder.Build()
		},
	}
}

// GetPredefinedQueries returns predefined query patterns
func (gts *GojaTreeSitter) GetPredefinedQueries() map[string]string {
	return PredefinedQueries
}

// CreateAdvancedQuery creates an advanced query engine with caching and optimization
func (gts *GojaTreeSitter) CreateAdvancedQuery(language string) map[string]interface{} {
	engine := NewAdvancedQueryEngine(language)
	
	return map[string]interface{}{
		"execute": func(queryString string, treeObj map[string]interface{}) []map[string]interface{} {
			tree := gts.extractTreeFromGojaObject(treeObj)
			matches, err := engine.ExecuteAdvancedQuery(queryString, tree)
			if err != nil {
				panic(gts.runtime.NewGoError(err))
			}
			
			var result []map[string]interface{}
			for _, match := range matches {
				gojaMatch := make(map[string]interface{})
				for captureName, node := range match.Captures {
					gojaMatch[captureName] = gts.nodeToGojaObject(node)
				}
				result = append(result, gojaMatch)
			}
			return result
		},
		"getStatistics": func() map[string]interface{} {
			stats := engine.GetStatistics()
			return map[string]interface{}{
				"totalQueries":   stats.TotalQueries,
				"cacheHits":      stats.CacheHits,
				"cacheMisses":    stats.CacheMisses,
				"averageTimeMs":  stats.AverageTimeMs,
			}
		},
		"setCacheEnabled": func(enabled bool) {
			engine.SetCacheEnabled(enabled)
		},
		"clearCache": func() {
			engine.ClearCache()
		},
	}
}

// CreateASTUtilities creates AST utilities for advanced tree manipulation
func (gts *GojaTreeSitter) CreateASTUtilities() map[string]interface{} {
	utils := NewASTUtilities()
	
	return map[string]interface{}{
		"findNodesByType": func(rootObj map[string]interface{}, nodeType string) []map[string]interface{} {
			root := gts.extractNodeFromGojaObject(rootObj)
			nodes := utils.FindNodesByType(root, nodeType)
			
			var result []map[string]interface{}
			for _, node := range nodes {
				result = append(result, gts.nodeToGojaObject(node))
			}
			return result
		},
		"findNodesByProperty": func(rootObj map[string]interface{}, property, value string) []map[string]interface{} {
			root := gts.extractNodeFromGojaObject(rootObj)
			nodes := utils.FindNodesByProperty(root, property, value)
			
			var result []map[string]interface{}
			for _, node := range nodes {
				result = append(result, gts.nodeToGojaObject(node))
			}
			return result
		},
		"getTreeStatistics": func(rootObj map[string]interface{}) map[string]interface{} {
			root := gts.extractNodeFromGojaObject(rootObj)
			stats := utils.GetTreeStatistics(root)
			
			return map[string]interface{}{
				"totalNodes":     stats.TotalNodes,
				"leafNodes":      stats.LeafNodes,
				"maxDepth":       stats.MaxDepth,
				"nodeCounts":     stats.NodeCounts,
				"mostCommonType": stats.GetMostCommonNodeType(),
			}
		},
		"validateTree": func(rootObj map[string]interface{}) []string {
			root := gts.extractNodeFromGojaObject(rootObj)
			return utils.ValidateTree(root)
		},
		"traverseTree": func(rootObj map[string]interface{}, order string, callback func(map[string]interface{}, int) bool) {
			root := gts.extractNodeFromGojaObject(rootObj)
			
			var traversalOrder TraversalOrder
			switch order {
			case "preorder":
				traversalOrder = PreOrder
			case "postorder":
				traversalOrder = PostOrder
			case "breadthfirst":
				traversalOrder = BreadthFirst
			default:
				traversalOrder = PreOrder
			}
			
			utils.TraverseTree(root, traversalOrder, func(node *Node, depth int) bool {
				return callback(gts.nodeToGojaObject(node), depth)
			})
		},
	}
}

// CreateQueryBuilder2 creates an enhanced query builder
func (gts *GojaTreeSitter) CreateQueryBuilder2() map[string]interface{} {
	builder := NewQueryBuilder2()
	
	return map[string]interface{}{
		"addPattern": func(pattern string) map[string]interface{} {
			builder.AddPattern(pattern)
			return gts.queryBuilder2ToGojaObject(builder)
		},
		"functionWithName": func(name, captureName string) map[string]interface{} {
			builder.FunctionWithName(name, captureName)
			return gts.queryBuilder2ToGojaObject(builder)
		},
		"classWithMethod": func(className, methodName, classCapture, methodCapture string) map[string]interface{} {
			builder.ClassWithMethod(className, methodName, classCapture, methodCapture)
			return gts.queryBuilder2ToGojaObject(builder)
		},
		"variableWithValue": func(varName, captureName string) map[string]interface{} {
			builder.VariableWithValue(varName, captureName)
			return gts.queryBuilder2ToGojaObject(builder)
		},
		"callToFunction": func(funcName, captureName string) map[string]interface{} {
			builder.CallToFunction(funcName, captureName)
			return gts.queryBuilder2ToGojaObject(builder)
		},
		"build": func() string {
			return builder.Build()
		},
		"analyze": func(treeObj map[string]interface{}) map[string]interface{} {
			tree := gts.extractTreeFromGojaObject(treeObj)
			analysis := builder.Analyze(tree)
			
			return map[string]interface{}{
				"queryString": analysis.QueryString,
				"complexity":  analysis.Complexity,
				"patterns":    analysis.Patterns,
				"suggestions": analysis.Suggestions,
			}
		},
	}
}

// CreateQueryTemplateLibrary creates a query template library
func (gts *GojaTreeSitter) CreateQueryTemplateLibrary() map[string]interface{} {
	lib := NewQueryTemplateLibrary()
	
	return map[string]interface{}{
		"listTemplates": func() []string {
			return lib.ListTemplates()
		},
		"getTemplate": func(name string) map[string]interface{} {
			template := lib.GetTemplate(name)
			if template == nil {
				return nil
			}
			
			return map[string]interface{}{
				"name":        template.Name,
				"description": template.Description,
				"template":    template.Template,
				"parameters":  template.Parameters,
			}
		},
		"instantiateTemplate": func(name string, params map[string]interface{}) string {
			stringParams := make(map[string]string)
			for k, v := range params {
				if str, ok := v.(string); ok {
					stringParams[k] = str
				}
			}
			
			query, err := lib.InstantiateTemplate(name, stringParams)
			if err != nil {
				panic(gts.runtime.NewGoError(err))
			}
			return query
		},
	}
}

// Helper methods for converting between Go and Goja objects

// extractNodeFromGojaObject extracts a Node from a Goja object
func (gts *GojaTreeSitter) extractNodeFromGojaObject(nodeObj map[string]interface{}) *Node {
	// This is a simplified implementation
	// In a real implementation, this would reconstruct the node from the Goja object
	if getType, ok := nodeObj["type"].(func() string); ok {
		nodeType := getType()
		return &Node{
			Type_:      nodeType,
			Text_:      "",
			StartByte_: 0,
			EndByte_:   0,
			Children_:  []*Node{},
			Properties: make(map[string]string),
		}
	}
	
	return &Node{
		Type_:      "unknown",
		Text_:      "",
		StartByte_: 0,
		EndByte_:   0,
		Children_:  []*Node{},
		Properties: make(map[string]string),
	}
}

// queryBuilder2ToGojaObject converts a QueryBuilder2 to a Goja-compatible object
func (gts *GojaTreeSitter) queryBuilder2ToGojaObject(builder *QueryBuilder2) map[string]interface{} {
	return map[string]interface{}{
		"addPattern": func(pattern string) map[string]interface{} {
			builder.AddPattern(pattern)
			return gts.queryBuilder2ToGojaObject(builder)
		},
		"functionWithName": func(name, captureName string) map[string]interface{} {
			builder.FunctionWithName(name, captureName)
			return gts.queryBuilder2ToGojaObject(builder)
		},
		"classWithMethod": func(className, methodName, classCapture, methodCapture string) map[string]interface{} {
			builder.ClassWithMethod(className, methodName, classCapture, methodCapture)
			return gts.queryBuilder2ToGojaObject(builder)
		},
		"variableWithValue": func(varName, captureName string) map[string]interface{} {
			builder.VariableWithValue(varName, captureName)
			return gts.queryBuilder2ToGojaObject(builder)
		},
		"callToFunction": func(funcName, captureName string) map[string]interface{} {
			builder.CallToFunction(funcName, captureName)
			return gts.queryBuilder2ToGojaObject(builder)
		},
		"build": func() string {
			return builder.Build()
		},
		"analyze": func(treeObj map[string]interface{}) map[string]interface{} {
			tree := gts.extractTreeFromGojaObject(treeObj)
			analysis := builder.Analyze(tree)
			
			return map[string]interface{}{
				"queryString": analysis.QueryString,
				"complexity":  analysis.Complexity,
				"patterns":    analysis.Patterns,
				"suggestions": analysis.Suggestions,
			}
		},
	}
}

// nodeToGojaObject converts a Node to a Goja-compatible object
func (gts *GojaTreeSitter) nodeToGojaObject(node *Node) map[string]interface{} {
	if node == nil {
		return nil
	}
	
	children := make([]map[string]interface{}, len(node.Children_))
	for i, child := range node.Children_ {
		children[i] = gts.nodeToGojaObject(child)
	}
	
	return map[string]interface{}{
		"type": func() string { return node.Type() },
		"text": func() string { return node.Text() },
		"startByte": func() int { return node.StartByte() },
		"endByte": func() int { return node.EndByte() },
		"childCount": func() int { return node.ChildCount() },
		"child": func(index int) map[string]interface{} {
			child := node.Child(index)
			if child == nil {
				return nil
			}
			return gts.nodeToGojaObject(child)
		},
		"children": func() []map[string]interface{} {
			return children
		},
		"namedChildren": func() []map[string]interface{} {
			named := node.NamedChildren()
			result := make([]map[string]interface{}, len(named))
			for i, child := range named {
				result[i] = gts.nodeToGojaObject(child)
			}
			return result
		},
		"getProperty": func(key string) string {
			return node.GetProperty(key)
		},
		"hasProperty": func(key string) bool {
			_, exists := node.Properties[key]
			return exists
		},
	}
}

// queryBuilderToGojaObject converts a QueryBuilder to a Goja-compatible object
func (gts *GojaTreeSitter) queryBuilderToGojaObject(builder *QueryBuilder) map[string]interface{} {
	return map[string]interface{}{
		"functionDeclaration": func(captureName string) map[string]interface{} {
			builder.FunctionDeclaration(captureName)
			return gts.queryBuilderToGojaObject(builder)
		},
		"classDeclaration": func(captureName string) map[string]interface{} {
			builder.ClassDeclaration(captureName)
			return gts.queryBuilderToGojaObject(builder)
		},
		"variableDeclaration": func(captureName string) map[string]interface{} {
			builder.VariableDeclaration(captureName)
			return gts.queryBuilderToGojaObject(builder)
		},
		"arrowFunction": func(captureName string) map[string]interface{} {
			builder.ArrowFunction(captureName)
			return gts.queryBuilderToGojaObject(builder)
		},
		"methodDefinition": func(captureName string) map[string]interface{} {
			builder.MethodDefinition(captureName)
			return gts.queryBuilderToGojaObject(builder)
		},
		"callExpression": func(captureName string) map[string]interface{} {
			builder.CallExpression(captureName)
			return gts.queryBuilderToGojaObject(builder)
		},
		"build": func() string {
			return builder.Build()
		},
	}
}

// extractTreeFromGojaObject extracts a Tree from a Goja object
func (gts *GojaTreeSitter) extractTreeFromGojaObject(treeObj map[string]interface{}) *Tree {
	// For now, we'll store the tree reference in a global map
	// In a production implementation, this would be handled more elegantly
	if getSource, ok := treeObj["getSource"].(func() string); ok {
		source := getSource()
		// Re-parse the source to create a new tree
		parser := gts.factory.CreateParser("javascript")
		rootNode := parser.Parse(source)
		return &Tree{rootNode: rootNode, source: source}
	}
	
	// Fallback: create empty tree
	return &Tree{
		rootNode: &Node{
			Type_:      "program",
			Text_:      "",
			StartByte_: 0,
			EndByte_:   0,
			Children_:  []*Node{},
			Properties: make(map[string]string),
		},
		source: "",
	}
}

// RegisterTreeSitterModule registers the tree-sitter module with Goja
func RegisterTreeSitterModule(runtime *goja.Runtime) {
	gts := NewGojaTreeSitter(runtime)
	
	// Create the main treesitter object
	obj := runtime.NewObject()
	
	// Core functionality
	obj.Set("createParser", gts.CreateParser)
	obj.Set("createQuery", gts.CreateQuery)
	obj.Set("createQueryBuilder", gts.CreateQueryBuilder)
	obj.Set("getPredefinedQueries", gts.GetPredefinedQueries)
	
	// Advanced functionality
	obj.Set("createAdvancedQuery", gts.CreateAdvancedQuery)
	obj.Set("createASTUtilities", gts.CreateASTUtilities)
	obj.Set("createQueryBuilder2", gts.CreateQueryBuilder2)
	obj.Set("createQueryTemplateLibrary", gts.CreateQueryTemplateLibrary)
	
	// Utility functions
	obj.Set("version", func() string {
		return "1.0.0-alpha"
	})
	
	obj.Set("supportedLanguages", func() []string {
		return []string{"javascript", "js"}
	})
	
	obj.Set("features", func() []string {
		return []string{
			"parsing", "queries", "ast_utilities", "query_optimization", 
			"caching", "templates", "statistics", "validation",
		}
	})
	
	// Register the module
	runtime.Set("treesitter", obj)
}

