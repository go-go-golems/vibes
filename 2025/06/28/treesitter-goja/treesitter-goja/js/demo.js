console.log("=== Tree-sitter Goja Module Demo ===");
console.log("Version:", treesitter.version());
console.log("Supported languages:", treesitter.supportedLanguages());
console.log("Parser mode:", __USE_ADVANCED__ ? "Advanced" : "Standard");
console.log();

// Create a parser for JavaScript
const parser = treesitter.createParser("javascript");
console.log("Created parser for language:", parser.language);

// Parse the source code
const sourceCode = __SOURCE_CODE__;
const tree = parser.parse(sourceCode);
const rootNode = tree.rootNode();

console.log("=== Basic AST Information ===");
console.log("Root node type:", rootNode.type());
console.log("Root node children count:", rootNode.childCount());
console.log("Source code length:", sourceCode.length, "bytes");
console.log();

// Function to traverse the tree with better formatting
function traverseNode(node, depth = 0, maxDepth = 3) {
	if (depth > maxDepth) return;
	
	const indent = "  ".repeat(depth);
	const nodeInfo = node.type() + " [" + node.startByte() + "-" + node.endByte() + "]";
	
	// Add property information if available
	if (node.hasProperty && node.hasProperty("name")) {
		console.log(indent + nodeInfo + " (name: " + node.getProperty("name") + ")");
	} else if (node.hasProperty && node.hasProperty("async")) {
		console.log(indent + nodeInfo + " (async: " + node.getProperty("async") + ")");
	} else {
		console.log(indent + nodeInfo);
	}
	
	const children = node.children();
	for (let i = 0; i < children.length && i < 10; i++) { // Limit children shown
		if (children[i]) {
			traverseNode(children[i], depth + 1, maxDepth);
		}
	}
	
	if (children.length > 10) {
		console.log(indent + "  ... (" + (children.length - 10) + " more children)");
	}
}

// Traverse the tree
console.log("=== AST Structure (limited depth) ===");
traverseNode(rootNode);
console.log();

// Count different node types
function countNodeTypes(node, counts = {}) {
	const type = node.type();
	counts[type] = (counts[type] || 0) + 1;
	
	const children = node.children();
	for (let i = 0; i < children.length; i++) {
		if (children[i]) {
			countNodeTypes(children[i], counts);
		}
	}
	return counts;
}

console.log("=== Node Type Statistics ===");
const nodeCounts = countNodeTypes(rootNode);
const sortedTypes = Object.keys(nodeCounts).sort((a, b) => nodeCounts[b] - nodeCounts[a]);
for (let i = 0; i < Math.min(sortedTypes.length, 10); i++) {
	const type = sortedTypes[i];
	console.log(type + ":", nodeCounts[type]);
}
console.log();

// Demonstrate predefined queries
console.log("=== Predefined Queries ===");
const predefinedQueries = treesitter.getPredefinedQueries();
const queryNames = Object.keys(predefinedQueries);
console.log("Available queries:", queryNames.join(", "));
console.log();

// Test multiple queries
const queriesToTest = ["all_functions", "all_classes", "all_variables", "all_methods"];

for (let i = 0; i < queriesToTest.length; i++) {
	const queryName = queriesToTest[i];
	if (predefinedQueries[queryName]) {
		console.log("=== " + queryName.replace(/_/g, " ").toUpperCase() + " ===");
		try {
			const query = treesitter.createQuery("javascript", predefinedQueries[queryName]);
			const matches = query.execute(tree);
			
			console.log("Found", matches.length, "matches:");
			for (let j = 0; j < Math.min(matches.length, 5); j++) {
				const match = matches[j];
				const keys = Object.keys(match);
				if (keys.length > 0) {
					const key = keys[0];
					const node = match[key];
					if (node && node.text) {
						const name = node.text();
						const start = node.startByte();
						const end = node.endByte();
						console.log("- " + name + " at position " + start + "-" + end);
					}
				}
			}
			if (matches.length > 5) {
				console.log("... and " + (matches.length - 5) + " more");
			}
		} catch (e) {
			console.error("Query error for " + queryName + ":", e.message);
		}
		console.log();
	}
}

// Demonstrate query builder
console.log("=== Query Builder Demo ===");
try {
	const builder = treesitter.createQueryBuilder();
	const customQuery = builder
		.functionDeclaration("func_name")
		.classDeclaration("class_name")
		.methodDefinition("method_name")
		.build();
	
	console.log("Built custom query:");
	console.log(customQuery);
	
	const customQueryObj = treesitter.createQuery("javascript", customQuery);
	const customMatches = customQueryObj.execute(tree);
	console.log("Custom query found", customMatches.length, "matches");
} catch (e) {
	console.error("Query builder error:", e.message);
}
console.log();

// Demonstrate AST primitives
console.log("=== AST Primitives Demo ===");
console.log("Root node has", rootNode.childCount(), "children");

const children = rootNode.children();
console.log("Node type distribution in first 10 children:");
const typeCount = {};
for (let i = 0; i < Math.min(children.length, 10); i++) {
	const child = children[i];
	const type = child.type();
	typeCount[type] = (typeCount[type] || 0) + 1;
}

for (const [type, count] of Object.entries(typeCount)) {
	console.log("- " + type + ": " + count);
}

console.log();
console.log("=== Demo Complete ===");
console.log("Total nodes processed:", Object.values(nodeCounts).reduce((a, b) => a + b, 0)); 