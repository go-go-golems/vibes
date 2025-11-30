package main

import (
	"context"
	"flag"
	"fmt"
	"log"
	"os"
	
	"github.com/cayleygraph/cayley"
	"github.com/cayleygraph/cayley/graph"
	_ "github.com/cayleygraph/cayley/graph/kv/bolt"
	"github.com/fact-extraction/cayley-embeddings/pkg/embedding"
)

func main() {
	// Command line flags
	dbPath := flag.String("db", "../graph-query/cayley.db", "Path to Cayley database")
	embPath := flag.String("emb", "embeddings.json", "Path to embeddings JSON file")
	query := flag.String("query", "", "Search query (node ID)")
	k := flag.Int("k", 10, "Number of results")
	mode := flag.String("mode", "similar", "Search mode: similar, hybrid, explain")
	flag.Parse()
	
	if *query == "" {
		fmt.Println("Usage: cayley-search -query <nodeID> [-k 10] [-mode similar|hybrid|explain]")
		flag.PrintDefaults()
		os.Exit(1)
	}
	
	ctx := context.Background()
	
	// Open Cayley database
	fmt.Printf("Opening database: %s\n", *dbPath)
	store, err := cayley.NewGraph("bolt", *dbPath, nil)
	if err != nil {
		log.Fatalf("Failed to open database: %v", err)
	}
	defer store.Close()
	
	// Load embeddings
	fmt.Printf("Loading embeddings: %s\n", *embPath)
	index := embedding.NewInMemoryIndex()
	
	// Check if embeddings file exists
	if _, err := os.Stat(*embPath); err == nil {
		if err := index.LoadFromJSON(*embPath); err != nil {
			log.Fatalf("Failed to load embeddings: %v", err)
		}
		fmt.Printf("Loaded %d embeddings\n", index.Size())
	} else {
		fmt.Println("No embeddings file found, generating mock embeddings...")
		generateMockEmbeddings(ctx, store, index)
		// Save for next time
		if err := index.SaveToJSON(*embPath); err != nil {
			log.Printf("Warning: failed to save embeddings: %v", err)
		}
	}
	
	// Create search service
	service := embedding.NewSearchService(store, index)
	
	// Execute search based on mode
	switch *mode {
	case "similar":
		searchSimilar(ctx, service, *query, *k)
	case "hybrid":
		searchHybrid(ctx, service, *query, *k)
	case "explain":
		explainSearch(ctx, service, *query, *k)
	default:
		log.Fatalf("Unknown mode: %s", *mode)
	}
}

func searchSimilar(ctx context.Context, service *embedding.SearchService, nodeID string, k int) {
	fmt.Printf("\n=== Similar Nodes to: %s ===\n\n", nodeID)
	
	candidates, err := service.SearchSimilarToNode(ctx, nodeID, k)
	if err != nil {
		log.Fatalf("Search failed: %v", err)
	}
	
	if len(candidates) == 0 {
		fmt.Println("No results found")
		return
	}
	
	for i, cand := range candidates {
		if cand.NodeID == nodeID {
			continue // Skip self
		}
		fmt.Printf("%d. %s\n", i+1, cand.NodeID)
		fmt.Printf("   Similarity: %.3f\n\n", cand.Score)
	}
}

func searchHybrid(ctx context.Context, service *embedding.SearchService, nodeID string, k int) {
	fmt.Printf("\n=== Hybrid Search for: %s ===\n\n", nodeID)
	
	// Get query vector
	candidates, err := service.SearchSimilarToNode(ctx, nodeID, 1)
	if err != nil || len(candidates) == 0 {
		log.Fatalf("Failed to get query vector: %v", err)
	}
	
	queryVec := candidates[0].Vector
	
	// Hybrid search with reranking
	opts := embedding.DefaultHybridSearchOptions()
	opts.CandidateK = k * 2 // Get 2x candidates for reranking
	
	results, err := service.HybridSearch(ctx, queryVec, k, opts)
	if err != nil {
		log.Fatalf("Hybrid search failed: %v", err)
	}
	
	if len(results) == 0 {
		fmt.Println("No results found")
		return
	}
	
	embedding.PrintResults(results)
}

func explainSearch(ctx context.Context, service *embedding.SearchService, nodeID string, k int) {
	// Get query vector
	candidates, err := service.SearchSimilarToNode(ctx, nodeID, 1)
	if err != nil || len(candidates) == 0 {
		log.Fatalf("Failed to get query vector: %v", err)
	}
	
	queryVec := candidates[0].Vector
	
	explanation, err := service.ExplainSearch(ctx, queryVec, k)
	if err != nil {
		log.Fatalf("Explain failed: %v", err)
	}
	
	fmt.Println(explanation)
}

// generateMockEmbeddings creates mock embeddings for testing
func generateMockEmbeddings(ctx context.Context, store graph.QuadStore, index *embedding.InMemoryIndex) {
	fmt.Println("Generating mock embeddings from graph nodes...")
	
	// Get all nodes
	p := cayley.StartPath(store).Unique()
	itShape := p.BuildIterator(ctx)
	it := itShape.Iterate()
	defer it.Close()
	
	count := 0
	for it.Next(ctx) && count < 1000 { // Limit to 1000 nodes
		val := it.Result()
		nodeID := fmt.Sprintf("%v", val)
		
		// Generate mock embedding (random-ish based on node ID hash)
		vec := make(embedding.Vector, 384)
		hash := hashString(nodeID)
		for i := range vec {
			vec[i] = float32((hash*uint64(i+1))%1000) / 1000.0
		}
		
		index.Add(nodeID, vec)
		count++
	}
	
	fmt.Printf("Generated %d mock embeddings\n", count)
}

// Simple hash function for generating deterministic mock embeddings
func hashString(s string) uint64 {
	var hash uint64 = 5381
	for _, c := range s {
		hash = ((hash << 5) + hash) + uint64(c)
	}
	return hash
}
