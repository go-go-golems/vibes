package main

import (
	"context"
	"fmt"
	"sync"

	"github.com/cayleygraph/cayley/graph"
	"github.com/cayleygraph/cayley/graph/iterator"
	"github.com/cayleygraph/cayley/graph/refs"
	"github.com/cayleygraph/quad"
)

// intRef is our internal reference type
type intRef int64

func (r intRef) Key() interface{} { return int64(r) }

// SimpleBackend is a minimal in-memory QuadStore implementation
type SimpleBackend struct {
	mu sync.RWMutex

	// Storage
	quads     []quad.Quad
	values    map[string]int64
	valuesRev map[int64]quad.Value
	nextID    int64

	// Indexes: direction -> value_id -> []quad_id
	indexes [4]map[int64][]int64
}

func NewSimpleBackend() *SimpleBackend {
	sb := &SimpleBackend{
		quads:     make([]quad.Quad, 0),
		values:    make(map[string]int64),
		valuesRev: make(map[int64]quad.Value),
		nextID:    1,
	}

	// Initialize indexes for all 4 directions
	for i := 0; i < 4; i++ {
		sb.indexes[i] = make(map[int64][]int64)
	}

	return sb
}

// ValueOf implements Namer
func (sb *SimpleBackend) ValueOf(v quad.Value) (refs.Ref, error) {
	sb.mu.Lock()
	defer sb.mu.Unlock()

	if v == nil {
		return nil, nil
	}

	key := v.String()
	if id, exists := sb.values[key]; exists {
		return intRef(id), nil
	}

	// Create new ID
	id := sb.nextID
	sb.nextID++
	sb.values[key] = id
	sb.valuesRev[id] = v

	return intRef(id), nil
}

// NameOf implements Namer
func (sb *SimpleBackend) NameOf(ref refs.Ref) (quad.Value, error) {
	sb.mu.RLock()
	defer sb.mu.RUnlock()

	if ref == nil {
		return nil, nil
	}

	id := int64(ref.(intRef))
	if val, exists := sb.valuesRev[id]; exists {
		return val, nil
	}

	return nil, fmt.Errorf("value not found for ref %v", id)
}

// Quad implements QuadIndexer
func (sb *SimpleBackend) Quad(ref refs.Ref) (quad.Quad, error) {
	sb.mu.RLock()
	defer sb.mu.RUnlock()

	id := int64(ref.(intRef))
	if id >= 0 && id < int64(len(sb.quads)) {
		return sb.quads[id], nil
	}

	return quad.Quad{}, fmt.Errorf("quad not found")
}

// QuadIterator implements QuadIndexer
func (sb *SimpleBackend) QuadIterator(d quad.Direction, v refs.Ref) iterator.Shape {
	sb.mu.RLock()
	defer sb.mu.RUnlock()

	valueID := int64(v.(intRef))
	dirIndex := int(d) - 1

	quadIDs, exists := sb.indexes[dirIndex][valueID]
	if !exists {
		quadIDs = []int64{}
	}

	// Create a copy to avoid holding the lock
	ids := make([]int64, len(quadIDs))
	copy(ids, quadIDs)

	return &SimpleShape{
		backend: sb,
		ids:     ids,
	}
}

// QuadIteratorSize implements QuadIndexer
func (sb *SimpleBackend) QuadIteratorSize(ctx context.Context, d quad.Direction, v refs.Ref) (refs.Size, error) {
	sb.mu.RLock()
	defer sb.mu.RUnlock()

	valueID := int64(v.(intRef))
	dirIndex := int(d) - 1

	quadIDs, exists := sb.indexes[dirIndex][valueID]
	if !exists {
		return refs.Size{Value: 0, Exact: true}, nil
	}

	return refs.Size{Value: int64(len(quadIDs)), Exact: true}, nil
}

// QuadDirection implements QuadIndexer
func (sb *SimpleBackend) QuadDirection(id refs.Ref, d quad.Direction) (refs.Ref, error) {
	q, err := sb.Quad(id)
	if err != nil {
		return nil, err
	}

	return sb.ValueOf(q.Get(d))
}

// Stats implements QuadIndexer
func (sb *SimpleBackend) Stats(ctx context.Context, exact bool) (graph.Stats, error) {
	sb.mu.RLock()
	defer sb.mu.RUnlock()

	return graph.Stats{
		Nodes: refs.Size{Value: int64(len(sb.valuesRev)), Exact: true},
		Quads: refs.Size{Value: int64(len(sb.quads)), Exact: true},
	}, nil
}

// ApplyDeltas implements QuadStore
func (sb *SimpleBackend) ApplyDeltas(deltas []graph.Delta, opts graph.IgnoreOpts) error {
	sb.mu.Lock()
	defer sb.mu.Unlock()

	for _, delta := range deltas {
		if delta.Action == graph.Add {
			sb.addQuad(delta.Quad)
		} else if delta.Action == graph.Delete {
			sb.deleteQuad(delta.Quad)
		}
	}

	return nil
}

func (sb *SimpleBackend) addQuad(q quad.Quad) error {
	// Get or create IDs for all components
	sID := sb.getOrCreateID(q.Subject)
	pID := sb.getOrCreateID(q.Predicate)
	oID := sb.getOrCreateID(q.Object)
	lID := int64(0)
	if q.Label != nil {
		lID = sb.getOrCreateID(q.Label)
	}

	// Add quad
	quadID := int64(len(sb.quads))
	sb.quads = append(sb.quads, q)

	// Update indexes
	sb.indexes[0][sID] = append(sb.indexes[0][sID], quadID)
	sb.indexes[1][pID] = append(sb.indexes[1][pID], quadID)
	sb.indexes[2][oID] = append(sb.indexes[2][oID], quadID)
	if lID != 0 {
		sb.indexes[3][lID] = append(sb.indexes[3][lID], quadID)
	}

	return nil
}

func (sb *SimpleBackend) deleteQuad(q quad.Quad) error {
	// Simple implementation: mark as deleted (set to zero quad)
	// A production implementation would properly remove from indexes
	for i, existing := range sb.quads {
		if existing.Subject == q.Subject &&
			existing.Predicate == q.Predicate &&
			existing.Object == q.Object &&
			existing.Label == q.Label {
			sb.quads[i] = quad.Quad{}
			return nil
		}
	}
	return nil
}

func (sb *SimpleBackend) getOrCreateID(v quad.Value) int64 {
	if v == nil {
		return 0
	}

	key := v.String()
	if id, exists := sb.values[key]; exists {
		return id
	}

	id := sb.nextID
	sb.nextID++
	sb.values[key] = id
	sb.valuesRev[id] = v
	return id
}

// NewQuadWriter implements QuadStore
func (sb *SimpleBackend) NewQuadWriter() (quad.WriteCloser, error) {
	return &quadWriter{backend: sb}, nil
}

type quadWriter struct {
	backend *SimpleBackend
}

func (qw *quadWriter) WriteQuad(q quad.Quad) error {
	return qw.backend.ApplyDeltas([]graph.Delta{{Quad: q, Action: graph.Add}}, graph.IgnoreOpts{})
}

func (qw *quadWriter) WriteQuads(quads []quad.Quad) (int, error) {
	deltas := make([]graph.Delta, len(quads))
	for i, q := range quads {
		deltas[i] = graph.Delta{Quad: q, Action: graph.Add}
	}
	err := qw.backend.ApplyDeltas(deltas, graph.IgnoreOpts{})
	return len(quads), err
}

func (qw *quadWriter) Close() error {
	return nil
}

// NodesAllIterator implements QuadStore
func (sb *SimpleBackend) NodesAllIterator() iterator.Shape {
	sb.mu.RLock()
	defer sb.mu.RUnlock()

	ids := make([]int64, 0, len(sb.valuesRev))
	for id := range sb.valuesRev {
		ids = append(ids, id)
	}

	return &SimpleShape{
		backend: sb,
		ids:     ids,
	}
}

// QuadsAllIterator implements QuadStore
func (sb *SimpleBackend) QuadsAllIterator() iterator.Shape {
	sb.mu.RLock()
	defer sb.mu.RUnlock()

	ids := make([]int64, len(sb.quads))
	for i := range sb.quads {
		ids[i] = int64(i)
	}

	return &SimpleShape{
		backend: sb,
		ids:     ids,
	}
}

// Close implements QuadStore
func (sb *SimpleBackend) Close() error {
	return nil
}

// SimpleShape is the Shape implementation
type SimpleShape struct {
	backend *SimpleBackend
	ids     []int64
}

func (s *SimpleShape) String() string {
	return "SimpleShape"
}

func (s *SimpleShape) Iterate() iterator.Scanner {
	return &SimpleIterator{
		backend: s.backend,
		ids:     s.ids,
		pos:     -1,
	}
}

func (s *SimpleShape) Lookup() iterator.Index {
	return &SimpleLookup{
		backend: s.backend,
		ids:     s.ids,
	}
}

func (s *SimpleShape) Stats(ctx context.Context) (iterator.Costs, error) {
	return iterator.Costs{
		ContainsCost: 1,
		NextCost:     1,
		Size:         refs.Size{Value: int64(len(s.ids)), Exact: true},
	}, nil
}

func (s *SimpleShape) Optimize(ctx context.Context) (iterator.Shape, bool) {
	return s, false
}

func (s *SimpleShape) SubIterators() []iterator.Shape {
	return nil
}

// SimpleIterator is a Scanner implementation
type SimpleIterator struct {
	backend *SimpleBackend
	ids     []int64
	pos     int
	current refs.Ref
	err     error
}

func (it *SimpleIterator) Next(ctx context.Context) bool {
	it.pos++
	if it.pos >= len(it.ids) {
		return false
	}
	it.current = intRef(it.ids[it.pos])
	return true
}

func (it *SimpleIterator) Result() refs.Ref {
	return it.current
}

func (it *SimpleIterator) Err() error {
	return it.err
}

func (it *SimpleIterator) Close() error {
	return nil
}

func (it *SimpleIterator) String() string {
	return "SimpleIterator"
}

func (it *SimpleIterator) TagResults(tags map[string]refs.Ref) {}

func (it *SimpleIterator) NextPath(ctx context.Context) bool {
	return false
}

// SimpleLookup is an Index implementation
type SimpleLookup struct {
	backend *SimpleBackend
	ids     []int64
	current refs.Ref
}

func (it *SimpleLookup) Contains(ctx context.Context, v refs.Ref) bool {
	id := int64(v.(intRef))
	for _, candidate := range it.ids {
		if candidate == id {
			it.current = v
			return true
		}
	}
	return false
}

func (it *SimpleLookup) Result() refs.Ref {
	return it.current
}

func (it *SimpleLookup) Err() error {
	return nil
}

func (it *SimpleLookup) Close() error {
	return nil
}

func (it *SimpleLookup) String() string {
	return "SimpleLookup"
}

func (it *SimpleLookup) TagResults(tags map[string]refs.Ref) {}

func (it *SimpleLookup) NextPath(ctx context.Context) bool {
	return false
}
