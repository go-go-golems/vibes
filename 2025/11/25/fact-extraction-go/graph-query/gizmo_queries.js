// Gizmo API Queries for Fact Extraction Graph
// 
// This file contains advanced graph queries using Cayley's Gizmo API
// Run with: cayley http --dbpath=cayley.db --host=:64210
// Then execute queries via HTTP API or Cayley REPL

// ============================================================================
// MORPHISMS - Reusable path patterns
// ============================================================================

// Define a morphism for "acted upon" relationships
var actedUpon = g.Morphism()
  .out("action")
  .out("target");

// Define a morphism for reverse relationships (who acted on X)
var actedUponBy = g.Morphism()
  .in("target")
  .in("action");

// Define a morphism for 2-hop relationships
var twoHopRelationship = g.Morphism()
  .out("action")
  .out("target")
  .out("action")
  .out("target");

// ============================================================================
// QUERY 1: Find all relationships for a specific person
// ============================================================================

function findAllRelationships(personName) {
  return g.V(personName)
    .tag("person")
    .out("action")
    .tag("action")
    .out("target")
    .tag("target")
    .all();
}

// Example: findAllRelationships("Jeffrey Epstein")

// ============================================================================
// QUERY 2: Find mutual connections (people connected to both A and B)
// ============================================================================

function findMutualConnections(personA, personB) {
  var aConnections = g.V(personA).follow(actedUpon);
  var bConnections = g.V(personB).follow(actedUpon);
  
  return aConnections.intersect(bConnections).all();
}

// Example: findMutualConnections("Jeffrey Epstein", "Ghislaine Maxwell")

// ============================================================================
// QUERY 3: Find influencers (people with many incoming connections)
// ============================================================================

function findInfluencers(minConnections) {
  // Get all people
  var people = g.V().has("actor").toArray();
  
  var results = [];
  people.forEach(function(person) {
    var incomingCount = g.V(person)
      .followR(actedUpon)
      .count();
    
    if (incomingCount >= minConnections) {
      g.emit({
        person: person,
        incoming_connections: incomingCount
      });
    }
  });
}

// Example: findInfluencers(5)

// ============================================================================
// QUERY 4: Find paths between two people
// ============================================================================

function findPathsBetween(startPerson, endPerson, maxHops) {
  // Use followRecursive with a limit
  var connection = g.Morphism().out("action").out("target");
  
  return g.V(startPerson)
    .tag("start")
    .followRecursive(connection)
    .is(endPerson)
    .tag("end")
    .all();
}

// Example: findPathsBetween("Donald Trump", "Virginia Roberts Giuffre", 3)

// ============================================================================
// QUERY 5: Find people by relationship type (action)
// ============================================================================

function findByAction(actionType) {
  return g.V()
    .has("action", actionType)
    .tag("relationship")
    .in("action")
    .tag("actor")
    .out("target")
    .tag("target")
    .all();
}

// Example: findByAction("met with")

// ============================================================================
// QUERY 6: Find relationships within a cluster/topic
// ============================================================================

function findRelationshipsInCluster(clusterTheme) {
  return g.V()
    .has("cluster_themes", clusterTheme)
    .tag("triple")
    .out("actor")
    .tag("actor")
    .back("triple")
    .out("target")
    .tag("target")
    .back("triple")
    .out("action")
    .tag("action")
    .all();
}

// Example: findRelationshipsInCluster("Sexual Abuse and Misconduct")

// ============================================================================
// QUERY 7: Find temporal patterns (relationships in time range)
// ============================================================================

function findRelationshipsInTimeRange(startDate, endDate) {
  return g.V()
    .has("timestamp")
    .filter(function(node) {
      var ts = node.timestamp;
      return ts >= startDate && ts <= endDate;
    })
    .tag("triple")
    .out("actor")
    .tag("actor")
    .back("triple")
    .out("target")
    .tag("target")
    .all();
}

// Example: findRelationshipsInTimeRange("2000-01-01", "2005-12-31")

// ============================================================================
// QUERY 8: Find network neighborhood (N-hop connections)
// ============================================================================

function findNetworkNeighborhood(personName, hops) {
  var connection = g.Morphism().out("action").out("target");
  
  return g.V(personName)
    .tag("center")
    .followRecursive(connection)
    .tag("neighbor")
    .all();
}

// Example: findNetworkNeighborhood("Alan Dershowitz", 2)

// ============================================================================
// QUERY 9: Find relationship chains (A -> B -> C pattern)
// ============================================================================

function findRelationshipChains(startPerson) {
  return g.V(startPerson)
    .tag("person_a")
    .out("action")
    .tag("action_ab")
    .out("target")
    .tag("person_b")
    .out("action")
    .tag("action_bc")
    .out("target")
    .tag("person_c")
    .all();
}

// Example: findRelationshipChains("Jeffrey Epstein")

// ============================================================================
// QUERY 10: Find people with specific tags
// ============================================================================

function findByTags(tagList) {
  return g.V()
    .has("triple_tags")
    .filter(function(node) {
      var tags = JSON.parse(node.triple_tags || "[]");
      return tagList.some(function(tag) {
        return tags.indexOf(tag) >= 0;
      });
    })
    .tag("triple")
    .out("actor")
    .tag("actor")
    .back("triple")
    .out("target")
    .tag("target")
    .all();
}

// Example: findByTags(["sexual abuse", "trafficking"])

// ============================================================================
// QUERY 11: Find central figures (high degree centrality)
// ============================================================================

function findCentralFigures() {
  var allPeople = g.V().has("actor").toArray();
  
  var centrality = [];
  allPeople.forEach(function(person) {
    var outgoing = g.V(person).follow(actedUpon).count();
    var incoming = g.V(person).followR(actedUpon).count();
    
    g.emit({
      person: person,
      outgoing_connections: outgoing,
      incoming_connections: incoming,
      total_degree: outgoing + incoming
    });
  });
}

// Example: findCentralFigures()

// ============================================================================
// QUERY 12: Find common patterns (frequent relationship types)
// ============================================================================

function findCommonPatterns() {
  var actions = g.V().out("action").toArray();
  
  var actionCounts = {};
  actions.forEach(function(action) {
    actionCounts[action] = (actionCounts[action] || 0) + 1;
  });
  
  Object.keys(actionCounts).forEach(function(action) {
    g.emit({
      action: action,
      count: actionCounts[action]
    });
  });
}

// Example: findCommonPatterns()

// ============================================================================
// QUERY 13: Find isolated nodes (people with no connections)
// ============================================================================

function findIsolatedNodes() {
  return g.V()
    .except(
      g.V().out("action").out("target")
    )
    .except(
      g.V().followR(actedUpon)
    )
    .all();
}

// Example: findIsolatedNodes()

// ============================================================================
// QUERY 14: Find relationship triangles (A->B, B->C, C->A)
// ============================================================================

function findTriangles() {
  return g.V()
    .tag("a")
    .out("action").out("target")
    .tag("b")
    .out("action").out("target")
    .tag("c")
    .out("action").out("target")
    .is(g.V().tag("a"))
    .all();
}

// Example: findTriangles()

// ============================================================================
// QUERY 15: Find relationships with specific metadata
// ============================================================================

function findRelationshipsWithMetadata(field, value) {
  return g.V()
    .has(field, value)
    .tag("triple")
    .out("actor")
    .tag("actor")
    .back("triple")
    .out("target")
    .tag("target")
    .back("triple")
    .out("action")
    .tag("action")
    .all();
}

// Example: findRelationshipsWithMetadata("location", "Palm Beach")

// ============================================================================
// EXPORT FUNCTIONS (for use in Go)
// ============================================================================

module.exports = {
  findAllRelationships,
  findMutualConnections,
  findInfluencers,
  findPathsBetween,
  findByAction,
  findRelationshipsInCluster,
  findRelationshipsInTimeRange,
  findNetworkNeighborhood,
  findRelationshipChains,
  findByTags,
  findCentralFigures,
  findCommonPatterns,
  findIsolatedNodes,
  findTriangles,
  findRelationshipsWithMetadata
};
