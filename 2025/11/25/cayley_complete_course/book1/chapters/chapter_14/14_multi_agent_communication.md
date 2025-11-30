

# Chapter 14: Multi-Agent Communication

In the previous chapter, we laid the groundwork for our blackboard system by defining its core components and implementing a basic task-passing mechanism. Now, we will delve deeper into the heart of any multi-agent system: **communication**. Effective communication is essential for agents to coordinate their actions, share knowledge, and work together to achieve a common goal. This chapter explores advanced communication patterns, including agent discovery, message passing, and conflict resolution, all implemented on our Cayley blackboard.

## Agent Registration and Discovery

Before agents can communicate, they need to be aware of each other's existence and capabilities. A robust multi-agent system requires a mechanism for agents to register themselves and for other agents to discover them based on their expertise or status. We can implement an **agent registry** on our blackboard using quads.

When an agent comes online, it should add a set of quads to the blackboard that describe itself:

```go
func registerAgent(store *cayley.Handle, agentID, name string, expertise []string) {
	timestamp := time.Now().Format(time.RFC3339)
	t := cayley.NewTransaction()
	t.AddQuad(quad.Make(agentID, "rdf:type", "bb:Agent", timestamp))
	t.AddQuad(quad.Make(agentID, "bb:name", name, timestamp))
	t.AddQuad(quad.Make(agentID, "bb:status", "active", timestamp))
	for _, exp := range expertise {
		t.AddQuad(quad.Make(agentID, "bb:expertise", exp, timestamp))
	}
	store.ApplyTransaction(t)
}
```

Other agents can then discover available agents by querying the blackboard. For example, to find all active agents with expertise in "natural_language_processing":

```go
p := cayley.StartPath(store).
	Has(quad.IRI("rdf:type"), quad.IRI("bb:Agent")).
	Has(quad.IRI("bb:status"), quad.String("active")).
	Has(quad.IRI("bb:expertise"), quad.String("natural_language_processing"))
```

This simple pattern allows for a dynamic and decentralized system where agents can join and leave the network, and the system can adapt accordingly.

## Message Passing via Quads

Direct communication between agents can be modeled as message passing. We can represent messages as entities on the blackboard, with properties like sender, receiver, content, and timestamp. This creates an auditable trail of all communication within the system.

A message schema could look like this:

```go
type Message struct {
	ID        quad.IRI  `json:"@id"`
	Type      string    `quad:"@type > bb:Message"`
	From      quad.IRI  `json:"bb:from"`
	To        quad.IRI  `json:"bb:to"`
	Content   string    `json:"bb:content"`
	Timestamp time.Time `json:"bb:timestamp"`
	InReplyTo quad.IRI  `json:"bb:inReplyTo"`
}
```

An agent can send a message by creating a new message entity on the blackboard. Other agents can then query for messages addressed to them:

```go
// Find all new messages for agent:worker1
p := cayley.StartPath(store).
	Has(quad.IRI("rdf:type"), quad.IRI("bb:Message")).
	Has(quad.IRI("bb:to"), quad.IRI("agent:worker1")).
	Has(quad.IRI("bb:status"), quad.String("unread"))
```

## Task Assignment and Claiming

In many multi-agent systems, tasks are not assigned directly but are posted to a common pool for agents to claim. This **task claiming** pattern promotes autonomy and allows the most suitable or available agent to pick up the work. We implemented a basic version of this in the previous chapter. Now, let's refine it.

When a task is posted, it should have a `pending` status. An agent can then query for pending tasks that match its expertise. To prevent multiple agents from claiming the same task, the claiming process must be **atomic**. This is a perfect use case for Cayley's transactions.

An agent would perform the following steps to claim a task:

1.  **Find a pending task** that matches its expertise.
2.  **Create a transaction** that:
    a.  Changes the task's status from `pending` to `in_progress`.
    b.  Assigns the task to itself using the `bb:assignedTo` predicate.
3.  **Apply the transaction**.

If two agents try to claim the same task simultaneously, only one transaction will succeed (depending on the backend's concurrency control), ensuring that each task is handled by only one agent.

## Conflict Resolution

In any collaborative system, conflicts are inevitable. Two agents might propose contradictory hypotheses, or they might try to modify the same piece of information in incompatible ways. A robust blackboard system must have a strategy for resolving these conflicts.

One common approach is to use **confidence scores**. When an agent posts a hypothesis, it can also post a confidence score indicating how certain it is about the information. The control shell or other specialized agents can then use these scores to resolve conflicts.

For example, if we have two conflicting hypotheses:

*   `{hyp:001, bb:content, "The sky is blue", confidence: 0.95}`
*   `{hyp:002, bb:content, "The sky is green", confidence: 0.60}`

A reasoning agent could detect this conflict by querying for hypotheses about the same subject and, based on the confidence scores, mark the weaker hypothesis as `superseded`.

Another approach is to use **voting**. When a conflict arises, other agents can "vote" on the conflicting hypotheses by adding `supports` or `disputes` quads. The hypothesis with the most support wins.

```nquads
<agent:1> <bb:supports> <hyp:001> .
<agent:2> <bb:supports> <hyp:001> .
<agent:3> <bb:disputes> <hyp:001> .
```

By querying for the number of `supports` and `disputes` for each hypothesis, the system can reach a consensus.

These communication patterns—agent discovery, message passing, task claiming, and conflict resolution—are the building blocks of sophisticated multi-agent systems. By implementing them on a Cayley blackboard, you can create a flexible, scalable, and auditable architecture for collaborative AI.

---

### References

[1] Wooldridge, Michael. "An Introduction to MultiAgent Systems." John Wiley & Sons, 2009.

[2] Durfee, Edmund H. "Distributed Problem Solving and Planning." Multiagent Systems: A Modern Approach to Distributed Artificial Intelligence, 1999.
