// REPL:     quint -r distributed.qnt::gossip
// Simulate: quint run --max-steps=20 --invariant=allChainsValid distributed.qnt
//
// Pattern 1: Shared state indexed by process ID
//
// Processes are not separate modules — they are indices into maps.
// Communication happens through shared global state, not channels or
// mailboxes. The interleaving of process actions comes from nondet node
// selection in step.
//
// This is the standard TLA+/Quint pattern for distributed systems.
//
// Refs:
//   https://github.com/informalsystems/quint/blob/main/examples/classic/distributed/ewd840/ewd840.qnt
//   https://github.com/informalsystems/quint/blob/main/examples/classic/distributed/TeachingConcurrency/teaching.qnt
module gossip {
  type Node = int
  type Block = { slot: int, hash: str }

  val N = 3
  val Nodes = 0.to(N - 1)

  // Per-node state as maps: each node has its own chain
  var chains: Node -> List[Block]

  val genesis: Block = { slot: 0, hash: "genesis" }

  action init = {
    // mapBy builds a map from a set: for each node, the initial chain is [genesis].
    // _ means we don't use the node id — every node starts with the same chain.
    chains' = Nodes.mapBy(_ => [genesis])
  }

  // A node produces a new block extending its own chain
  action forge(n: Node): bool = {
    val chain = chains.get(n)
    val tip = chain[chain.length() - 1]
    val newBlock = { slot: tip.slot + 1, hash: "block" }
    all {
      // Guard: only forge if chain is short (bound the state space)
      chain.length() < 5,
      chains' = chains.set(n, chain.append(newBlock)),
    }
  }

  // A node adopts another node's chain if it is longer.
  // This is an abstraction: in reality, sender would transmit headers/blocks
  // over the network, and n would receive and validate them over time. Here
  // we collapse that into one atomic step — n reads sender's chain directly.
  // This is the right level for checking high-level properties (e.g.
  // convergence). To model network delays, message loss, or partial
  // transfers, use pattern 2: explicit message buffers as state variables
  // (see messaging.qnt).
  action adopt(n: Node, sender: Node): bool = all {
    n != sender,
    chains.get(sender).length() > chains.get(n).length(),
    // NOTE: here is where we model "communication" between sender receiver (`n` in this case).
    // We're assuming we're receiving the whole chain at once.
    chains' = chains.set(n, chains.get(sender)),
  }

  // Step: nondeterministically pick a node and an action.
  // This is where interleaving comes from — each step simulates one
  // process acting, while all other processes stay unchanged.
  action step = {
    nondet n = Nodes.oneOf()
    any {
      forge(n),
      // nondet is a binding statement: bind sender, then evaluate the next
      // line that uses it (like let ... in). You can't inline it as
      // adopt(n, Nodes.oneOf()) — the nondet keyword is required.
      // This nondet is scoped to the adopt branch — not evaluated if
      // forge(n) is chosen. Guards inside adopt filter out invalid
      // combinations (same node, shorter chain).
      nondet sender = Nodes.oneOf()
      adopt(n, sender),
    }
  }

  // Invariants
  val allChainsValid = Nodes.forall(n => chains.get(n).length() > 0)
  val allChainsStartWithGenesis = Nodes.forall(n => chains.get(n)[0] == genesis)
}
