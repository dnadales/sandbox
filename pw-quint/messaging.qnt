// REPL:     quint -r messaging.qnt::messaging
// Simulate: quint run --max-steps=30 --invariant=noMessageLoss messaging.qnt
//
// Pattern 2: Message passing via state variables
//
// Messages are not first-class in Quint. Instead, model them as state
// variables: a per-node inbox (set of messages), a global in-flight set,
// or a queue. Actions enqueue/dequeue explicitly.
//
// This lets you model network delays, reordering, loss, and duplication
// — things pattern 1 (shared state) abstracts away.
//
// Refs:
//   https://learntla.com/topics/message-queues.html
//   https://github.com/informalsystems/quint/blob/main/examples/classic/distributed/ewd840/ewd840.qnt
module messaging {
  type Node = int
  type Block = { slot: int, hash: str }

  // A message carries a block from a sender to a receiver
  type Msg = { from: Node, to: Node, block: Block }

  val N = 3
  val Nodes = 0.to(N - 1)

  val genesis: Block = { slot: 0, hash: "genesis" }

  // Per-node state
  var chains: Node -> List[Block]
  // The network: a set of in-flight messages.
  // Messages sit here until the receiver picks them up.
  var network: Set[Msg]
  // Track total sent/received for the invariant
  var sent: int
  var received: int

  action init = all {
    chains' = Nodes.mapBy(_ => [genesis]),
    network' = Set(),
    sent' = 0,
    received' = 0,
  }

  // A node forges a block and broadcasts it to all other nodes.
  // The block doesn't appear in anyone's chain yet — it goes into the
  // network as a set of messages, one per recipient.
  action forge(n: Node): bool = {
    val chain = chains.get(n)
    val tip = chain[chain.length() - 1]
    val newBlock = { slot: tip.slot + 1, hash: "block" }
    val msgs = Nodes.exclude(Set(n)).map(r => { from: n, to: r, block: newBlock })
    all {
      chain.length() < 5,
      chains' = chains.set(n, chain.append(newBlock)),
      network' = network.union(msgs),
      sent' = sent + msgs.size(),
      received' = received,
    }
  }

  // A node receives one message from the network.
  // The message is removed from the network (consumed).
  action receive(n: Node, msg: Msg): bool = all {
    // Guard: message exists and is addressed to this node
    network.contains(msg),
    msg.to == n,
    // Only append if the block extends our chain
    val chain = chains.get(n)
    val tip = chain[chain.length() - 1]
    all {
      msg.block.slot > tip.slot,
      chains' = chains.set(n, chain.append(msg.block)),
      network' = network.exclude(Set(msg)),
      sent' = sent,
      received' = received + 1,
    }
  }

  // A message is dropped from the network (models network loss)
  action drop(msg: Msg): bool = all {
    network.contains(msg),
    network' = network.exclude(Set(msg)),
    chains' = chains,
    sent' = sent,
    received' = received,
  }

  action step = {
    nondet n = Nodes.oneOf()
    any {
      forge(n),
      // Pick an arbitrary message from the network for this node to receive
      nondet msg = network.oneOf()
      receive(n, msg),
      // Optionally: model message loss
      nondet msg = network.oneOf()
      drop(msg),
    }
  }

  // With message loss enabled, this invariant will eventually be violated:
  // sent > received means some messages were dropped, not lost from the system.
  val noMessageLoss = sent == received + network.size()
}
