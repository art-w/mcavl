A lock-free Set for OCaml multicore:

- The queries and updates are linearizable, such that you can pretend that they happened in a specific order when debugging your algorithms (even though their execution was interleaved.) The `copy` operation provides an `O(1)` snapshot of the set to observe a coherent copy-on-write view of the elements in the linearized timeline:

  ![Linearized timeline](https://art-w.github.io/mcset/linearized.png)

- The algorithms are a crossover between the imperative and purely functional AVL tree. Atomic updates are done in place such that other threads get immediate access... but rebalancing may need to update multiple nodes at once and so it creates new nodes to swap them in one atomic update:

  ![Atomic rebalancing](https://art-w.github.io/mcset/balance.png)

  Since the old nodes are still valid, this doesn't impact concurrent threads that could be traversing the old branches. The replaced nodes are marked "dead" to avoid loosing concurrent removes of their corresponding values (which must now be done on the new rebalanced nodes).

- The binary tree rebalancing is optimistic (!) The AVL tree can end up locally imbalanced when a node rebalancing was disturbed by nearby concurrent updates, either because it observed the wrong subtree's heights or because it was unable to complete the rebalancing without loosing progress from another thread. This seems to work well in practice, because this type of contention happens near the leaves: The higher-up the tree a node is, the less frequently it will be rebalanced and the less likely contention is to happen. Further operations on an imbalanced subtree will also generate new opportunities to repair it.

- The `add` operation always insert new elements at the leaves, and all other operations are careful to not drop a leaf that could concurrently welcome an insertion.

- The `remove` function requires a bit more care to play nice with concurrent threads. The standard procedure would "teleport" a leaf value to replace the removed one... but this risks hiding the teleported element from concurrent traversals that would then believe it didn't exist in the set. So the remove operation first mark the element as removed, then performs safe rotations to push that node towards the leaves where it can finally be cleared.

- The `copy`/`snapshot` functionality is in itself trivial, as it only signals that the original tree is now read-only and that further modifications have to perform copy-on-write on the visited nodes. However, a concurrent `add` or `remove` is now in a difficult situation as it is unclear if their effect happened before or after the copy... and choosing wrong breaks linearizability. The solution is to first signal their intent, then check that no other threads have indicated a causality violation before actually committing their effect. Time traveling is fine if no one can catch us!

- Given enough cores, performances are reasonable! In presence of contention, a concurrent operation will help other threads finish their work in order to avoid having to wait for them. There should be little busy looping to make progress: The worst case is a concurrent `copy` that requires the operation to restart from scratch on the new copy-on-write root. The expected cases are contention by inserting new elements at the exact same leaf (which only requires a local retry at that spot), or when encountering "dead" nodes (where it doesn't take long to walk back up the tree to discover the freshly rebalanced path.)

  ![Add 1m elements](https://art-w.github.io/mcset/test_add.png) ![Remove 1m elements](https://art-w.github.io/mcset/test_remove.png)
