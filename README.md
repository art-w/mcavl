A lock-free Set for OCaml multicore:

- The queries and updates are linearizable, such that you can pretend that they happened in a specific order when debugging your algorithms (even though their execution was interleaved.) The `copy` operation provides an `O(1)` snapshot of the set to observe a coherent copy-on-write view of the elements in the linearized timeline:

  ![Linearized timeline](https://art-w.github.io/mcset/linearized.png)

- The algorithms are a crossover between the imperative and purely functional AVL tree. Atomic updates are done in place such that other threads get immediate access... but rebalancing may need to update multiple nodes at once and so it creates new nodes to swap them in one atomic update:

  ![Atomic rebalancing](https://art-w.github.io/mcset/balance.png)

- The binary tree rebalancing is optimistic (!) The AVL tree can end up locally imbalanced when a node rebalancing was disturbed by nearby concurrent updates, either because it observed the wrong subtree's heights or because it was unable to complete the rebalancing. This seems to work well in practice, because this type of contention happens near the leaves: The higher-up the tree a node is, the less frequently it will be rebalanced and the less likely contention is to happen. Further operations on an imbalanced subtree will also generate new opportunities to repair it.

- Given enough cores, performances are reasonnable! In presence of contention, a concurrent operation may help other threads finish their work in order to avoid having to wait for them.

  ![Add 1m elements](https://art-w.github.io/mcset/test_add.png) ![Remove 1m elements](https://art-w.github.io/mcset/test_remove.png)
