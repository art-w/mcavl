(** An imperative, mutable, implementation of totally ordered Sets and Maps
    datastructures, with the following properties:

  - {b Thread-safe:} concurrent updates from multiple domains are possible.
    These collections can typically be used to synchronize multiple workers
    towards a common goal.
  - {b Lock-free:} concurrent modifications are able to make progress without
    waiting for others to finish.
  - {b Linearizable:} concurrent operations appears to take place sequentially
    on a linearized timeline, providing a coherent collection of elements at
    all time. It is furthermore possible to take an {b O(1)} snapshot/copy
    to observe its collection of elements at a given point in time.
*)

module type Ordered = S.Ordered

module Set (Ord : Ordered) : S.Set(Ord).S

module Map (Ord : Ordered) : S.Map(Ord).S
