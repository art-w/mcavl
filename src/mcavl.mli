(** An imperative, mutable, implementation of a totally ordered Set
    datastructure, with the following properties:

  - {b Thread-safe:} concurrent updates from multiple domains are possible.
    These sets can typically be used to synchronize multiple workers on a common goal.
  - {b Lock-free:} concurrent modifications are able to make progress without
    waiting for others to finish.
  - {b Linearizable:} concurrent operations appears to take place sequentially
    on a linearized timeline, providing a coherent set of elements at all time.
    It is furthermore possible to take an {b O(1)} snapshot/copy of the set to
    observe its collection of elements at a given point in time.
*)

(** {1 Sets} *)

module type Ordered = S.Ordered

module Set (Ord : Ordered) : S.Set(Ord).S

(** {1 Polymorphic sets} *)

module type Ordered_poly = S.Ordered_poly

module Set_poly (Ord : Ordered_poly) : S.Set_poly(Ord).S
