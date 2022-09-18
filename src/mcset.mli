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

module Make (Ord : Set.OrderedType) : sig
  (** Functor building an implementation of the mutable set structure
      given a totally ordered type. *)

  type elt = Ord.t
  (** The type of set elements. *)

  type t
  (** The type of mutable sets. *)

  val empty : unit -> t
  (** [empty ()] returns a new empty set. {b O(1)} *)

  val singleton : elt -> t
  (** [singleton x] returns a new set containing only the element [x].
      {b O(1)} *)

  val add : elt -> t -> unit
  (** [add x t] inserts the element [x] into the set [t]. If [x] was already
      a member, then the set is unchanged. {b O(logN)} *)

  val remove : elt -> t -> bool
  (** [remove x t] removes the element [x] from the set [t]. It returns
      a boolean indicating if the removal was successful. If [false], the
      element [x] was already not a member and the set is unchanged.
      {b O(logN)} *)

  (** @inline *)
  include S.QUERY with type elt := elt and type t := t

  (** {2 Snapshots}

  Concurrent modifications of a set are linearizable. The snapshot/copy
  provides a coherent view of the elements of a set along this linearized
  timeline.

  Further updates to the original set (or its copies) will trigger a minimal
  copy-on-write of the internal substructures of the set. This doesn't impact
  the time complexity of any operations, but induces a corresponding memory
  complexity for copying the modified subparts once.

  *)

  val copy : t -> t
  (** [copy t] returns an independently mutable copy of the set [t]. Further
      modifications of the set [t] will not affect its copies (and vice-versa.)
      {b O(1)} *)

  module View : sig
    (** A read-only, non-mutable view of a set (with more operations,
        similar to the purely functional {! Stdlib.Set} interface) *)

    type elt = Ord.t
    (** The type of set elements. *)

    type t
    (** The type of read-only sets. *)

    val empty : t
    (** The empty set. *)

    val singleton : elt -> t
    (** [singleton x] returns a set containing only the element [x]. {b O(1)} *)

    val add : elt -> t -> t
    (** [add x t] returns a set containing the element [x] and all the
        elements of [t]. If [x] was already in the set [t], then the result
        is physically equal to [t]. {b O(logN)} *)

    val remove : elt -> t -> t
    (** [remove x t] returns a set containing the elements of [t] without [x].
        If [x] was not a member of the set [t], then the result is physically
        equal to [t]. {b O(logN)} *)

    val union : t -> t -> t
    (** [union t1 t2] returns a set containing all the elements
        of [t1] and [t2]. {b O(N)} worst-case *)

    val inter : t -> t -> t
    (** [inter t1 t2] returns a set containing the shared elements
        of [t1] and [t2]. {b O(N)} worst-case *)

    val diff : t -> t -> t
    (** [diff t1 t2] returns a set containing the elements of [t1] that are
        not members of the set [t2]. {b O(N)} worst-case *)

    val split : elt -> t -> t * bool * t
    (** [split x t] returns a triple [(smaller, found, larger)] such that:
        - [smaller] is the subset of elements strictly smaller than [x]
        - [larger] is the subset of elements strictly larger than [x]
        - [found] is [true] if [x] is a member of the set [t], [false] otherwise.
        {b O(logN)}
    *)

    val pop_min : t -> elt * t
    (** [pop_min t] returns the smallest element and the other elements
        of the set [t], or raises [Not_found] if the set [t] is empty.
        {b O(logN)} *)

    val pop_min_opt : t -> (elt * t) option
    (** [pop_min_opt t] returns the smallest element and the other elements
        of the set [t], or [None] if the set [t] is empty. {b O(logN)} *)

    val pop_max : t -> elt * t
    (** [pop_max t] returns the largest element and the other elements
        of the set [t], or raises [Not_found] if the set [t] is empty.
        {b O(logN)} *)

    val pop_max_opt : t -> (elt * t) option
    (** [pop_max_opt t] returns the largest element and the other elements
        of the set [t], or [None] if the set [t] is empty. *)

    val map : (elt -> elt) -> t -> t
    (** [map f t] returns a set containing the elements [f x0], [f x1], ..., [f xN]
        where [x0] ... [xN] are all the elements of the set [t].
        - The elements are passed to [f] in increasing order.
        - The result is physically equal to [t] if [f] always returned a physically
          equal element. {O(NlogN)} worst-case
    *)

    val filter : (elt -> bool) -> t -> t
    (** [filter predicate t] returns the subset of elements of the set [t] that
        satistifies the [predicate] (called in increasing order). The resulting
        set is physical equal to [t] if no element was rejected. {O(N)}
    *)

    val filter_map : (elt -> elt option) -> t -> t
    (** [filter_map predicate t] returns a set containing the [Some] elements
        of [f x0], [f x1], ..., [f xN] where [x0] ... [xN] are all the elements
        of the set [t].
        - The elements are passed to [f] in increasing order.
        - The result is physically equal to [t] if [f] always returned [Some]
          physically equal element. {O(NlogN)} worst-case
    *)

    val partition : (elt -> bool) -> t -> t * t
    (** [partiton predicate t] returns two sets, the first one
        containing all the elements of the set [t] that satisfies [predicate],
        while the second contains all the rejected ones.
        - The elements are passed to [f] in increasing order.
        - The first set is physically equal to [t] if [f] always returned
          [true]
        - The second set is physically equal to [t] if [f] always returned
          [false]. {O(N)}
    *)

    (** @inline *)
    include S.QUERY with type elt := elt and type t := t

    (** {2 Iterators} *)

    (** @inline *)
    include S.ITER with type elt := elt and type t := t
  end

  val snapshot : t -> View.t
  (** [snapshot t] returns a read-only view of the elements of the set [t].
      {b O(1)} *)

  val to_view : t -> View.t
  (** Same as [snapshot]. *)

  val of_view : View.t -> t
  (** [of_view v] returns a new mutable set containing all the elements
      of the view [v]. {b O(1)} *)

  (** {2 Iterators}

      The following functions all proceed on a coherent {! snapshot} of their
      set [t] argument, created at the start of their execution. Concurrent
      modifications of the original set [t] during their execution will not
      be observed by these traversals.
  *)

  (** @inline *)
  include S.ITER with type elt := elt and type t := t
end
