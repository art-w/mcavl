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
    (** A read-only, non-mutable view of a set (similar to the purely
        functional {! Stdlib.Set}.)  *)

    type elt = Ord.t
    (** The type of set elements. *)

    type t
    (** The type of read-only sets. *)

    val empty : t
    (** The empty set. *)

    val singleton : elt -> t
    (** [singleton x] returns a set containing only the element [x]. *)

    val add : elt -> t -> t
    (** [add x t] returns a set containing the element [x] and all the
        elements of [t]. If [x] was already in the set [t], then the result
        is physically equal to [t]. *)

    val remove : elt -> t -> t
    (** [remove x t] returns a set containing the elements of [t] without [x].
        If [x] was not a member of the set [t], then the result is physically
        equal to [t]. *)

    val pop_min : t -> elt * t
    (** [pop_min t] returns the smallest element and the other elements
        of the set [t], or raises [Not_found] if the set [t] is empty. *)

    val pop_min_opt : t -> (elt * t) option
    (** [pop_min_opt t] returns the smallest element and the other elements
        of the set [t], or [None] if the set [t] is empty. *)

    val pop_max : t -> elt * t
    (** [pop_max t] returns the largest element and the other elements
        of the set [t], or raises [Not_found] if the set [t] is empty. *)

    val pop_max_opt : t -> (elt * t) option
    (** [pop_max_opt t] returns the largest element and the other elements
        of the set [t], or [None] if the set [t] is empty. *)

    (** @inline *)
    include S.QUERY with type elt := elt and type t := t

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

  (** {2 Iterators} *)

  (** The following functions all proceed on a coherent {! snapshot} of their
      set [t] argument, created at the start of their execution. Concurrent
      modifications of the original set [t] during their execution will not
      be observed by these traversals.
  *)

  (** @inline *)
  include S.ITER with type elt := elt and type t := t
end
