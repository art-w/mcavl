module type QUERY = sig
  type elt

  type t

  (** {2 Queries} *)

  val is_empty : t -> bool
  (** [is_empty t] returns [true] when the set [t] contains no elements,
      [false] if it has at least one member. {b O(1)}  *)

  val mem : elt -> t -> bool
  (** [mem x t] returns [true] if the element [x] belongs to the set [t],
      [false] otherwise. {b O(logN)} *)

  val min_elt : t -> elt
  (** [min_elt t] returns the smallest element of the set [t],
      or raises [Not_found] if the set is empty. {b O(logN)} *)

  val min_elt_opt : t -> elt option
  (** [min_elt_opt t] returns the smallest element of the set [t],
      or [None] if the set is empty. {b O(logN)} *)

  val max_elt : t -> elt
  (** [max_elt t] returns the largest element of the set [t],
      or raises [Not_found] if the set is empty. {b O(logN)} *)

  val max_elt_opt : t -> elt option
  (** [max_elt_opt t] returns the largest element of the set [t],
      or [None] if the set is empty. {b O(logN)} *)

  val choose : t -> elt
  (** [choose t] returns an arbitrary element of the set [t],
      or raises [Not_found] if the set is empty. {b O(1)} *)

  val choose_opt : t -> elt option
  (** [choose_opt t] returns an arbitrary element of the set [t],
      or [None] if the set is empty. {b O(1)} *)

  val find : elt -> t -> elt
  (** [find x t] returns the element of the set that compares equal to [x],
      or raises [Not_found] if no such element exists. {b O(logN)} *)

  val find_opt : elt -> t -> elt option
  (** [find_opt x t] returns the element of the set that compares equal to [x],
      or [None] if no such element exists. {b O(logN)} *)

  val find_first : (elt -> bool) -> t -> elt
  (** [find_first predicate t] returns the smallest element of the set [t]
      that satisfies that monotonically increasing [predicate],
      or raises [Not_found] if no such element exists. {b O(logN)} *)

  val find_first_opt : (elt -> bool) -> t -> elt option
  (** [find_first_opt predicate t] returns the smallest element of the set [t]
      that satisfies that monotonically increasing [predicate],
      or [None] if no such element exists. {b O(logN)} *)

  val find_last : (elt -> bool) -> t -> elt
  (** [find_last predicate t] returns the largest element of the set [t]
      that satisfies that monotonically decreasing [predicate],
      or raises [Not_found] if no such element exists. {b O(logN)} *)

  val find_last_opt : (elt -> bool) -> t -> elt option
  (** [find_last_opt predicate t] returns the smallest element of the set [t]
      that satisfies that monotonically increasing [predicate],
      or [None] if no such element exists. {b O(logN)} *)
end

module type ITER = sig
  type elt

  type t

  val cardinal : t -> int
  (** [cardinal t] returns the number of unique elements in the set [t].
      {b O(N)} *)

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f t init] computes [f x0 init |> f x1 |> ... |> f xN],
      where [x0] ... [xN] are the elements of the set [t] in increasing order.
      Returns [init] if the set [t] was empty. *)

  val iter : (elt -> unit) -> t -> unit
  (** [iter f t] calls [f] on all the elements of the set [t] in increasing
      order. *)

  val for_all : (elt -> bool) -> t -> bool
  (** [for_all predicate t] returns [true] when all the elements of the set [t]
      satisfies the [predicate]. No ordering is guaranteed and the function
      will exit early if it finds an invalid element. *)

  val exists : (elt -> bool) -> t -> bool
  (** [exists predicate t] returns [true] when at least one element of the set
      [t] satisfies the [predicate]. No ordering is guaranteed and the function
      will exit early if it finds a valid element. *)

  val elements : t -> elt list
  (** [elements t] is the list of all the elements of [t] in increasing
      order. *)

  val to_list : t -> elt list
  (** Same as [elements]. *)

  val of_list : elt list -> t
  (** [of_list lst] is the set containing all the elements of the list [lst]. *)

  val to_seq : t -> elt Seq.t
  (** [to_seq t] is the sequence containing all the elements of the set [t] in
      increasing order. {b O(1)} creation then {b O(1)} amortized for each
      consumed element of the sequence (with {b O(logN) worst-case}) *)

  val to_rev_seq : t -> elt Seq.t
  (** [to_rev_seq t] is the sequence containing all the elements of the set [t]
      in decreasing order. *)

  val to_seq_from : elt -> t -> elt Seq.t
  (** [to_seq_from x t] is the sequence containing all the elements
      of the set [t] that are larger than or equal to [x],
      in increasing order. *)

  val of_seq : elt Seq.t -> t
  (** [of_set seq] is the set containing all the elements
      of the sequence [seq]. *)
end

module type Ordered = sig
  type t
  (** The type of comparable elements. *)

  val compare : t -> t -> int
  (** [compare a b] must return [0] if [a] equals [b], a negative number
      if [a] is stricly less than [b] and a positive number if [a] is strictly
      larger than [b]. *)
end

module Make (Ord : Ordered) = struct
  module type S = sig
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
    include QUERY with type elt := elt and type t := t

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

      val map : (elt -> elt) -> t -> t
      (** [map f t] returns a set containing the elements [f x0], [f x1], ..., [f xN]
          where [x0] ... [xN] are all the elements of the set [t].
          - The elements are passed to [f] in increasing order.
          - The result is physically equal to [t] if [f] always returned a physically
            equal element. {b O(NlogN)} worst-case
      *)

      val filter : (elt -> bool) -> t -> t
      (** [filter predicate t] returns the subset of elements of the set [t] that
          satistifies the [predicate] (called in increasing order). The resulting
          set is physical equal to [t] if no element was rejected. {b O(N)} *)

      val filter_map : (elt -> elt option) -> t -> t
      (** [filter_map predicate t] returns a set containing the [Some] elements
          of [f x0], [f x1], ..., [f xN] where [x0] ... [xN] are all the elements
          of the set [t].
          - The elements are passed to [f] in increasing order.
          - The result is physically equal to [t] if [f] always returned [Some]
            physically equal element. {b O(NlogN)} worst-case
      *)

      val partition : (elt -> bool) -> t -> t * t
      (** [partiton predicate t] returns two sets, the first one
          containing all the elements of the set [t] that satisfies [predicate],
          while the second contains all the rejected ones.
          - The elements are passed to [f] in increasing order.
          - The first set is physically equal to [t] if [f] always returned
            [true]
          - The second set is physically equal to [t] if [f] always returned
            [false]. {b O(N)}
      *)

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

      (** {2 Comparisons} *)

      val equal : t -> t -> bool
      (** [equal t1 t2] returns [true] if the sets [t1] and [t2] contain the same
          elements. *)

      val disjoint : t -> t -> bool
      (** [disjoint t1 t2] returns [true] if the sets [t1] and [t2] have
          no elements in common. *)

      val compare : t -> t -> int
      (** [compare t1 t2] is a total order, suitable for building sets of sets. *)

      (** @inline *)
      include QUERY with type elt := elt and type t := t

      (** {2 Iterators} *)

      (** @inline *)
      include ITER with type elt := elt and type t := t
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
    include ITER with type elt := elt and type t := t
  end
end
