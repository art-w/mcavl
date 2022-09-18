module type QUERY = sig
  type 'a elt

  type 'a t

  (** {1 Queries} *)

  val is_empty : 'a t -> bool
  (** [is_empty t] returns [true] when the set [t] contains no elements,
      [false] if it has at least one member. {b O(1)}  *)

  val mem : 'a elt -> 'a t -> bool
  (** [mem x t] returns [true] if the element [x] belongs to the set [t],
      [false] otherwise. {b O(logN)} *)

  val min_elt : 'a t -> 'a elt
  (** [min_elt t] returns the smallest element of the set [t],
      or raises [Not_found] if the set is empty. {b O(logN)} *)

  val min_elt_opt : 'a t -> 'a elt option
  (** [min_elt_opt t] returns the smallest element of the set [t],
      or [None] if the set is empty. {b O(logN)} *)

  val max_elt : 'a t -> 'a elt
  (** [max_elt t] returns the largest element of the set [t],
      or raises [Not_found] if the set is empty. {b O(logN)} *)

  val max_elt_opt : 'a t -> 'a elt option
  (** [max_elt_opt t] returns the largest element of the set [t],
      or [None] if the set is empty. {b O(logN)} *)

  val choose : 'a t -> 'a elt
  (** [choose t] returns an arbitrary element of the set [t],
      or raises [Not_found] if the set is empty. {b O(1)} *)

  val choose_opt : 'a t -> 'a elt option
  (** [choose_opt t] returns an arbitrary element of the set [t],
      or [None] if the set is empty. {b O(1)} *)

  val find : 'a elt -> 'a t -> 'a elt
  (** [find x t] returns the element of the set that compares equal to [x],
      or raises [Not_found] if no such element exists. {b O(logN)} *)

  val find_opt : 'a elt -> 'a t -> 'a elt option
  (** [find_opt x t] returns the element of the set that compares equal to [x],
      or [None] if no such element exists. {b O(logN)} *)

  val find_first : ('a elt -> bool) -> 'a t -> 'a elt
  (** [find_first predicate t] returns the smallest element of the set [t]
      that satisfies that monotonically increasing [predicate],
      or raises [Not_found] if no such element exists. {b O(logN)} *)

  val find_first_opt : ('a elt -> bool) -> 'a t -> 'a elt option
  (** [find_first_opt predicate t] returns the smallest element of the set [t]
      that satisfies that monotonically increasing [predicate],
      or [None] if no such element exists. {b O(logN)} *)

  val find_last : ('a elt -> bool) -> 'a t -> 'a elt
  (** [find_last predicate t] returns the largest element of the set [t]
      that satisfies that monotonically decreasing [predicate],
      or raises [Not_found] if no such element exists. {b O(logN)} *)

  val find_last_opt : ('a elt -> bool) -> 'a t -> 'a elt option
  (** [find_last_opt predicate t] returns the smallest element of the set [t]
      that satisfies that monotonically increasing [predicate],
      or [None] if no such element exists. {b O(logN)} *)
end

module type ITER = sig
  type 'a elt

  type 'a t

  val cardinal : 'a t -> int
  (** [cardinal t] returns the number of unique elements in the set [t].
      {b O(N)} *)

  val fold : ('a elt -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** [fold f t init] computes [f x0 init |> f x1 |> ... |> f xN],
      where [x0] ... [xN] are the elements of the set [t] in increasing order.
      Returns [init] if the set [t] was empty. *)

  val iter : ('a elt -> unit) -> 'a t -> unit
  (** [iter f t] calls [f] on all the elements of the set [t] in increasing
      order. *)

  val for_all : ('a elt -> bool) -> 'a t -> bool
  (** [for_all predicate t] returns [true] when all the elements of the set [t]
      satisfies the [predicate]. No ordering is guaranteed and the function
      will exit early if it finds an invalid element. *)

  val exists : ('a elt -> bool) -> 'a t -> bool
  (** [exists predicate t] returns [true] when at least one element of the set
      [t] satisfies the [predicate]. No ordering is guaranteed and the function
      will exit early if it finds a valid element. *)

  val elements : 'a t -> 'a elt list
  (** [elements t] is the list of all the elements of [t] in increasing
      order. *)

  val to_list : 'a t -> 'a elt list
  (** Same as [elements]. *)

  val of_list : 'a elt list -> 'a t
  (** [of_list lst] is the set containing all the elements of the list [lst]. *)

  val to_seq : 'a t -> 'a elt Seq.t
  (** [to_seq t] is the sequence containing all the elements of the set [t] in
      increasing order. {b O(1)} creation then {b O(1)} amortized for each
      consumed element of the sequence (with {b O(logN) worst-case}) *)

  val to_rev_seq : 'a t -> 'a elt Seq.t
  (** [to_rev_seq t] is the sequence containing all the elements of the set [t]
      in decreasing order. *)

  val to_seq_from : 'a elt -> 'a t -> 'a elt Seq.t
  (** [to_seq_from x t] is the sequence containing all the elements
      of the set [t] that are larger than or equal to [x],
      in increasing order. *)

  val of_seq : 'a elt Seq.t -> 'a t
  (** [of_set seq] is the set containing all the elements
      of the sequence [seq]. *)
end

module type Ordered_poly = sig
  (** Totally ordered polymorphic type. *)

  type 'a t
  (** The type of comparable polymorphic elements. *)

  val compare : 'a t -> 'a t -> int
  (** [compare a b] must return [0] if [a] equals [b], a negative number
      if [a] is stricly less than [b] and a positive number if [a] is strictly
      larger than [b]. *)
end

module type Ordered = sig
  (** Totally ordered type. *)

  type t
  (** The type of comparable elements. *)

  include Ordered_poly with type _ t := t
end

module View_poly (Ord : Ordered_poly) = struct
  module type S = sig
    (** A read-only, non-mutable view of a polymorphic set (with more
        operations, similar to the purely functional {! Stdlib.Set}
        interface) *)

    type 'a elt = 'a Ord.t
    (** The type of polymorphic set elements. *)

    type 'a t
    (** The type of read-only polymorphic sets. *)

    val empty : 'a t
    (** The empty set. *)

    val singleton : 'a elt -> 'a t
    (** [singleton x] returns a set containing only the element [x]. {b O(1)} *)

    val add : 'a elt -> 'a t -> 'a t
    (** [add x t] returns a set containing the element [x] and all the
        elements of [t]. If [x] was already in the set [t], then the result
        is physically equal to [t]. {b O(logN)} *)

    val remove : 'a elt -> 'a t -> 'a t
    (** [remove x t] returns a set containing the elements of [t] without [x].
        If [x] was not a member of the set [t], then the result is physically
        equal to [t]. {b O(logN)} *)

    val union : 'a t -> 'a t -> 'a t
    (** [union t1 t2] returns a set containing all the elements
        of [t1] and [t2]. {b O(N)} worst-case *)

    val inter : 'a t -> 'a t -> 'a t
    (** [inter t1 t2] returns a set containing the shared elements
        of [t1] and [t2]. {b O(N)} worst-case *)

    val diff : 'a t -> 'a t -> 'a t
    (** [diff t1 t2] returns a set containing the elements of [t1] that are
        not members of the set [t2]. {b O(N)} worst-case *)

    val map : ('a elt -> 'a elt) -> 'a t -> 'a t
    (** [map f t] returns a set containing the elements [f x0], [f x1], ..., [f xN]
        where [x0] ... [xN] are all the elements of the set [t].
        - The elements are passed to [f] in increasing order.
        - The result is physically equal to [t] if [f] always returned a physically
          equal element. {b O(NlogN)} worst-case
      *)

    val filter : ('a elt -> bool) -> 'a t -> 'a t
    (** [filter predicate t] returns the subset of elements of the set [t] that
        satistifies the [predicate] (called in increasing order). The resulting
        set is physical equal to [t] if no element was rejected. {b O(N)} *)

    val filter_map : ('a elt -> 'a elt option) -> 'a t -> 'a t
    (** [filter_map predicate t] returns a set containing the [Some] elements
        of [f x0], [f x1], ..., [f xN] where [x0] ... [xN] are all the elements
        of the set [t].
        - The elements are passed to [f] in increasing order.
        - The result is physically equal to [t] if [f] always returned [Some]
          physically equal element. {b O(NlogN)} worst-case
      *)

    val partition : ('a elt -> bool) -> 'a t -> 'a t * 'a t
    (** [partiton predicate t] returns two sets, the first one
        containing all the elements of the set [t] that satisfies [predicate],
        while the second contains all the rejected ones.
        - The elements are passed to [f] in increasing order.
        - The first set is physically equal to [t] if [f] always returned
          [true]
        - The second set is physically equal to [t] if [f] always returned
          [false]. {b O(N)}
      *)

    val split : 'a elt -> 'a t -> 'a t * bool * 'a t
    (** [split x t] returns a triple [(smaller, found, larger)] such that:
        - [smaller] is the subset of elements strictly smaller than [x]
        - [larger] is the subset of elements strictly larger than [x]
        - [found] is [true] if [x] is a member of the set [t], [false] otherwise.
        {b O(logN)}
      *)

    val pop_min : 'a t -> 'a elt * 'a t
    (** [pop_min t] returns the smallest element and the other elements
        of the set [t], or raises [Not_found] if the set [t] is empty.
        {b O(logN)} *)

    val pop_min_opt : 'a t -> ('a elt * 'a t) option
    (** [pop_min_opt t] returns the smallest element and the other elements
        of the set [t], or [None] if the set [t] is empty. {b O(logN)} *)

    val pop_max : 'a t -> 'a elt * 'a t
    (** [pop_max t] returns the largest element and the other elements
        of the set [t], or raises [Not_found] if the set [t] is empty.
        {b O(logN)} *)

    val pop_max_opt : 'a t -> ('a elt * 'a t) option
    (** [pop_max_opt t] returns the largest element and the other elements
        of the set [t], or [None] if the set [t] is empty. *)

    (** {1 Comparisons} *)

    val equal : 'a t -> 'a t -> bool
    (** [equal t1 t2] returns [true] if the sets [t1] and [t2] contain the same
        elements. *)

    val disjoint : 'a t -> 'a t -> bool
    (** [disjoint t1 t2] returns [true] if the sets [t1] and [t2] have
        no elements in common. *)

    val compare : 'a t -> 'a t -> int
    (** [compare t1 t2] is a total order, suitable for building sets of sets. *)

    (** @inline *)
    include QUERY with type 'a elt := 'a elt and type 'a t := 'a t

    (** {1 Iterators} *)

    (** @inline *)
    include ITER with type 'a elt := 'a elt and type 'a t := 'a t
  end
end

module Set_poly (Ord : Ordered_poly) = struct
  module type S = sig
    (** Thread-safe mutable set structure given
        a totally ordered polymorphic type. *)

    type 'a elt = 'a Ord.t
    (** The type of set elements. *)

    type 'a t
    (** The type of mutable sets. *)

    val empty : unit -> 'a t
    (** [empty ()] returns a new empty set. {b O(1)} *)

    val singleton : 'a elt -> 'a t
    (** [singleton x] returns a new set containing only the element [x].
        {b O(1)} *)

    val add : 'a elt -> 'a t -> unit
    (** [add x t] inserts the element [x] into the set [t]. If [x] was already
        a member, then the set is unchanged. {b O(logN)} *)

    val remove : 'a elt -> 'a t -> bool
    (** [remove x t] removes the element [x] from the set [t]. It returns
        a boolean indicating if the removal was successful. If [false], the
        element [x] was already not a member and the set is unchanged.
        {b O(logN)} *)

    (** @inline *)
    include QUERY with type 'a elt := 'a elt and type 'a t := 'a t

    (** {1 Snapshots}

        Concurrent modifications of a set are linearizable. The snapshot/copy
        provides a coherent view of the elements of a set along this linearized
        timeline.

        Further updates to the original set (or its copies) will trigger a minimal
        copy-on-write of the internal substructures of the set. This doesn't impact
        the time complexity of any operations, but induces a corresponding memory
        complexity for copying the modified subparts once.
    *)

    val copy : 'a t -> 'a t
    (** [copy t] returns an independently mutable copy of the set [t]. Further
        modifications of the set [t] will not affect its copies (and vice-versa.)
        {b O(1)} *)

    module View : View_poly(Ord).S

    val snapshot : 'a t -> 'a View.t
    (** [snapshot t] returns a read-only view of the elements of the set [t].
      {b O(1)} *)

    val to_view : 'a t -> 'a View.t
    (** Same as [snapshot]. *)

    val of_view : 'a View.t -> 'a t
    (** [of_view v] returns a new mutable set containing all the elements
        of the view [v]. {b O(1)} *)

    (** {1 Iterators}

        The following functions all proceed on a coherent {! snapshot} of their
        set [t] argument, created at the start of their execution. Concurrent
        modifications of the original set [t] during their execution will not
        be observed by these traversals.
    *)

    (** @inline *)
    include ITER with type 'a elt := 'a elt and type 'a t := 'a t
  end
end

module Set (Ord : Ordered) = struct
  module Ord_poly = struct
    type _ t = Ord.t

    let compare = Ord.compare
  end

  module type Sig = Set_poly(Ord_poly).S

  module type View_not_poly = sig end

  module type S = sig
    (** Thread-safe mutable set structure given a totally ordered type. *)

    type elt = Ord.t
    (** The type of set elements. *)

    type t
    (** The type of mutable sets. *)

    module View : sig
      (** A read-only, non-mutable view of a set (with more operations,
          compatible with the purely functional {! Stdlib.Set} interface) *)

      type elt = Ord.t
      (** The type of set elements. *)

      type t
      (** The type of read-only sets. *)

      include View_poly(Ord_poly).S with type _ elt := elt and type _ t := t
    end

    include
      Sig
        with type _ elt := elt
         and type _ t := t
         and type _ View.elt := View.elt
         and type _ View.t := View.t
         and module View := View
  end
end

module Map_poly (Ord : Ordered_poly) = struct
  module type S = sig
    (** Thread-safe mutable map structure given
        a totally ordered polymorphic type. *)

    type 'a key = 'a Ord.t
    (** The type of the map keys. *)

    type 'a t
    (** The type of maps from type ['a key] to values of type ['a]. *)

    val empty : unit -> 'a t
    (** [empty ()] returns a new empty map. {b O(1)} *)

    val singleton : 'a key -> 'a -> 'a t
    (** [singleton k v] returns a new map containing only
        the binding [k] for [v]. {b O(1)} *)

    val add : 'a key -> 'a -> 'a t -> unit
    (** [add k v t] adds the binding [k] for [v] into the map [t].
        If the key [k] was already bound, the previously bound value
        is replaced by [v]. {b O(logN)} *)

    val remove : 'a key -> 'a t -> bool
    (** [remove k t] deletes any binding of the key [k] in the map [t].
        Returns [true] if the binding was removed, or [false] if no binding
        existed for this key. {b O(logN)} *)

    val find : 'a key -> 'a t -> 'a
    (** [find k t] returns the value associated with the key [k] in the map [t],
        or raises [Not_found] if no such binding existed. {b O(logN)} *)

    val find_opt : 'a key -> 'a t -> 'a option
    (** [find_opt k t] returns the value associated with the key [k]
    in the map [t], or returns [None] if no such binding existed. {b O(logN)} *)

    val copy : 'a t -> 'a t
    (** [copy t] returns an independently mutable copy of the map [t]. Further
        modifications of the map [t] will not affect its copies
        (and vice-versa.) {b O(1)} *)

    val cardinal : 'a t -> int
    (** [cardinal t] returns the number of bindings in the map [t]. {b O(N)} *)
  end
end

module Map (Ord : Ordered) = struct
  module Ord_poly = struct
    type _ t = Ord.t

    let compare = Ord.compare
  end

  module type Sig = Map_poly(Ord_poly).S

  module type S = sig
    (** Thread-safe mutable map structure given a totally ordered type. *)

    type key = Ord.t
    (** The type of the map keys. *)

    type 'a t
    (** The type of maps from type [key] to values of type ['a]. *)

    include Sig with type _ key := key and type 'a t := 'a t
  end
end
