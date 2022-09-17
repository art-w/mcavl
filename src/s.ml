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
      or raises [Not_found] if no such element exists. {b O(log N)} *)

  val find_opt : elt -> t -> elt option
  (** [find_opt x t] returns the element of the set that compares equal to [x],
      or [None] if no such element exists. {b O(log N)} *)
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
