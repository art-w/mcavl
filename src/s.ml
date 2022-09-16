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
