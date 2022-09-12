module Make (E : Set.OrderedType) : sig
  type elt = E.t

  type t

  val empty : unit -> t

  val singleton : elt -> t

  val add : elt -> t -> unit

  val remove : elt -> t -> bool

  val mem : elt -> t -> bool

  val cardinal : t -> int

  val copy : t -> t

  module View : sig
    type elt = E.t

    type t

    val empty : t

    val singleton : elt -> t

    val add : elt -> t -> t

    val mem : elt -> t -> bool

    val cardinal : t -> int

    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

    val iter : (elt -> unit) -> t -> unit

    val for_all : (elt -> bool) -> t -> bool

    val exists : (elt -> bool) -> t -> bool

    val elements : t -> elt list

    val to_list : t -> elt list

    val of_list : elt list -> t

    val of_seq : elt Seq.t -> t

    val to_seq : t -> elt Seq.t

    val to_rev_seq : t -> elt Seq.t

    val to_seq_from : elt -> t -> elt Seq.t
  end

  val snapshot : t -> View.t

  val to_view : t -> View.t

  val of_view : View.t -> t

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  val iter : (elt -> unit) -> t -> unit

  val for_all : (elt -> bool) -> t -> bool

  val exists : (elt -> bool) -> t -> bool

  val elements : t -> elt list

  val to_list : t -> elt list

  val of_list : elt list -> t

  val of_seq : elt Seq.t -> t

  val to_seq : t -> elt Seq.t

  val to_rev_seq : t -> elt Seq.t

  val to_seq_from : elt -> t -> elt Seq.t
end
