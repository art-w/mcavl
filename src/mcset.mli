module Make (E : Set.OrderedType) : sig
  type elt = E.t

  type t

  val make : unit -> t

  val add : elt -> t -> unit

  val remove : elt -> t -> unit

  val mem : elt -> t -> bool

  val cardinal : t -> int

  val copy : t -> t

  module View : sig
    type elt = E.t

    type t

    val cardinal : t -> int

    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

    val iter : (elt -> unit) -> t -> unit

    val for_all : (elt -> bool) -> t -> bool

    val exists : (elt -> bool) -> t -> bool

    val elements : t -> elt list

    val to_seq : t -> elt Seq.t

    val to_rev_seq : t -> elt Seq.t

    val to_seq_from : elt -> t -> elt Seq.t
  end

  val snapshot : t -> View.t

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  val iter : (elt -> unit) -> t -> unit

  val for_all : (elt -> bool) -> t -> bool

  val exists : (elt -> bool) -> t -> bool

  val elements : t -> elt list

  val to_seq : t -> elt Seq.t

  val to_rev_seq : t -> elt Seq.t

  val to_seq_from : elt -> t -> elt Seq.t
end
