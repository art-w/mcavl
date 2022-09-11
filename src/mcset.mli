module Make (E : Set.OrderedType) : sig
  type elt = E.t
  type t

  val make : unit -> t
  val add : elt -> t -> unit
  val remove : elt -> t -> unit
  val mem : elt -> t -> bool
  val cardinal : t -> int
  val copy : t -> t
end
