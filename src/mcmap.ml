module Make_poly (Ord : S.Ordered_poly) = struct
  type 'a key = 'a Ord.t

  type 'a kv = K of 'a key | Kv of 'a key * 'a

  module Kv = struct
    type 'a t = 'a kv

    let key = function
      | K k -> k
      | Kv (k, _) -> k

    let value = function
      | K _ -> invalid_arg "Kv.value missing"
      | Kv (_, v) -> v

    let value_opt = function
      | Some (Kv (_, v)) -> Some v
      | None -> None
      | Some (K _) -> invalid_arg "Kv.value_opt missing"

    let compare a b = Ord.compare (key a) (key b)
  end

  module S = Mcset.Make_poly (Kv)

  type 'a t = 'a S.t

  let empty = S.empty

  let singleton k v = S.singleton (Kv (k, v))

  let add k v t = S.add_or_replace (Kv (k, v)) t

  let remove k t = S.remove (K k) t

  let find k t = Kv.value (S.find (K k) t)

  let find_opt k t = Kv.value_opt (S.find_opt (K k) t)

  let copy = S.copy

  let cardinal = S.cardinal
end

module Make (Ord : S.Ordered) = struct
  module Ord_poly = struct
    type 'a t = Ord.t

    let compare = Ord.compare
  end

  module I = Make_poly (Ord_poly)

  type key = Ord.t

  include (I : S.Map(Ord).S with type key := key)
end
