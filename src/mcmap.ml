module Make_poly (Ord : S.Ordered_poly) = struct
  type 'a key = 'a Ord.t

  type 'a kv = K of 'a key | Kv of 'a key * 'a

  module Kv = struct
    type 'a t = 'a kv

    let make (k, v) = Kv (k, v)

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

    let binding = function
      | K _ -> invalid_arg "Kv.binding missing"
      | Kv (k, v) -> k, v

    let binding_opt = function
      | Some (Kv (k, v)) -> Some (k, v)
      | None -> None
      | Some (K _) -> invalid_arg "Kv.value_opt missing"

    let apply f kv =
      let k, v = binding kv in
      f k v

    let compare a b = Ord.compare (key a) (key b)
  end

  module S = Mcset.Make_poly (Kv)

  type 'a t = 'a S.t

  let empty = S.empty

  let singleton k v = S.singleton (Kv (k, v))

  let add k v t = S.add_or_replace (Kv (k, v)) t

  let remove k t = S.remove (K k) t

  let copy = S.copy

  let cardinal = S.cardinal

  let is_empty = S.is_empty

  let mem k t = S.mem (K k) t

  let min_binding t = Kv.binding (S.min_elt t)

  let min_binding_opt t = Kv.binding_opt (S.min_elt_opt t)

  let max_binding t = Kv.binding (S.max_elt t)

  let max_binding_opt t = Kv.binding_opt (S.max_elt_opt t)

  let find k t = Kv.value (S.find (K k) t)

  let find_opt k t = Kv.value_opt (S.find_opt (K k) t)

  let find_first f t = Kv.binding (S.find_first (fun kv -> f (Kv.key kv)) t)

  let find_first_opt f t =
    Kv.binding_opt (S.find_first_opt (fun kv -> f (Kv.key kv)) t)

  let find_last f t = Kv.binding (S.find_last (fun kv -> f (Kv.key kv)) t)

  let find_last_opt f t =
    Kv.binding_opt (S.find_last_opt (fun kv -> f (Kv.key kv)) t)

  let choose t = Kv.binding (S.choose t)

  let choose_opt t = Kv.binding_opt (S.choose_opt t)

  let fold f t init = S.fold (fun kv acc -> Kv.apply f kv acc) t init

  let iter f t = S.iter (fun kv -> Kv.apply f kv) t

  let for_all f t = S.for_all (fun kv -> Kv.apply f kv) t

  let exists f t = S.exists (fun kv -> Kv.apply f kv) t

  let bindings t = List.map Kv.binding (S.elements t)

  let to_list t = List.map Kv.binding (S.to_list t)

  let of_list lst = S.of_list (List.map Kv.make lst)

  let to_seq t = Seq.map Kv.binding (S.to_seq t)

  let of_seq seq = S.of_seq (Seq.map Kv.make seq)

  let to_rev_seq t = Seq.map Kv.binding (S.to_rev_seq t)

  let to_seq_from k t = Seq.map Kv.binding (S.to_seq_from (K k) t)
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
