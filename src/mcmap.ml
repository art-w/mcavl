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

  module View = struct
    module S = S.View

    type 'a key = 'a Ord.t

    type 'a t = 'a S.t

    let empty = S.empty

    let singleton k v = S.singleton (Kv (k, v))

    let add k v t = S.add_or_replace (Kv (k, v)) t

    let remove k t = S.remove (K k) t

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

    let union = S.union

    let inter = S.inter

    let diff = S.diff

    let map f t =
      S.map
        (fun kv ->
          let k, v = Kv.binding kv in
          let v' = f v in
          if v == v' then kv else Kv (k, v') )
        t

    let mapi f t =
      S.map
        (fun kv ->
          let k, v = Kv.binding kv in
          let v' = f k v in
          if v == v' then kv else Kv (k, v') )
        t

    let filter f t = S.filter (fun kv -> Kv.apply f kv) t

    let filter_map f t =
      S.filter_map
        (fun kv ->
          let k, v = Kv.binding kv in
          match f k v with
          | None -> None
          | Some v -> Some (Kv (k, v)) )
        t

    let partition f t = S.partition (fun kv -> Kv.apply f kv) t

    let split k t =
      let k = K k in
      let smaller, found, larger = S.split k t in
      let found = if found then Some (Kv.value (S.find k t)) else None in
      smaller, found, larger

    let pop_min t =
      let kv, t = S.pop_min t in
      let k, v = Kv.binding kv in
      k, v, t

    let pop_min_opt t =
      match S.pop_min_opt t with
      | None -> None
      | Some (kv, t) ->
          let k, v = Kv.binding kv in
          Some (k, v, t)

    let pop_max t =
      let kv, t = S.pop_max t in
      let k, v = Kv.binding kv in
      k, v, t

    let pop_max_opt t =
      match S.pop_max_opt t with
      | None -> None
      | Some (kv, t) ->
          let k, v = Kv.binding kv in
          Some (k, v, t)
  end

  let copy = S.copy

  let snapshot = S.snapshot

  let to_view = S.to_view

  let of_view = S.of_view
end

module Make (Ord : S.Ordered) = struct
  module Ord_poly = struct
    type 'a t = Ord.t

    let compare = Ord.compare
  end

  module I = Make_poly (Ord_poly)

  module View = struct
    type key = Ord.t

    type 'a t = 'a I.View.t

    include (
      I.View :
        S.View_map_poly(Ord_poly).S with type _ key := key and type 'a t := 'a t )
  end

  type key = Ord.t

  include (I : S.Map(Ord).S with type key := key and module View := View)
end
