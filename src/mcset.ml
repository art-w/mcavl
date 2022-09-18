module type Ordered = S.Ordered

module Make (E : Ordered) = struct
  include Core.Make (E)

  let rec add ~gen x t =
    let s = Atomic.get t in
    match s with
    | Leaf Dead | Node (Dead, _, _, _, _) -> Looking_for_life
    | Leaf Read_only | Node (Read_only, _, _, _, _) -> Retry
    | Leaf Alive ->
        let root, expected = gen in
        if Atomic.get root != expected
        then Retry
        else begin
          let attempt = Atomic.make Unknown in
          let state = Attempt_add (attempt, root, expected) in
          let repr = Node (state, 1, Atomic.make s, x, Atomic.make s) in
          if Atomic.compare_and_set t s repr
          then begin
            finalize_op t repr ;
            match Atomic.get attempt with
            | Success -> Overflow
            | Failure -> Retry
            | Unknown -> assert false
          end
          else add ~gen x t
        end
    | Node (Alive, _, left, pivot, right) -> begin
        match E.compare x pivot with
        | 0 -> Ok
        | c when c < 0 -> begin
            match add ~gen x left with
            | Ok -> Ok
            | Retry -> Retry
            | Overflow -> balance t
            | Looking_for_life -> add ~gen x t
          end
        | _ -> begin
            match add ~gen x right with
            | Ok -> Ok
            | Retry -> Retry
            | Overflow -> balance t
            | Looking_for_life -> add ~gen x t
          end
      end
    | _ ->
        let res = fixup t in
        begin
          match add ~gen x t with
          | Ok -> res
          | other -> other
        end

  let rec add_retry x t =
    let root = Atomic.get t in
    match add ~gen:(t, root) x root with
    | Ok | Overflow -> ()
    | Retry -> add_retry x t
    | Looking_for_life -> assert false

  let add x t = add_retry x t

  let rec remove ~gen elt t =
    let s = Atomic.get t in
    match s with
    | Leaf Alive -> Ok, false (* not found *)
    | Leaf Read_only | Node (Read_only, _, _, _, _) -> Retry, false
    | Leaf Dead | Node (Dead, _, _, _, _) -> Looking_for_life, false
    | Node (Alive, h, left, pivot, right) -> begin
        match E.compare elt pivot with
        | 0 ->
            let root, expected = gen in
            if Atomic.get root != expected
            then Retry, false
            else begin
              let attempt = Atomic.make Unknown in
              let state = Attempt_remove (attempt, root, expected) in
              let s_removing = Node (state, h, left, pivot, right) in
              if Atomic.compare_and_set t s s_removing
              then begin
                finalize_op t s_removing ;
                match Atomic.get attempt with
                | Success -> fixup t, true
                | Failure -> Retry, false
                | Unknown -> assert false
              end
              else remove ~gen elt t
            end
        | c when c < 0 -> begin
            match remove ~gen elt left with
            | Ok, found -> Ok, found
            | Retry, found -> Retry, found
            | Overflow, found -> balance t, found
            | Looking_for_life, _ -> remove ~gen elt t
          end
        | _ -> begin
            match remove ~gen elt right with
            | Ok, found -> Ok, found
            | Retry, found -> Retry, found
            | Overflow, found -> balance t, found
            | Looking_for_life, _ -> remove ~gen elt t
          end
      end
    | _ ->
        let res = fixup t in
        begin
          match remove ~gen elt t with
          | Ok, found -> res, found
          | other, found -> other, found
        end

  let rec remove_retry elt t =
    let root = Atomic.get t in
    match remove ~gen:(t, root) elt root with
    | Ok, found | Overflow, found -> found
    | Retry, _ -> remove_retry elt t
    | Looking_for_life, _ -> assert false

  let remove elt t = remove_retry elt t

  let rec is_empty t =
    match Atomic.get t with
    | Leaf _ -> true
    | Node ((Alive | Dead | Read_only), _, _, _, _) -> false
    | _ ->
        let (_ : result) = fixup t in
        is_empty t

  let is_empty t = is_empty (Atomic.get t)

  let rec choose_opt t =
    match Atomic.get t with
    | Leaf _ -> None
    | Node ((Alive | Dead | Read_only), _, _, pivot, _) -> Some pivot
    | _ ->
        let (_ : result) = fixup t in
        choose_opt t

  let choose_opt t = choose_opt (Atomic.get t)

  let choose t =
    match choose_opt t with
    | Some x -> x
    | None -> raise Not_found

  let rec mem x t =
    match Atomic.get t with
    | Leaf _ -> false
    | Node ((Alive | Dead | Read_only), _, left, pivot, right) -> begin
        match E.compare x pivot with
        | 0 -> true
        | c when c < 0 -> mem x left
        | _ -> mem x right
      end
    | _ ->
        let (_ : result) = fixup t in
        mem x t

  let mem x t = mem x (Atomic.get t)

  let rec find_opt x t =
    match Atomic.get t with
    | Leaf _ -> None
    | Node ((Alive | Dead | Read_only), _, left, pivot, right) -> begin
        match E.compare x pivot with
        | 0 -> Some pivot
        | c when c < 0 -> find_opt x left
        | _ -> find_opt x right
      end
    | _ ->
        let (_ : result) = fixup t in
        find_opt x t

  let find_opt x t = find_opt x (Atomic.get t)

  let find x t =
    match find_opt x t with
    | Some x -> x
    | None -> raise Not_found

  let rec find_first_opt f t =
    match Atomic.get t with
    | Leaf _ -> None
    | Node ((Alive | Dead | Read_only), _, left, pivot, right) ->
        if f pivot
        then
          match find_first_opt f left with
          | None -> Some pivot
          | some -> some
        else find_first_opt f right
    | _ ->
        let (_ : result) = fixup t in
        find_first_opt f t

  let find_first_opt f t = find_first_opt f (Atomic.get t)

  let find_first f t =
    match find_first_opt f t with
    | Some x -> x
    | None -> raise Not_found

  let rec find_last_opt f t =
    match Atomic.get t with
    | Leaf _ -> None
    | Node ((Alive | Dead | Read_only), _, left, pivot, right) ->
        if f pivot
        then
          match find_last_opt f right with
          | None -> Some pivot
          | some -> some
        else find_last_opt f left
    | _ ->
        let (_ : result) = fixup t in
        find_last_opt f t

  let find_last_opt f t = find_last_opt f (Atomic.get t)

  let find_last f t =
    match find_last_opt f t with
    | Some x -> x
    | None -> raise Not_found

  let rec min_elt_opt t =
    match Atomic.get t with
    | Leaf _ -> None
    | Node ((Alive | Dead | Read_only), _, left, pivot, _) -> begin
        match min_elt_opt left with
        | None -> Some pivot
        | some -> some
      end
    | _ ->
        let (_ : result) = fixup t in
        min_elt_opt t

  let min_elt_opt t = min_elt_opt (Atomic.get t)

  let min_elt t =
    match min_elt_opt t with
    | Some x -> x
    | None -> raise Not_found

  let rec max_elt_opt t =
    match Atomic.get t with
    | Leaf _ -> None
    | Node ((Alive | Dead | Read_only), _, _, pivot, right) -> begin
        match max_elt_opt right with
        | None -> Some pivot
        | some -> some
      end
    | _ ->
        let (_ : result) = fixup t in
        max_elt_opt t

  let max_elt_opt t = max_elt_opt (Atomic.get t)

  let max_elt t =
    match max_elt_opt t with
    | Some x -> x
    | None -> raise Not_found

  let rec snapshot t =
    let root = Atomic.get t in
    if Atomic.compare_and_set t root (Atomic.make (Copy root))
    then root
    else snapshot t

  let to_view = snapshot

  let of_view r = Atomic.make (Atomic.make (Copy r))

  let copy t = of_view (to_view t)

  module View = struct
    type elt = E.t

    type t = r

    let empty = Pure.empty ~s:Read_only

    let singleton elt = Pure.singleton ~s:Read_only elt

    let of_list lst = Pure.of_list ~s:Read_only lst

    let of_seq lst = Pure.of_seq ~s:Read_only lst

    let add elt t = Pure.add ~s:Read_only ~get:ensure_read_only elt t

    let balance left pivot right =
      Pure.balance ~s:Read_only ~get:ensure_read_only left pivot right

    let rec pop_min_opt t =
      match ensure_read_only t with
      | Leaf Read_only -> None
      | Node (Read_only, _, left, pivot, right) -> begin
          match pop_min_opt left with
          | None -> Some (pivot, right)
          | Some (min, left) -> Some (min, balance left pivot right)
        end
      | _ -> assert false

    let pop_min t =
      match pop_min_opt t with
      | Some x -> x
      | None -> raise Not_found

    let rec pop_max_opt t =
      match ensure_read_only t with
      | Leaf Read_only -> None
      | Node (Read_only, _, left, pivot, right) -> begin
          match pop_max_opt right with
          | None -> Some (pivot, left)
          | Some (max, right) -> Some (max, balance left pivot right)
        end
      | _ -> assert false

    let pop_max t =
      match pop_max_opt t with
      | Some x -> x
      | None -> raise Not_found

    let rec add_min elt t =
      match ensure_read_only t with
      | Leaf Read_only -> singleton elt
      | Node (Read_only, _, left, pivot, right) ->
          balance (add_min elt left) pivot right
      | _ -> assert false

    let rec add_max elt t =
      match ensure_read_only t with
      | Leaf Read_only -> singleton elt
      | Node (Read_only, _, left, pivot, right) ->
          balance left pivot (add_max elt right)
      | _ -> assert false

    let rec join left pivot right =
      match ensure_read_only left, ensure_read_only right with
      | Leaf Read_only, Leaf Read_only -> singleton pivot
      | Leaf Read_only, _ -> add_min pivot right
      | _, Leaf Read_only -> add_max pivot left
      | Node (Read_only, lh, ll, lp, lr), Node (Read_only, rh, rl, rp, rr) ->
          if lh > rh + 2
          then balance ll lp (join lr pivot right)
          else if rh > lh + 2
          then balance (join left pivot rl) rp rr
          else create left pivot right
      | _ -> assert false

    let append left right =
      match pop_min_opt right with
      | None -> left
      | Some (pivot, right) -> join left pivot right

    let rec remove elt t =
      match ensure_read_only t with
      | Leaf Read_only -> t
      | Node (Read_only, _, left, pivot, right) -> begin
          match E.compare elt pivot with
          | 0 -> append left right
          | c when c < 0 ->
              let left' = remove elt left in
              if left == left' then t else balance left' pivot right
          | _ ->
              let right' = remove elt right in
              if right == right' then t else balance left pivot right'
        end
      | _ -> assert false

    let rec split elt t =
      match ensure_read_only t with
      | Leaf Read_only -> empty, false, empty
      | Node (Read_only, _, left, pivot, right) -> begin
          match E.compare elt pivot with
          | 0 -> left, true, right
          | c when c < 0 ->
              let l, found, r = split elt left in
              l, found, join r pivot right
          | _ ->
              let l, found, r = split elt right in
              join left pivot l, found, r
        end
      | _ -> assert false

    let rec union t1 t2 =
      match ensure_read_only t1, ensure_read_only t2 with
      | Leaf Read_only, _ -> t2
      | _, Leaf Read_only -> t1
      | Node (Read_only, h1, l1, p1, r1), Node (Read_only, h2, _, _, _)
        when h1 >= h2 ->
          let l2, _, r2 = split p1 t2 in
          join (union l1 l2) p1 (union r1 r2)
      | Node (Read_only, _, _, _, _), Node (Read_only, _, l2, p2, r2) ->
          let l1, _, r1 = split p2 t1 in
          join (union l1 l2) p2 (union r1 r2)
      | _ -> assert false

    let rec inter t1 t2 =
      match ensure_read_only t1, ensure_read_only t2 with
      | Leaf Read_only, _ | _, Leaf Read_only -> empty
      | Node (Read_only, h1, l1, p1, r1), Node (Read_only, h2, _, _, _)
        when h1 >= h2 ->
          let l2, found, r2 = split p1 t2 in
          let l12, r12 = inter l1 l2, inter r1 r2 in
          if found then join l12 p1 r12 else append l12 r12
      | Node (Read_only, _, _, _, _), Node (Read_only, _, l2, p2, r2) ->
          let l1, found, r1 = split p2 t1 in
          let l12, r12 = inter l1 l2, inter r1 r2 in
          if found then join l12 p2 r12 else append l12 r12
      | _ -> assert false

    let rec diff t1 t2 =
      match ensure_read_only t1, ensure_read_only t2 with
      | Leaf Read_only, _ -> empty
      | _, Leaf Read_only -> t1
      | Node (Read_only, _, l1, p1, r1), _ ->
          let l2, found, r2 = split p1 t2 in
          let l12, r12 = diff l1 l2, diff r1 r2 in
          if found then append l12 r12 else join l12 p1 r12
      | _ -> assert false

    let rec mem x t =
      match ensure_read_only t with
      | Leaf Read_only -> false
      | Node (Read_only, _, left, pivot, right) -> begin
          match E.compare x pivot with
          | 0 -> true
          | c when c < 0 -> mem x left
          | _ -> mem x right
        end
      | _ -> assert false

    let rec find_opt x t =
      match ensure_read_only t with
      | Leaf Read_only -> None
      | Node (Read_only, _, left, pivot, right) -> begin
          match E.compare x pivot with
          | 0 -> Some pivot
          | c when c < 0 -> find_opt x left
          | _ -> find_opt x right
        end
      | _ -> assert false

    let find x t =
      match find_opt x t with
      | Some x -> x
      | None -> raise Not_found

    let rec find_first_opt f t =
      match ensure_read_only t with
      | Leaf Read_only -> None
      | Node (Read_only, _, left, pivot, right) ->
          if f pivot
          then
            match find_first_opt f left with
            | None -> Some pivot
            | some -> some
          else find_first_opt f right
      | _ ->
          let (_ : result) = fixup t in
          find_first_opt f t

    let find_first f t =
      match find_first_opt f t with
      | Some x -> x
      | None -> raise Not_found

    let rec find_last_opt f t =
      match ensure_read_only t with
      | Leaf Read_only -> None
      | Node (Read_only, _, left, pivot, right) ->
          if f pivot
          then
            match find_last_opt f right with
            | None -> Some pivot
            | some -> some
          else find_last_opt f left
      | _ ->
          let (_ : result) = fixup t in
          find_last_opt f t

    let find_last f t =
      match find_last_opt f t with
      | Some x -> x
      | None -> raise Not_found

    let is_empty t =
      match ensure_read_only t with
      | Leaf Read_only -> true
      | Node (Read_only, _, _, _, _) -> false
      | _ -> assert false

    let choose_opt t =
      match ensure_read_only t with
      | Leaf Read_only -> None
      | Node (Read_only, _, _, pivot, _) -> Some pivot
      | _ -> assert false

    let choose t =
      match choose_opt t with
      | Some x -> x
      | None -> raise Not_found

    let rec min_elt_opt t =
      match ensure_read_only t with
      | Leaf Read_only -> None
      | Node (Read_only, _, left, pivot, _) -> begin
          match min_elt_opt left with
          | None -> Some pivot
          | some -> some
        end
      | _ -> assert false

    let min_elt t =
      match min_elt_opt t with
      | Some x -> x
      | None -> raise Not_found

    let rec max_elt_opt t =
      match ensure_read_only t with
      | Leaf Read_only -> None
      | Node (Read_only, _, _, pivot, right) -> begin
          match max_elt_opt right with
          | None -> Some pivot
          | some -> some
        end
      | _ -> assert false

    let max_elt t =
      match max_elt_opt t with
      | Some x -> x
      | None -> raise Not_found

    let rec cardinal acc t =
      match ensure_read_only t with
      | Leaf Read_only -> acc
      | Node (Read_only, _, left, _, right) ->
          let acc = acc + 1 in
          let acc = cardinal acc left in
          cardinal acc right
      | _ -> assert false

    let cardinal t = cardinal 0 t

    let rec fold f t acc =
      match ensure_read_only t with
      | Leaf Read_only -> acc
      | Node (Read_only, _, left, pivot, right) ->
          let acc = fold f left acc in
          let acc = f pivot acc in
          fold f right acc
      | _ -> assert false

    let rec iter f t =
      match ensure_read_only t with
      | Leaf Read_only -> ()
      | Node (Read_only, _, left, pivot, right) ->
          iter f left ; f pivot ; iter f right
      | _ -> assert false

    let rec for_all f t =
      match ensure_read_only t with
      | Leaf Read_only -> true
      | Node (Read_only, _, left, pivot, right) ->
          f pivot && for_all f left && for_all f right
      | _ -> assert false

    let rec exists f t =
      match ensure_read_only t with
      | Leaf Read_only -> false
      | Node (Read_only, _, left, pivot, right) ->
          f pivot || exists f left || exists f right
      | _ -> assert false

    let elements t = List.rev (fold (fun x xs -> x :: xs) t [])

    let to_list = elements

    let rec to_seq t k =
      match ensure_read_only t with
      | Leaf Read_only -> k
      | Node (Read_only, _, left, pivot, right) ->
          to_seq left (fun () -> Seq.Cons (pivot, to_seq right k))
      | _ -> assert false

    let rec to_seq_from elt t k =
      match ensure_read_only t with
      | Leaf Read_only -> k
      | Node (Read_only, _, left, pivot, right) -> begin
          match E.compare elt pivot with
          | 0 -> fun () -> Seq.Cons (pivot, to_seq right k)
          | c when c < 0 ->
              to_seq_from elt left (fun () -> Seq.Cons (pivot, to_seq right k))
          | _ -> to_seq_from elt right k
        end
      | _ -> assert false

    let to_seq_from elt t = to_seq_from elt t (fun () -> Seq.Nil)

    let to_seq t = to_seq t (fun () -> Seq.Nil)

    let rec to_rev_seq t k =
      match ensure_read_only t with
      | Leaf Read_only -> k
      | Node (Read_only, _, left, pivot, right) ->
          to_rev_seq right (fun () -> Seq.Cons (pivot, to_rev_seq left k))
      | _ -> assert false

    let to_rev_seq t = to_rev_seq t (fun () -> Seq.Nil)

    let fast_union left pivot right =
      match max_elt_opt left, min_elt_opt right with
      | None, None -> singleton pivot
      | Some max, None when E.compare max pivot < 0 -> add_max pivot left
      | None, Some min when E.compare pivot min < 0 -> add_min pivot right
      | Some max, Some min
        when E.compare max pivot < 0 && E.compare pivot min < 0 ->
          join left pivot right
      | _ -> union left (add pivot right)

    let fast_append left right =
      match max_elt_opt left, min_elt_opt right with
      | None, None -> empty
      | Some _, None -> left
      | None, Some _ -> right
      | Some max, Some min when E.compare max min < 0 -> append left right
      | _ -> union left right

    let rec map f t =
      match ensure_read_only t with
      | Leaf Read_only -> t
      | Node (Read_only, _, left, pivot, right) ->
          let left' = map f left in
          let pivot' = f pivot in
          let right' = map f right in
          if left == left' && pivot == pivot' && right == right'
          then t
          else fast_union left' pivot' right'
      | _ -> assert false

    let rec filter f t =
      match ensure_read_only t with
      | Leaf Read_only -> t
      | Node (Read_only, _, left, pivot, right) ->
          let left' = filter f left in
          let keep = f pivot in
          let right' = filter f right in
          if left == left' && keep && right == right'
          then t
          else if keep
          then join left' pivot right'
          else append left' right'
      | _ -> assert false

    let rec filter_map f t =
      match ensure_read_only t with
      | Leaf Read_only -> t
      | Node (Read_only, _, left, pivot, right) ->
          let left' = filter_map f left in
          let keep = f pivot in
          let right' = filter_map f right in
          begin
            match keep with
            | None -> fast_append left' right'
            | Some pivot' ->
                if left == left' && pivot == pivot' && right == right'
                then t
                else fast_union left' pivot' right'
          end
      | _ -> assert false

    let rec partition f t =
      match ensure_read_only t with
      | Leaf Read_only -> t, t
      | Node (Read_only, _, left, pivot, right) ->
          let left', not_left' = partition f left in
          let keep = f pivot in
          let right', not_right' = partition f right in
          if left == left' && keep && right == right'
          then t, empty
          else if left == not_left' && (not keep) && right == not_right'
          then empty, t
          else if keep
          then join left' pivot right', append not_left' not_right'
          else append left' right', join not_left' pivot not_right'
      | _ -> assert false

    let compare t1 t2 = Seq.compare E.compare (to_seq t1) (to_seq t2)

    let equal t1 t2 = compare t1 t2 = 0

    let rec disjoint t1 t2 =
      match ensure_read_only t1, ensure_read_only t2 with
      | Leaf Read_only, _ | _, Leaf Read_only -> true
      | Node (Read_only, h1, l1, p1, r1), Node (Read_only, h2, l2, p2, r2) ->
          if h1 > h2
          then
            let l2, found, r2 = split p1 t2 in
            (not found) && disjoint l1 l2 && disjoint r1 r2
          else
            let l1, found, r1 = split p2 t1 in
            (not found) && disjoint l1 l2 && disjoint r1 r2
      | _ -> assert false
  end

  let cardinal t = View.cardinal (snapshot t)

  let fold f t acc = View.fold f (snapshot t) acc

  let iter f t = View.iter f (snapshot t)

  let for_all f t = View.for_all f (snapshot t)

  let exists f t = View.exists f (snapshot t)

  let elements t = View.elements (snapshot t)

  let to_list = elements

  let of_list lst = Atomic.make (Pure.of_list ~s:Alive lst)

  let of_seq seq = Atomic.make (Pure.of_seq ~s:Alive seq)

  let to_seq t = View.to_seq (snapshot t)

  let to_rev_seq t = View.to_rev_seq (snapshot t)

  let to_seq_from elt t = View.to_seq_from elt (snapshot t)
end
