module Make (E : Set.OrderedType) = struct
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

    let mem = mem

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
  end

  let mem x t = mem x (Atomic.get t)

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
