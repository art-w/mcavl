module Make (E : Set.OrderedType) = struct
  type elt = E.t

  type t = r Atomic.t

  and r = s Atomic.t

  and s = Leaf of state | Node of state * int * r * elt * r | Copy of r

  and state =
    | Alive
    | Dead
    | Removing
    | Read_only
    | Balancing_left of s * s
    | Balancing_left_center of s * s * s * s
    | Balancing_right of s * s
    | Balancing_right_center of s * s * s * s
    | Attempt_add of attempt * t * r
    | Attempt_remove of attempt * t * r

  and attempt = attempt_state Atomic.t

  and attempt_state = Unknown | Success | Failure

  type result = Ok | Overflow | Looking_for_life | Retry

  let make () = Atomic.make (Atomic.make (Leaf Alive))

  let rec height t = s_height (Atomic.get t)

  and s_height = function
    | Leaf _ -> 0
    | Node (_, h, _, _, _) -> h
    | Copy t -> height t

  let rec create left pivot right = Atomic.make (s_create left pivot right)

  and s_create left pivot right =
    let hl, hr = height left, height right in
    let h = if hl >= hr then hl + 1 else hr + 1 in
    Node (Alive, h, left, pivot, right)

  let rec fixup t =
    let s = Atomic.get t in
    match s with
    | Leaf _ -> Ok
    | Node ((Alive | Read_only | Dead), _, _, _, _) -> Ok
    | Node (Removing, _, _, _, _) ->
        let res = push_down t s in
        begin
          match balance t with
          | Ok -> res
          | other -> other
        end
    | Node ((Attempt_add _ | Attempt_remove _), _, _, _, _) ->
        finalize_op t s ; fixup t
    | Copy t' ->
        let repr = make_copy t' in
        let (_ : bool) = Atomic.compare_and_set t s repr in
        fixup t
    | _ -> balance t

  and finalize_op t s =
    match s with
    | Node (Attempt_add (attempt, root, expected), height, left, pivot, right)
      -> begin
        match Atomic.get attempt with
        | Unknown ->
            let result =
              if Atomic.get root == expected then Success else Failure
            in
            let (_ : bool) = Atomic.compare_and_set attempt Unknown result in
            finalize_op t s
        | Success ->
            let repr = Node (Alive, height, left, pivot, right) in
            let (_ : bool) = Atomic.compare_and_set t s repr in
            ()
        | Failure ->
            let repr = Leaf Alive in
            let (_ : bool) = Atomic.compare_and_set t s repr in
            ()
      end
    | Node (Attempt_remove (attempt, root, expected), height, left, pivot, right)
      -> begin
        match Atomic.get attempt with
        | Unknown ->
            let result =
              if Atomic.get root == expected then Success else Failure
            in
            let (_ : bool) = Atomic.compare_and_set attempt Unknown result in
            finalize_op t s
        | Success ->
            let repr = Node (Removing, height, left, pivot, right) in
            let (_ : bool) = Atomic.compare_and_set t s repr in
            ()
        | Failure ->
            let repr = Node (Alive, height, left, pivot, right) in
            let (_ : bool) = Atomic.compare_and_set t s repr in
            ()
      end
    | _ -> ()

  and get_stable t =
    let s = Atomic.get t in
    match s with
    | Leaf Alive | Node (Alive, _, _, _, _) -> s
    | Leaf Dead | Node (Dead, _, _, _, _) -> s
    | Leaf Read_only | Node (Read_only, _, _, _, _) -> s
    | _ ->
        let (_ : result) = fixup t in
        get_stable t

  and ensure_read_only t =
    let s = Atomic.get t in
    match s with
    | Leaf Read_only -> s
    | Node (Read_only, _, _, _, _) -> s
    | Node (Dead, _, _, _, _) -> assert false
    | Leaf _ ->
        let ro = Leaf Read_only in
        if Atomic.compare_and_set t s ro then ro else ensure_read_only t
    | Node (Alive, h, left, pivot, right) ->
        let ro = Node (Read_only, h, left, pivot, right) in
        if Atomic.compare_and_set t s ro then ro else ensure_read_only t
    | Copy t' ->
        let ro = ensure_read_only t' in
        if Atomic.compare_and_set t s ro then ro else ensure_read_only t
    | _ ->
        let (_ : result) = fixup t in
        ensure_read_only t

  and make_copy t =
    let s = ensure_read_only t in
    match s with
    | Leaf Read_only -> Leaf Alive
    | Node (Read_only, h, left, pivot, right) ->
        Node (Alive, h, Atomic.make (Copy left), pivot, Atomic.make (Copy right))
    | _ -> assert false

  and push_down t s =
    match s with
    | _ when s != Atomic.get t -> Ok
    | Node (Removing, _, left, pivot_to_remove, right) ->
        let s_left = Atomic.get left in
        begin
          match s_left with
          | Leaf Dead -> push_down_left_locked t s right
          | Leaf Alive ->
              let s_left_dead = Leaf Dead in
              if Atomic.compare_and_set left s_left s_left_dead
              then push_down_left_locked t s right
              else push_down t s
          | Node (Removing, _, _, _, _) ->
              let result = push_down left s_left in
              begin
                match push_down t s with
                | Ok -> result
                | r -> r
              end
          | Node (Dead, _, left_left, left_pivot, left_right) ->
              let expected_height =
                1 + max (height left_right) (height right)
              in
              let s_t_new =
                Node
                  (Removing, expected_height, left_right, pivot_to_remove, right)
              in
              let t_new = Atomic.make s_t_new in
              let expected_height =
                1 + max expected_height (height left_left)
              in
              let s_new =
                Node (Alive, expected_height, left_left, left_pivot, t_new)
              in
              if Atomic.compare_and_set t s s_new
              then begin
                match push_down t_new s_t_new with
                | Ok -> Ok
                | Overflow -> balance t
                | Retry | Looking_for_life -> assert false
              end
              else Ok
          | Node (Alive, left_height, left_left, left_pivot, left_right) ->
              let s_left_dead =
                Node (Dead, left_height, left_left, left_pivot, left_right)
              in
              let (_ : bool) = Atomic.compare_and_set left s_left s_left_dead in
              push_down t s
          | _ ->
              let (_ : result) = fixup left in
              push_down t s
        end
    | _ -> Ok

  and push_down_left_locked t s right =
    if Atomic.get t != s
    then Ok
    else begin
      let s_right = Atomic.get right in
      match s_right with
      | Leaf Dead ->
          if Atomic.compare_and_set t s (Leaf Alive) then Overflow else Ok
      | Leaf Alive ->
          let s_right_dead = Leaf Dead in
          if Atomic.compare_and_set right s_right s_right_dead
          then if Atomic.compare_and_set t s s_right then Overflow else Ok
          else push_down_left_locked t s right
      | Node (Dead, right_height, right_left, right_pivot, right_right) ->
          let s_right_alive =
            Node (Alive, right_height, right_left, right_pivot, right_right)
          in
          if Atomic.compare_and_set t s s_right_alive then Overflow else Ok
      | Node (Alive, right_height, right_left, right_pivot, right_right) ->
          let s_right_dead =
            Node (Dead, right_height, right_left, right_pivot, right_right)
          in
          if Atomic.compare_and_set right s_right s_right_dead
          then if Atomic.compare_and_set t s s_right then Overflow else Ok
          else push_down_left_locked t s right
      | _ ->
          let result = fixup right in
          begin
            match push_down_left_locked t s right with
            | Ok -> result
            | other -> other
          end
    end

  and balance t =
    let s = Atomic.get t in
    match s with
    | Node (Balancing_left (s_left, s_left_dead), _, left, pivot, right) ->
        if Atomic.compare_and_set left s_left s_left_dead
           || Atomic.get left == s_left_dead
        then begin
          match s_left with
          | Node (Alive, _, a, b, c) ->
              let repr = s_create a b (create c pivot right) in
              if Atomic.compare_and_set t s repr then Overflow else Ok
          | _ -> assert false
        end
        else begin
          let (_ : bool) =
            Atomic.compare_and_set t s (s_create left pivot right)
          in
          Ok
        end
    | Node
        ( Balancing_left_center (s_left, s_left_dead, s_center, s_center_dead)
        , _
        , left
        , pivot
        , right ) ->
        if Atomic.compare_and_set left s_left s_left_dead
           || Atomic.get left == s_left_dead
        then begin
          match s_left with
          | Node (Alive, _, a, b, c) ->
              if Atomic.compare_and_set c s_center s_center_dead
                 || Atomic.get c == s_center_dead
              then begin
                match s_center with
                | Node (Alive, _, c, d, e) ->
                    let repr =
                      s_create (create a b c) d (create e pivot right)
                    in
                    if Atomic.compare_and_set t s repr then Overflow else Ok
                | _ -> assert false
              end
              else begin
                let s_left_new = s_create a b c in
                let (_ : bool) =
                  Atomic.compare_and_set left s_left_dead s_left_new
                in
                let (_ : bool) =
                  Atomic.compare_and_set t s (s_create left pivot right)
                in
                Ok
              end
          | _ -> assert false
        end
        else begin
          let (_ : bool) =
            Atomic.compare_and_set t s (s_create left pivot right)
          in
          Ok
        end
    | Node (Balancing_right (s_right, s_right_dead), _, left, pivot, right) ->
        if Atomic.compare_and_set right s_right s_right_dead
           || Atomic.get right == s_right_dead
        then begin
          match s_right with
          | Node (Alive, _, a, b, c) ->
              let repr = s_create (create left pivot a) b c in
              if Atomic.compare_and_set t s repr then Overflow else Ok
          | _ -> assert false
        end
        else begin
          let (_ : bool) =
            Atomic.compare_and_set t s (s_create left pivot right)
          in
          Ok
        end
    | Node
        ( Balancing_right_center (s_right, s_right_dead, s_center, s_center_dead)
        , _
        , left
        , pivot
        , right ) ->
        if Atomic.compare_and_set right s_right s_right_dead
           || Atomic.get right == s_right_dead
        then begin
          match s_right with
          | Node (Alive, _, x, y, z) ->
              if Atomic.compare_and_set x s_center s_center_dead
                 || Atomic.get x == s_center_dead
              then begin
                match s_center with
                | Node (Alive, _, v, w, x) ->
                    let repr =
                      s_create (create left pivot v) w (create x y z)
                    in
                    if Atomic.compare_and_set t s repr then Overflow else Ok
                | _ -> assert false
              end
              else begin
                let s_right_new = s_create x y z in
                let (_ : bool) =
                  Atomic.compare_and_set right s_right_dead s_right_new
                in
                let (_ : bool) =
                  Atomic.compare_and_set t s (s_create left pivot right)
                in
                Ok
              end
          | _ -> assert false
        end
        else begin
          let (_ : bool) =
            Atomic.compare_and_set t s (s_create left pivot right)
          in
          Ok
        end
    | Node (Alive, ht, left, pivot, right) ->
        let s_left = get_stable left in
        let s_right = get_stable right in
        let hl = s_height s_left in
        let hr = s_height s_right in
        if hl > hr + 2
        then begin
          match s_left with
          | Node (Alive, hl, left_left, left_pivot, left_right) ->
              let hll = height left_left in
              let s_left_right = get_stable left_right in
              if hll >= s_height s_left_right
              then begin
                let s_left_dead =
                  Node (Dead, hl, left_left, left_pivot, left_right)
                in
                let plan = Balancing_left (s_left, s_left_dead) in
                if Atomic.compare_and_set t s
                     (Node (plan, ht, left, pivot, right))
                then balance t
                else Ok
              end
              else begin
                match s_left_right with
                | Node (Alive, hlr, a, b, c) ->
                    let s_left_right_dead = Node (Dead, hlr, a, b, c) in
                    let s_left_dead =
                      Node (Dead, hl, left_left, left_pivot, left_right)
                    in
                    let plan =
                      Balancing_left_center
                        (s_left, s_left_dead, s_left_right, s_left_right_dead)
                    in
                    if Atomic.compare_and_set t s
                         (Node (plan, ht, left, pivot, right))
                    then balance t
                    else Ok
                | _ -> Ok
              end
          | _ -> Ok
        end
        else if hr > hl + 2
        then begin
          match s_right with
          | Node (Alive, hr, right_left, right_pivot, right_right) ->
              let hrr = height right_right in
              let s_right_left = get_stable right_left in
              if hrr >= s_height s_right_left
              then begin
                let s_right_dead =
                  Node (Dead, hr, right_left, right_pivot, right_right)
                in
                let plan = Balancing_right (s_right, s_right_dead) in
                if Atomic.compare_and_set t s
                     (Node (plan, ht, left, pivot, right))
                then balance t
                else Ok
              end
              else begin
                match s_right_left with
                | Node (Alive, hrl, a, b, c) ->
                    let s_right_left_dead = Node (Dead, hrl, a, b, c) in
                    let s_right_dead =
                      Node (Dead, hr, right_left, right_pivot, right_right)
                    in
                    let plan =
                      Balancing_right_center
                        (s_right, s_right_dead, s_right_left, s_right_left_dead)
                    in
                    if Atomic.compare_and_set t s
                         (Node (plan, ht, left, pivot, right))
                    then balance t
                    else Ok
                | _ -> Ok
              end
          | _ -> Ok
        end
        else begin
          let expected_height = 1 + max hl hr in
          if ht = expected_height
          then Ok
          else begin
            let repr = Node (Alive, expected_height, left, pivot, right) in
            if Atomic.compare_and_set t s repr then Overflow else Ok
          end
        end
    | _ -> Ok

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

  let mem x t = mem x (Atomic.get t)

  let rec snapshot t =
    let root = Atomic.get t in
    if Atomic.compare_and_set t root (Atomic.make (Copy root))
    then root
    else snapshot t

  let copy t =
    let root = snapshot t in
    Atomic.make (Atomic.make (Copy root))

  module View = struct
    type elt = E.t

    type t = r

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

  let cardinal t = View.cardinal (snapshot t)

  let fold f t acc = View.fold f (snapshot t) acc

  let iter f t = View.iter f (snapshot t)

  let for_all f t = View.for_all f (snapshot t)

  let exists f t = View.exists f (snapshot t)

  let elements t = View.elements (snapshot t)

  let to_seq t = View.to_seq (snapshot t)

  let to_rev_seq t = View.to_rev_seq (snapshot t)

  let to_seq_from elt t = View.to_seq_from elt (snapshot t)
end
