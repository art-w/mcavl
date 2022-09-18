module Make (E : S.Ordered_poly) = struct
  include Pure.Make (E)

  type result = Ok | Overflow | Looking_for_life | Retry

  let empty () = Atomic.make (Pure.empty ~s:Alive)

  let singleton elt = Atomic.make (Pure.singleton ~s:Alive elt)

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
end
