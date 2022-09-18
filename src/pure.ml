module Make (E : S.Ordered_poly) = struct
  type 'a elt = 'a E.t

  type 'a t = 'a r Atomic.t

  and 'a r = 'a s Atomic.t

  and 'a s =
    | Leaf of 'a state
    | Node of 'a state * int * 'a r * 'a elt * 'a r
    | Copy of 'a r

  and 'a state =
    | Alive
    | Dead
    | Removing
    | Read_only
    | Balancing_left of 'a s * 'a s
    | Balancing_left_center of 'a s * 'a s * 'a s * 'a s
    | Balancing_right of 'a s * 'a s
    | Balancing_right_center of 'a s * 'a s * 'a s * 'a s
    | Attempt_add of attempt * 'a t * 'a r
    | Attempt_remove of attempt * 'a t * 'a r
    | Attempt_replace of attempt * 'a t * 'a r * 'a elt

  and attempt = attempt_state Atomic.t

  and attempt_state = Unknown | Success | Failure

  module Pure = struct
    let empty ~s = Atomic.make (Leaf s)

    let singleton ~s elt =
      let leaf = Leaf s in
      Atomic.make (Node (s, 1, Atomic.make leaf, elt, Atomic.make leaf))

    let rec height ~get t = s_height ~get (get t)

    and s_height ~get = function
      | Leaf _ -> 0
      | Node (_, h, _, _, _) -> h
      | Copy t -> height ~get t

    let create ~s ~h left pivot right =
      Atomic.make (Node (s, h, left, pivot, right))

    let balance ~s ~get left pivot right =
      let s_left = get left in
      let s_right = get right in
      let hl = s_height ~get s_left in
      let hr = s_height ~get s_right in
      if hl > hr + 2
      then begin
        match s_left with
        | Node (_, _, left_left, left_pivot, left_right) ->
            let hll = height ~get left_left in
            let s_left_right = get left_right in
            let hlr = s_height ~get s_left_right in
            if hll >= hlr
            then
              let h = 1 + max hlr hr in
              create ~s
                ~h:(1 + max hll h)
                left_left left_pivot
                (create ~s ~h left_right pivot right)
            else begin
              match s_left_right with
              | Node (_, _, center_left, center_pivot, center_right) ->
                  let hlc = 1 + max hll (height ~get center_left) in
                  let hcr = 1 + max (height ~get center_right) hr in
                  let h = 1 + max hlc hcr in
                  create ~s ~h
                    (create ~s ~h:hlc left_left left_pivot center_left)
                    center_pivot
                    (create ~s ~h:hcr center_right pivot right)
              | _ -> assert false
            end
        | _ -> assert false
      end
      else if hr > hl + 2
      then begin
        match s_right with
        | Node (_, _, right_left, right_pivot, right_right) ->
            let hrr = height ~get right_right in
            let s_right_left = get right_left in
            let hrl = s_height ~get s_right_left in
            if hrr >= hrl
            then
              let h = 1 + max hl hrl in
              create ~s
                ~h:(1 + max h hrr)
                (create ~s ~h left pivot right_left)
                right_pivot right_right
            else begin
              match get right_left with
              | Node (_, _, center_left, center_pivot, center_right) ->
                  let hlc = 1 + max hl (height ~get center_left) in
                  let hcr = 1 + max (height ~get center_right) hrr in
                  let h = 1 + max hlc hcr in
                  create ~s ~h
                    (create ~s ~h:hlc left pivot center_left)
                    center_pivot
                    (create ~s ~h:hcr center_right right_pivot right_right)
              | _ -> assert false
            end
        | _ -> assert false
      end
      else
        let height = 1 + max hl hr in
        Atomic.make (Node (s, height, left, pivot, right))

    let rec add ~s ~get x t =
      match get t with
      | Leaf _ -> singleton ~s x
      | Node (_, _, left, pivot, right) -> begin
          match E.compare x pivot with
          | 0 -> t
          | c when c < 0 ->
              let left' = add ~s ~get x left in
              if left' == left then t else balance ~s ~get left' pivot right
          | _ ->
              let right' = add ~s ~get x right in
              if right' == right then t else balance ~s ~get left pivot right'
        end
      | _ -> assert false

    let of_list ~s lst =
      List.fold_left (fun t x -> add ~s ~get:Atomic.get x t) (empty ~s) lst

    let of_seq ~s seq =
      Seq.fold_left (fun t x -> add ~s ~get:Atomic.get x t) (empty ~s) seq
  end
end
