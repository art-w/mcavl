module MS = Mcset.Make (Int)
module S = MS.View

let shuffle a =
  let n = Array.length a in
  let a = Array.copy a in
  for i = n - 1 downto 1 do
    let k = Random.int (i + 1) in
    let x = a.(k) in
    a.(k) <- a.(i) ;
    a.(i) <- x
  done

let shuffle_list lst =
  let a = Array.of_list lst in
  shuffle a ; Array.to_list a

let test_empty () =
  let t = S.empty in
  Alcotest.(check int) "cardinal" 0 (S.cardinal t)

let test_singleton () =
  let t = S.singleton 42 in
  Alcotest.(check int) "singleton" 1 (S.cardinal t) ;
  Alcotest.(check bool) "member" true (S.mem 42 t) ;
  ()

let test_shuffle () =
  let lst = List.init 1000 (fun i -> 2 * i) in
  let t = S.of_list @@ shuffle_list lst in
  Alcotest.(check int) "cardinal" 1000 (S.cardinal t) ;
  Alcotest.(check bool) "is_empty" false (S.is_empty t) ;
  let lst' = S.to_list t in
  Alcotest.(check (list int)) "iso" lst lst' ;
  let elt = S.choose t in
  Alcotest.(check bool) "choose mem" true (S.mem elt t) ;
  let elt = S.min_elt t in
  Alcotest.(check bool) "min_elt mem" true (S.mem elt t) ;
  Alcotest.(check bool)
    "min_elt smallest" true
    (S.for_all (fun e -> elt <= e) t) ;
  let elt = S.max_elt t in
  Alcotest.(check bool) "max_elt mem" true (S.mem elt t) ;
  Alcotest.(check bool) "max_elt largest" true (S.for_all (fun e -> elt >= e) t) ;
  Alcotest.(check int) "find" 42 (S.find 42 t) ;
  Alcotest.(check int) "find_first" 68 (S.find_first (fun x -> x > 66) t) ;
  Alcotest.(check (option int))
    "find_first_opt missing" None
    (S.find_first_opt (fun x -> x > 99999) t) ;
  Alcotest.(check int) "find_last" 522 (S.find_last (fun x -> x < 523) t) ;
  Alcotest.(check int)
    "find_last bound" (S.max_elt t)
    (S.find_last (fun x -> x < 9999) t) ;
  Alcotest.(check (option int))
    "find_last_opt missing" None
    (S.find_last_opt (fun x -> x < 0) t) ;
  Alcotest.(check bool) "mem" true (List.for_all (fun i -> S.mem i t) lst) ;
  Alcotest.(check bool)
    "not mem" false
    (List.exists (fun i -> S.mem i t) @@ List.map (fun i -> i + 1) lst) ;
  let t' =
    List.fold_left
      (fun t i ->
        let x, t' = S.pop_min t in
        Alcotest.(check int) "pop_min" i x ;
        let card = S.cardinal t - 1 in
        Alcotest.(check int) "pop_min cardinal" card (S.cardinal t') ;
        let t'' = S.remove i t in
        Alcotest.(check bool) "remove not equal" false (t == t'') ;
        Alcotest.(check int) "remove cardinal" card (S.cardinal t'') ;
        t'' )
      t lst
  in
  Alcotest.(check bool) "is_empty" true (S.is_empty t') ;
  Alcotest.(check bool) "not mem 43" false (S.mem 43 t) ;
  let t' = S.remove 43 t in
  Alcotest.(check bool) "remove physeq" true (t' == t) ;
  Alcotest.(check bool) "mem 78" true (S.mem 78 t) ;
  let t' = S.add 78 t in
  Alcotest.(check bool) "add physeq" true (t' == t) ;
  let lst', t' =
    List.fold_left
      (fun (acc, t) _ ->
        let x, t = S.pop_max t in
        x :: acc, t )
      ([], t) lst
  in
  Alcotest.(check bool) "pop_max empty" true (S.is_empty t') ;
  Alcotest.(check (list int)) "pop_max sorted" lst lst'

let test_iter () =
  let t = S.of_list @@ List.init 1000 (fun i -> i) in
  let lst = ref [] in
  S.iter (fun i -> lst := i :: !lst) t ;
  Alcotest.(check (list int))
    "order"
    (List.init 1000 (fun i -> i))
    (List.rev !lst)

let test_fold () =
  let t = S.of_list @@ List.init 1000 (fun i -> i) in
  let lst = S.fold (fun i lst -> i :: lst) t [] in
  Alcotest.(check (list int))
    "order"
    (List.init 1000 (fun i -> i))
    (List.rev lst)

let test_seq () =
  let t = S.of_seq @@ List.to_seq @@ List.init 1000 (fun i -> i) in
  Alcotest.(check int) "cardinal" 1000 (S.cardinal t) ;
  let seq = S.to_seq t in
  let lst = Seq.fold_left (fun lst i -> i :: lst) [] seq in
  Alcotest.(check (list int))
    "to_seq"
    (List.init 1000 (fun i -> i))
    (List.rev lst) ;
  let seq = S.to_rev_seq t in
  let lst = Seq.fold_left (fun lst i -> i :: lst) [] seq in
  Alcotest.(check (list int)) "to_rev_seq" (List.init 1000 (fun i -> i)) lst ;
  let seq = S.to_seq_from 42 t in
  let lst = Seq.fold_left (fun lst i -> i :: lst) [] seq in
  Alcotest.(check (list int))
    "order"
    (List.filter (fun i -> i >= 42) (List.init 1000 (fun i -> i)))
    (List.rev lst)

let test_exists () =
  Alcotest.(check bool) "is_empty" true (S.is_empty S.empty) ;
  Alcotest.(check bool)
    "not exists empty" false
    (S.exists (fun _ -> assert false) S.empty) ;
  let t = S.of_list @@ List.init 1000 (fun i -> i) in
  let lst = ref [] in
  Alcotest.(check bool)
    "exists" true
    (S.exists
       (fun i ->
         lst := i :: !lst ;
         i = 789 )
       t ) ;
  Alcotest.(check bool) "exists shortcut" true (List.length !lst < 999) ;
  let lst = ref [] in
  Alcotest.(check bool)
    "not exists" false
    (S.exists
       (fun i ->
         lst := i :: !lst ;
         i = -1 )
       t ) ;
  Alcotest.(check (list int))
    "not exists no shortcut"
    (List.init 1000 (fun i -> i))
    (List.sort Int.compare !lst)

let test_for_all () =
  Alcotest.(check bool)
    "for_all empty" true
    (S.for_all (fun _ -> assert false) S.empty) ;
  let t = S.of_list @@ List.init 1000 (fun i -> i) in
  let lst = ref [] in
  Alcotest.(check bool)
    "for_all" true
    (S.for_all
       (fun i ->
         lst := i :: !lst ;
         i >= 0 )
       t ) ;
  Alcotest.(check (list int))
    "for_all no shortcut"
    (List.init 1000 (fun i -> i))
    (List.sort Int.compare !lst) ;
  let lst = ref [] in
  Alcotest.(check bool)
    "not for_all" false
    (S.for_all
       (fun i ->
         lst := i :: !lst ;
         i = 0 )
       t ) ;
  Alcotest.(check int) "for_all shortcut" 1 (List.length !lst)

let tests =
  let open Alcotest in
  [ test_case "empty" `Quick test_empty
  ; test_case "singleton" `Quick test_singleton
  ; test_case "iter" `Quick test_iter
  ; test_case "fold" `Quick test_fold
  ; test_case "seq" `Quick test_seq
  ; test_case "exists" `Quick test_exists
  ; test_case "for_all" `Quick test_for_all
  ; test_case "shuffle" `Quick test_shuffle ]
