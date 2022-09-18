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

let test_union () =
  let t1 = S.of_list @@ List.init 1000 (fun i -> 3 * i) in
  let t2 = S.of_list @@ List.init 1000 (fun i -> (3 * i) + 1) in
  let t3 = S.of_list @@ List.init 1000 (fun i -> 100 + i) in
  let t12 = S.union t1 t2 in
  Alcotest.(check bool)
    "union t1 t2: subset t1" true
    (S.for_all (fun e -> S.mem e t12) t1) ;
  Alcotest.(check bool)
    "union t1 t2: subset t2" true
    (S.for_all (fun e -> S.mem e t12) t2) ;
  let t123 = S.union t12 t3 in
  Alcotest.(check bool)
    "union t12 t3: subset t12" true
    (S.for_all (fun e -> S.mem e t123) t12) ;
  Alcotest.(check bool)
    "union t12 t3: subset t3" true
    (S.for_all (fun e -> S.mem e t123) t3)

let test_inter () =
  let t1 = S.of_list @@ List.init 1000 (fun i -> 3 * i) in
  let t2 = S.of_list @@ List.init 1000 (fun i -> (3 * i) + 1) in
  let t3 = S.of_list @@ List.init 1000 (fun i -> 100 + i) in
  let t12 = S.inter t1 t2 in
  Alcotest.(check bool) "inter empty" true (S.is_empty t12) ;
  let t13 = S.inter t1 t3 in
  Alcotest.(check bool)
    "inter t1 t3: t1" true
    (S.for_all (fun e -> S.mem e t13 = S.mem e t3) t1) ;
  Alcotest.(check bool)
    "inter t1 t3: t3" true
    (S.for_all (fun e -> S.mem e t13 = S.mem e t1) t3)

let test_diff () =
  let t1 = S.of_list @@ List.init 1000 (fun i -> 3 * i) in
  let t3 = S.of_list @@ List.init 1000 (fun i -> 100 + i) in
  let t13 = S.diff t1 t3 in
  Alcotest.(check bool)
    "diff t1 t3: t1" true
    (S.for_all (fun e -> S.mem e t13 = not (S.mem e t3)) t1) ;
  Alcotest.(check bool)
    "diff t1 t3: t3" true
    (S.for_all (fun e -> not (S.mem e t13)) t3)

let test_split () =
  let t = S.of_list @@ List.init 1000 (fun i -> 2 * i) in
  let smaller, found, larger = S.split 789 t in
  Alcotest.(check bool) "split: not found" false found ;
  Alcotest.(check int) "split: cardinal smaller" 395 (S.cardinal smaller) ;
  Alcotest.(check bool)
    "split: is smaller" true
    (S.for_all (fun e -> e < 789) smaller) ;
  Alcotest.(check int) "split: cardinal larger" 605 (S.cardinal larger) ;
  Alcotest.(check bool)
    "split: is larger" true
    (S.for_all (fun e -> e > 789) larger) ;
  Alcotest.(check bool)
    "split: found" true
    (S.for_all (fun e -> S.mem e (if e < 789 then smaller else larger)) t) ;
  let smaller, found, larger = S.split 100 t in
  Alcotest.(check bool) "split: found" true found ;
  Alcotest.(check bool) "split: not smaller" false (S.mem 100 smaller) ;
  Alcotest.(check bool) "split: not larger" false (S.mem 100 larger)

let test_map () =
  let t = S.of_list @@ List.init 1000 (fun i -> i) in
  let t' = S.map (fun x -> x) t in
  Alcotest.(check bool) "map physeq" true (t == t') ;
  let t' = S.map (fun x -> if x > 500 then -x else x) t in
  Alcotest.(check bool)
    "map partial physeq" true
    (S.for_all (fun x -> S.mem (if x > 500 then -x else x) t') t) ;
  let t' = S.map (fun x -> -x) t in
  Alcotest.(check bool)
    "map worst-case" true
    (S.for_all (fun x -> S.mem (-x) t') t) ;
  let t' = S.map (fun x -> x + 1) t in
  Alcotest.(check bool)
    "map in-order" true
    (S.for_all (fun x -> S.mem (x + 1) t') t)

let test_filter () =
  let t = S.of_list @@ List.init 1000 (fun i -> i) in
  let t' = S.filter (fun _ -> true) t in
  Alcotest.(check bool) "filter physeq" true (t == t') ;
  let t' = S.filter (fun _ -> false) t in
  Alcotest.(check bool) "filter all" true (S.is_empty t') ;
  let t' = S.filter (fun x -> x > 123) t in
  Alcotest.(check bool)
    "filter some half" true
    (S.for_all (fun x -> x > 123 = S.mem x t') t) ;
  let t' = S.filter (fun x -> x mod 2 = 0) t in
  Alcotest.(check bool)
    "filter some even" true
    (S.for_all (fun x -> x mod 2 = 0 = S.mem x t') t)

let test_partition () =
  let t = S.of_list @@ List.init 1000 (fun i -> i) in
  let t', tn = S.partition (fun _ -> true) t in
  Alcotest.(check bool) "partition physeq" true (t == t') ;
  Alcotest.(check bool) "partition is_empty" true (S.is_empty tn) ;
  let t', tn = S.partition (fun _ -> false) t in
  Alcotest.(check bool) "partition all is_empty" true (S.is_empty t') ;
  Alcotest.(check bool) "partition all physeq" true (t == tn) ;
  let t', tn = S.partition (fun x -> x > 123) t in
  Alcotest.(check bool)
    "partition some half" true
    (S.for_all (fun x -> x > 123 = S.mem x t' && x <= 123 = S.mem x tn) t) ;
  let t', tn = S.partition (fun x -> x mod 2 = 0) t in
  Alcotest.(check bool)
    "partition some even" true
    (S.for_all
       (fun x -> x mod 2 = 0 = S.mem x t' && x mod 2 <> 0 = S.mem x tn)
       t )

let test_filter_map () =
  let t = S.of_list @@ List.init 1000 (fun i -> i) in
  let t' = S.filter_map (fun x -> Some x) t in
  Alcotest.(check bool) "filter_map physeq" true (t == t') ;
  let t' = S.filter_map (fun _ -> None) t in
  Alcotest.(check bool) "filter_map all" true (S.is_empty t') ;
  let t' = S.filter_map (fun x -> if x > 123 then None else Some x) t in
  Alcotest.(check bool)
    "filter_map some half" true
    (S.for_all (fun x -> x <= 123 = S.mem x t') t) ;
  Printf.printf "------------------------\n%!" ;
  let t' = S.filter_map (fun x -> if x mod 2 = 0 then Some (-x) else None) t in
  Alcotest.(check bool)
    "filter_map some even" true
    (S.for_all (fun x -> x mod 2 = 0 = S.mem (-x) t') t)

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
  ; test_case "union" `Quick test_union
  ; test_case "inter" `Quick test_inter
  ; test_case "diff" `Quick test_diff
  ; test_case "split" `Quick test_split
  ; test_case "map" `Quick test_map
  ; test_case "filter" `Quick test_filter
  ; test_case "partition" `Quick test_partition
  ; test_case "filter_map" `Quick test_filter_map
  ; test_case "iter" `Quick test_iter
  ; test_case "fold" `Quick test_fold
  ; test_case "seq" `Quick test_seq
  ; test_case "exists" `Quick test_exists
  ; test_case "for_all" `Quick test_for_all
  ; test_case "shuffle" `Quick test_shuffle ]
