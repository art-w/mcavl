module M = Map.Make (Int)
module T = Mcavl.Map (Int)
open QCheck
open STM

module Conf = struct
  type cmd =
    | Add of int * int
    | Remove of int
    | Find_opt of int
    | Copy_find_opt of int
    | Cardinal
  [@@deriving show {with_path= false}]

  type state = int M.t

  type sut = int T.t

  let arb_cmd _ =
    let int_gen = Gen.int_bound 10 in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [ Gen.map2 (fun i j -> Add (i, j)) int_gen Gen.nat
         ; Gen.map (fun i -> Remove i) int_gen
         ; Gen.map (fun i -> Find_opt i) int_gen
         ; Gen.map (fun i -> Copy_find_opt i) int_gen
         ; Gen.return Cardinal ] )

  let init_state = M.empty

  let init_sut () = T.empty ()

  let cleanup _ = ()

  let next_state c s =
    match c with
    | Add (k, v) -> M.add k v s
    | Remove k -> M.remove k s
    | Find_opt _ | Copy_find_opt _ | Cardinal -> s

  let run c r =
    match c with
    | Add (k, v) -> Res (unit, T.add k v r)
    | Remove k -> Res (bool, T.remove k r)
    | Find_opt k -> Res (option int, T.find_opt k r)
    | Copy_find_opt k -> Res (option int, T.find_opt k (T.copy r))
    | Cardinal -> Res (int, T.cardinal r)

  let precond _ _ = true

  let postcond c s res =
    match c, res with
    | Add _, Res ((Unit, _), _) -> true
    | Remove k, Res ((Bool, _), found) -> found = M.mem k s
    | (Find_opt k | Copy_find_opt k), Res ((Option Int, _), (m : int option)) ->
        m = M.find_opt k s
    | Cardinal, Res ((Int, _), m) -> m = M.cardinal s
    | _ -> assert false
end

module CT = STM.Make (Conf)

let () =
  QCheck_runner.run_tests_main
    [ CT.agree_test ~count:10_000 ~name:"seq"
    ; CT.agree_test_par ~count:10_000 ~name:"par" ]
