module S = Set.Make (Int)
module T = Mcset.Make (Int)
open QCheck
open STM

module Conf = struct
  type cmd =
    | Add of int
    | Remove of int
    | Mem of int
    | Cardinal
    | Min_elt_opt
    | Choose_opt
  [@@deriving show {with_path= false}]

  type state = S.t

  type sut = T.t

  let arb_cmd _ =
    let int_gen = Gen.int_bound 10 in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [ Gen.map (fun i -> Add i) int_gen
         ; Gen.map (fun i -> Remove i) int_gen
         ; Gen.map (fun i -> Mem i) int_gen
         ; Gen.return Cardinal
         ; Gen.return Choose_opt
         ; Gen.return Min_elt_opt ] )

  let init_state = S.empty

  let init_sut () = T.empty ()

  let cleanup _ = ()

  let next_state c s =
    match c with
    | Add i -> S.add i s
    | Remove i -> S.remove i s
    | Mem _ | Cardinal | Choose_opt | Min_elt_opt -> s

  let run c r =
    match c with
    | Add i -> Res (unit, T.add i r)
    | Remove i -> Res (bool, T.remove i r)
    | Mem i -> Res (bool, T.mem i r)
    | Cardinal -> Res (int, T.cardinal r)
    | Choose_opt -> Res (option int, T.choose_opt r)
    | Min_elt_opt -> Res (option int, T.min_elt_opt r)

  let precond _ _ = true

  let postcond c s res =
    match c, res with
    | Add _, Res ((Unit, _), _) -> true
    | Remove i, Res ((Bool, _), found) -> found = S.mem i s
    | Mem i, Res ((Bool, _), m) -> m = S.mem i s
    | Cardinal, Res ((Int, _), m) -> m = S.cardinal s
    | Choose_opt, Res ((Option Int, _), None) -> S.is_empty s
    | Choose_opt, Res ((Option Int, _), Some x) -> S.mem x s
    | Min_elt_opt, Res ((Option Int, _), m) -> m = S.min_elt_opt s
    | _ -> assert false
end

module CT = STM.Make (Conf)

let () =
  QCheck_runner.run_tests_main
    [ CT.agree_test ~count:10_000 ~name:"seq"
    ; CT.agree_test_par ~count:10_000 ~name:"par" ]
