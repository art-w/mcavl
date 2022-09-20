module Set_sig = struct
  module S = Mcavl.Set (Int)

  type t = S.t

  let init () = S.empty ()

  let cleanup _ = ()

  open Lin_api

  let nat = nat_small

  let api =
    [ val_ "S.add" S.add (nat @-> t @-> returning unit)
    ; val_ "S.remove" S.remove (nat @-> t @-> returning bool)
    ; val_ "S.mem" S.mem (nat @-> t @-> returning bool)
    ; val_ "S.cardinal" S.cardinal (t @-> returning int) ]
end

module HT = Lin_api.Make (Set_sig) ;;

QCheck_base_runner.run_tests_main
  [HT.lin_test `Domain ~count:10_000 ~name:"Mcavl.Set"]
