let nb =
  try int_of_string Sys.argv.(1) with
  | _ -> 100_000

let max_domains =
  try int_of_string Sys.argv.(2) with
  | _ -> 8

module T = Domainslib.Task

module type SET = sig
  type elt = int
  type t

  val make : unit -> t
  val copy : t -> t
  val add : elt -> t -> unit
  val remove : elt -> t -> unit
  val cardinal : t -> int
end

module Mcset_int = Mcset.Make (Int)

module Naive = struct
  module S = Set.Make (Int)

  type elt = S.elt
  type t = S.t Atomic.t

  let make () = Atomic.make S.empty

  let copy t =
    let s = Atomic.get t in
    Atomic.make s

  let rec add x t =
    let s = Atomic.get t in
    let s' = S.add x s in
    if Atomic.compare_and_set t s s' then () else add x t

  let rec remove x t =
    let s = Atomic.get t in
    let s' = S.remove x s in
    if Atomic.compare_and_set t s s' then () else remove x t

  let cardinal t = S.cardinal (Atomic.get t)
end

module Test (Config : sig
  val pool : T.pool
  val nb_threads : int

  module S : SET
end) =
struct
  module S = Config.S

  let () = Printf.printf "%i%!" Config.nb_threads

  let bench ~init fn =
    let last = ref None in
    let metrics =
      Array.init 11
      @@ fun _ ->
      let input = init () in
      let t0 = Unix.gettimeofday () in
      let r = fn input in
      let t1 = Unix.gettimeofday () in
      last := Some r ;
      t1 -. t0
    in
    Array.sort Float.compare metrics ;
    let median = metrics.(Array.length metrics / 2) in
    Printf.printf "\t%f%!" (1000.0 *. median) ;
    match !last with
    | Some result -> result
    | None -> assert false

  let iter start finish body = T.parallel_for Config.pool ~start ~finish ~body

  let t_full =
    let t =
      bench
        ~init:(fun () -> S.make ())
        (fun t ->
          iter 1 nb (fun i -> S.add i t) ;
          t)
    in
    assert (S.cardinal t = nb) ;
    t

  let () =
    bench
      ~init:(fun () ->
        let t = S.make () in
        iter 1 nb (fun i -> S.add i t) ;
        t)
      (fun t ->
        iter 1 nb (fun i -> S.remove i t) ;
        assert (S.cardinal t = 0))

  let () =
    bench
      ~init:(fun () -> S.copy t_full)
      (fun t ->
        iter 1 nb (fun i -> S.remove i t) ;
        assert (S.cardinal t = 0)) ;
    assert (S.cardinal t_full = nb)
end

let run domains (module Impl : SET) =
  let module Config = struct
    let pool = T.setup_pool ~num_domains:domains ()
    let nb_threads = domains + 1

    module S = Impl
  end
  in
  T.run Config.pool (fun () ->
    let module Run = Test (Config) in
    ()) ;
  T.teardown_pool Config.pool ;
  Printf.printf "\n%!" ;
  ()

let () =
  Printf.printf "Mcset.t\n%!" ;
  Printf.printf "CPU\tADD\tREMOVE\tCOPY REMOVE\n%!" ;
  for domains = 0 to max_domains - 1 do
    run domains (module Mcset_int)
  done ;
  Printf.printf "\n%!"

let () =
  Printf.printf "Stdlib.Set.t Atomic.t\n%!" ;
  Printf.printf "CPU\tADD\tREMOVE\tCOPY REMOVE\n%!" ;
  for domains = 0 to max_domains - 1 do
    run domains (module Naive)
  done ;
  Printf.printf "\n%!"
