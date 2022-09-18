module S = Mcavl.Set (Int)

let test_all () =
  let t = S.empty () in
  for i = 1 to 1000 do
    S.add ((4 * i) + 2) t
  done ;
  let started = Array.init 4 (fun _ -> Atomic.make false) in
  let started_other = Atomic.make false in
  let await ~d i =
    if i = 100
    then begin
      Atomic.set started.(d) true ;
      while not (Atomic.get started_other) do
        Domain.cpu_relax ()
      done
    end
  in
  let copy = ref t in
  Array.iter Domain.join
    [| Domain.spawn (fun () ->
           for i = 1 to 1000 do
             S.add (4 * i) t ;
             await ~d:0 i
           done )
     ; Domain.spawn (fun () ->
           for i = 1 to 1000 do
             S.add (4 * i) t ;
             await ~d:1 i
           done )
     ; Domain.spawn (fun () ->
           for i = 1 to 1000 do
             S.add ((4 * i) + 1) t ;
             await ~d:2 i
           done )
     ; Domain.spawn (fun () ->
           for i = 1 to 1000 do
             assert (S.remove ((4 * i) + 2) t) ;
             await ~d:3 i
           done )
     ; Domain.spawn (fun () ->
           while not (Array.for_all Atomic.get started) do
             Domain.cpu_relax ()
           done ;
           Atomic.set started_other true ;
           let t' = S.copy t in
           Array.iter Domain.join
             [| Domain.spawn (fun () ->
                    let ok = ref true in
                    assert (S.mem 4 t') ;
                    for i = 1 to 1000 do
                      let rem = S.remove ((4 * i) + 0) t' in
                      if !ok then ok := rem else assert (not rem)
                    done ;
                    assert (not !ok) )
              ; Domain.spawn (fun () ->
                    let ok = ref true in
                    assert (S.mem 5 t') ;
                    for i = 1 to 1000 do
                      let rem = S.remove ((4 * i) + 1) t' in
                      if !ok then ok := rem else assert (not rem)
                    done ;
                    assert (not !ok) )
              ; Domain.spawn (fun () ->
                    let ok = ref false in
                    for i = 1 to 1000 do
                      let rem = S.remove ((4 * i) + 2) t' in
                      if not !ok then ok := rem else assert rem
                    done ;
                    assert !ok )
              ; Domain.spawn (fun () ->
                    for i = 1 to 1000 do
                      S.add ((4 * i) + 3) t'
                    done ) |] ;
           copy := t' ) |] ;
  Alcotest.(check int) "cardinal" 2000 (S.cardinal t) ;
  Alcotest.(check int) "cardinal copy" 1000 (S.cardinal !copy)

let test_all () =
  for _ = 0 to 100 do
    test_all ()
  done

let tests =
  let open Alcotest in
  [test_case "all" `Quick test_all]
