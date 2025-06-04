open! Containers
open Functional_stack

let test_push =
  QCheck.(
    Test.make ~count:1000 (list int) (fun l ->
        List.equal Int.equal (push 5 l) (5 :: l)))

let test_pop =
  QCheck.(
    Test.make ~count:1000 (list int) (fun l ->
        assume (not (List.is_empty l));
        let h, l' = pop l in
        List.equal Int.equal l (h :: l')))

let test_drop =
  QCheck.(
    Test.make ~count:1000 (list int) (fun l ->
        assume (not (List.is_empty l));
        let l' = drop l in
        List.equal Int.equal l' (List.tl l)))

let test_peek =
  QCheck.(
    Test.make ~count:1000 (list int) (fun l ->
        assume (not (List.is_empty l));
        let l' = peek l in
        Int.equal l' (List.hd l)))

let test_swap =
  QCheck.(
    Test.make ~count:1000 (list int) (fun l ->
        assume (List.length l > 2);
        let l' = swap l in
        List.equal Int.equal l'
          (List.hd (List.tl l) :: List.hd l :: List.tl (List.tl l))))

let () =
  Alcotest.run "functional stack"
    [
      ( "operations",
        List.map QCheck_alcotest.to_alcotest
          [ test_push; test_pop; test_drop; test_peek; test_swap ] );
    ]
