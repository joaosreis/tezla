open Core_kernel

type 'a t = 'a list

exception Unsufficient_length

let empty = []

let push = List.cons

let%test "push empty" = List.equal Int.equal (push 1 []) [ 1 ]

let%test "push" = List.equal Int.equal (push 2 [ 1 ]) [ 2; 1 ]

let pop = function [] -> raise Unsufficient_length | hd :: tl -> (hd, tl)

let%test "pop empty" =
  try
    let _ = pop [] in
    false
  with Unsufficient_length -> true

let%test "pop" =
  let x, y = pop [ 1; 2 ] in
  x = 1 && List.equal Int.equal y [ 2 ]

let drop = function [] -> raise Unsufficient_length | _ :: tl -> tl

let%test "drop empty" =
  try
    let _ = drop [] in
    false
  with Unsufficient_length -> true

let%test "drop" = List.equal Int.equal (drop [ 1; 2 ]) [ 2 ]

let peek = function [] -> raise Unsufficient_length | hd :: _ -> hd

let%test "peek empty" =
  try
    let _ = peek [] in
    false
  with Unsufficient_length -> true

let%test "peek" = peek [ 1; 2 ] = 1

let swap = function
  | hd_1 :: hd_2 :: tl -> hd_2 :: hd_1 :: tl
  | _ :: _ -> raise Unsufficient_length
  | [] -> raise Unsufficient_length

let%test "swap empty" =
  try
    let _ = swap [] in
    false
  with Unsufficient_length -> true

let%test "swap size 1" =
  try
    let _ = swap [ 1 ] in
    false
  with Unsufficient_length -> true

let%test "swap" = List.equal Int.equal (swap [ 1; 2 ]) [ 2; 1 ]

let dig s n =
  if Bigint.(n = zero) then s
  else if Bigint.(n = one) then swap s
  else if Bigint.(of_int (List.length s) <= n) then raise Unsufficient_length
  else
    let rec aux (l_h, l_t) =
      if Bigint.(of_int (List.length l_h) = n) then
        (List.hd_exn l_t :: l_h) @ List.tl_exn l_t
      else aux (l_h @ [ List.hd_exn l_t ], List.tl_exn l_t)
    in
    aux ([], s)

let%test "dig invalid length" =
  try
    let _ = dig [ 1; 2 ] Bigint.(of_int 2) in
    false
  with Unsufficient_length -> true

let%test "dig 0" =
  List.equal Int.equal (dig [ 1; 2; 3 ] Bigint.zero) [ 1; 2; 3 ]

let%test "dig 1" = List.equal Int.equal (dig [ 1; 2; 3 ] Bigint.one) [ 2; 1; 3 ]

let%test "dig 2" =
  List.equal Int.equal (dig [ 1; 2; 3 ] Bigint.(of_int 2)) [ 3; 1; 2 ]

let dug s n =
  if Bigint.(n = of_int 0) then s
  else if Bigint.(n = of_int 1) then swap s
  else if Bigint.(of_int (List.length s) <= n) then raise Unsufficient_length
  else
    let h = List.hd_exn s in
    let rec aux (l_h, l_t) =
      if Bigint.(of_int (List.length l_h) = n) then l_h @ (h :: l_t)
      else aux (l_h @ [ List.hd_exn l_t ], List.tl_exn l_t)
    in
    aux ([], List.tl_exn s)

let%test "dug invalid length" =
  try
    let _ = dug [ 1; 2 ] Bigint.(of_int 2) in
    false
  with Unsufficient_length -> true

let%test "dug 0" = List.equal Int.equal (dug [ 1 ] Bigint.zero) [ 1 ]

let%test "dug 1" = List.equal Int.equal (dug [ 1; 2; 3 ] Bigint.one) [ 2; 1; 3 ]

let%test "dug 2" =
  List.equal Int.equal (dug [ 1; 2; 3 ] Bigint.(of_int 2)) [ 2; 3; 1 ]

let map f = List.map ~f

let%test "map" = List.equal Int.equal (map (( + ) 1) [ 1; 2; 3 ]) [ 2; 3; 4 ]

let map2 f = List.map2_exn ~f

let%test "map2" =
  List.equal Int.equal (map2 ( + ) [ 1; 2; 3 ] [ 2; 3; 4 ]) [ 3; 5; 7 ]

let find f = List.find ~f
