type 'a t = 'a list

exception Unsufficient_length

let empty = []

let push = List.cons

let%test "push empty" = push 1 [] = [ 1 ]

let%test "push" = push 2 [ 1 ] = [ 2; 1 ]

let pop = function [] -> raise Unsufficient_length | hd :: tl -> (hd, tl)

let%test "pop empty" =
  try
    let _ = pop [] in
    false
  with Unsufficient_length -> true

let%test "pop" = pop [ 1; 2 ] = (1, [ 2 ])

let drop = function [] -> raise Unsufficient_length | _ :: tl -> tl

let%test "drop empty" =
  try
    let _ = drop [] in
    false
  with Unsufficient_length -> true

let%test "drop" = drop [ 1; 2 ] = [ 2 ]

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

let%test "swap" = swap [ 1; 2 ] = [ 2; 1 ]

let dig s n =
  if Z.(n = ~$0) then s
  else if Z.(n = ~$1) then swap s
  else if Z.(~$(List.length s)) <= n then raise Unsufficient_length
  else
    let rec aux (l_h, l_t) =
      if Z.(~$(List.length l_h) = n) then (List.hd l_t :: l_h) @ List.tl l_t
      else aux (l_h @ [ List.hd l_t ], List.tl l_t)
    in
    aux ([], s)

let%test "dig invalid length" =
  try
    let _ = dig [ 1; 2 ] Z.(~$2) in
    false
  with Unsufficient_length -> true

let%test "dig 0" = dig [ 1; 2; 3 ] Z.zero = [ 1; 2; 3 ]

let%test "dig 1" = dig [ 1; 2; 3 ] Z.one = [ 2; 1; 3 ]

let%test "dig 2" = dig [ 1; 2; 3 ] Z.(~$2) = [ 3; 1; 2 ]

let dug s n =
  if Z.(n = ~$0) then s
  else if Z.(n = ~$1) then swap s
  else if Z.(~$(List.length s)) <= n then raise Unsufficient_length
  else
    let h = List.hd s in
    let rec aux (l_h, l_t) =
      if Z.(~$(List.length l_h) = n) then l_h @ (h :: l_t)
      else aux (l_h @ [ List.hd l_t ], List.tl l_t)
    in
    aux ([], List.tl s)

let%test "dug invalid length" =
  try
    let _ = dug [ 1; 2 ] Z.(~$2) in
    false
  with Unsufficient_length -> true

let%test "dug 0" = dug [ 1 ] Z.zero = [ 1 ]

let%test "dug 1" = dug [ 1; 2; 3 ] Z.one = [ 2; 1; 3 ]

let%test "dug 2" = dug [ 1; 2; 3 ] Z.(~$2) = [ 2; 3; 1 ]

let map = List.map

let%test "map" = map (( + ) 1) [ 1; 2; 3 ] = [ 2; 3; 4 ]

let map2 = List.map2

let%test "map2" = map2 ( + ) [ 1; 2; 3 ] [ 2; 3; 4 ] = [ 3; 5; 7 ]

let find = List.find
