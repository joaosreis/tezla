open Containers

type 'a t = 'a list

exception Unsufficient_length

let empty = []
let push = List.cons
let pop = function [] -> raise Unsufficient_length | hd :: tl -> (hd, tl)
let drop = function [] -> raise Unsufficient_length | _ :: tl -> tl
let peek = function [] -> raise Unsufficient_length | hd :: _ -> hd

let swap = function
  | hd_1 :: hd_2 :: tl -> hd_2 :: hd_1 :: tl
  | _ :: _ -> raise Unsufficient_length
  | [] -> raise Unsufficient_length

let dig s n =
  if Z.(equal n zero) then s
  else if Z.(equal n one) then swap s
  else if Z.(leq ~$(List.length s) n) then raise Unsufficient_length
  else
    let rec aux (l_h, l_t) =
      if Z.(equal ~$(List.length l_h) n) then (List.hd l_t :: l_h) @ List.tl l_t
      else aux (l_h @ [ List.hd l_t ], List.tl l_t)
    in
    aux ([], s)

let%test "dig invalid length" =
  try
    let _ = dig [ 1; 2 ] Z.(of_int 2) in
    false
  with Unsufficient_length -> true

let%test "dig 0" = List.equal Int.equal (dig [ 1; 2; 3 ] Z.zero) [ 1; 2; 3 ]
let%test "dig 1" = List.equal Int.equal (dig [ 1; 2; 3 ] Z.one) [ 2; 1; 3 ]

let%test "dig 2" =
  List.equal Int.equal (dig [ 1; 2; 3 ] Z.(of_int 2)) [ 3; 1; 2 ]

let dug s n =
  if Z.(equal n zero) then s
  else if Z.(equal n one) then swap s
  else if Z.(leq ~$(List.length s) n) then raise Unsufficient_length
  else
    let h = List.hd s in
    let rec aux (l_h, l_t) =
      if Z.(equal ~$(List.length l_h) n) then l_h @ (h :: l_t)
      else aux (l_h @ [ List.hd l_t ], List.tl l_t)
    in
    aux ([], List.tl s)

let%test "dug invalid length" =
  try
    let _ = dug [ 1; 2 ] Z.(of_int 2) in
    false
  with Unsufficient_length -> true

let%test "dug 0" = List.equal Int.equal (dug [ 1 ] Z.zero) [ 1 ]
let%test "dug 1" = List.equal Int.equal (dug [ 1; 2; 3 ] Z.one) [ 2; 1; 3 ]

let%test "dug 2" =
  List.equal Int.equal (dug [ 1; 2; 3 ] Z.(of_int 2)) [ 2; 3; 1 ]

let dup s n =
  if Z.(lt ~$(List.length s) n) then raise Unsufficient_length
  else
    let rec aux i s =
      if Z.(equal i one) then peek s else aux Z.(i - one) (drop s)
    in
    let x = aux n s in
    push x s

let%test "dup 1" = List.equal Int.equal (dup [ 1; 2; 3 ] Z.(one)) [ 1; 1; 2; 3 ]
let%test "dup 2" = List.equal Int.equal (dup [ 1; 2; 3 ] Z.(one)) [ 2; 1; 2; 3 ]
let%test "dup 3" = List.equal Int.equal (dup [ 1; 2; 3 ] Z.(one)) [ 3; 1; 2; 3 ]
let map f = List.map f
let%test "map" = List.equal Int.equal (map (( + ) 1) [ 1; 2; 3 ]) [ 2; 3; 4 ]
let map2 f = List.map2 f

let%test "map2" =
  List.equal Int.equal (map2 ( + ) [ 1; 2; 3 ] [ 2; 3; 4 ]) [ 3; 5; 7 ]

let find = List.find_opt
