type 'a env = 'a list

let empty_env = []

let var_counter = ref (-1)

let next_var () =
  let () = var_counter := !var_counter + 1 in
  Printf.sprintf "v%d" !var_counter

(* let push ?name x =
  let v = match name with None -> next_var () | Some s -> s in
  List.cons (x, v) *)
let push = List.cons

let pop env = (List.hd env, List.tl env)

let drop = List.tl

let peek = List.hd

let swap = function h :: h' :: t -> h' :: h :: t | l -> l

(* let rename v = function (x, _) :: t -> (x, v) :: t | l -> l *)

let join string_to_expr env_1 =
  List.map2 (fun _ _ -> string_to_expr (next_var ())) env_1
