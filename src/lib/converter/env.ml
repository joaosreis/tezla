module S = Functional_stack

type elem = Adt.var
type env = Failed | Stack of elem Functional_stack.t

let empty_env = Stack S.empty
let failed_env = Failed

let next_var var_counter =
  let () = var_counter := !var_counter + 1 in
  Printf.sprintf "v%s" (string_of_int !var_counter)

exception Stack_failed

let stack_or_failed = function Failed -> raise Stack_failed | Stack s -> s

let push x env =
  let s = stack_or_failed env in
  Stack (S.push x s)

let pop env =
  let s = stack_or_failed env in
  let x, s' = S.pop s in
  (x, Stack s')

let drop env =
  let s = stack_or_failed env in
  let _, s' = S.pop s in
  Stack s'

let peek env =
  let s = stack_or_failed env in
  S.peek s

let swap env =
  let s = stack_or_failed env in
  Stack (S.swap s)

let dig env n =
  let s = stack_or_failed env in
  Stack (S.dig s n)

let dug env n =
  let s = stack_or_failed env in
  Stack (S.dug s n)

let dip env n =
  if Bigint.(n = zero) then ([], env)
  else
    let rec aux (acc, env') n' =
      if Bigint.(n' = zero) then (acc, env')
      else
        let x, env'' = pop env' in
        let acc' = x :: acc in
        aux (acc', env'') Bigint.(n' - one)
    in
    aux ([], env) n

let dup env n =
  let s = stack_or_failed env in
  Stack (S.dup s n)

(* let rename v = function (x, _) :: t -> (x, v) :: t | l -> l *)

let length = function Failed -> raise Stack_failed | Stack s -> List.length s
