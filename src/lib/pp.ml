open Printf
open Adt

let string_of_typ = Michelson.Pp.string_of_typ

let string_of_data = Michelson.Pp.string_of_data

let rec string_of_operation =
  let open Printf in
  function
  | O_create_account (e_1, e_2, e_3, e_4) ->
      sprintf "CREATE_ACCOUNT %s %s %s %s" e_1 e_2 e_3 e_4
  | O_create_contract (_, e_1, e_2, e_3) ->
      sprintf "CREATE_CONTRACT {...} %s %s %s" e_1 e_2 e_3
  | O_set_delegate e -> sprintf "SET_DELEGATE %s" e
  | O_transfer_tokens (e_1, e_2, e_3) ->
      sprintf "TRANSFER_TOKENS %s %s %s" e_1 e_2 e_3

and string_of_expr = function
  | E_push (d, t) -> sprintf "PUSH %s %s" (string_of_typ t) (string_of_data d)
  | E_car e -> sprintf "CAR %s" e
  | E_cdr e -> sprintf "CDR %s" e
  | E_abs e -> sprintf "ABS %s" e
  | E_neg e -> sprintf "NEG %s" e
  | E_not e -> sprintf "NOT %s" e
  | E_eq e -> sprintf "EQ %s" e
  | E_neq e -> sprintf "NEQ %s" e
  | E_lt e -> sprintf "LT %s" e
  | E_gt e -> sprintf "GT %s" e
  | E_leq e -> sprintf "LEQ %s" e
  | E_geq e -> sprintf "GEQ %s" e
  | E_left (e, t) -> sprintf "LEFT %s %s" (string_of_typ t) e
  | E_right (e, t) -> sprintf "RIGHT %s %s" (string_of_typ t) e
  | E_some e -> sprintf "SOME %s" e
  | E_cast e -> sprintf "CAST %s" e
  | E_pack e -> sprintf "PACK %s" e
  | E_contract_of_address e -> sprintf "CONTRACT %s" e
  | E_implicit_account e -> sprintf "IMPLICIT_ACCOUNT %s" e
  | E_blake2b e -> sprintf "BLAKE2B %s" e
  | E_sha256 e -> sprintf "SHA256 %s" e
  | E_sha512 e -> sprintf "SHA512 %s" e
  | E_hash_key e -> sprintf "HASH_KEY %s" e
  | E_unit -> sprintf "UNIT"
  | E_none t -> sprintf "NONE %s" (string_of_typ t)
  | E_add (e_1, e_2) -> sprintf "ADD %s %s" e_1 e_2
  | E_sub (e_1, e_2) -> sprintf "SUB %s %s" e_1 e_2
  | E_mul (e_1, e_2) -> sprintf "MUL %s %s" e_1 e_2
  | E_div (e_1, e_2) -> sprintf "EDIV %s %s" e_1 e_2
  | E_mod (e_1, e_2) -> sprintf "mod %s %s" e_1 e_2
  | E_shiftL (e_1, e_2) -> sprintf "LSL %s %s" e_1 e_2
  | E_shiftR (e_1, e_2) -> sprintf "LSR %s %s" e_1 e_2
  | E_and (e_1, e_2) -> sprintf "AND %s %s" e_1 e_2
  | E_or (e_1, e_2) -> sprintf "OR %s %s" e_1 e_2
  | E_xor (e_1, e_2) -> sprintf "XOR %s %s" e_1 e_2
  | E_compare (e_1, e_2) -> sprintf "COMPARE %s %s" e_1 e_2
  | E_cons (e_1, e_2) -> sprintf "CONS %s %s" e_1 e_2
  | E_pair (e_1, e_2) -> sprintf "PAIR %s %s" e_1 e_2
  | E_mem (e_1, e_2) -> sprintf "MEM %s %s" e_1 e_2
  | E_get (e_1, e_2) -> sprintf "GET %s %s" e_1 e_2
  | E_concat (e_1, e_2) -> sprintf "CONCAT %s %s" e_1 e_2
  | E_concat_list e -> sprintf "CONCAT %s" e
  | E_update (e_1, e_2, e_3) -> sprintf "UPDATE %s %s %s" e_1 e_2 e_3
  | E_slice (e_1, e_2, e_3) -> sprintf "SLICE %s %s %s" e_1 e_2 e_3
  | E_check_signature (e_1, e_2, e_3) ->
      sprintf "CHECK_SIGNATURE %s %s %s" e_1 e_2 e_3
  | E_unpack (t, e) -> sprintf "UNPACK %s %s" (string_of_typ t) e
  | E_self -> sprintf "SELF"
  | E_now -> sprintf "NOW"
  | E_amount -> sprintf "AMOUNT"
  | E_balance -> sprintf "BALANCE"
  | E_steps_to_quota -> sprintf "STEPS_TO_QUOTA"
  | E_source -> sprintf "SOURCE"
  | E_sender -> sprintf "SENDER"
  | E_address_of_contract e -> sprintf "ADDRESS %s" e
  | E_size e -> sprintf "SIZE %s" e
  | E_unlift_option e -> sprintf "unlift_option %s" e
  | E_unlift_or e -> sprintf "unlift_or %s" e
  | E_hd e -> sprintf "hd %s" e
  | E_tl e -> sprintf "tl %s" e
  | E_isnat e -> sprintf "ISNAT %s" e
  | E_int_of_nat e -> sprintf "INT %s" e
  | E_chain_id -> sprintf "CHAIN_ID"
  | E_lambda (t_1, t_2, _) ->
      sprintf "LAMBDA %s %s {...}" (string_of_typ t_1) (string_of_typ t_2)
  | E_exec (e_1, e_2) -> sprintf "EXEC %s %s" e_1 e_2
  | E_create_contract_address _ -> (* TODO: *) sprintf ""
  | E_create_account_address _ -> (* TODO: *) sprintf ""
  | E_operation o -> string_of_operation o
  | E_dup s -> sprintf "DUP %s" s
  | E_nil t -> sprintf "NIL %s" (string_of_typ t)
  | E_empty_set t -> sprintf "EMPTY_SET %s" (string_of_typ t)
  | E_empty_map (t_k, t_v) ->
      sprintf "EMPTY_MAP %s %s" (string_of_typ t_k) (string_of_typ t_v)
  | E_empty_big_map (t_k, t_v) ->
      sprintf "EMPTY_BIG_MAP %s %s" (string_of_typ t_k) (string_of_typ t_v)
  | E_append (v_1, v_2) -> sprintf "append(%s, %s)" v_1 v_2
  | E_phi (v_1, v_2) -> sprintf "phi(%s, %s)" v_1 v_2
  | E_special_nil_list -> sprintf "[]"

let string_of_list f l =
  let open Printf in
  let values =
    let rec aux acc = function
      | [] -> ""
      | h :: tl ->
          aux
            ( if String.length acc > 0 then sprintf "%s; %s" acc (f h)
            else sprintf "%s" (f h) )
            tl
    in
    aux "" l
  in
  "[ " ^ values ^ " ]"

let rec print_stmt i ppf n =
  match n.stm with
  | S_seq ({ id = _; stm = S_skip }, s) | S_seq (s, { id = _; stm = S_skip }) ->
      print_stmt i ppf s
  | S_seq (s_1, s_2) ->
      fprintf ppf "%a;\n%a" (print_stmt i) s_1 (print_stmt i) s_2
  | S_assign (s, e) -> fprintf ppf "%s := %s" s (string_of_expr e)
  | S_skip -> fprintf ppf ""
  | S_drop l -> fprintf ppf "DROP %s" (string_of_list (fun x -> x) l)
  | S_swap -> fprintf ppf "SWAP"
  | S_dig -> fprintf ppf "DIG"
  | S_dug -> fprintf ppf "DUG"
  | S_if (s, s_1, s_2) ->
      let i' = i + 1 in
      fprintf ppf "IF %s\n{\n%a\n}\n{\n%a\n}" s (print_stmt i') s_1
        (print_stmt i') s_2
  | S_if_none (s, s_1, s_2) ->
      let i' = i + 1 in
      fprintf ppf "IF_NONE %s\n{\n%a\n}\n{\n%a\n}" s (print_stmt i') s_1
        (print_stmt i') s_2
  | S_if_left (s, s_1, s_2) ->
      let i' = i + 1 in
      fprintf ppf "IF_LEFT %s\n{\n%a\n}\n{\n%a\n}" s (print_stmt i') s_1
        (print_stmt i') s_2
  | S_if_cons (s, s_1, s_2) ->
      let i' = i + 1 in
      fprintf ppf "IF_CONS %s\n{\n%a\n}\n{\n%a\n}" s (print_stmt i') s_1
        (print_stmt i') s_2
  | S_loop (s, (v_1, v_2), b) ->
      let i' = i + 1 in
      fprintf ppf "LOOP %s := phi(%s, %s)\n{\n%a\n}" s v_1 v_2 (print_stmt i') b
  | S_loop_left (s, (v_1, v_2), b) ->
      let i' = i + 1 in
      fprintf ppf "LOOP_LEFT %s := phi(%s, %s)\n{\n%a\n}" s v_1 v_2
        (print_stmt i') b
  | S_map ((c, (c_1, c_2)), (r, (r_1, r_2)), b) ->
      let i' = i + 1 in
      fprintf ppf "MAP %s := phi(%s, %s) with %s := phi(%s, %s)\n{\n%a\n}" c c_1
        c_2 r r_1 r_2 (print_stmt i') b
  | S_iter (s, (v_1, v_2), b) ->
      let i' = i + 1 in
      fprintf ppf "ITER %s := phi(%s, %s)\n{\n%a\n}" s v_1 v_2 (print_stmt i') b
  | S_failwith s -> fprintf ppf "FAILWITH %s" s

let func ppf (b, v) = fprintf ppf "@[<1>%s => {\n%a\n}" v (print_stmt 2) b

let program ppf (_, _, b) = func ppf b
