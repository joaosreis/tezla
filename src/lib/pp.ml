open Adt
open Format

let pp_print_list f ppf =
  fprintf ppf "{ %a }" (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ";") f)

let rec pp_typ ppf = function
  | T_int -> fprintf ppf "int"
  | T_nat -> fprintf ppf "nat"
  | T_string -> fprintf ppf "string"
  | T_bytes -> fprintf ppf "bytes"
  | T_mutez -> fprintf ppf "mutez"
  | T_bool -> fprintf ppf "bool"
  | T_key_hash -> fprintf ppf "key_hash"
  | T_timestamp -> fprintf ppf "timestamp"
  | T_address -> fprintf ppf "address"
  | T_key -> fprintf ppf "key"
  | T_unit -> fprintf ppf "unit"
  | T_signature -> fprintf ppf "signature"
  | T_option t -> fprintf ppf "(option %a)" pp_typ t
  | T_list t -> fprintf ppf "(list %a)" pp_typ t
  | T_set t -> fprintf ppf "(set %a)" pp_typ t
  | T_operation -> fprintf ppf "operation"
  | T_contract t -> fprintf ppf "(contract %a)" pp_typ t
  | T_pair (t_1, t_2) -> fprintf ppf "(pair %a %a)" pp_typ t_1 pp_typ t_2
  | T_or (t_1, t_2) -> fprintf ppf "(or %a %a)" pp_typ t_1 pp_typ t_2
  | T_lambda (t_1, t_2) -> fprintf ppf "(lambda %a %a)" pp_typ t_1 pp_typ t_2
  | T_map (t_1, t_2) -> fprintf ppf "(map %a %a)" pp_typ t_1 pp_typ t_2
  | T_big_map (t_1, t_2) -> fprintf ppf "(big_map %a %a)" pp_typ t_1 pp_typ t_2
  | T_chain_id -> fprintf ppf "chain_id"

let pp_var ppf v = fprintf ppf "%s" v.var_name

let pp_operation ppf = function
  | O_create_account (v_1, v_2, v_3, v_4) ->
      fprintf ppf "CREATE_ACCOUNT %s %s %s %s" v_1.var_name v_2.var_name
        v_3.var_name v_4.var_name
  | O_create_contract (_, v_1, v_2, v_3) ->
      fprintf ppf "CREATE_CONTRACT {...} %s %s %s" v_1.var_name v_2.var_name
        v_3.var_name
  | O_set_delegate e -> fprintf ppf "SET_DELEGATE %a" pp_var e
  | O_transfer_tokens (v_1, v_2, v_3) ->
      fprintf ppf "TRANSFER_TOKENS %s %s %s" v_1.var_name v_2.var_name
        v_3.var_name

let rec pp_data ppf = function
  | D_int d -> Z.pp_print ppf d
  | D_string s -> fprintf ppf "\"%s\"" s
  | D_bytes b -> fprintf ppf "%s" (Bytes.to_string b)
  | D_elt (d_1, d_2) -> fprintf ppf "Elt %a %a" pp_data d_1 pp_data d_2
  | D_left d -> fprintf ppf "Left %a" pp_data d
  | D_right d -> fprintf ppf "Right %a" pp_data d
  | D_some d -> fprintf ppf "Some %a" pp_data d
  | D_none -> fprintf ppf "None"
  | D_unit -> fprintf ppf "Unit"
  | D_bool b -> fprintf ppf (match b with true -> "True" | false -> "False")
  | D_pair (d_1, d_2) -> fprintf ppf "(Pair %a %a)" pp_data d_1 pp_data d_2
  | D_list d -> pp_print_list pp_data ppf d
  | D_instruction s -> pp_stmt ppf s

and pp_expr ppf = function
  | E_push (d, t) -> fprintf ppf "PUSH %a %a" pp_typ t pp_data d
  | E_car e -> fprintf ppf "CAR %a" pp_var e
  | E_cdr e -> fprintf ppf "CDR %a" pp_var e
  | E_abs e -> fprintf ppf "ABS %a" pp_var e
  | E_neg e -> fprintf ppf "NEG %a" pp_var e
  | E_not e -> fprintf ppf "NOT %a" pp_var e
  | E_eq e -> fprintf ppf "EQ %a" pp_var e
  | E_neq e -> fprintf ppf "NEQ %a" pp_var e
  | E_lt e -> fprintf ppf "LT %a" pp_var e
  | E_gt e -> fprintf ppf "GT %a" pp_var e
  | E_leq e -> fprintf ppf "LEQ %a" pp_var e
  | E_geq e -> fprintf ppf "GEQ %a" pp_var e
  | E_left (e, t) -> fprintf ppf "LEFT %a %a" pp_typ t pp_var e
  | E_right (e, t) -> fprintf ppf "RIGHT %a %a" pp_typ t pp_var e
  | E_some e -> fprintf ppf "SOME %a" pp_var e
  | E_pack e -> fprintf ppf "PACK %a" pp_var e
  | E_implicit_account e -> fprintf ppf "IMPLICIT_ACCOUNT %a" pp_var e
  | E_blake2b e -> fprintf ppf "BLAKE2B %a" pp_var e
  | E_sha256 e -> fprintf ppf "SHA256 %a" pp_var e
  | E_sha512 e -> fprintf ppf "SHA512 %a" pp_var e
  | E_hash_key e -> fprintf ppf "HASH_KEY %a" pp_var e
  | E_unit -> fprintf ppf "UNIT"
  | E_none t -> fprintf ppf "NONE %a" pp_typ t
  | E_add (v_1, v_2) -> fprintf ppf "ADD %a %a" pp_var v_1 pp_var v_2
  | E_sub (v_1, v_2) -> fprintf ppf "SUB %a %a" pp_var v_1 pp_var v_2
  | E_mul (v_1, v_2) -> fprintf ppf "MUL %a %a" pp_var v_1 pp_var v_2
  | E_div (v_1, v_2) -> fprintf ppf "EDIV %a %a" pp_var v_1 pp_var v_2
  | E_shiftL (v_1, v_2) -> fprintf ppf "LSL %a %a" pp_var v_1 pp_var v_2
  | E_shiftR (v_1, v_2) -> fprintf ppf "LSR %a %a" pp_var v_1 pp_var v_2
  | E_and (v_1, v_2) -> fprintf ppf "AND %a %a" pp_var v_1 pp_var v_2
  | E_or (v_1, v_2) -> fprintf ppf "OR %a %a" pp_var v_1 pp_var v_2
  | E_xor (v_1, v_2) -> fprintf ppf "XOR %a %a" pp_var v_1 pp_var v_2
  | E_compare (v_1, v_2) -> fprintf ppf "COMPARE %a %a" pp_var v_1 pp_var v_2
  | E_cons (v_1, v_2) -> fprintf ppf "CONS %a %a" pp_var v_1 pp_var v_2
  | E_pair (v_1, v_2) -> fprintf ppf "PAIR %a %a" pp_var v_1 pp_var v_2
  | E_mem (v_1, v_2) -> fprintf ppf "MEM %a %a" pp_var v_1 pp_var v_2
  | E_get (v_1, v_2) -> fprintf ppf "GET %a %a" pp_var v_1 pp_var v_2
  | E_concat (v_1, v_2) -> fprintf ppf "CONCAT %a %a" pp_var v_1 pp_var v_2
  | E_concat_list e -> fprintf ppf "CONCAT %a" pp_var e
  | E_update (v_1, v_2, v_3) ->
      fprintf ppf "UPDATE %a %a %a" pp_var v_1 pp_var v_2 pp_var v_3
  | E_slice (v_1, v_2, v_3) ->
      fprintf ppf "SLICE %a %a %a" pp_var v_1 pp_var v_2 pp_var v_3
  | E_check_signature (v_1, v_2, v_3) ->
      fprintf ppf "CHECK_SIGNATURE %a %a %a" pp_var v_1 pp_var v_2 pp_var v_3
  | E_unpack (t, v) -> fprintf ppf "UNPACK %a %a" pp_typ t pp_var v
  | E_self -> fprintf ppf "SELF"
  | E_now -> fprintf ppf "NOW"
  | E_amount -> fprintf ppf "AMOUNT"
  | E_balance -> fprintf ppf "BALANCE"
  | E_source -> fprintf ppf "SOURCE"
  | E_sender -> fprintf ppf "SENDER"
  | E_address_of_contract e -> fprintf ppf "ADDRESS %a" pp_var e
  | E_size e -> fprintf ppf "SIZE %a" pp_var e
  | E_unlift_option e -> fprintf ppf "unlift_option %a" pp_var e
  | E_unlift_or_left e -> fprintf ppf "unlift_or_left %a" pp_var e
  | E_unlift_or_right e -> fprintf ppf "unlift_or_right %a" pp_var e
  | E_hd e -> fprintf ppf "hd %a" pp_var e
  | E_tl e -> fprintf ppf "tl %a" pp_var e
  | E_isnat e -> fprintf ppf "ISNAT %a" pp_var e
  | E_int_of_nat e -> fprintf ppf "INT %a" pp_var e
  | E_chain_id -> fprintf ppf "CHAIN_ID"
  | E_lambda (t_1, t_2, i) ->
      fprintf ppf "LAMBDA %a %a %a" pp_typ t_1 pp_typ t_2 pp_stmt i
  | E_exec (v_1, v_2) -> fprintf ppf "EXEC %a %a" pp_var v_1 pp_var v_2
  | E_contract_of_address (t, v) ->
      fprintf ppf "CONTRACT %a %a" pp_typ t pp_var v
  | E_create_contract_address (p, v_1, v_2, v_3) ->
      fprintf ppf "CREATE_CONTRACT %a %a %a %a" Michelson.Pp.pp_program p pp_var
        v_1 pp_var v_2 pp_var v_3
  | E_operation o -> pp_operation ppf o
  | E_dup v -> fprintf ppf "DUP %a" pp_var v
  | E_nil t -> fprintf ppf "NIL %a" pp_typ t
  | E_empty_set t -> fprintf ppf "EMPTY_SET %a" pp_typ t
  | E_empty_map (t_k, t_v) ->
      fprintf ppf "EMPTY_MAP %a %a" pp_typ t_k pp_typ t_v
  | E_empty_big_map (t_k, t_v) ->
      fprintf ppf "EMPTY_BIG_MAP %a %a" pp_typ t_k pp_typ t_v
  | E_apply (v_1, v_2) -> fprintf ppf "APPLY %a %a" pp_var v_1 pp_var v_2
  | E_append (v_1, v_2) -> fprintf ppf "append(%a, %a)" pp_var v_1 pp_var v_2
  | E_phi (v_1, v_2) -> fprintf ppf "phi(%a, %a)" pp_var v_1 pp_var v_2
  | E_special_empty_list _ -> fprintf ppf "{  }"
  | E_special_empty_map _ -> fprintf ppf "{  }"

and pp_stmt ppf s =
  match s.stm with
  | S_seq ({ id = _; stm = S_skip }, s) | S_seq (s, { id = _; stm = S_skip }) ->
      pp_stmt ppf s
  | S_seq (s_1, s_2) -> fprintf ppf "%a;\n%a" pp_stmt s_1 pp_stmt s_2
  | S_assign (v, e) -> fprintf ppf "%a := %a" pp_var v pp_expr e
  | S_skip -> fprintf ppf ""
  | S_drop l ->
      fprintf ppf "DROP %a"
        (Format.pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ";") pp_var)
        l
  | S_swap -> fprintf ppf "SWAP"
  | S_dig -> fprintf ppf "DIG"
  | S_dug -> fprintf ppf "DUG"
  | S_if (v, s_1, s_2) ->
      fprintf ppf "IF %a\n{\n%a\n}\n{\n%a\n}" pp_var v pp_stmt s_1 pp_stmt s_2
  | S_if_none (v, s_1, s_2) ->
      fprintf ppf "IF_NONE %s\n{\n%a\n}\n{\n%a\n}" v.var_name pp_stmt s_1
        pp_stmt s_2
  | S_if_left (v, s_1, s_2) ->
      fprintf ppf "IF_LEFT %s\n{\n%a\n}\n{\n%a\n}" v.var_name pp_stmt s_1
        pp_stmt s_2
  | S_if_cons (v, s_1, s_2) ->
      fprintf ppf "IF_CONS %s\n{\n%a\n}\n{\n%a\n}" v.var_name pp_stmt s_1
        pp_stmt s_2
  | S_loop (v, (v_1, v_2), b) ->
      fprintf ppf "LOOP %s := phi(%s, %s)\n{\n%a\n}" v.var_name v_1.var_name
        v_2.var_name pp_stmt b
  | S_loop_left (v, (v_1, v_2), b) ->
      fprintf ppf "LOOP_LEFT %s := phi(%s, %s)\n{\n%a\n}" v.var_name
        v_1.var_name v_2.var_name pp_stmt b
  | S_map ((c, (c_1, c_2)), (r, (r_1, r_2)), b) ->
      fprintf ppf "MAP %s := phi(%s, %s) with %s := phi(%s, %s)\n{\n%a\n}"
        c.var_name c_1.var_name c_2.var_name r.var_name r_1.var_name
        r_2.var_name pp_stmt b
  | S_iter (v, (v_1, v_2), b) ->
      fprintf ppf "ITER %s := phi(%s, %s)\n{\n%a\n}" v.var_name v_1.var_name
        v_2.var_name pp_stmt b
  | S_failwith v -> fprintf ppf "FAILWITH %s" v.var_name
  | S_return v -> fprintf ppf "return %s" v.var_name

let pp_func ppf (b, v) = fprintf ppf "@[<1>%s => {\n%a\n}" v pp_stmt b

let pp_program ppf (_, _, b) = pp_func ppf b
