open Adt

exception Type_error of string

let type_expr e =
  match e with
  | E_abs _ | E_shiftL (_, _) | E_shiftR (_, _) -> T_nat
  | E_unit -> T_unit
  | E_now -> T_timestamp
  | E_self | E_amount | E_balance -> T_mutez
  | E_source | E_sender -> T_address
  | E_chain_id -> T_chain_id
  | E_special_empty_list t -> T_list t
  | E_special_empty_map (t_1, t_2) -> T_map (t_1, t_2)
  | E_push (_, t) -> t
  | E_car v -> (
      match v.var_type with
      | T_pair (t, _) -> t
      | _ ->
          let open Format in
          let () =
            fprintf err_formatter "type %a is not expected" Pp.pp_typ v.var_type
          in
          raise (Type_error (flush_str_formatter ())) )
  | E_cdr v -> (
      match v.var_type with
      | T_pair (_, t) -> t
      | _ ->
          let open Format in
          let () =
            fprintf err_formatter "type %a is not expected" Pp.pp_typ v.var_type
          in
          raise (Type_error (flush_str_formatter ())) )
  | E_neg _ | E_compare (_, _) -> T_int
  | E_and (v_1, v_2) -> (
      match (v_1.var_type, v_2.var_type) with
      | T_bool, T_bool -> T_bool
      | T_nat, T_nat | T_int, T_nat -> T_nat
      | _ ->
          let open Format in
          let () =
            fprintf err_formatter "types %a and %a were not expected" Pp.pp_typ
              v_1.var_type Pp.pp_typ v_2.var_type
          in
          raise (Type_error (flush_str_formatter ())) )
  | E_or (v_1, v_2) -> (
      match (v_1.var_type, v_2.var_type) with
      | T_bool, T_bool -> T_bool
      | T_nat, T_nat -> T_nat
      | _ ->
          let open Format in
          let () =
            fprintf err_formatter "types %a and %a were not expected" Pp.pp_typ
              v_1.var_type Pp.pp_typ v_2.var_type
          in
          raise (Type_error (flush_str_formatter ())) )
  | E_xor (v_1, v_2) -> (
      match (v_1.var_type, v_2.var_type) with
      | T_bool, T_bool -> T_bool
      | T_nat, T_nat -> T_nat
      | _ ->
          let open Format in
          let () =
            fprintf err_formatter "types %a and %a were not expected" Pp.pp_typ
              v_1.var_type Pp.pp_typ v_2.var_type
          in
          raise (Type_error (flush_str_formatter ())) )
  | E_eq _ | E_neq _ | E_lt _ | E_gt _ | E_leq _ | E_geq _ | E_not _
  | E_mem (_, _) ->
      T_bool
  | E_add (v_1, v_2) -> (
      match (v_1.var_type, v_2.var_type) with
      | T_nat, T_nat -> T_nat
      | T_nat, T_int | T_int, T_nat | T_int, T_int -> T_int
      | T_timestamp, T_int | T_int, T_timestamp -> T_timestamp
      | T_mutez, T_mutez -> T_mutez
      | _ ->
          let open Format in
          let () =
            fprintf err_formatter "types %a and %a were not expected" Pp.pp_typ
              v_1.var_type Pp.pp_typ v_2.var_type
          in
          raise (Type_error (flush_str_formatter ())) )
  | E_sub (v_1, v_2) -> (
      match (v_1.var_type, v_2.var_type) with
      | T_nat, T_nat
      | T_nat, T_int
      | T_int, T_nat
      | T_int, T_int
      | T_timestamp, T_timestamp ->
          T_int
      | T_timestamp, T_int -> T_timestamp
      | T_mutez, T_mutez -> T_mutez
      | _ ->
          let open Format in
          let () =
            fprintf err_formatter "types %a and %a were not expected" Pp.pp_typ
              v_1.var_type Pp.pp_typ v_2.var_type
          in
          raise (Type_error (flush_str_formatter ())) )
  | E_mul (v_1, v_2) -> (
      match (v_1.var_type, v_2.var_type) with
      | T_nat, T_nat -> T_nat
      | T_nat, T_int | T_int, T_nat | T_int, T_int -> T_int
      | T_mutez, T_nat | T_nat, T_mutez -> T_mutez
      | _ ->
          let open Format in
          let () =
            fprintf err_formatter "types %a and %a were not expected" Pp.pp_typ
              v_1.var_type Pp.pp_typ v_2.var_type
          in
          raise (Type_error (flush_str_formatter ())) )
  | E_div (v_1, v_2) -> (
      match (v_1.var_type, v_2.var_type) with
      | T_nat, T_nat -> T_option (T_pair (T_nat, T_nat))
      | T_nat, T_int | T_int, T_nat | T_int, T_int ->
          T_option (T_pair (T_int, T_nat))
      | T_mutez, T_nat -> T_option (T_pair (T_mutez, T_mutez))
      | T_mutez, T_mutez -> T_option (T_pair (T_nat, T_mutez))
      | _ ->
          let open Format in
          let () =
            fprintf err_formatter "types %a and %a were not expected" Pp.pp_typ
              v_1.var_type Pp.pp_typ v_2.var_type
          in
          raise (Type_error (flush_str_formatter ())) )
  | E_cons (_, v) -> v.var_type
  | E_operation _ -> T_operation
  | E_pair (v_1, v_2) -> T_pair (v_1.var_type, v_2.var_type)
  | E_left (v, t) -> T_or (v.var_type, t)
  | E_right (v, t) -> T_or (t, v.var_type)
  | E_some v -> T_option v.var_type
  | E_none t -> T_option t
  | E_get (_, v) -> (
      match v.var_type with
      | T_map (_, t) | T_big_map (_, t) -> T_option t
      | _ ->
          let open Format in
          let () =
            fprintf err_formatter "type %a was not expected" Pp.pp_typ
              v.var_type
          in
          raise (Type_error (flush_str_formatter ())) )
  | E_update (_, _, v) -> v.var_type
  | E_concat (v, _) -> v.var_type
  | E_concat_list v -> (
      match v.var_type with
      | T_list t -> t
      | _ ->
          let open Format in
          let () =
            fprintf err_formatter "type %a was not expected" Pp.pp_typ
              v.var_type
          in
          raise (Type_error (flush_str_formatter ())) )
  | E_slice (_, _, v) -> T_option v.var_type
  | E_pack _ -> T_bytes
  | E_unpack (t, _) -> T_option t
  | E_contract_of_address (t, _) -> T_option (T_contract t)
  | E_implicit_account _ -> T_contract T_unit
  | E_check_signature _ -> T_bool
  | E_blake2b _ | E_sha256 _ | E_sha512 _ -> T_bytes
  | E_hash_key _ -> T_key_hash
  | E_address_of_contract _ | E_create_contract_address _ -> T_address
  | E_unlift_option v -> (
      match v.var_type with
      | T_option t -> t
      | _ ->
          let open Format in
          let () =
            fprintf err_formatter "type %a was not expected" Pp.pp_typ
              v.var_type
          in
          raise (Type_error (flush_str_formatter ())) )
  | E_unlift_or_left v -> (
      match v.var_type with
      | T_or (t, _) -> t
      | _ ->
          let open Format in
          let () =
            fprintf err_formatter "type %a was not expected" Pp.pp_typ
              v.var_type
          in
          raise (Type_error (flush_str_formatter ())) )
  | E_unlift_or_right v -> (
      match v.var_type with
      | T_or (_, t) -> t
      | _ ->
          let open Format in
          let () =
            fprintf err_formatter "type %a was not expected" Pp.pp_typ
              v.var_type
          in
          raise (Type_error (flush_str_formatter ())) )
  | E_hd v -> (
      match v.var_type with
      | T_list t -> t
      | T_set t -> t
      | T_map (k, v) -> T_pair (k, v)
      | _ ->
          let open Format in
          let () =
            fprintf err_formatter "type %a was not expected" Pp.pp_typ
              v.var_type
          in
          raise (Type_error (flush_str_formatter ())) )
  | E_tl v -> v.var_type
  | E_size _ -> T_nat
  | E_isnat _ -> T_option T_nat
  | E_int_of_nat _ -> T_int
  | E_lambda (t_1, t_2, _) -> T_lambda (t_1, t_2)
  | E_exec (_, v) -> (
      match v.var_type with
      | T_lambda (_, t) -> t
      | _ ->
          let open Format in
          let () =
            fprintf err_formatter "type %a was not expected" Pp.pp_typ
              v.var_type
          in
          raise (Type_error (flush_str_formatter ())) )
  | E_dup v -> v.var_type
  | E_nil t -> T_list t
  | E_empty_set t -> T_set t
  | E_empty_map (k_t, v_t) -> T_map (k_t, v_t)
  | E_empty_big_map (k_t, v_t) -> T_big_map (k_t, v_t)
  | E_apply (_, l) -> (
      match l.var_type with
      | T_lambda (T_pair (_, b), c) -> T_lambda (b, c)
      | _ ->
          let open Format in
          let () =
            fprintf err_formatter "type %a was not expected" Pp.pp_typ
              l.var_type
          in
          raise (Type_error (flush_str_formatter ())) )
  | E_append (v, _) -> v.var_type
  | E_phi (v, _) -> v.var_type

let%test "E_dup" = type_expr (E_dup { var_name = ""; var_type = T_int }) = T_int

let%test "E_cdr" =
  type_expr (E_cdr { var_name = ""; var_type = T_pair (T_int, T_nat) }) = T_nat

let%test "E_cdr" =
  type_expr (E_car { var_name = ""; var_type = T_pair (T_int, T_nat) }) = T_int

let%test "E_unlift_or_left" =
  type_expr (E_unlift_or_left { var_name = ""; var_type = T_or (T_int, T_nat) })
  = T_int

let%test "E_unlift_or_right" =
  type_expr
    (E_unlift_or_right { var_name = ""; var_type = T_or (T_int, T_nat) })
  = T_nat

let%test "E_sender" = type_expr E_sender = T_address

let%test "E_compare" =
  type_expr
    (E_compare
       ( { var_name = ""; var_type = T_address },
         { var_name = ""; var_type = T_address } ))
  = T_int

let%test "E_eq" = type_expr (E_eq { var_name = ""; var_type = T_int }) = T_bool

let%test "E_push" = type_expr (E_push (D_int Z.zero, T_int)) = T_int

let%test "E_pair" =
  type_expr
    (E_pair
       ({ var_name = ""; var_type = T_int }, { var_name = ""; var_type = T_nat }))
  = T_pair (T_int, T_nat)

let%test "abs" = type_expr (E_abs { var_name = ""; var_type = T_int }) = T_nat
