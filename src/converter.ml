open Env

let string_to_expr s = Michelson_scil.E_ident s

let join_envs_if s_t env_t s_f env_f =
  let open Michelson_scil in
  let env = join string_to_expr env_t env_f in
  let decls =
    List.fold_left
      (fun acc -> function E_ident v -> S_seq (acc, S_var_decl v) | _ ->
            assert false )
      S_skip env
  in
  let assigns_t =
    List.fold_left2
      (fun acc x -> function E_ident v -> S_seq (acc, S_assign (v, x)) | _ ->
            assert false )
      S_skip env_t env
  in
  let assigns_f =
    List.fold_left2
      (fun acc x -> function E_ident v -> S_seq (acc, S_assign (v, x)) | _ ->
            assert false )
      S_skip env_f env
  in
  let s_t' = S_seq (decls, S_seq (s_t, assigns_t)) in
  let s_f' = S_seq (decls, S_seq (s_f, assigns_f)) in
  (env, s_t', s_f')

let join_envs_while s env =
  let open Michelson_scil in
  let env' = join string_to_expr env env in
  let decls =
    List.fold_left
      (fun acc -> function E_ident v -> S_seq (acc, S_var_decl v) | _ ->
            assert false )
      S_skip env
  in
  let assigns =
    List.fold_left2
      (fun acc x -> function E_ident v -> S_seq (acc, S_assign (v, x)) | _ ->
            assert false )
      S_skip env env'
  in
  let s' = S_seq (decls, S_seq (s, assigns)) in
  (env', s')

let rec data_to_expr =
  let open Michelson_ast in
  let open Michelson_scil in
  function
  | D_int i ->
      E_int i
  | D_nat i ->
      E_nat i
  | D_string s ->
      E_string s
  | D_timestamp s ->
      E_timestamp s
  | D_signature s ->
      E_signature s
  | D_key s ->
      E_key s
  | D_key_hash s ->
      E_key_hash s
  | D_mutez s ->
      E_mutez s
  | D_contract s ->
      E_contract s
  | D_unit ->
      E_unit
  | D_bool x ->
      E_bool x
  | D_pair (x, y) ->
      E_pair (data_to_expr x, data_to_expr y)
  | D_left x ->
      E_left (data_to_expr x)
  | D_right x ->
      E_right (data_to_expr x)
  | D_some x ->
      E_some (data_to_expr x)
  | D_none ->
      E_none
  | D_list x ->
      E_list (List.map data_to_expr x)
  | D_set x ->
      E_set (List.map data_to_expr x)
  | D_map x ->
      E_map (List.map (fun (x, y) -> (data_to_expr x, data_to_expr y)) x)
  | D_instruction x ->
      let s, _ = convert empty_env x in
      E_stmt s

and convert env =
  let open Michelson_ast in
  let open Michelson_scil in
  function
  | I_seq (i_1, i_2) ->
      let s_1, env_1 = convert env i_1 in
      let s_2, env_2 = convert env_1 i_2 in
      (S_seq (s_1, s_2), env_2)
  | I_drop ->
      (S_skip, drop env)
  | I_dup ->
      let x = peek env in
      (S_skip, push x env)
  | I_swap ->
      let env' = swap env in
      (S_skip, env')
  | I_push (_, x) ->
      (S_skip, push (data_to_expr x) env)
  | I_some ->
      let x, env' = pop env in
      (S_skip, push (E_some x) env')
  | I_none _ ->
      (S_skip, push E_none env)
  | I_unit ->
      (S_skip, push E_unit env)
  | I_if_none (i_t, i_f) ->
      let x, env' = pop env in
      let e = E_is_none x in
      let s_t, env_t = convert env' i_t in
      let s_f, env_f = convert (push (E_lift_option x) env') i_f in
      let env', s_t', s_f' = join_envs_if s_t env_t s_f env_f in
      (S_if (e, s_t', s_f'), env')
  | I_pair ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_pair (x_1, x_2)) env')
  | I_car ->
      let x, env' = pop env in
      (S_skip, push (E_unop (Fst, x)) env')
  | I_cdr ->
      let x, env' = pop env in
      (S_skip, push (E_unop (Snd, x)) env')
  | I_left _ ->
      let x, env' = pop env in
      (S_skip, push (E_left x) env')
  | I_right _ ->
      let x, env' = pop env in
      (S_skip, push (E_right x) env')
  | I_if_left (i_t, i_f) ->
      let x, env' = pop env in
      let c = E_is_left x in
      let e = E_lift_or x in
      let env' = push e env' in
      let s_t, env_t = convert env' i_t in
      let s_f, env_f = convert env' i_f in
      let env', s_t', s_f' = join_envs_if s_t env_t s_f env_f in
      (S_if (c, s_t', s_f'), env')
  | I_if_right (i_t, i_f) ->
      let x, env' = pop env in
      let c = E_unop (Not, E_is_left x) in
      let e = E_lift_or x in
      let env' = push e env' in
      let s_t, env_t = convert env' i_t in
      let s_f, env_f = convert env' i_f in
      let env', s_t', s_f' = join_envs_if s_t env_t s_f env_f in
      (S_if (c, s_t', s_f'), env')
  | I_nil _ ->
      (S_skip, push (E_list []) env)
  | I_cons ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_cons (x_1, x_2)) env')
  | I_if_cons (i_t, i_f) ->
      let x, env_t = pop env in
      let c = E_is_list_empty x in
      let env_f = push (E_list_hd x) (push (E_list_tl x) env_t) in
      let s_t, env_t' = convert env_t i_t in
      let s_f, env_f' = convert env_f i_f in
      let env', s_t', s_f' = join_envs_if s_t env_t' s_f env_f' in
      (S_if (c, s_t', s_f'), env')
  | I_size ->
      let x, env' = pop env in
      (S_skip, push (E_size x) env')
  | I_empty_set _ ->
      (S_skip, push (E_set []) env)
  | I_empty_map _ ->
      (S_skip, push (E_map []) env)
  | I_map i ->
      let _ = convert empty_env i in
      (S_todo, env)
  | I_iter _ ->
      (S_todo, env)
  | I_mem ->
      let elt, env' = pop env in
      let set, env' = pop env' in
      (S_skip, push (E_mem (elt, set)) env')
  | I_get ->
      let key, env' = pop env in
      let map, env' = pop env' in
      (S_skip, push (E_get (key, map)) env')
  | I_update ->
      let key, env' = pop env in
      let value, env' = pop env' in
      let map, env' = pop env' in
      (S_skip, push (E_update (key, value, map)) env')
  | I_if (i_t, i_f) ->
      let c, env' = pop env in
      let s_t, env_t' = convert env' i_t in
      let s_f, env_f' = convert env' i_f in
      let env', s_t', s_f' = join_envs_if s_t env_t' s_f env_f' in
      (S_if (c, s_t', s_f'), env')
  | I_loop i ->
      let c, env' = pop env in
      let s, env' = convert env' i in
      let env', s' = join_envs_while s env' in
      (S_while (c, s'), env')
  | I_loop_left i ->
      let x, env' = pop env in
      let c = E_is_left x in
      let e = E_lift_or x in
      let s, env' = convert (push e env') i in
      let x', env' = pop env' in
      let e' = E_lift_or x' in
      let env' = push e' env' in
      let env', s' = join_envs_while s env' in
      (S_while (c, s'), env')
  | I_lambda (_, _, i) ->
      let s, _ = convert empty_env i in
      (S_skip, push (E_stmt s) env)
  | I_exec ->
      (S_todo, env)
  | I_dip i ->
      let x, env' = pop env in
      let s, env' = convert env' i in
      (s, push x env')
  | I_failwith _ ->
      (S_todo, env)
  | I_cast ->
      (S_todo, env)
  | I_rename ->
      (S_todo, env)
  | I_concat ->
      let s, env' = pop env in
      let t, env' = pop env' in
      (S_skip, push (E_concat (s, t)) env')
  | I_slice ->
      let offset, env' = pop env in
      let length, env' = pop env' in
      let x, env' = pop env' in
      (S_skip, push (E_slice (offset, length, x)) env')
  | I_pack ->
      let x, env' = pop env in
      (S_skip, push (E_pack x) env')
  | I_unpack ->
      let x, env' = pop env in
      (S_skip, push (E_unpack x) env')
  | I_add ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (Add, x_1, x_2)) env')
  | I_sub ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (Sub, x_1, x_2)) env')
  | I_mul ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (Mul, x_1, x_2)) env')
  | I_ediv ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (Div, x_1, x_2)) env')
  | I_abs ->
      let x, env' = pop env in
      (S_skip, push (E_unop (Abs, x)) env')
  | I_neg ->
      let x, env' = pop env in
      (S_skip, push (E_unop (Neg, x)) env')
  | I_lsl ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (ShiftL, x_1, x_2)) env')
  | I_lsr ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (ShiftR, x_1, x_2)) env')
  | I_or ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (Or, x_1, x_2)) env')
  | I_and ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (And, x_1, x_2)) env')
  | I_xor ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (Xor, x_1, x_2)) env')
  | I_not ->
      let x, env' = pop env in
      (S_skip, push (E_unop (Not, x)) env')
  | I_compare ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (Compare, x_1, x_2)) env')
  | I_eq ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (Eq, x_1, x_2)) env')
  | I_neq ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (Neq, x_1, x_2)) env')
  | I_lt ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (Lt, x_1, x_2)) env')
  | I_gt ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (Gt, x_1, x_2)) env')
  | I_le ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (Leq, x_1, x_2)) env')
  | I_ge ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (Geq, x_1, x_2)) env')
  | I_self ->
      (S_skip, push E_self env)
  | I_contract _ ->
      let x, env' = pop env in
      (S_skip, push (E_contract_of_address x) env')
  | I_transfer_tokens ->
      (* let (x,_), env' = pop env in
      let (amount,_), env' = pop env' in
      let (contract,_), env' = pop env' in
      (S_skip, push (E_set_delegate x) env') *)
      (S_todo, env)
  | I_set_delegate ->
      (* let (x,_), env' = pop env in
      (S_skip, push (E_set_delegate x) env') *)
      (S_todo, env)
  | I_create_account ->
      let manager, env' = pop env in
      let delegate, env' = pop env' in
      let delegatable, env' = pop env' in
      let amount, env' = pop env' in
      ( S_skip
      , push (E_create_account (manager, delegate, delegatable, amount)) env'
      )
  | I_create_contract _ ->
      (S_todo, env)
  | I_implicit_account ->
      let x, env' = pop env in
      (S_skip, push (E_implicit_account x) env')
  | I_now ->
      (S_skip, push E_now env)
  | I_amount ->
      (S_skip, push E_amount env)
  | I_balance ->
      (S_skip, push E_balance env)
  | I_check_signature ->
      let key, env' = pop env in
      let signature, env' = pop env' in
      let bytes, env' = pop env' in
      (S_skip, push (E_check_signature (key, signature, bytes)) env')
  | I_blake2b ->
      let x, env' = pop env in
      (S_skip, push (E_blake2b x) env')
  | I_sha256 ->
      let x, env' = pop env in
      (S_skip, push (E_sha256 x) env')
  | I_sha512 ->
      let x, env' = pop env in
      (S_skip, push (E_sha512 x) env')
  | I_hash_key ->
      let x, env' = pop env in
      (S_skip, push (E_hash_key x) env')
  | I_steps_to_quota ->
      (S_skip, push E_steps_to_quota env)
  | I_source ->
      (S_skip, push E_source env)
  | I_sender ->
      (S_skip, push E_sender env)
  | I_address ->
      let x, env' = pop env in
      (S_skip, push (E_address_of_contact x) env')
