open Env

let join counter env_t env_f =
  let open Adt in
  match (env_t, env_f) with
  | Failed, env | env, Failed -> (env, create_stmt S_skip)
  | Stack env_t, Stack env_f ->
      assert (List.for_all2 (fun t f -> t.var_type = f.var_type) env_t env_f);
      let env_after =
        List.map2
          (fun v_t v_f ->
            if v_t.var_name = v_f.var_name then v_t
            else { var_name = next_var counter; var_type = v_t.var_type })
          env_t env_f
      in
      let rec phi acc env_after env_t env_f =
        match (env_after, env_t, env_f) with
        | [], [], [] -> acc
        | v_after :: env_after, v_t :: env_t, v_f :: env_f
          when v_t.var_name <> v_f.var_name ->
            let s = create_stmt (S_assign (v_after, E_phi (v_t, v_f))) in
            phi (create_stmt (S_seq (s, acc))) env_after env_t env_f
        | _ :: env_after, _ :: env_t, _ :: env_f ->
            phi acc env_after env_t env_f
        | _ -> assert false
      in
      let phis =
        phi (create_stmt S_skip) (List.rev env_after) (List.rev env_t)
          (List.rev env_f)
      in
      (Stack env_after, phis)

let unlift_option_t t =
  let open Adt in
  match t with
  | T_option t -> t
  | _ ->
      let () =
        let open Format in
        fprintf err_formatter "Expected: option 'a but got %a\n" Pp.pp_typ t
      in
      assert false

let car_t t =
  let open Adt in
  match t with
  | T_pair (t, _) -> t
  | _ ->
      let () =
        let open Format in
        fprintf err_formatter "Expected: pair 'a 'b but got %a\n" Pp.pp_typ t
      in
      assert false

let cdr_t t =
  let open Adt in
  match t with
  | T_pair (_, t) -> t
  | _ ->
      let open Format in
      let () =
        fprintf err_formatter "Expected: pair 'a 'b but got %a\n" Pp.pp_typ t
      in
      assert false

let unlift_left_t t =
  let open Adt in
  match t with
  | T_or (t, _) -> t
  | _ ->
      let open Format in
      let () =
        fprintf err_formatter "Expected: or 'a 'b but got %a\n" Pp.pp_typ t
      in
      assert false

let unlift_right_t t =
  let open Adt in
  match t with
  | T_or (_, t) -> t
  | _ ->
      let open Format in
      let () =
        fprintf err_formatter "Expected: or 'a 'b but got %a\n" Pp.pp_typ t
      in
      assert false

let list_elem_t t =
  let open Adt in
  match t with
  | T_list t -> t
  | _ ->
      let open Format in
      let () =
        fprintf err_formatter "Expected: list 'a but got %a\n" Pp.pp_typ t
      in
      assert false

let map_iter_elem_t t =
  let open Adt in
  match t with
  | T_list t -> t
  | T_set t -> t
  | T_map (k, v) | T_big_map (k, v) -> T_pair (k, v)
  | _ ->
      let open Format in
      let () =
        fprintf err_formatter
          "Expected: list 'a or set 'a or map 'a 'b but got %a" Pp.pp_typ t
      in
      assert false

let lambda_t t =
  let open Adt in
  match t with
  | T_lambda (_, t) -> t
  | _ ->
      let open Format in
      let () =
        fprintf err_formatter "Expected: lambda 'a 'b but got %a\n" Pp.pp_typ t
      in
      assert false

let rec assert_type (_, d) (_, t, _) =
  let open Michelson.Adt in
  match (d, t) with
  | D_int _, (T_int | T_nat | T_mutez | T_timestamp)
  | D_unit, T_unit
  | D_none, T_option _
  | ( D_string _,
      (T_string | T_key | T_key_hash | T_signature | T_address | T_timestamp) )
  | D_bytes _, (T_bytes | T_address)
  | D_bool _, T_bool ->
      true
  | D_pair (d_1, d_2), T_pair (t_1, t_2) ->
      assert_type d_1 t_1 && assert_type d_2 t_2
  | D_left d', T_or (t', _) | D_right d', T_or (_, t') | D_some d', T_option t'
    ->
      assert_type d' t'
  | D_list l, (T_list t' | T_set t') ->
      if List.length l = 0 then true
      else List.for_all (fun d' -> assert_type d' t') l
  | D_list l, (T_map (k, v) | T_big_map (k, v)) ->
      let assert_type_map d k v =
        match d with
        | D_elt (d_k, d_v) -> assert_type d_k k && assert_type d_v v
        | _ -> false
      in
      List.for_all (fun (_, d') -> assert_type_map d' k v) l
  | D_instruction _, T_lambda _ -> true
  | _ -> false

let rec typ_strip_location (_, t, a) =
  let open Michelson.Adt in
  let t =
    match t with
    | T_option t -> T_option (typ_strip_location t)
    | T_or (t_1, t_2) -> T_or (typ_strip_location t_1, typ_strip_location t_2)
    | T_pair (t_1, t_2) ->
        T_pair (typ_strip_location t_1, typ_strip_location t_2)
    | T_set t -> T_set (typ_strip_location t)
    | T_big_map (t_1, t_2) ->
        T_big_map (typ_strip_location t_1, typ_strip_location t_2)
    | T_map (t_1, t_2) -> T_map (typ_strip_location t_1, typ_strip_location t_2)
    | T_contract t -> T_contract (typ_strip_location t)
    | T_lambda (t_1, t_2) ->
        T_lambda (typ_strip_location t_1, typ_strip_location t_2)
    | T_list t -> T_list (typ_strip_location t)
    | T_nat -> T_nat
    | T_key -> T_key
    | T_unit -> T_unit
    | T_signature -> T_signature
    | T_operation -> T_operation
    | T_chain_id -> T_chain_id
    | T_int -> T_int
    | T_string -> T_string
    | T_bytes -> T_bytes
    | T_mutez -> T_mutez
    | T_bool -> T_bool
    | T_key_hash -> T_key_hash
    | T_timestamp -> T_timestamp
    | T_address -> T_address
  in
  ((), t, a)

and data_strip_location (_, d) =
  let open Michelson.Adt in
  let d =
    match d with
    | D_bool b -> D_bool b
    | D_unit -> D_unit
    | D_none -> D_none
    | D_int n -> D_int n
    | D_string s -> D_string s
    | D_bytes b -> D_bytes b
    | D_pair (d_1, d_2) ->
        D_pair (data_strip_location d_1, data_strip_location d_2)
    | D_left d -> D_left (data_strip_location d)
    | D_right d -> D_right (data_strip_location d)
    | D_some d -> D_some (data_strip_location d)
    | D_elt (d_1, d_2) ->
        D_elt (data_strip_location d_1, data_strip_location d_2)
    | D_list l -> D_list (List.map data_strip_location l)
    | D_instruction i -> D_instruction (inst_strip_location i)
  in
  ((), d)

and inst_strip_location (_, i, a) =
  let open Michelson.Adt in
  let i =
    match i with
    | I_cast t -> I_cast (typ_strip_location t)
    | I_noop -> I_noop
    | I_failwith -> I_failwith
    | I_exec -> I_exec
    | I_apply -> I_apply
    | I_drop -> I_drop
    | I_dup -> I_dup
    | I_swap -> I_swap
    | I_unit -> I_unit
    | I_eq -> I_eq
    | I_neq -> I_neq
    | I_lt -> I_lt
    | I_gt -> I_gt
    | I_le -> I_le
    | I_ge -> I_ge
    | I_or -> I_or
    | I_and -> I_and
    | I_xor -> I_xor
    | I_not -> I_not
    | I_neg -> I_neg
    | I_abs -> I_abs
    | I_isnat -> I_isnat
    | I_int -> I_int
    | I_add -> I_add
    | I_sub -> I_sub
    | I_mul -> I_mul
    | I_ediv -> I_ediv
    | I_lsl -> I_lsl
    | I_lsr | I_compare -> I_compare
    | I_concat -> I_concat
    | I_size -> I_size
    | I_slice -> I_slice
    | I_pair -> I_pair
    | I_car -> I_car
    | I_cdr -> I_cdr
    | I_mem | I_update -> I_update
    | I_get -> I_get
    | I_some -> I_some
    | I_cons -> I_cons
    | I_transfer_tokens -> I_transfer_tokens
    | I_set_delegate | I_balance -> I_balance
    | I_address -> I_address
    | I_source -> I_source
    | I_sender -> I_sender
    | I_self -> I_self
    | I_amount | I_implicit_account -> I_implicit_account
    | I_now -> I_now
    | I_chain_id -> I_chain_id
    | I_pack -> I_pack
    | I_hash_key -> I_hash_key
    | I_blake2b | I_sha256 -> I_sha256
    | I_sha512 -> I_sha512
    | I_check_signature -> I_check_signature
    | I_unpair -> I_unpair
    | I_rename -> I_rename
    | I_seq l -> I_seq (List.map inst_strip_location l)
    | I_if (i_1, i_2) -> I_if (inst_strip_location i_1, inst_strip_location i_2)
    | I_loop i -> I_loop (inst_strip_location i)
    | I_loop_left i -> I_loop_left (inst_strip_location i)
    | I_dip i -> I_dip (inst_strip_location i)
    | I_dip_n (n, i) -> I_dip_n (n, inst_strip_location i)
    | I_drop_n n -> I_drop_n n
    | I_dig n -> I_dig n
    | I_dug n -> I_dug n
    | I_push (t, d) -> I_push (typ_strip_location t, data_strip_location d)
    | I_lambda (t_1, t_2, i) ->
        I_lambda
          (typ_strip_location t_1, typ_strip_location t_2, inst_strip_location i)
    | I_empty_set t -> I_empty_set (typ_strip_location t)
    | I_iter i -> I_iter (inst_strip_location i)
    | I_empty_map (t_1, t_2) ->
        I_empty_map (typ_strip_location t_1, typ_strip_location t_2)
    | I_map i -> I_map (inst_strip_location i)
    | I_empty_big_map (t_1, t_2) ->
        I_empty_big_map (typ_strip_location t_1, typ_strip_location t_2)
    | I_none t -> I_none (typ_strip_location t)
    | I_if_none (i_1, i_2) ->
        I_if_none (inst_strip_location i_1, inst_strip_location i_2)
    | I_left t -> I_left (typ_strip_location t)
    | I_right t -> I_right (typ_strip_location t)
    | I_if_left (i_1, i_2) ->
        I_if_left (inst_strip_location i_1, inst_strip_location i_2)
    | I_nil t -> I_nil (typ_strip_location t)
    | I_if_cons (i_1, i_2) ->
        I_if_cons (inst_strip_location i_1, inst_strip_location i_2)
    | I_contract t -> I_contract (typ_strip_location t)
    | I_unpack t -> I_unpack (typ_strip_location t)
    | I_create_contract { code; param; storage } ->
        let code = inst_strip_location code in
        let param = typ_strip_location param in
        let storage = typ_strip_location storage in
        I_create_contract { code; param; storage }
  in
  ((), i, a)

let rec convert_typ (_, t, _) =
  let open Adt in
  match t with
  | Michelson.Adt.T_address -> T_address
  | Michelson.Adt.T_key -> T_key
  | Michelson.Adt.T_unit -> T_unit
  | Michelson.Adt.T_signature -> T_signature
  | Michelson.Adt.T_operation -> T_operation
  | Michelson.Adt.T_chain_id -> T_chain_id
  | Michelson.Adt.T_int -> T_int
  | Michelson.Adt.T_nat -> T_nat
  | Michelson.Adt.T_string -> T_string
  | Michelson.Adt.T_bytes -> T_bytes
  | Michelson.Adt.T_mutez -> T_mutez
  | Michelson.Adt.T_bool -> T_bool
  | Michelson.Adt.T_key_hash -> T_key_hash
  | Michelson.Adt.T_timestamp -> T_timestamp
  | Michelson.Adt.T_option t -> T_option (convert_typ t)
  | Michelson.Adt.T_list t -> T_list (convert_typ t)
  | Michelson.Adt.T_set t -> T_set (convert_typ t)
  | Michelson.Adt.T_contract t -> T_contract (convert_typ t)
  | Michelson.Adt.T_pair (t_1, t_2) -> T_pair (convert_typ t_1, convert_typ t_2)
  | Michelson.Adt.T_or (t_1, t_2) -> T_or (convert_typ t_1, convert_typ t_2)
  | Michelson.Adt.T_lambda (t_1, t_2) ->
      T_lambda (convert_typ t_1, convert_typ t_2)
  | Michelson.Adt.T_map (t_1, t_2) -> T_map (convert_typ t_1, convert_typ t_2)
  | Michelson.Adt.T_big_map (t_1, t_2) ->
      T_big_map (convert_typ t_1, convert_typ t_2)

let rec convert_data t (_, d) =
  let open Adt in
  match (t, d) with
  | _, Michelson.Adt.D_int n -> D_int n
  | _, Michelson.Adt.D_unit -> D_unit
  | _, Michelson.Adt.D_none -> D_none
  | _, Michelson.Adt.D_string s -> D_string s
  | _, Michelson.Adt.D_bytes b -> D_bytes b
  | _, Michelson.Adt.D_bool b -> D_bool b
  | T_pair (t_1, t_2), Michelson.Adt.D_pair (d_1, d_2) ->
      D_pair (convert_data t_1 d_1, convert_data t_2 d_2)
  | T_or (t, _), Michelson.Adt.D_left d -> D_left (convert_data t d)
  | T_or (_, t), Michelson.Adt.D_right d -> D_right (convert_data t d)
  | T_option t, Michelson.Adt.D_some d -> D_some (convert_data t d)
  | (T_list t | T_set t), Michelson.Adt.D_list d_l ->
      D_list (List.map (convert_data t) d_l)
  | (T_map (t_1, t_2) | T_big_map (t_1, t_2)), Michelson.Adt.D_list d_l ->
      D_list (List.map (convert_data_elt t_1 t_2) d_l)
  | T_lambda (t, _), Michelson.Adt.D_instruction i ->
      let env =
        Env.push { var_name = "parameter"; var_type = t } Env.empty_env
      in
      let i, _ = inst_to_stmt (ref (-1)) env i in
      D_instruction i
  | _ -> assert false

and convert_data_elt t_1 t_2 (_, d) =
  let open Adt in
  match d with
  | Michelson.Adt.D_elt (d_1, d_2) ->
      D_elt (convert_data t_1 d_1, convert_data t_2 d_2)
  | _ -> assert false

and inst_to_stmt counter env
    ((l, i, _) :
      (Michelson.Location.t, Michelson.Adt.annot list) Michelson.Adt.inst) =
  let open Michelson.Adt in
  let open Adt in
  let loop_n f =
    let rec loop acc n =
      if Z.(n = zero) then acc else loop (f acc n) Z.(n - one)
    in
    loop
  in
  let next_var () = next_var counter in
  let create_assign e =
    let var_name = next_var () in
    let var_type = Typer.type_expr e in
    let v = { var_name; var_type } in
    (v, create_stmt (S_assign (v, e)))
  in
  try
    match i with
    | I_failwith ->
        let x, _ = pop env in
        (create_stmt (S_failwith x), Failed)
    | I_seq i_l -> (
        match i_l with
        | [] -> (create_stmt S_skip, env)
        | h :: tl ->
            let s_h, env_h = inst_to_stmt counter env h in
            List.fold_left
              (fun (s, env) i ->
                let s', env' = inst_to_stmt counter env i in
                (create_stmt (S_seq (s, s')), env'))
              (s_h, env_h) tl )
    | I_if (i_t, i_f) ->
        let c, env' = pop env in
        let s_t, env_t = inst_to_stmt counter env' i_t in
        let s_f, env_f = inst_to_stmt counter env' i_f in
        let env', phis = join counter env_t env_f in
        let s = create_stmt (S_seq (create_stmt (S_if (c, s_t, s_f)), phis)) in
        (s, env')
    | I_loop i ->
        let c, env' = pop env in
        let loop_var = { var_name = next_var (); var_type = c.var_type } in
        let body, env' = inst_to_stmt counter env' i in
        let loop_result, env' = pop env' in
        let s = create_stmt (S_loop (loop_var, (c, loop_result), body)) in
        (s, env')
    | I_loop_left i ->
        let c, env' = pop env in
        let loop_var = { var_name = next_var (); var_type = c.var_type } in
        let e = E_unlift_or_left loop_var in
        let v, assign_unlift = create_assign e in
        let body, env' =
          let body_env = push v env' in
          inst_to_stmt counter body_env i
        in
        let loop_result, env' = pop env' in
        let body = create_stmt (S_seq (assign_unlift, body)) in
        let post_loop_unlift = E_unlift_or_right loop_var in
        let v_post_loop, post_loop_assign_unlift =
          create_assign post_loop_unlift
        in
        let s =
          create_stmt
            (S_seq
               ( create_stmt (S_loop_left (loop_var, (c, loop_result), body)),
                 post_loop_assign_unlift ))
        in
        let env' = push v_post_loop env' in
        (s, env')
    | I_push (t, x) ->
        assert (assert_type x t);
        let t = convert_typ t in
        let d = convert_data t x in
        let e = E_push (d, t) in
        let v, assign = create_assign e in
        (assign, push v env)
    | I_drop ->
        let v, env' = pop env in
        (create_stmt (S_drop [ v ]), env')
    | I_drop_n n ->
        let env', l =
          loop_n
            (fun (env, l) _ ->
              let v, env = pop env in
              (env, v :: l))
            (env, []) n
        in
        (create_stmt (S_drop l), env')
    | I_dup ->
        let v = peek env in
        let v', assign = create_assign (E_dup v) in
        let env' = push v' env in
        (assign, env')
    | I_dig n -> (create_stmt S_dig, dig env n)
    | I_dug n -> (create_stmt S_dug, dug env n)
    | I_swap ->
        let env' = swap env in
        (create_stmt S_swap, env')
    | I_some ->
        let v, env' = pop env in
        let v', assign = create_assign (E_some v) in
        (assign, push v' env')
    | I_none t ->
        let t = convert_typ t in
        let v, assign = create_assign (E_none t) in
        (assign, push v env)
    | I_unit ->
        let v, assign = create_assign E_unit in
        (assign, push v env)
    | I_if_none (i_t, i_f) ->
        let v, env' = pop env in
        let s_t, env_t = inst_to_stmt counter env' i_t in
        let v', assign = create_assign (E_unlift_option v) in
        let s_f, env_f = inst_to_stmt counter (push v' env') i_f in
        let env', phis = join counter env_t env_f in
        let s_f = create_stmt (S_seq (assign, s_f)) in
        let s =
          create_stmt (S_seq (create_stmt (S_if_none (v, s_t, s_f)), phis))
        in
        (s, env')
    | I_pair ->
        let v_1, env' = pop env in
        let t_2, env' = pop env' in
        let v, assign = create_assign (E_pair (v_1, t_2)) in
        (assign, push v env')
    | I_car ->
        let v, env' = pop env in
        let v', assign = create_assign (E_car v) in
        (assign, push v' env')
    | I_cdr ->
        let v, env' = pop env in
        let v', assign = create_assign (E_cdr v) in
        (assign, push v' env')
    | I_left t ->
        let t = convert_typ t in
        let v, env' = pop env in
        let v', assign = create_assign (E_left (v, t)) in
        (assign, push v' env')
    | I_right t ->
        let t = convert_typ t in
        let v, env' = pop env in
        let v', assign = create_assign (E_right (v, t)) in
        (assign, push v' env')
    | I_if_left (i_t, i_f) ->
        let v, env' = pop env in
        let e_t = E_unlift_or_left v in
        let e_f = E_unlift_or_right v in
        let v_t, assign_t = create_assign e_t in
        let v_f, assign_f = create_assign e_f in
        let env_t = push v_t env' in
        let env_f = push v_f env' in
        let s_t, env_t = inst_to_stmt counter env_t i_t in
        let s_f, env_f = inst_to_stmt counter env_f i_f in
        let env', phis = join counter env_t env_f in
        let s_t = create_stmt (S_seq (assign_t, s_t)) in
        let s_f = create_stmt (S_seq (assign_f, s_f)) in
        let s =
          create_stmt (S_seq (create_stmt (S_if_left (v, s_t, s_f)), phis))
        in
        (s, env')
    | I_nil t ->
        let t = convert_typ t in
        let v, assign = create_assign (E_nil t) in
        (assign, push v env)
    | I_cons ->
        let v_1, env' = pop env in
        let v_2, env' = pop env' in
        let v, assign = create_assign (E_cons (v_1, v_2)) in
        (assign, push v env')
    | I_if_cons (i_t, i_f) ->
        let c, env' = pop env in
        let v_hd, assign_hd =
          let e_hd = E_hd c in
          create_assign e_hd
        in
        let v_tl, assign_tl =
          let e_tl = E_tl c in
          create_assign e_tl
        in
        let env_t = push v_hd (push v_tl env') in
        let env_f = env' in
        let s_t, env_t = inst_to_stmt counter env_t i_t in
        let s_f, env_f = inst_to_stmt counter env_f i_f in
        let env', phis = join counter env_t env_f in
        let s_t =
          create_stmt (S_seq (assign_hd, create_stmt (S_seq (assign_tl, s_t))))
        in
        let s =
          create_stmt (S_seq (create_stmt (S_if_cons (c, s_t, s_f)), phis))
        in
        (s, env')
    | I_size ->
        let v, env' = pop env in
        let v', assign = create_assign (E_size v) in
        (assign, push v' env')
    | I_empty_set t ->
        let t = convert_typ t in
        let v, assign = create_assign (E_empty_set t) in
        (assign, push v env)
    | I_empty_map (t_k, t_v) ->
        let t_k = convert_typ t_k in
        let t_v = convert_typ t_v in
        let v, assign = create_assign (E_empty_map (t_k, t_v)) in
        (assign, push v env)
    | I_empty_big_map (t_k, t_v) ->
        let t_k = convert_typ t_k in
        let t_v = convert_typ t_v in
        let v, assign = create_assign (E_empty_big_map (t_k, t_v)) in
        (assign, push v env)
    | I_map b ->
        let c, env' = pop env in
        let loop_var = { var_name = next_var (); var_type = c.var_type } in
        let hd, assign_hd = create_assign (E_hd loop_var) in
        let body, env_after_body =
          let body_env = push hd env' in
          inst_to_stmt counter body_env b
        in
        let tl, assign_tl = create_assign (E_tl loop_var) in
        let body_result, env' = pop env_after_body in
        let empty_initial_list, initial_empty_list_assign =
          match c.var_type with
          | T_list _ ->
              create_assign (E_special_empty_list body_result.var_type)
          | T_map (t, _) ->
              create_assign (E_special_empty_map (t, body_result.var_type))
          | _ -> assert false
        in
        let result_list =
          { var_name = next_var (); var_type = empty_initial_list.var_type }
        in
        let append, assign_append =
          create_assign (E_append (result_list, body_result))
        in
        let body =
          create_stmt
            (S_seq
               ( assign_hd,
                 create_stmt
                   (S_seq (body, create_stmt (S_seq (assign_append, assign_tl))))
               ))
        in
        let s =
          create_stmt
            (S_seq
               ( initial_empty_list_assign,
                 create_stmt
                   (S_map
                      ( (loop_var, (c, tl)),
                        (result_list, (empty_initial_list, append)),
                        body )) ))
        in
        (s, push result_list env')
    | I_iter b ->
        let c, env' = pop env in
        let loop_var = { var_name = next_var (); var_type = c.var_type } in
        let hd, assign_hd = create_assign (E_hd loop_var) in
        let body, env' =
          let body_env = push hd env' in
          inst_to_stmt counter body_env b
        in
        let tl, assign_tl = create_assign (E_tl loop_var) in
        let body =
          create_stmt (S_seq (assign_hd, create_stmt (S_seq (body, assign_tl))))
        in
        let s = create_stmt (S_iter (loop_var, (c, tl), body)) in
        (s, env')
    | I_mem ->
        let elt, env' = pop env in
        let set, env' = pop env' in
        let v, assign = create_assign (E_mem (elt, set)) in
        (assign, push v env')
    | I_get ->
        let key, env' = pop env in
        let map, env' = pop env' in
        let v, assign = create_assign (E_get (key, map)) in
        (assign, push v env')
    | I_update ->
        let key, env' = pop env in
        let value, env' = pop env' in
        let map, env' = pop env' in
        let v, assign = create_assign (E_update (key, value, map)) in
        (assign, push v env')
    | I_lambda (t_1, t_2, i) ->
        let t_1 = convert_typ t_1 in
        let t_2 = convert_typ t_2 in
        let b, lambda_env =
          let v = { var_name = "param"; var_type = t_1 } in
          inst_to_stmt counter (push v empty_env) i
        in
        let b =
          match lambda_env with
          | Failed -> b
          | Stack _ ->
              let r = peek lambda_env in
              create_stmt (S_seq (b, create_stmt (S_return r)))
        in
        let v, assign = create_assign (E_lambda (t_1, t_2, b)) in
        (assign, push v env)
    | I_exec ->
        let param, env' = pop env in
        let lambda, env' = pop env' in
        let v, assign = create_assign (E_exec (param, lambda)) in
        (assign, push v env')
    | I_dip i ->
        let x, env' = pop env in
        let s, env' = inst_to_stmt counter env' i in
        (s, push x env')
    | I_dip_n (n, i) ->
        let xl, env' = dip env n in
        let s, env' = inst_to_stmt counter env' i in
        let env' = List.fold_left (fun acc x -> push x acc) env' xl in
        (s, env')
    | I_cast _ -> (create_stmt S_skip, env)
    | I_rename -> (create_stmt S_skip, env)
    | I_concat ->
        let v, env' = pop env in
        let (v', assign), env' =
          match v.var_type with
          | T_list T_string | T_list T_bytes ->
              (create_assign (E_concat_list v), env')
          | T_string | T_bytes ->
              let s_2, env' = pop env' in
              (create_assign (E_concat (v, s_2)), env')
          | _ -> assert false
        in
        (assign, push v' env')
    | I_slice ->
        let offset, env' = pop env in
        let length, env' = pop env' in
        let x, env' = pop env' in
        let v, assign = create_assign (E_slice (offset, length, x)) in
        (assign, push v env')
    | I_pack ->
        let x, env' = pop env in
        let v, assign = create_assign (E_pack x) in
        (assign, push v env')
    | I_unpack t ->
        let t = convert_typ t in
        let v, env' = pop env in
        let v', assign = create_assign (E_unpack (t, v)) in
        (assign, push v' env')
    | I_add ->
        let v_1, env' = pop env in
        let v_2, env' = pop env' in
        let v, assign = create_assign (E_add (v_1, v_2)) in
        (assign, push v env')
    | I_sub ->
        let v_1, env' = pop env in
        let v_2, env' = pop env' in
        let v, assign = create_assign (E_sub (v_1, v_2)) in
        (assign, push v env')
    | I_mul ->
        let t_1, env' = pop env in
        let t_2, env' = pop env' in
        let v, assign = create_assign (E_mul (t_1, t_2)) in
        (assign, push v env')
    | I_ediv ->
        let v_1, env' = pop env in
        let v_2, env' = pop env' in
        let v, assign = create_assign (E_div (v_1, v_2)) in
        (assign, push v env')
    | I_abs ->
        let x, env' = pop env in
        let v, assign = create_assign (E_abs x) in
        (assign, push v env')
    | I_neg ->
        let x, env' = pop env in
        let v, assign = create_assign (E_neg x) in
        (assign, push v env')
    | I_lsl ->
        let x_1, env' = pop env in
        let x_2, env' = pop env' in
        let v, assign = create_assign (E_shiftL (x_1, x_2)) in
        (assign, push v env')
    | I_lsr ->
        let x_1, env' = pop env in
        let x_2, env' = pop env' in
        let v, assign = create_assign (E_shiftR (x_1, x_2)) in
        (assign, push v env')
    | I_or ->
        let x_1, env' = pop env in
        let x_2, env' = pop env' in
        let v, assign = create_assign (E_or (x_1, x_2)) in
        (assign, push v env')
    | I_and ->
        let x_1, env' = pop env in
        let x_2, env' = pop env' in
        let v, assign = create_assign (E_and (x_1, x_2)) in
        (assign, push v env')
    | I_xor ->
        let x_1, env' = pop env in
        let x_2, env' = pop env' in
        let v, assign = create_assign (E_xor (x_1, x_2)) in
        (assign, push v env')
    | I_not ->
        let x, env' = pop env in
        let v, assign = create_assign (E_not x) in
        (assign, push v env')
    | I_compare ->
        let x_1, env' = pop env in
        let x_2, env'' = pop env' in
        let v, assign = create_assign (E_compare (x_1, x_2)) in
        (assign, push v env'')
    | I_eq ->
        let x, env' = pop env in
        let v, assign = create_assign (E_eq x) in
        (assign, push v env')
    | I_neq ->
        let x, env' = pop env in
        let v, assign = create_assign (E_neq x) in
        (assign, push v env')
    | I_lt ->
        let x, env' = pop env in
        let v, assign = create_assign (E_lt x) in
        (assign, push v env')
    | I_gt ->
        let x, env' = pop env in
        let v, assign = create_assign (E_gt x) in
        (assign, push v env')
    | I_le ->
        let x, env' = pop env in
        let v, assign = create_assign (E_leq x) in
        (assign, push v env')
    | I_ge ->
        let x, env' = pop env in
        let v, assign = create_assign (E_geq x) in
        (assign, push v env')
    | I_self ->
        let v, assign = create_assign E_self in
        (assign, push v env)
    | I_contract t ->
        let t = convert_typ t in
        let x, env' = pop env in
        let v, assign = create_assign (E_contract_of_address (t, x)) in
        (assign, push v env')
    | I_transfer_tokens ->
        let x, env' = pop env in
        let amount, env' = pop env' in
        let contract, env' = pop env' in
        let operation = O_transfer_tokens (x, amount, contract) in
        let v, assign = create_assign (E_operation operation) in
        (assign, push v env')
    | I_set_delegate ->
        let x, env' = pop env in
        let o = O_set_delegate x in
        let v, assign = create_assign (E_operation o) in
        (assign, push v env')
    | I_create_contract c ->
        let delegate, env' = pop env in
        let amount, env' = pop env' in
        let storage, env' = pop env' in
        let o = O_create_contract (c, delegate, amount, storage) in
        let v_o, assign_o = create_assign (E_operation o) in
        let v_a, assign_a =
          create_assign
            (E_create_contract_address (c, delegate, amount, storage))
        in
        let env' = push v_o (push v_a env') in
        (create_stmt (S_seq (assign_o, assign_a)), env')
    | I_implicit_account ->
        let v, env' = pop env in
        let v', assign = create_assign (E_implicit_account v) in
        (assign, push v' env')
    | I_now ->
        let v, assign = create_assign E_now in
        (assign, push v env)
    | I_amount ->
        let v, assign = create_assign E_amount in
        (assign, push v env)
    | I_balance ->
        let v, assign = create_assign E_balance in
        (assign, push v env)
    | I_check_signature ->
        let key, env' = pop env in
        let signature, env' = pop env' in
        let bytes, env' = pop env' in
        let v, assign =
          create_assign (E_check_signature (key, signature, bytes))
        in
        (assign, push v env')
    | I_blake2b ->
        let x, env' = pop env in
        let v, assign = create_assign (E_blake2b x) in
        (assign, push v env')
    | I_sha256 ->
        let v, env' = pop env in
        let v', assign = create_assign (E_sha256 v) in
        (assign, push v' env')
    | I_sha512 ->
        let v, env' = pop env in
        let v', assign = create_assign (E_sha512 v) in
        (assign, push v' env')
    | I_hash_key ->
        let v, env' = pop env in
        let v', assign = create_assign (E_hash_key v) in
        (assign, push v' env')
    | I_source ->
        let v, assign = create_assign E_source in
        (assign, push v env)
    | I_sender ->
        let v, assign = create_assign E_sender in
        (assign, push v env)
    | I_address ->
        let x, env' = pop env in
        let v, assign = create_assign (E_address_of_contract x) in
        (assign, push v env')
    | I_isnat ->
        let x, env' = pop env in
        let v, assign = create_assign (E_isnat x) in
        (assign, push v env')
    | I_int ->
        let x, env' = pop env in
        let v, assign = create_assign (E_int_of_nat x) in
        (assign, push v env')
    | I_chain_id ->
        let v, assign = create_assign E_chain_id in
        (assign, push v env)
    | I_noop -> (create_stmt S_skip, env)
    | I_unpair ->
        let x, env' = pop env in
        let v_1, assign_1 = create_assign (E_car x) in
        let v_2, assign_2 = create_assign (E_cdr x) in
        (create_stmt (S_seq (assign_1, assign_2)), push v_1 (push v_2 env'))
    | I_apply ->
        let x_1, env = pop env in
        let x_2, env = pop env in
        let v, assign = create_assign (E_apply (x_1, x_2)) in
        (assign, push v env)
  with
  | Functional_stack.Unsufficient_length ->
      let open Michelson.Location in
      failwith
        (Printf.sprintf "Unsufficent_length: Line %d, columns %d-%d\n" l.s.lin
           l.s.col l.e.col)
  | Typer.Type_error e ->
      let open Michelson.Location in
      failwith
        (Printf.sprintf "Type error: %s, Line %d, columns %d-%d\n" e l.s.lin
           l.s.col l.e.col)
  | Assert_failure (f, lin, col) ->
      let open Michelson.Location in
      failwith
        (Printf.sprintf
           "Assert failure on %s:%d:%d\n\
            Michelson file, Line %d, columns %d-%d\n"
           f lin col l.s.lin l.s.col l.e.col)
  | Invalid_argument s ->
      let open Michelson.Location in
      failwith
        (Printf.sprintf
           "Invalide arguement: %s\nMichelson file, Line %d, columns %d-%d\n" s
           l.s.lin l.s.col l.e.col)

and convert_program counter { Michelson.Adt.param; code; storage } =
  let param = convert_typ param in
  let storage = convert_typ storage in
  (* let code = inst_strip_location code in *)
  let env =
    Env.push
      { var_name = "parameter_storage"; var_type = T_pair (param, storage) }
      Env.empty_env
  in
  (param, storage, fst (inst_to_stmt counter env code) |> Adt.simpl)
