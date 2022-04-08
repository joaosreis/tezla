open! Core
open Env
module Michelson_adt = Edo_adt.Typed_adt
module Tezla_adt = Adt
open Common_adt

let create_stmt = Tezla_adt.Stmt.create
let create_expr = Tezla_adt.Expr.create
let create_data = Tezla_adt.Data.create

let join_loop env_before env_after body =
  match (env_before, env_after) with
  | Stack env_before, Stack env_after -> (
      match
        List.fold2 ~init:body
          ~f:(fun acc before after ->
            if not (Tezla_adt.Var.equal before after) then
              let e_after = create_expr (E_var after) in
              let assign = create_stmt (S_assign (before, e_after)) in
              create_stmt (S_seq (acc, assign))
            else acc)
          env_before env_after
      with
      | List.Or_unequal_lengths.Ok l -> l
      | List.Or_unequal_lengths.Unequal_lengths ->
          Debug.amf [%here] "unexpected loop body stack";
          assert false)
  | _ ->
      Debug.amf [%here] "should not have reached here";
      assert false

let join env_t env_f s =
  match (env_t, env_f) with
  | Failed, env | env, Failed -> (env, s)
  | Stack env_t, Stack env_f -> (
      match
        List.fold2 env_t env_f ~init:s ~f:(fun acc v_t v_f ->
            let e_var = create_expr (E_var v_t) in
            let assign = create_stmt (S_assign (v_f, e_var)) in
            create_stmt (S_seq (acc, assign)))
      with
      | List.Or_unequal_lengths.Ok s -> (Stack env_t, s)
      | List.Or_unequal_lengths.Unequal_lengths ->
          Debug.amf [%here] "unexpected if body stack";
          assert false)

let unlift_option_t t =
  let open Edo_adt.Typ in
  match fst t with
  | Option t -> t
  | _ ->
      let () = Debug.eprintf "Expected: option 'a but got %s\n" (to_string t) in
      assert false

let car_t t =
  let open Edo_adt.Typ in
  match fst t with
  | Pair (t, _) -> t
  | _ ->
      let () =
        Debug.eprintf "Expected: pair 'a 'b but got %s\n" (to_string t)
      in
      assert false

let cdr_t t =
  let open Edo_adt.Typ in
  match fst t with
  | Pair (_, t) -> t
  | _ ->
      let () =
        Debug.eprintf "Expected: pair 'a 'b but got %s\n" (to_string t)
      in
      assert false

let unlift_left_t t =
  let open Edo_adt.Typ in
  match fst t with
  | Or (t, _) -> t
  | _ ->
      let () = Debug.eprintf "Expected: or 'a 'b but got %s\n" (to_string t) in
      assert false

let unlift_right_t t =
  let open Edo_adt.Typ in
  match fst t with
  | Or (_, t) -> t
  | _ ->
      let () = Debug.eprintf "Expected: or 'a 'b but got %s\n" (to_string t) in
      assert false

let list_elem_t t =
  let open Edo_adt.Typ in
  match fst t with
  | List t -> t
  | _ ->
      let () = Debug.eprintf "Expected: list 'a but got %s\n" (to_string t) in
      assert false

let map_iter_elem_t t =
  let open Edo_adt.Typ in
  match fst t with
  | List t -> t
  | Set t -> t
  | Map (k, v) | Big_map (k, v) -> (Pair (k, v), [])
  | _ ->
      let () =
        Debug.eprintf "Expected: list 'a or set 'a or map 'a 'b but got %s"
          (to_string t)
      in
      assert false

let lambda_t t =
  let open Edo_adt.Typ in
  match fst t with
  | Lambda (_, t) -> t
  | _ ->
      let () =
        Debug.eprintf "Expected: lambda 'a 'b but got %s\n" (to_string t)
      in
      assert false

let rec assert_type t (d : Edo_adt.Typed_adt.data) =
  let open Edo_adt in
  match (fst (fst d.Node.value).Node.value, snd d.Node.value, fst t) with
  | T_int, _, Typ.Int
  | T_nat, _, Nat
  | T_bool, _, Bool
  | T_string, _, String
  | T_unit, _, Unit
  | T_address, _, Address
  | T_operation, _, Operation
  | T_never, _, Never
  | T_bytes, _, Bytes
  | T_bls12_381_fr, _, Bls12_381_fr
  | T_bls12_381_g1, _, Bls12_381_g1
  | T_bls12_381_g2, _, Bls12_381_g2
  | T_chain_id, _, Chain_id
  | T_chest, _, Chest
  | T_chest_key, _, Chest_key
  | T_timestamp, _, Timestamp ->
      true
  | T_sapling_transaction n, _, Sapling_transaction n'
  | T_sapling_state n, _, Sapling_state n'
    when Bigint.(n = n') ->
      true
  | T_pair _, D_pair (d_1, d_2), Pair (t_1, t_2) ->
      assert_type t_1 d_1 && assert_type t_2 d_2
  | T_or _, D_left d', Or (t', _)
  | T_or _, D_right d', Or (_, t')
  | T_option _, D_some d', Option t' ->
      assert_type t' d'
  | T_list _, D_list l, List t' | T_set _, D_list l, Set t' ->
      if List.length l = 0 then true else List.for_all ~f:(assert_type t') l
  | T_map _, D_map l, Map (k, v) | T_big_map _, D_map l, Big_map (k, v) ->
      let assert_type_map k v (d_k, d_v) =
        assert_type k d_k && assert_type v d_v
      in
      List.for_all ~f:(assert_type_map k v) l
  | T_lambda _, D_instruction _, Lambda _ -> true
  | _ -> false

let rec convert_data counter =
  let rec convert_data { Node.value = t, d; _ } =
    let d =
      match d with
      | Edo_adt.Typed_adt.D_int n -> Tezla_adt.D_int n
      | D_unit -> D_unit
      | D_none -> D_none
      | D_string s -> D_string s
      | D_bytes b -> D_bytes b
      | D_bool b -> D_bool b
      | D_pair (d_1, d_2) -> D_pair (convert_data d_1, convert_data d_2)
      | D_left d -> D_left (convert_data d)
      | D_right d -> D_right (convert_data d)
      | D_some d -> D_some (convert_data d)
      | D_list d_l -> D_list (List.map ~f:convert_data d_l)
      | D_map d_l ->
          D_map
            (List.map
               ~f:(fun (d_1, d_2) -> (convert_data d_1, convert_data d_2))
               d_l)
      | D_instruction i ->
          let param =
            let var_name = next_var counter in
            let var_type = lazy (Typer.convert_typ t) in
            Tezla_adt.Var.{ var_name; var_type }
          in
          let env = Env.push param Env.empty_env in
          let i, _ = inst_to_stmt (ref (-1)) env i in
          D_instruction (param, i)
      | _ -> assert false
    in
    create_data (t, d)
  in
  convert_data

and inst_to_stmt counter env i =
  let inst_to_stmt = inst_to_stmt counter in
  let i', annots = i.Node.value in
  let loc = i.location in
  let loop_n f =
    let rec loop acc n =
      if Bigint.(n = zero) then acc else loop (f acc n) Bigint.(n - one)
    in
    loop
  in
  let next_var () = next_var counter in
  let create_assign ?var_name e =
    let var_name = match var_name with None -> next_var () | Some v -> v in
    let e = create_expr e in
    let v = Tezla_adt.Var.{ var_name; var_type = lazy (Typer.type_expr e) } in
    (v, create_stmt (S_assign (v, e)))
  in
  let create_assign_annot_1 e =
    let open Annot in
    let annots =
      List.filter ~f:(function A_var _ -> true | _ -> false) annots
    in
    match annots with
    | A_var var_name :: _ -> create_assign ~var_name e
    | _ -> create_assign e
  in
  let create_assign_annot_2 e =
    let open Annot in
    let annots =
      List.filter ~f:(function A_var _ -> true | _ -> false) annots
    in
    match annots with
    | _ :: A_var var_name :: _ -> create_assign ~var_name e
    | _ -> create_assign e
  in
  try
    match i' with
    | Michelson_adt.I_failwith ->
        let x, _ = pop env in
        (create_stmt (S_failwith x), Failed)
    | I_seq i_l -> (
        match i_l with
        | [] -> (create_stmt S_skip, env)
        | h :: tl ->
            let s_h, env_h = inst_to_stmt env h in
            List.fold_left
              ~f:(fun (s, env) i ->
                let s', env' = inst_to_stmt env i in
                (create_stmt (S_seq (s, s')), env'))
              ~init:(s_h, env_h) tl)
    | I_if (i_t, i_f) ->
        let c, env = pop env in
        let s_t, env_t = inst_to_stmt env i_t in
        let s_f, env_f = inst_to_stmt env i_f in
        let env', s_f' = join env_t env_f s_f in
        let s = create_stmt (S_if (c, s_t, s_f')) in
        (s, env')
    | I_loop i ->
        (*
          LOOP c {
            ... // body

            c := top;
            ... // join_loop
          }
        *)
        let c, env_before = pop env in
        let body, env' = inst_to_stmt env_before i in
        let top, env_after = pop env' in
        let e_top = create_expr (E_var top) in
        let assign_c = create_stmt (S_assign (c, e_top)) in
        let body = create_stmt (S_seq (body, assign_c)) in
        let body = join_loop env_before env_after body in
        let s = create_stmt (S_loop (c, body)) in
        (s, env_after)
    | I_loop_left i ->
        let c, env_before = pop env in
        let v, assign_unlift = create_assign (Tezla_adt.E_unlift_or_left c) in
        let body, env' =
          let body_env = push v env_before in
          inst_to_stmt body_env i
        in
        let top, env_after = pop env' in
        let e_top = create_expr (E_var top) in
        let assign_c = create_stmt (S_assign (c, e_top)) in
        let body =
          create_stmt
            (S_seq (create_stmt (S_seq (assign_unlift, body)), assign_c))
        in
        let body = join_loop env_before env_after body in
        let post_loop_unlift = Tezla_adt.E_unlift_or_right c in
        let v_post_loop, post_loop_assign_unlift =
          create_assign post_loop_unlift
        in
        let s =
          create_stmt
            (S_seq (create_stmt (S_loop_left (c, body)), post_loop_assign_unlift))
        in
        let env' = push v_post_loop env_after in
        (s, env')
    | I_push x ->
        let d = convert_data counter x in
        let v, assign = create_assign_annot_1 (E_push d) in
        (assign, push v env)
    | I_drop n ->
        let env', l =
          loop_n
            (fun (env, l) _ ->
              let v, env = pop env in
              (env, v :: l))
            (env, []) n
        in
        (create_stmt (S_drop l), env')
    | I_dig n -> (create_stmt S_dig, dig env n)
    | I_dug n -> (create_stmt S_dug, dug env n)
    | I_swap ->
        let env' = swap env in
        (create_stmt S_swap, env')
    | I_some ->
        let v, env' = pop env in
        let e = Tezla_adt.E_some v in
        let v', assign = create_assign_annot_1 e in
        (assign, push v' env')
    | I_none t ->
        let e = Tezla_adt.E_none t in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_unit ->
        let e = Tezla_adt.E_unit in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_if_none (i_t, i_f) ->
        let v, env' = pop env in
        let s_t, env_t = inst_to_stmt env' i_t in
        let v', assign = create_assign (E_unlift_option v) in
        let s_f, env_f = inst_to_stmt (push v' env') i_f in
        let s_f = create_stmt (S_seq (assign, s_f)) in
        let env', s_f' = join env_t env_f s_f in
        let s = create_stmt (S_if_none (v, s_t, s_f')) in
        (s, env')
    | I_pair ->
        let v_1, env' = pop env in
        let t_2, env' = pop env' in
        let e = Tezla_adt.E_pair (v_1, t_2) in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_car ->
        let v, env' = pop env in
        let e = Tezla_adt.E_car v in
        let v', assign = create_assign_annot_1 e in
        (assign, push v' env')
    | I_cdr ->
        let v, env' = pop env in
        let e = Tezla_adt.E_cdr v in
        let v', assign = create_assign_annot_1 e in
        (assign, push v' env')
    | I_left t ->
        let v, env' = pop env in
        let e = Tezla_adt.E_left (v, t) in
        let v', assign = create_assign_annot_1 e in
        (assign, push v' env')
    | I_right t ->
        let v, env' = pop env in
        let e = Tezla_adt.E_right (v, t) in
        let v', assign = create_assign_annot_1 e in
        (assign, push v' env')
    | I_if_left (i_t, i_f) ->
        let v, env' = pop env in
        let e_t = Tezla_adt.E_unlift_or_left v in
        let v_t, assign_t = create_assign e_t in
        let env_t = push v_t env' in
        let s_t, env_t = inst_to_stmt env_t i_t in
        let s_t = create_stmt (S_seq (assign_t, s_t)) in

        let e_f = Tezla_adt.E_unlift_or_right v in
        let v_f, assign_f = create_assign e_f in
        let env_f = push v_f env' in
        let s_f, env_f = inst_to_stmt env_f i_f in
        let s_f = create_stmt (S_seq (assign_f, s_f)) in

        let env', s_f' = join env_t env_f s_f in
        let s = create_stmt (S_if_left (v, s_t, s_f')) in
        (s, env')
    | I_nil t ->
        let e = Tezla_adt.E_nil t in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_cons ->
        let v_1, env' = pop env in
        let v_2, env' = pop env' in
        let e = Tezla_adt.E_cons (v_1, v_2) in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_if_cons (i_t, i_f) ->
        let c, env' = pop env in
        let v_hd, assign_hd =
          let e_hd = Tezla_adt.E_hd c in
          create_assign e_hd
        in
        let v_tl, assign_tl =
          let e_tl = Tezla_adt.E_tl c in
          create_assign e_tl
        in
        let env_t = push v_hd (push v_tl env') in
        let env_f = env' in
        let s_t, env_t = inst_to_stmt env_t i_t in
        let s_t =
          create_stmt (S_seq (assign_hd, create_stmt (S_seq (assign_tl, s_t))))
        in
        let s_f, env_f = inst_to_stmt env_f i_f in
        let env', s_f' = join env_t env_f s_f in
        let s = create_stmt (S_if_cons (c, s_t, s_f')) in
        (s, env')
    | I_size_list ->
        let v, env' = pop env in
        let e = Tezla_adt.E_size_list v in
        let v', assign = create_assign_annot_1 e in
        (assign, push v' env')
    | I_size_set ->
        let v, env' = pop env in
        let e = Tezla_adt.E_size_set v in
        let v', assign = create_assign_annot_1 e in
        (assign, push v' env')
    | I_size_map ->
        let v, env' = pop env in
        let e = Tezla_adt.E_size_map v in
        let v', assign = create_assign_annot_1 e in
        (assign, push v' env')
    | I_size_string ->
        let v, env' = pop env in
        let e = Tezla_adt.E_size_string v in
        let v', assign = create_assign_annot_1 e in
        (assign, push v' env')
    | I_size_bytes ->
        let v, env' = pop env in
        let e = Tezla_adt.E_size_bytes v in
        let v', assign = create_assign_annot_1 e in
        (assign, push v' env')
    | I_empty_set t ->
        let e = Tezla_adt.E_empty_set t in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_empty_map (t_k, t_v) ->
        let e = Tezla_adt.E_empty_map (t_k, t_v) in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_empty_big_map (t_k, t_v) ->
        let e = Tezla_adt.E_empty_big_map (t_k, t_v) in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_map_list b ->
        (*
           acc := special_nil_list ();
           MAP c {
             v := hd c;

             // body
             ...
             top := ...;
             ...

             acc := append (acc, top);
             c := tl c;
           } *)
        let c, env' = pop env in
        let hd, assign_hd = create_assign (E_hd c) in
        let body, env_after_body =
          let body_env = push hd env' in
          inst_to_stmt body_env b
        in
        let e_tl = create_expr (E_tl c) in
        let assign_tl = create_stmt (S_assign (c, e_tl)) in
        let body_result, env_after_loop = pop env_after_body in
        let acc, initial_acc_assign =
          create_assign (E_special_empty_list (force body_result.var_type))
        in
        let e_append = create_expr (E_append (acc, body_result)) in
        let assign_append = create_stmt (S_assign (acc, e_append)) in
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
            (S_seq (initial_acc_assign, create_stmt (S_map (c, body))))
        in
        (s, push acc env_after_loop)
    | I_map_map b ->
        let c, env' = pop env in
        let hd, assign_hd = create_assign (E_hd c) in
        let body, env_after_body =
          let body_env = push hd env' in
          inst_to_stmt body_env b
        in
        let e_tl = create_expr (E_tl c) in
        let assign_tl = create_stmt (S_assign (c, e_tl)) in
        let body_result, env_after_loop = pop env_after_body in
        let acc, initial_acc_assign =
          match force c.var_type |> fst with
          | Map (t, _) ->
              create_assign
                (E_special_empty_map (t, force body_result.var_type))
          | _ -> assert false
        in
        let e_append = create_expr (E_append (acc, body_result)) in
        let assign_append = create_stmt (S_assign (acc, e_append)) in
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
            (S_seq (initial_acc_assign, create_stmt (S_map (c, body))))
        in
        (s, push acc env_after_loop)
    | I_iter_set b | I_iter_list b | I_iter_map b ->
        (*
           ITER c {
             v := hd c;

             // body
             ...
             
             c := tl c;
           } *)
        let c, env' = pop env in
        let hd, assign_hd = create_assign (E_hd c) in
        let body, env' =
          let body_env = push hd env' in
          inst_to_stmt body_env b
        in
        let e_tl = create_expr (E_tl c) in
        let assign_tl = create_stmt (S_assign (c, e_tl)) in
        let body =
          create_stmt (S_seq (assign_hd, create_stmt (S_seq (body, assign_tl))))
        in
        let s = create_stmt (S_iter (c, body)) in
        (s, env')
    | I_mem_set | I_mem_map | I_mem_big_map ->
        let elt, env' = pop env in
        let set, env' = pop env' in
        let e = Tezla_adt.E_mem (elt, set) in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_get_map | I_get_big_map ->
        let key, env' = pop env in
        let map, env' = pop env' in
        let e = Tezla_adt.E_get (key, map) in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_update_set | I_update_map | I_update_big_map ->
        let key, env' = pop env in
        let value, env' = pop env' in
        let map, env' = pop env' in
        let e = Tezla_adt.E_update (key, value, map) in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_lambda (t_1, t_2, i) ->
        let param =
          let var_name = next_var () in
          let var_type = lazy (Typer.convert_typ t_1) in
          Tezla_adt.Var.{ var_name; var_type }
        in
        let b, lambda_env = inst_to_stmt (push param empty_env) i in
        let b =
          match lambda_env with
          | Failed -> b
          | Stack _ ->
              let r = peek lambda_env in
              create_stmt (S_seq (b, create_stmt (S_return r)))
        in
        let e = Tezla_adt.E_lambda (t_1, t_2, param, b) in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_exec ->
        let param, env' = pop env in
        let lambda, env' = pop env' in
        let e = Tezla_adt.E_exec (param, lambda) in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_dip i ->
        let x, env' = pop env in
        let s, env' = inst_to_stmt env' i in
        (s, push x env')
    | I_dip_n (n, i) ->
        let xl, env' = dip env n in
        let s, env' = inst_to_stmt env' i in
        let env' = List.fold_left ~f:(fun acc x -> push x acc) ~init:env' xl in
        (s, env')
    | I_cast _ -> (create_stmt S_skip, env)
    | I_concat_string | I_concat_bytes ->
        let v, env' = pop env in
        let e, env' =
          let s_2, env' = pop env' in
          (Tezla_adt.E_concat (v, s_2), env')
        in

        let v', assign = create_assign_annot_1 e in
        (assign, push v' env')
    | I_concat_list_string | I_concat_list_bytes ->
        let v, env' = pop env in
        let e, env' = (Tezla_adt.E_concat_list v, env') in

        let v', assign = create_assign_annot_1 e in
        (assign, push v' env')
    | I_slice_string | I_slice_bytes ->
        let offset, env' = pop env in
        let length, env' = pop env' in
        let x, env' = pop env' in
        let e = Tezla_adt.E_slice (offset, length, x) in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_pack ->
        let x, env' = pop env in
        let e = Tezla_adt.E_pack x in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_unpack t ->
        let v, env' = pop env in
        let e = Tezla_adt.E_unpack (t, v) in
        let v', assign = create_assign_annot_1 e in
        (assign, push v' env')
    | I_add_nat | I_add_nat_int | I_add_int | I_add_timestamp_int | I_add_mutez
    | I_add_bls12_381_g1 | I_add_bls12_381_g2 | I_add_bls12_381_fr ->
        let v_1, env' = pop env in
        let v_2, env' = pop env' in
        let e = Tezla_adt.E_add (v_1, v_2) in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_sub_nat | I_sub_nat_int | I_sub_int | I_sub_timestamp_int
    | I_sub_timestamp | I_sub_mutez ->
        let v_1, env' = pop env in
        let v_2, env' = pop env' in
        let e = Tezla_adt.E_sub (v_1, v_2) in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_mul_nat | I_mul_nat_int | I_mul_int | I_mul_mutez_nat
    | I_mul_bls12_381_g1_bls12_381_fr | I_mul_bls12_381_g2_bls12_381_fr
    | I_mul_bls12_381_fr_bls12_381_fr | I_mul_nat_bls12_381_fr
    | I_mul_int_bls12_381_fr ->
        let t_1, env' = pop env in
        let t_2, env' = pop env' in
        let e = Tezla_adt.E_mul (t_1, t_2) in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_ediv_nat | I_ediv_nat_int | I_ediv_int | I_ediv_mutez_nat | I_ediv_mutez
      ->
        let v_1, env' = pop env in
        let v_2, env' = pop env' in
        let e = Tezla_adt.E_div (v_1, v_2) in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_abs ->
        let x, env' = pop env in
        let e = Tezla_adt.E_abs x in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_neg_nat | I_neg_int | I_neg_bls12_381_g1 | I_neg_bls12_381_g2
    | I_neg_bls12_381_fr ->
        let x, env' = pop env in
        let e = Tezla_adt.E_neg x in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_lsl ->
        let x_1, env' = pop env in
        let x_2, env' = pop env' in
        let e = Tezla_adt.E_shiftL (x_1, x_2) in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_lsr ->
        let x_1, env' = pop env in
        let x_2, env' = pop env' in
        let e = Tezla_adt.E_shiftR (x_1, x_2) in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_or_bool | I_or_nat ->
        let x_1, env' = pop env in
        let x_2, env' = pop env' in
        let e = Tezla_adt.E_or (x_1, x_2) in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_and_bool | I_and_nat | I_and_int_nat ->
        let x_1, env' = pop env in
        let x_2, env' = pop env' in
        let e = Tezla_adt.E_and (x_1, x_2) in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_xor_bool | I_xor_nat ->
        let x_1, env' = pop env in
        let x_2, env' = pop env' in
        let e = Tezla_adt.E_xor (x_1, x_2) in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_not_bool | I_not_nat | I_not_int ->
        let x, env' = pop env in
        let e = Tezla_adt.E_not x in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_compare ->
        let x_1, env' = pop env in
        let x_2, env'' = pop env' in
        let e = Tezla_adt.E_compare (x_1, x_2) in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env'')
    | I_eq ->
        let x, env' = pop env in
        let e = Tezla_adt.E_eq x in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_neq ->
        let x, env' = pop env in
        let e = Tezla_adt.E_neq x in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_lt ->
        let x, env' = pop env in
        let e = Tezla_adt.E_lt x in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_gt ->
        let x, env' = pop env in
        let e = Tezla_adt.E_gt x in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_le ->
        let x, env' = pop env in
        let e = Tezla_adt.E_leq x in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_ge ->
        let x, env' = pop env in
        let e = Tezla_adt.E_geq x in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_self ->
        let e = Tezla_adt.E_self in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_contract t ->
        let x, env' = pop env in
        let e = Tezla_adt.E_contract_of_address (t, x) in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_transfer_tokens ->
        let x, env' = pop env in
        let amount, env' = pop env' in
        let contract, env' = pop env' in
        let operation =
          Tezla_adt.Operation.O_transfer_tokens (x, amount, contract)
        in
        let e = Tezla_adt.E_operation operation in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_set_delegate ->
        let x, env' = pop env in
        let o = Tezla_adt.Operation.O_set_delegate x in
        let e = Tezla_adt.E_operation o in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_create_contract c ->
        let delegate, env' = pop env in
        let amount, env' = pop env' in
        let storage, env' = pop env' in
        let o =
          Tezla_adt.Operation.O_create_contract (c, delegate, amount, storage)
        in
        let v_o, assign_o = create_assign_annot_1 (E_operation o) in
        let v_a, assign_a =
          create_assign_annot_2
            (E_create_contract_address (c, delegate, amount, storage))
        in
        let env' = push v_o (push v_a env') in
        (create_stmt (S_seq (assign_o, assign_a)), env')
    | I_implicit_account ->
        let v, env' = pop env in
        let e = Tezla_adt.E_implicit_account v in
        let v', assign = create_assign_annot_1 e in
        (assign, push v' env')
    | I_now ->
        let e = Tezla_adt.E_now in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_amount ->
        let e = Tezla_adt.E_amount in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_balance ->
        let e = Tezla_adt.E_balance in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_check_signature ->
        let key, env' = pop env in
        let signature, env' = pop env' in
        let bytes, env' = pop env' in
        let e = Tezla_adt.E_check_signature (key, signature, bytes) in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_blake2b ->
        let x, env' = pop env in
        let e = Tezla_adt.E_blake2b x in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_sha256 ->
        let v, env' = pop env in
        let e = Tezla_adt.E_sha256 v in
        let v', assign = create_assign_annot_1 e in
        (assign, push v' env')
    | I_sha512 ->
        let v, env' = pop env in
        let e = Tezla_adt.E_sha512 v in
        let v', assign = create_assign_annot_1 e in
        (assign, push v' env')
    | I_hash_key ->
        let v, env' = pop env in
        let e = Tezla_adt.E_hash_key v in
        let v', assign = create_assign_annot_1 e in
        (assign, push v' env')
    | I_source ->
        let e = Tezla_adt.E_source in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_sender ->
        let e = Tezla_adt.E_sender in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_address ->
        let x, env' = pop env in
        let e = Tezla_adt.E_address_of_contract x in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_isnat ->
        let x, env' = pop env in
        let e = Tezla_adt.E_isnat x in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_int_nat | I_int_bls12_381_fr ->
        let x, env' = pop env in
        let e = Tezla_adt.E_int_of_nat x in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env')
    | I_chain_id ->
        let e = Tezla_adt.E_chain_id in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_noop -> (create_stmt S_skip, env)
    | I_apply ->
        let x_1, env = pop env in
        let x_2, env = pop env in
        let e = Tezla_adt.E_apply (x_1, x_2) in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_create_account ->
        let x_1, env = pop env in
        let x_2, env = pop env in
        let x_3, env = pop env in
        let x_4, env = pop env in
        let e_1 = Tezla_adt.E_create_account_operation (x_1, x_2, x_3, x_4) in
        let e_2 = Tezla_adt.E_create_account_address (x_1, x_2, x_3, x_4) in
        let v_1, assign_1 = create_assign_annot_1 e_1 in
        let v_2, assign_2 = create_assign_annot_1 e_2 in
        (create_stmt (S_seq (assign_1, assign_2)), push v_1 (push v_2 env))
    | I_voting_power ->
        let x, env = pop env in
        let e = Tezla_adt.E_voting_power x in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_keccak ->
        let x, env = pop env in
        let e = Tezla_adt.E_keccak x in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_sha3 ->
        let x, env = pop env in
        let e = Tezla_adt.E_sha3 x in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_total_voting_power ->
        let e = Tezla_adt.E_total_voting_power in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_pairing_check ->
        let x, env = pop env in
        let e = Tezla_adt.E_pairing_check x in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_sapling_verify_update ->
        let x_1, env = pop env in
        let x_2, env = pop env in
        let e = Tezla_adt.E_sapling_verify_update (x_1, x_2) in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_sapling_empty_state n ->
        let e = Tezla_adt.E_sapling_empty_state n in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_ticket ->
        let x_1, env = pop env in
        let x_2, env = pop env in
        let e = Tezla_adt.E_ticket (x_1, x_2) in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_read_ticket ->
        let x, env = pop env in
        let e_1 = Tezla_adt.E_read_ticket_pair x in
        let e_2 = Tezla_adt.E_read_ticket_ticket x in
        let v_1, assign_1 = create_assign_annot_1 e_1 in
        let v_2, assign_2 = create_assign_annot_1 e_2 in
        (create_stmt (S_seq (assign_1, assign_2)), push v_1 (push v_2 env))
    | I_split_ticket ->
        let x_1, env = pop env in
        let x_2, env = pop env in
        let e = Tezla_adt.E_split_ticket (x_1, x_2) in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_join_tickets ->
        let x, env = pop env in
        let e = Tezla_adt.E_join_ticket x in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_self_address ->
        let e = Tezla_adt.E_self_address in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_level ->
        let e = Tezla_adt.E_level in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_open_chest ->
        let x_1, env = pop env in
        let x_2, env = pop env in
        let x_3, env = pop env in
        let e = Tezla_adt.E_open_chest (x_1, x_2, x_3) in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_get_and_update_map | I_get_and_update_big_map ->
        let x_1, env = pop env in
        let x_2, env = pop env in
        let x_3, env = pop env in
        let e_1 = Tezla_adt.E_get_and_update_val (x_1, x_2, x_3) in
        let e_2 = Tezla_adt.E_get_and_update_map (x_1, x_2, x_3) in
        let v_1, assign_1 = create_assign_annot_1 e_1 in
        let v_2, assign_2 = create_assign_annot_1 e_2 in
        (create_stmt (S_seq (assign_1, assign_2)), push v_1 (push v_2 env))
    | I_dup n ->
        let env = dup env n in
        let x, env = pop env in
        let e = Tezla_adt.E_dup_n (n, x) in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_unpair n ->
        let rec unpair_n n (s, env) =
          let x, env = pop env in
          let e = Tezla_adt.E_car x in
          let v_1, assign_1 = create_assign_annot_1 e in
          let e = Tezla_adt.E_cdr x in
          let v_2, assign_2 = create_assign_annot_2 e in
          let s =
            create_stmt (S_seq (s, create_stmt (S_seq (assign_1, assign_2))))
          in
          if Bigint.(n = of_int 2) then
            let env = push v_1 (push v_2 env) in
            (s, env)
          else
            let env = push v_2 env in
            let s, env = unpair_n Bigint.(n - one) (s, env) in
            let env = push v_1 env in
            (s, env)
        in
        unpair_n n (create_stmt S_skip, env)
    | I_pair_n n ->
        let rec pair_n i s env =
          let x_1, env = pop env in
          let x_2, env = pop env in
          let v, s' = create_assign_annot_1 (Tezla_adt.E_pair (x_1, x_2)) in
          let s = create_stmt (S_seq (s, s')) in
          let env = push v env in
          if Bigint.(i = of_int 2) then (s, env)
          else pair_n Bigint.(i - one) s env
        in
        pair_n n (create_stmt S_skip) env
    | I_get_n n ->
        let x, env = pop env in
        let e = Tezla_adt.E_get_n (n, x) in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_update_n n ->
        let x_1, env = pop env in
        let x_2, env = pop env in
        let e = Tezla_adt.E_update_n (n, x_1, x_2) in
        let v, assign = create_assign_annot_1 e in
        (assign, push v env)
    | I_never (* TODO: *) -> (create_stmt S_skip, env)
  with
  | Functional_stack.Unsufficient_length ->
      failwith
        (Printf.sprintf "Unsufficent_length: %s\n"
           (Common_adt.Loc.to_string loc))
  | Assert_failure (f, lin, col) ->
      failwith
        (Printf.sprintf "Assert failure on %s:%d:%d\nMichelson file, %s\n" f lin
           col
           (Common_adt.Loc.to_string loc))
  | Invalid_argument s ->
      failwith
        (Printf.sprintf "Invalid arguement: %s\nMichelson file, %s\n" s
           (Common_adt.Loc.to_string loc))

and convert_program counter Edo_adt.Typed_adt.{ param; code; storage } =
  (* let code = inst_strip_location code in *)
  let param_storage =
    let var_name = "parameter_storage" in
    let var_type =
      lazy
        ( Edo_adt.Typ.Pair (Typer.convert_typ param, Typer.convert_typ storage),
          [] )
    in
    Tezla_adt.Var.{ var_name; var_type }
  in
  let env = Env.push param_storage Env.empty_env in
  let code, env = inst_to_stmt counter env code in
  let code = Tezla_adt.Stmt.simpl code in
  match env with
  | Failed -> (param, storage, code)
  | Stack _ ->
      let v = Env.peek env in
      let i = create_stmt (S_return v) in
      (param, storage, create_stmt (S_seq (code, i)))
