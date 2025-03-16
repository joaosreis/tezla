open! Core
open Tezla_adt.Adt
open Edo_adt
open Common_adt

exception Type_error of string

let convert_typ = Edo_adt.Typer.stack_typ_of_typ

let type_expr e =
  let open Typ in
  let ct t = (t, []) in
  match e.Node.value with
  | E_abs _ | E_lsl (_, _) | E_lsr (_, _) -> ct Nat
  | E_unit -> ct Unit
  | E_now -> ct Timestamp
  | E_self | E_amount | E_balance -> ct Mutez
  | E_source | E_sender -> ct Address
  | E_chain_id -> ct Chain_id
  | E_special_empty_list t -> ct (List t)
  | E_special_empty_map (t_1, t_2) -> ct (Map (t_1, t_2))
  | E_push d -> convert_typ (fst d.Node.value)
  | E_car v -> (
      match v.var_type |> fst with
      | Pair (t, _) -> t
      | t -> failwith ("car: expected pair but got " ^ t'_to_string t))
  | E_cdr v -> (
      match v.var_type |> fst with Pair (_, t) -> t | _ -> assert false)
  | E_neg_nat _ | E_neg_int _ | E_compare (_, _) -> ct Int
  | E_neg_bls12_381_g1 _ -> ct Bls12_381_g1
  | E_neg_bls12_381_g2 _ -> ct Bls12_381_g2
  | E_neg_bls12_381_fr _ -> ct Bls12_381_fr
  | E_and_bool (_, _) -> ct Bool
  | E_and_nat (_, _) -> ct Nat
  | E_and_int_nat (_, _) -> ct Nat
  | E_or_bool (_, _) -> ct Bool
  | E_or_nat (_, _) -> ct Nat
  | E_xor_bool (_, _) -> ct Bool
  | E_xor_nat (_, _) -> ct Nat
  | E_eq _ | E_neq _ | E_lt _ | E_gt _ | E_leq _ | E_geq _ | E_not_bool _
  | E_mem_set (_, _)
  | E_mem_map (_, _)
  | E_mem_big_map (_, _) ->
      ct Bool
  | E_not_nat _ -> ct Int
  | E_not_int _ -> ct Int
  | E_add_nat (_, _) -> ct Nat
  | E_add_nat_int (_, _) -> ct Int
  | E_add_int (_, _) -> ct Int
  | E_add_timestamp_int (_, _) -> ct Int
  | E_add_mutez (_, _) -> ct Mutez
  | E_add_bls12_381_g1 (_, _) -> ct Bls12_381_g1
  | E_add_bls12_381_g2 (_, _) -> ct Bls12_381_g2
  | E_add_bls12_381_fr (_, _) -> ct Bls12_381_fr
  | E_sub_int (_, _) -> ct Int
  | E_sub_mutez (_, _) -> ct Mutez
  | E_sub_nat (_, _) -> ct Int
  | E_sub_nat_int (_, _) -> ct Int
  | E_sub_timestamp_int (_, _) -> ct Timestamp
  | E_sub_timestamp (_, _) -> ct Int
  | E_mul_nat (_, _) -> ct Nat
  | E_mul_nat_int (_, _) -> ct Int
  | E_mul_int (_, _) -> ct Int
  | E_mul_mutez_nat (_, _) -> ct Mutez
  | E_mul_bls12_381_g1_bls12_381_fr (_, _) -> ct Bls12_381_g1
  | E_mul_bls12_381_g2_bls12_381_fr (_, _) -> ct Bls12_381_g2
  | E_mul_bls12_381_fr_bls12_381_fr (_, _) -> ct Bls12_381_fr
  | E_mul_nat_bls12_381_fr (_, _) -> ct Bls12_381_fr
  | E_mul_int_bls12_381_fr (_, _) -> ct Bls12_381_fr
  | E_ediv_int (_, _) -> ct (Option (ct (Pair (ct Int, ct Nat))))
  | E_ediv_mutez (_, _) -> ct (Option (ct (Pair (ct Nat, ct Mutez))))
  | E_ediv_nat (_, _) -> ct (Option (ct (Pair (ct Nat, ct Nat))))
  | E_ediv_nat_int (_, _) -> ct (Option (ct (Pair (ct Int, ct Nat))))
  | E_ediv_mutez_nat (_, _) -> ct (Option (ct (Pair (ct Mutez, ct Mutez))))
  | E_cons (_, v) -> v.var_type
  | E_operation _ -> ct Operation
  | E_pair (v_1, v_2) -> ct (Pair (v_1.var_type, v_2.var_type))
  | E_left (v, t) -> ct (Or (v.var_type, convert_typ t))
  | E_right (v, t) -> ct (Or (convert_typ t, v.var_type))
  | E_some v -> ct (Option v.var_type)
  | E_none t -> ct (Option (convert_typ t))
  | E_get_map (_, v) | E_get_big_map (_, v) -> (
      match v.var_type |> fst with
      | Map (_, t) | Big_map (_, t) -> ct (Option t)
      | _ -> assert false)
  | E_update_set (_, _, v) | E_update_map (_, _, v) | E_update_big_map (_, _, v)
    ->
      v.var_type
  | E_concat_bytes _ -> ct Bytes
  | E_concat_string _ -> ct String
  | E_concat_list_string _ -> ct (List (ct String))
  | E_concat_list_bytes _ -> ct (List (ct Bytes))
  | E_slice_bytes (_, _, _) -> ct (Option (ct Bytes))
  | E_slice_string (_, _, _) -> ct (Option (ct String))
  | E_pack _ -> ct Bytes
  | E_unpack (t, _) -> ct (Option (convert_typ t))
  | E_contract_of_address (t, _) -> ct (Option (ct (Contract (convert_typ t))))
  | E_implicit_account _ -> ct (Contract (ct Unit))
  | E_check_signature _ -> ct Bool
  | E_blake2b _ | E_sha256 _ | E_sha512 _ -> ct Bytes
  | E_hash_key _ -> ct Key_hash
  | E_address_of_contract _ | E_create_contract_address _ -> ct Address
  | E_unlift_option v -> (
      match v.var_type |> fst with Option t -> t | _ -> assert false)
  | E_unlift_or_left v -> (
      match v.var_type |> fst with Or (t, _) -> t | _ -> assert false)
  | E_unlift_or_right v -> (
      match v.var_type |> fst with Or (_, t) -> t | _ -> assert false)
  | E_hd v -> (
      match v.var_type |> fst with
      | List t -> t
      | Set t -> t
      | Map (k, v) -> ct (Pair (k, v))
      | _ -> assert false)
  | E_tl v -> v.var_type
  | E_size_bytes _ -> ct Nat
  | E_size_string _ -> ct Nat
  | E_size_list _ -> ct Nat
  | E_size_set _ -> ct Nat
  | E_size_map _ -> ct Nat
  | E_isnat _ -> ct (Option (ct Nat))
  | E_int_of_nat _ -> ct Int
  | E_lambda (r, param, _) -> ct (Lambda (param.var_type, convert_typ r))
  | E_exec (_, v) -> (
      match v.var_type |> fst with Lambda (_, t) -> t | _ -> assert false)
  | E_dup v | E_var v -> v.var_type
  | E_nil t -> ct (List (convert_typ t))
  | E_empty_set t -> ct (Set (convert_typ t))
  | E_empty_map (k_t, v_t) -> ct (Map (convert_typ k_t, convert_typ v_t))
  | E_empty_big_map (k_t, v_t) ->
      ct (Big_map (convert_typ k_t, convert_typ v_t))
  | E_apply (_, l) -> (
      match l.var_type |> fst with
      | Lambda ((Pair (_, b), _), c) -> ct (Lambda (b, c))
      | _ -> assert false)
  | E_list_append (v, _) -> v.var_type
  | E_total_voting_power -> ct Nat
  | E_self_address -> ct Address
  | E_level -> ct Nat
  | E_create_account_address (_, _, _, _) -> ct Address
  | E_voting_power _ -> ct Nat
  | E_keccak _ -> ct Bytes
  | E_sha3 _ -> ct Bytes
  | E_pairing_check _ -> ct Bool
  | E_sapling_verify_update (t, s) -> (
      match (t.var_type |> fst, s.var_type |> fst) with
      | Sapling_transaction n, Sapling_state n' when Bigint.(n = n') ->
          ct (Option (ct (Pair (ct Int, ct (Sapling_state n)))))
      | _ -> assert false)
  | E_sapling_empty_state n -> ct (Sapling_state n)
  | E_ticket (t, _) -> ct (Ticket t.var_type)
  | E_read_ticket_pair t -> (
      match t.var_type |> fst with
      | Ticket t -> ct (Pair (ct Address, ct (Pair (t, ct Nat))))
      | _ -> assert false)
  | E_read_ticket_ticket t -> (
      match t.var_type |> fst with
      | Ticket t -> ct (Ticket t)
      | _ -> assert false)
  | E_split_ticket (t, p) -> (
      match (t.var_type |> fst, p.var_type |> fst) with
      | Ticket t, Pair ((Nat, _), (Nat, _)) ->
          ct (Option (ct (Pair (ct (Ticket t), ct (Ticket t)))))
      | _ -> assert false)
  | E_join_ticket t -> (
      match t.var_type |> fst with
      | Pair ((Ticket t, _), (Ticket t', _)) when are_compatible t t' ->
          ct (Option (ct (Ticket t)))
      | _ -> assert false)
  | E_open_chest (k, c, n) -> (
      match (k.var_type |> fst, c.var_type |> fst, n.var_type |> fst) with
      | Chest_key, Chest, Nat -> ct (Or (ct Bytes, ct Bool))
      | _ -> assert false)
  | E_get_and_update_val (k, v, m) -> (
      match (k.var_type, v.var_type |> fst, m.var_type |> fst) with
      | k, Option v, Map (k', v') ->
          if are_compatible k k' && are_compatible v v' then ct (Option v)
          else assert false
      | k, Option v, Big_map (k', v') ->
          if are_compatible k k' && are_compatible v v' then ct (Option v)
          else assert false
      | _ -> assert false)
  | E_get_and_update_map (k, v, m) -> (
      match (k.var_type, v.var_type |> fst, m.var_type |> fst) with
      | k, Option v, Map (k', v') ->
          if are_compatible k k' && are_compatible v v' then ct (Map (k, v))
          else assert false
      | k, Option v, Big_map (k', v') ->
          if are_compatible k k' && are_compatible v v' then ct (Big_map (k, v))
          else assert false
      | _ -> assert false)
  | E_dup_n (_, v) -> v.var_type
  | E_get_n (n, v) ->
      let rec get_n n t =
        if Bigint.(n = zero) then t
        else
          match fst t with
          | Pair (l, r) ->
              if Bigint.(n = one) then l else get_n Bigint.(n - one - one) r
          | _ -> assert false
      in
      get_n n v.var_type
  | E_update_n (n, k, v) ->
      let rec update_n n t_1 t_2 =
        if Bigint.(n = zero) then t_1
        else
          match fst t_2 with
          | Pair (l, r) ->
              if Bigint.(n = one) then (Pair (t_1, r), [])
              else (Pair (l, update_n Bigint.(n - one - one) t_1 r), [])
          | _ -> assert false
      in
      update_n n k.var_type v.var_type
  | E_pair_n v_l -> (
      match List.rev v_l with
      | [ v_2; v_1 ] -> ct (Pair (v_1.var_type, v_2.var_type))
      | v_2 :: v_1 :: t ->
          List.fold_left t
            ~init:(ct (Pair (v_1.var_type, v_2.var_type)))
            ~f:(fun acc v -> ct (Pair (v.var_type, acc)))
      | _ -> assert false)
