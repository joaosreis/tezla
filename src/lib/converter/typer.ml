open! Core
open Adt
open Edo_adt
open Common_adt

exception Type_error of string

let convert_typ = Edo_adt.Typer.stack_typ_of_typ

let type_expr e =
  let open Typ in
  let ct t = (t, []) in
  match e.Node.value with
  | E_abs _ | E_shiftL (_, _) | E_shiftR (_, _) -> ct Nat
  | E_unit -> ct Unit
  | E_now -> ct Timestamp
  | E_self | E_amount | E_balance -> ct Mutez
  | E_source | E_sender -> ct Address
  | E_chain_id -> ct Chain_id
  | E_special_empty_list t -> ct (List t)
  | E_special_empty_map (t_1, t_2) -> ct (Map (t_1, t_2))
  | E_push d -> convert_typ (fst d.Node.value)
  | E_car v -> (
      match force v.var_type |> fst with Pair (t, _) -> t | _ -> assert false)
  | E_cdr v -> (
      match force v.var_type |> fst with Pair (_, t) -> t | _ -> assert false)
  | E_neg _ | E_compare (_, _) -> ct Int
  | E_and (v_1, v_2) -> (
      match (force v_1.var_type |> fst, force v_2.var_type |> fst) with
      | Bool, Bool -> ct Bool
      | Nat, Nat | Int, Nat -> ct Nat
      | _ -> assert false)
  | E_or (v_1, v_2) -> (
      match (force v_1.var_type |> fst, force v_2.var_type |> fst) with
      | Bool, Bool -> ct Bool
      | Nat, Nat -> ct Nat
      | _ -> assert false)
  | E_xor (v_1, v_2) -> (
      match (force v_1.var_type |> fst, force v_2.var_type |> fst) with
      | Bool, Bool -> ct Bool
      | Nat, Nat -> ct Nat
      | _ -> assert false)
  | E_eq _ | E_neq _ | E_lt _ | E_gt _ | E_leq _ | E_geq _ | E_not _
  | E_mem (_, _) ->
      ct Bool
  | E_add (v_1, v_2) -> (
      match (force v_1.var_type |> fst, force v_2.var_type |> fst) with
      | Nat, Nat -> ct Nat
      | Nat, Int | Int, Nat | Int, Int -> ct Int
      | Timestamp, Int | Int, Timestamp -> ct Timestamp
      | Mutez, Mutez -> ct Mutez
      | Bls12_381_g1, Bls12_381_g1 -> ct Bls12_381_g1
      | Bls12_381_g2, Bls12_381_g2 -> ct Bls12_381_g2
      | Bls12_381_fr, Bls12_381_fr -> ct Bls12_381_fr
      | _ -> assert false)
  | E_sub (v_1, v_2) -> (
      match (force v_1.var_type |> fst, force v_2.var_type |> fst) with
      | Nat, Nat | Nat, Int | Int, Nat | Int, Int | Timestamp, Timestamp ->
          ct Int
      | Timestamp, Int -> ct Timestamp
      | Mutez, Mutez -> ct Mutez
      | _ -> assert false)
  | E_mul (v_1, v_2) -> (
      match (force v_1.var_type |> fst, force v_2.var_type |> fst) with
      | Nat, Nat -> ct Nat
      | Nat, Int | Int, Nat | Int, Int -> ct Int
      | Mutez, Nat | Nat, Mutez -> ct Mutez
      | Bls12_381_g1, Bls12_381_fr -> ct Bls12_381_g1
      | Bls12_381_g2, Bls12_381_fr -> ct Bls12_381_g2
      | Bls12_381_fr, Bls12_381_fr -> ct Bls12_381_fr
      | Nat, Bls12_381_fr -> ct Bls12_381_fr
      | Int, Bls12_381_fr -> ct Bls12_381_fr
      | Bls12_381_fr, Nat -> ct Bls12_381_fr
      | Bls12_381_fr, Int -> ct Bls12_381_fr
      | _ -> assert false)
  | E_div (v_1, v_2) -> (
      match (force v_1.var_type |> fst, force v_2.var_type |> fst) with
      | Nat, Nat -> ct (Option (ct (Pair (ct Nat, ct Nat))))
      | Nat, Int | Int, Nat | Int, Int ->
          ct (Option (ct (Pair (ct Int, ct Nat))))
      | Mutez, Nat -> ct (Option (ct (Pair (ct Mutez, ct Mutez))))
      | Mutez, Mutez -> ct (Option (ct (Pair (ct Nat, ct Mutez))))
      | _ -> assert false)
  | E_cons (_, v) -> force v.var_type
  | E_operation _ -> ct Operation
  | E_pair (v_1, v_2) -> ct (Pair (force v_1.var_type, force v_2.var_type))
  | E_left (v, t) -> ct (Or (force v.var_type, convert_typ t))
  | E_right (v, t) -> ct (Or (convert_typ t, force v.var_type))
  | E_some v -> ct (Option (force v.var_type))
  | E_none t -> ct (Option (convert_typ t))
  | E_get (_, v) -> (
      match force v.var_type |> fst with
      | Map (_, t) | Big_map (_, t) -> ct (Option t)
      | _ -> assert false)
  | E_update (_, _, v) -> force v.var_type
  | E_concat (v, _) -> force v.var_type
  | E_concat_list v -> (
      match force v.var_type |> fst with List t -> t | _ -> assert false)
  | E_slice (_, _, v) -> ct (Option (force v.var_type))
  | E_pack _ -> ct Bytes
  | E_unpack (t, _) -> ct (Option (convert_typ t))
  | E_contract_of_address (t, _) -> ct (Option (ct (Contract (convert_typ t))))
  | E_implicit_account _ -> ct (Contract (ct Unit))
  | E_check_signature _ -> ct Bool
  | E_blake2b _ | E_sha256 _ | E_sha512 _ -> ct Bytes
  | E_hash_key _ -> ct Key_hash
  | E_address_of_contract _ | E_create_contract_address _ -> ct Address
  | E_unlift_option v -> (
      match force v.var_type |> fst with Option t -> t | _ -> assert false)
  | E_unlift_or_left v -> (
      match force v.var_type |> fst with Or (t, _) -> t | _ -> assert false)
  | E_unlift_or_right v -> (
      match force v.var_type |> fst with Or (_, t) -> t | _ -> assert false)
  | E_hd v -> (
      match force v.var_type |> fst with
      | List t -> t
      | Set t -> t
      | Map (k, v) -> ct (Pair (k, v))
      | _ -> assert false)
  | E_tl v -> force v.var_type
  | E_size_bytes _ -> ct Nat
  | E_size_string _ -> ct Nat
  | E_size_list _ -> ct Nat
  | E_size_set _ -> ct Nat
  | E_size_map _ -> ct Nat
  | E_isnat _ -> ct (Option (ct Nat))
  | E_int_of_nat _ -> ct Int
  | E_lambda (t_1, t_2, _, _) -> ct (Lambda (convert_typ t_1, convert_typ t_2))
  | E_exec (_, v) -> (
      match force v.var_type |> fst with
      | Lambda (_, t) -> t
      | _ -> assert false)
  | E_dup v | E_var v -> force v.var_type
  | E_nil t -> ct (List (convert_typ t))
  | E_empty_set t -> ct (Set (convert_typ t))
  | E_empty_map (k_t, v_t) -> ct (Map (convert_typ k_t, convert_typ v_t))
  | E_empty_big_map (k_t, v_t) ->
      ct (Big_map (convert_typ k_t, convert_typ v_t))
  | E_apply (_, l) -> (
      match force l.var_type |> fst with
      | Lambda ((Pair (_, b), _), c) -> ct (Lambda (b, c))
      | _ -> assert false)
  | E_append (v, _) -> force v.var_type
  | E_total_voting_power -> ct Nat
  | E_self_address -> ct Address
  | E_level -> ct Nat
  | E_create_account_operation (_, _, _, _) -> ct Operation
  | E_create_account_address (_, _, _, _) -> ct Address
  | E_voting_power _ -> ct Nat
  | E_keccak _ -> ct Bytes
  | E_sha3 _ -> ct Bytes
  | E_pairing_check _ -> ct Bool
  | E_sapling_verify_update (t, s) -> (
      match (force t.var_type |> fst, force s.var_type |> fst) with
      | Sapling_transaction n, Sapling_state n' when Bigint.(n = n') ->
          ct (Option (ct (Pair (ct Int, ct (Sapling_state n)))))
      | _ -> assert false)
  | E_sapling_empty_state n -> ct (Sapling_state n)
  | E_ticket (t, _) -> ct (Ticket (force t.var_type))
  | E_read_ticket_pair t -> (
      match force t.var_type |> fst with
      | Ticket t -> ct (Pair (ct Address, ct (Pair (t, ct Nat))))
      | _ -> assert false)
  | E_read_ticket_ticket t -> (
      match force t.var_type |> fst with
      | Ticket t -> ct (Pair (ct Address, ct (Pair (t, ct Nat))))
      | _ -> assert false)
  | E_split_ticket (t, p) -> (
      match (force t.var_type |> fst, force p.var_type |> fst) with
      | Ticket t, Pair ((Nat, _), (Nat, _)) ->
          ct (Option (ct (Pair (ct (Ticket t), ct (Ticket t)))))
      | _ -> assert false)
  | E_join_ticket t -> (
      match force t.var_type |> fst with
      | Pair ((Ticket t, _), (Ticket t', _)) when t = t' ->
          ct (Option (ct (Ticket t)))
      | _ -> assert false)
  | E_open_chest (k, c, n) -> (
      match
        ( force k.var_type |> fst,
          force c.var_type |> fst,
          force n.var_type |> fst )
      with
      | Chest_key, Chest, Nat -> ct (Or (ct Bytes, ct Bool))
      | _ -> assert false)
  | E_get_and_update_val (k, v, m) -> (
      match
        (force k.var_type, force v.var_type |> fst, force m.var_type |> fst)
      with
      | k, Option v, Map (k', v') when k = k' && v = v' -> ct (Option v)
      | _ -> assert false)
  | E_get_and_update_map (k, v, m) -> (
      match
        (force k.var_type, force v.var_type |> fst, force m.var_type |> fst)
      with
      | k, Option v, Map (k', v') when k = k' && v = v' -> ct (Map (k, v))
      | k, Option v, Big_map (k', v') when k = k' && v = v' ->
          ct (Big_map (k, v))
      | _ -> assert false)
  | E_dup_n (_, v) -> force v.var_type
  | E_get_n (n, v) ->
      let rec get_n n t =
        if Bigint.(n = zero) then t
        else
          match fst t with
          | Pair (l, r) ->
              if Bigint.(n = one) then l else get_n Bigint.(n - one - one) r
          | _ -> assert false
      in
      get_n n (force v.var_type)
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
      update_n n (force k.var_type) (force v.var_type)
