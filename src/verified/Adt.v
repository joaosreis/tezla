From Coq Require Import ZArith.
From Coq Require Import Bool.Bool.
From Coq Require Import Strings.String.

From Tezla Require Import Edo_adt.
From Tezla Require Import Common_adt.
From Tezla Require Import Var.
From Tezla Require Import Operation.

Definition var : Type := Var.t.
Definition operation : Type := Operation.t.
Definition adt_typ : Type := Adt.typ.
Definition ttyp : Type := Typ.t.
Definition node (A: Type) : Type := Node.t A.

Inductive data_t : Type :=
| D_int : Z -> data_t
| D_string : string -> data_t
(* | D_bytes : Bytes.t *)
| D_unit : data_t
| D_bool : bool -> data_t
| D_pair : data -> data -> data_t

with data : Type :=
data_raw : node (adt_typ -> data_t) -> data

with expr_t : Type :=
| E_var : var -> expr_t
| E_push : data -> expr_t
| E_car : var -> expr_t
| E_cdr : var -> expr_t
| E_abs : var -> expr_t
| E_neg_nat : var -> expr_t
| E_neg_int : var -> expr_t
| E_neg_bls12_381_g1 : var -> expr_t
| E_neg_bls12_381_g2 : var -> expr_t
| E_neg_bls12_381_fr : var -> expr_t
| E_not_bool : var -> expr_t
| E_not_nat : var -> expr_t
| E_not_int : var -> expr_t
| E_add_nat : var -> var -> expr_t
| E_add_nat_int : var -> var -> expr_t
| E_add_int : var -> var -> expr_t
| E_add_timestamp_int : var -> var -> expr_t
| E_add_mutez : var -> var -> expr_t
| E_add_bls12_381_g1 : var -> var -> expr_t
| E_add_bls12_381_g2 : var -> var -> expr_t
| E_add_bls12_381_fr : var -> var -> expr_t
| E_sub_nat : var -> var -> expr_t
| E_sub_nat_int : var -> var -> expr_t
| E_sub_int : var -> var -> expr_t
| E_sub_timestamp_int : var -> var -> expr_t
| E_sub_timestamp : var -> var -> expr_t
| E_sub_mutez : var -> var -> expr_t
| E_mul_nat : var -> var -> expr_t
| E_mul_nat_int : var -> var -> expr_t
| E_mul_int : var -> var -> expr_t
| E_mul_mutez_nat : var -> var -> expr_t
| E_mul_bls12_381_g1_bls12_381_fr : var -> var -> expr_t
| E_mul_bls12_381_g2_bls12_381_fr : var -> var -> expr_t
| E_mul_bls12_381_fr_bls12_381_fr : var -> var -> expr_t
| E_mul_nat_bls12_381_fr : var -> var -> expr_t
| E_mul_int_bls12_381_fr : var -> var -> expr_t
| E_ediv_nat : var -> var -> expr_t
| E_ediv_nat_int : var -> var -> expr_t
| E_ediv_int : var -> var -> expr_t
| E_ediv_mutez_nat : var -> var -> expr_t
| E_ediv_mutez : var -> var -> expr_t
| E_lsl : var -> var -> expr_t
| E_lsr : var -> var -> expr_t
| E_and_bool : var -> var -> expr_t
| E_and_nat : var -> var -> expr_t
| E_and_int_nat : var -> var -> expr_t
| E_or_bool : var -> var -> expr_t
| E_or_nat : var -> var -> expr_t
| E_xor_bool : var -> var -> expr_t
| E_xor_nat : var -> var -> expr_t
| E_eq : var -> expr_t
| E_neq : var -> expr_t
| E_lt : var -> expr_t
| E_gt : var -> expr_t
| E_leq : var -> expr_t
| E_geq : var -> expr_t
| E_compare : var -> var -> expr_t
| E_cons : var -> var -> expr_t
| E_operation : operation -> expr_t
| E_unit : expr_t
| E_pair : var -> var -> expr_t
| E_pair_n : list var -> expr_t
| E_left : var -> adt_typ -> expr_t
| E_right : var -> adt_typ -> expr_t
| E_some : var -> expr_t
| E_none : adt_typ -> expr_t
| E_mem_set : var -> var -> expr_t
| E_mem_map : var -> var -> expr_t
| E_mem_big_map : var -> var -> expr_t
| E_get_map : var -> var -> expr_t
| E_get_big_map : var -> var -> expr_t
| E_update_set : var -> var -> var -> expr_t
| E_update_map : var -> var -> var -> expr_t
| E_update_big_map : var -> var -> var -> expr_t
| E_concat_string : var -> var -> expr_t
| E_concat_bytes : var -> var -> expr_t
| E_concat_list_string : var -> expr_t
| E_concat_list_bytes : var -> expr_t
| E_slice_string : var -> var -> var -> expr_t
| E_slice_bytes : var -> var -> var -> expr_t
| E_pack : var -> expr_t
| E_unpack : adt_typ -> var -> expr_t
| E_self : expr_t
| E_contract_of_address : adt_typ -> var -> expr_t
| E_implicit_account : var -> expr_t
| E_now : expr_t
| E_amount : expr_t
| E_balance : expr_t
| E_check_signature : var -> var -> var -> expr_t
| E_blake2b : var -> expr_t
| E_sha256 : var -> expr_t
| E_sha512 : var -> expr_t
| E_hash_key : var -> expr_t
| E_source : expr_t
| E_sender : expr_t
| E_address_of_contract : var -> expr_t
| E_create_contract_address : Edo_adt.Typed_adt.program -> var -> var -> var -> expr_t
| E_unlift_option : var -> expr_t
| E_unlift_or_left : var -> expr_t
| E_unlift_or_right : var -> expr_t
| E_hd : var -> expr_t
| E_tl : var -> expr_t
| E_size_list : var -> expr_t
| E_size_set : var -> expr_t
| E_size_map : var -> expr_t
| E_size_string : var -> expr_t
| E_size_bytes : var -> expr_t
| E_isnat : var -> expr_t
| E_int_of_nat : var -> expr_t
| E_chain_id : expr_t
| E_lambda : adt_typ -> var -> stmt -> expr_t
| E_exec : var -> var -> expr_t
| E_dup : var -> expr_t
| E_nil : adt_typ -> expr_t
| E_empty_set : adt_typ -> expr_t
| E_empty_map : adt_typ -> adt_typ -> expr_t
| E_empty_big_map : adt_typ -> adt_typ -> expr_t
| E_apply : var -> var -> expr_t
| E_list_append : var -> var -> expr_t
| E_special_empty_list : ttyp -> expr_t
| E_special_empty_map : ttyp -> ttyp -> expr_t
| E_create_account_address : var -> var -> var -> var -> expr_t
| E_voting_power : var -> expr_t
| E_keccak : var -> expr_t
| E_sha3 : var -> expr_t
| E_total_voting_power : expr_t
| E_pairing_check : var -> expr_t
| E_sapling_verify_update : var -> var -> expr_t
| E_sapling_empty_state : Z -> expr_t
| E_ticket : var -> var -> expr_t
| E_read_ticket_pair : var -> expr_t
| E_read_ticket_ticket : var -> expr_t
| E_split_ticket : var -> var -> expr_t
| E_join_ticket : var -> expr_t
| E_self_address : expr_t
| E_level : expr_t
| E_open_chest : var -> var -> var -> expr_t
| E_get_and_update_val : var -> var -> var -> expr_t
| E_get_and_update_map : var -> var -> var -> expr_t
| E_dup_n : Z -> var -> expr_t
| E_get_n : Z -> var -> expr_t
| E_update_n : Z -> var -> var -> expr_t

with expr : Type :=
expr_raw : node (expr_t) -> expr

with stmt_t : Type :=
| S_seq : stmt -> stmt -> stmt_t
| S_assign : var -> expr -> stmt_t
| S_skip: stmt_t
| S_drop : list var -> stmt_t
| S_swap: stmt_t
| S_dig : Z -> stmt_t
| S_dug : Z -> stmt_t
| S_if : var -> stmt -> stmt -> stmt_t
| S_if_none : var -> stmt -> stmt -> stmt_t
| S_if_left : var -> stmt -> stmt -> stmt_t
| S_if_cons : var -> stmt -> stmt -> stmt_t
| S_loop : var -> stmt -> stmt_t
| S_loop_left : var -> stmt -> stmt_t
| S_map_list : var -> stmt -> stmt_t
| S_map_map : var -> stmt -> stmt_t
| S_iter_set : var -> stmt -> stmt_t
| S_iter_list : var -> stmt -> stmt_t
| S_iter_map : var -> stmt -> stmt_t
| S_failwith : var -> stmt_t
| S_return : var -> stmt_t

with stmt : Type :=
stmt_raw : node stmt_t -> stmt.
