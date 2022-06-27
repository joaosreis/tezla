From Coq Require Import ZArith.
From Coq Require Import Bool.Bool.
From Coq Require Import Strings.String.

From Tezla Require Import Edo_adt.
From Tezla Require Import Common_adt.
From Tezla Require Import Var.
From Tezla Require Import Operation.

Definition var : Type := Var.t.
Definition adt_typ : Type := Adt.typ.
Definition node (A: Type) : Type := Node.t A.

Inductive data_t : Type :=
| D_int : Z -> data_t
| D_string : string -> data_t
(* | D_bytes of Bytes.t *)
| D_unit
| D_bool : bool -> data_t
| D_pair : data -> data -> data_t

with data : Type :=
  data_raw : node (adt_typ * data_t) -> data

with expr_t : Type :=
| E_var : var -> expr_t
| E_push : data -> expr_t
| E_car : var -> expr_t
| E_cdr : var -> expr_t
| E_abs : var -> expr_t
| E_neg : var -> expr_t
| E_not : var -> expr_t
| E_add : var -> var -> expr_t
| E_sub : var -> var -> expr_t
| E_mul : var -> var -> expr_t
| E_div : var -> var -> expr_t
| E_shiftL : var -> var -> expr_t
| E_shiftR : var -> var -> expr_t
| E_and : var -> var -> expr_t
| E_or : var -> var -> expr_t
| E_xor : var -> var -> expr_t
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
| E_pair_n : var list -> expr_t
| E_left : var -> adt_typ -> expr_t
| E_right : var -> adt_typ -> expr_t
| E_some : var -> expr_t
| E_none : adt_typ -> expr_t
| E_mem : var -> var -> expr_t
| E_get : var -> var -> expr_t
| E_update : var -> var -> var -> expr_t
| E_concat : var -> var -> expr_t
| E_concat_list : var -> expr_t
| E_slice : var -> var -> var -> expr_t
| E_pack : var -> expr_t
| E_unpack : adt_typ -> var -> expr_t
| E_self : expr_t
| E_contract_of_address : adt_typ -> var -> expr_t
| E_implicit_account : var -> expr_t
| E_now : expr_t
| E_amount : expr_t
| E_balance
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
| E_size : var -> expr_t
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
| E_append : var -> var -> expr_t
| E_special_empty_list : ttyp -> expr_t
| E_special_empty_map : ttyp -> ttyp -> expr_t
| E_create_account_operation : var -> var -> var -> var -> expr_t
| E_create_account_address : var -> var -> var -> var -> expr_t
| E_voting_power : var -> expr_t
| E_keccak : var -> expr_t
| E_sha3 : var -> expr_t
| E_total_voting_power
| E_pairing_check : var -> expr_t
| E_sapling_verify_update : var -> var -> expr_t
| E_sapling_empty_state : Bigint.t -> expr_t
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
| E_dup_n : Bigint.t -> var -> expr_t
| E_get_n : Bigint.t -> var -> expr_t
| E_update_n : Bigint.t -> var -> var -> expr_t.
