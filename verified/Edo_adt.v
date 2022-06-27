From Tezla Require Import Common_adt.

From Coq Require Import Lists.List.
From Coq Require Import Init.Datatypes.
From Coq Require Import ZArith.
From Coq Require Import Bool.Bool.
From Coq Require Import Strings.String.

Module Adt.

  Definition annot : Type := Annot.t.
  Definition node (A: Type) : Type := Node.t A.

  Inductive typ_t : Type :=
  | T_unit : typ_t
  | T_never : typ_t
  | T_bool : typ_t
  | T_int : typ_t
  | T_nat : typ_t
  | T_string : typ_t
  | T_chain_id : typ_t
  | T_bytes : typ_t
  | T_mutez : typ_t
  | T_key_hash : typ_t
  | T_key : typ_t
  | T_signature : typ_t
  | T_timestamp : typ_t
  | T_address : typ_t
  | T_option : typ -> typ_t
  | T_list : typ -> typ_t
  | T_set : typ -> typ_t
  | T_operation
  | T_contract : typ -> typ_t
  | T_ticket : typ -> typ_t
  | T_pair : typ -> typ -> typ_t
  | T_or : typ -> typ -> typ_t
  | T_lambda : typ -> typ -> typ_t
  | T_map : typ -> typ -> typ_t
  | T_big_map : typ -> typ -> typ_t
  | T_bls12_381_g1
  | T_bls12_381_g2
  | T_bls12_381_fr
  | T_sapling_transaction : Z -> typ_t
  | T_sapling_state : Z -> typ_t
  | T_chest : typ_t
  | T_chest_key : typ_t

  with typ : Type :=
  | self : node (typ_t * list annot) -> typ.

End Adt.

Module Typ.

  Inductive t' : Type :=
  | Unit : t'
  | Never : t'
  | Bool : t'
  | Int : t'
  | Nat : t'
  | String : t'
  | Chain_id : t'
  | Bytes : t'
  | Mutez : t'
  | Key_hash : t'
  | Key : t'
  | Signature : t'
  | Timestamp : t'
  | Address : t'
  | Option : t -> t'
  | List : t -> t'
  | SSet : t -> t'
  | Operation : t'
  | Contract : t -> t'
  | Ticket : t -> t'
  | Pair : t -> t -> t'
  | Or : t -> t -> t'
  | Lambda : t -> t -> t'
  | Map : t -> t -> t'
  | Big_map : t -> t -> t'
  | Bls12_381_g1 : t'
  | Bls12_381_g2 : t'
  | Bls12_381_fr : t'
  | Sapling_transaction : Z -> t'
  | Sapling_state : Z -> t'
  | Chest : t'
  | Chest_key : t'

  with t : Type :=
    self : t' -> list Annot.t -> t.

End Typ.

Module Typed_adt.

  Definition annot : Type := Annot.t.
  Definition node (A: Type) : Type := Node.t A.
  Definition typ : Type := Adt.typ.

  Inductive data_t : Type :=
  | D_int : Z -> data_t
  | D_nat : Z -> data_t
  | D_string : string -> data_t
  (* | D_bytes of Bytes.t *)
  | D_unit : data_t
  | D_bool : bool-> data_t
  | D_pair : data -> data -> data_t
  | D_left : data -> data_t
  | D_right : data -> data_t
  | D_some : data -> data_t
  | D_none : data_t
  | D_list : list data -> data_t
  | D_map : list (data * data) -> data_t
  | D_instruction : inst -> data_t

  with data : Type :=
  | self : node (typ * data_t) -> data

  with inst_t : Type :=
  | I_abs : inst_t
  | I_add_nat : inst_t
  | I_add_nat_int : inst_t
  | I_add_int : inst_t
  | I_add_timestamp_int : inst_t
  | I_add_mutez : inst_t
  | I_add_bls12_381_g1 : inst_t
  | I_add_bls12_381_g2 : inst_t
  | I_add_bls12_381_fr : inst_t
  | I_address : inst_t
  | I_amount : inst_t
  | I_and_bool : inst_t
  | I_and_nat : inst_t
  | I_and_int_nat : inst_t
  | I_apply : inst_t
  | I_balance : inst_t
  | I_blake2b : inst_t
  | I_car : inst_t
  | I_cdr : inst_t
  | I_chain_id : inst_t
  | I_check_signature : inst_t
  | I_compare : inst_t
  | I_concat_string : inst_t
  | I_concat_list_string : inst_t
  | I_concat_bytes : inst_t
  | I_concat_list_bytes : inst_t
  | I_cons : inst_t
  | I_contract : Adt.typ -> inst_t
  | I_create_contract : program -> inst_t
  | I_dig : Z -> inst_t
  | I_dip : inst -> inst_t
  | I_dip_n : Z -> inst -> inst_t
  | I_drop : Z -> inst_t
  | I_dug : Z -> inst_t
  | I_dup : Z -> inst_t
  | I_ediv_nat : inst_t
  | I_ediv_nat_int : inst_t
  | I_ediv_int : inst_t
  | I_ediv_mutez_nat : inst_t
  | I_ediv_mutez : inst_t
  | I_empty_big_map : Adt.typ -> Adt.typ -> inst_t
  | I_empty_map : Adt.typ -> Adt.typ -> inst_t
  | I_empty_set : Adt.typ -> inst_t
  | I_eq : inst_t
  | I_exec : inst_t
  | I_failwith : inst_t
  | I_ge : inst_t
  | I_get_map : inst_t
  | I_get_big_map : inst_t
  | I_get_n : Z -> inst_t
  | I_get_and_update_map : inst_t
  | I_get_and_update_big_map : inst_t
  | I_gt : inst_t
  | I_hash_key : inst_t
  | I_if : inst -> inst -> inst_t
  | I_if_cons : inst -> inst -> inst_t
  | I_if_left : inst -> inst -> inst_t
  | I_if_none : inst -> inst -> inst_t
  | I_implicit_account : inst_t
  | I_int_nat : inst_t
  | I_int_bls12_381_fr : inst_t
  | I_isnat : inst_t
  | I_iter_set : inst -> inst_t
  | I_iter_map : inst -> inst_t
  | I_iter_list : inst -> inst_t
  | I_join_tickets : inst_t
  | I_keccak : inst_t
  | I_lambda : Adt.typ -> Adt.typ -> inst -> inst_t
  | I_le : inst_t
  | I_left : Adt.typ -> inst_t
  | I_level : inst_t
  | I_loop : inst -> inst_t
  | I_loop_left : inst -> inst_t
  | I_lsl : inst_t
  | I_lsr : inst_t
  | I_lt : inst_t
  | I_map_list : inst -> inst_t
  | I_map_map : inst -> inst_t
  | I_mem_set : inst_t
  | I_mem_map : inst_t
  | I_mem_big_map : inst_t
  | I_mul_nat : inst_t
  | I_mul_nat_int : inst_t
  | I_mul_int : inst_t
  | I_mul_mutez_nat : inst_t
  | I_mul_bls12_381_g1_bls12_381_fr : inst_t
  | I_mul_bls12_381_g2_bls12_381_fr : inst_t
  | I_mul_bls12_381_fr_bls12_381_fr : inst_t
  | I_mul_nat_bls12_381_fr : inst_t
  | I_mul_int_bls12_381_fr : inst_t
  | I_neg_nat : inst_t
  | I_neg_int : inst_t
  | I_neg_bls12_381_g1 : inst_t
  | I_neg_bls12_381_g2 : inst_t
  | I_neg_bls12_381_fr : inst_t
  | I_neq : inst_t
  | I_never : inst_t
  | I_nil : Adt.typ -> inst_t
  | I_none : Adt.typ -> inst_t
  | I_not_bool : inst_t
  | I_not_nat : inst_t
  | I_not_int : inst_t
  | I_now : inst_t
  | I_or_bool : inst_t
  | I_or_nat : inst_t
  | I_pack : inst_t
  | I_pair : inst_t
  | I_pair_n : Z -> inst_t
  | I_pairing_check : inst_t
  | I_push : data -> inst_t
  | I_read_ticket : inst_t
  | I_right : Adt.typ -> inst_t
  | I_sapling_empty_state : Z -> inst_t
  | I_sapling_verify_update : inst_t
  | I_self : inst_t
  | I_self_address : inst_t
  | I_sender : inst_t
  | I_set_delegate : inst_t
  | I_sha256 : inst_t
  | I_sha512 : inst_t
  | I_sha3 : inst_t
  | I_size_set : inst_t
  | I_size_map : inst_t
  | I_size_list : inst_t
  | I_size_string : inst_t
  | I_size_bytes : inst_t
  | I_slice_string : inst_t
  | I_slice_bytes : inst_t
  | I_some : inst_t
  | I_source : inst_t
  | I_split_ticket : inst_t
  | I_sub_nat : inst_t
  | I_sub_nat_int : inst_t
  | I_sub_int : inst_t
  | I_sub_timestamp_int : inst_t
  | I_sub_timestamp : inst_t
  | I_sub_mutez : inst_t
  | I_swap : inst_t
  | I_ticket : inst_t
  | I_total_voting_power : inst_t
  | I_transfer_tokens : inst_t
  | I_unit : inst_t
  | I_unpack : Adt.typ -> inst_t
  | I_unpair : Z -> inst_t
  | I_update_set : inst_t
  | I_update_map : inst_t
  | I_update_big_map : inst_t
  | I_update_n : Z -> inst_t
  | I_voting_power : inst_t
  | I_xor_bool : inst_t
  | I_xor_nat : inst_t
  | I_seq : list inst -> inst_t
  | I_noop : inst_t
  | I_open_chest : inst_t
  | I_cast : Adt.typ -> inst_t
  | I_create_account : inst_t

  with inst : Type :=
    Inst : inst_t -> node (list annot) -> inst

  with seq : Type :=
    | Seq_i : inst -> seq | Seq_s : inst -> seq -> seq

  with program : Type :=
    Program (param : typ) (storage : typ) (code : inst).

End Typed_adt.
