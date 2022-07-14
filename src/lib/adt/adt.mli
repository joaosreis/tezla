open Core
module Var = Var
module Operation = Operation
module Node : module type of Common_adt.Node

type adt_typ = Edo_adt.Adt.typ
type ttyp = Edo_adt.Typ.t
type var = Var.t
type operation = Operation.t
type 'a node = 'a Node.t

type data_t =
  | D_int of Bigint.t
  | D_string of string
  | D_bytes of Bytes.t
  | D_unit
  | D_bool of bool
  | D_pair of data * data
  | D_left of data
  | D_right of data
  | D_some of data
  | D_none
  | D_list of data list
  | D_map of (data * data) list
  | D_instruction of var * stmt
[@@deriving sexp]

and data = (Edo_adt.Adt.typ * data_t) node [@@deriving sexp]

and expr_t =
  | E_var of var
  | E_push of data
  | E_car of var
  | E_cdr of var
  | E_abs of var
  | E_neg_nat of var
  | E_neg_int of var
  | E_neg_bls12_381_g1 of var
  | E_neg_bls12_381_g2 of var
  | E_neg_bls12_381_fr of var
  | E_not_bool of var
  | E_not_nat of var
  | E_not_int of var
  | E_add_nat of var * var
  | E_add_nat_int of var * var
  | E_add_int of var * var
  | E_add_timestamp_int of var * var
  | E_add_mutez of var * var
  | E_add_bls12_381_g1 of var * var
  | E_add_bls12_381_g2 of var * var
  | E_add_bls12_381_fr of var * var
  | E_sub_nat of var * var
  | E_sub_nat_int of var * var
  | E_sub_int of var * var
  | E_sub_timestamp_int of var * var
  | E_sub_timestamp of var * var
  | E_sub_mutez of var * var
  | E_mul_nat of var * var
  | E_mul_nat_int of var * var
  | E_mul_int of var * var
  | E_mul_mutez_nat of var * var
  | E_mul_bls12_381_g1_bls12_381_fr of var * var
  | E_mul_bls12_381_g2_bls12_381_fr of var * var
  | E_mul_bls12_381_fr_bls12_381_fr of var * var
  | E_mul_nat_bls12_381_fr of var * var
  | E_mul_int_bls12_381_fr of var * var
  | E_ediv_nat of var * var
  | E_ediv_nat_int of var * var
  | E_ediv_int of var * var
  | E_ediv_mutez_nat of var * var
  | E_ediv_mutez of var * var
  | E_lsl of var * var
  | E_lsr of var * var
  | E_and_bool of var * var
  | E_and_nat of var * var
  | E_and_int_nat of var * var
  | E_or_bool of var * var
  | E_or_nat of var * var
  | E_xor_bool of var * var
  | E_xor_nat of var * var
  | E_eq of var
  | E_neq of var
  | E_lt of var
  | E_gt of var
  | E_leq of var
  | E_geq of var
  | E_compare of var * var
  | E_cons of var * var
  | E_operation of operation
  | E_unit
  | E_pair of var * var
  | E_pair_n of var list
  | E_left of var * adt_typ
  | E_right of var * adt_typ
  | E_some of var
  | E_none of adt_typ
  | E_mem_set of var * var
  | E_mem_map of var * var
  | E_mem_big_map of var * var
  | E_get_map of var * var
  | E_get_big_map of var * var
  | E_update_set of var * var * var
  | E_update_map of var * var * var
  | E_update_big_map of var * var * var
  | E_concat_string of var * var
  | E_concat_bytes of var * var
  | E_concat_list_string of var
  | E_concat_list_bytes of var
  | E_slice_string of var * var * var
  | E_slice_bytes of var * var * var
  | E_pack of var
  | E_unpack of adt_typ * var
  | E_self
  | E_contract_of_address of adt_typ * var
  | E_implicit_account of var
  | E_now
  | E_amount
  | E_balance
  | E_check_signature of var * var * var
  | E_blake2b of var
  | E_sha256 of var
  | E_sha512 of var
  | E_hash_key of var
  | E_source
  | E_sender
  | E_address_of_contract of var
  | E_create_contract_address of Edo_adt.Typed_adt.program * var * var * var
  | E_unlift_option of var
  | E_unlift_or_left of var
  | E_unlift_or_right of var
  | E_hd of var
  | E_tl of var
  | E_size_list of var
  | E_size_set of var
  | E_size_map of var
  | E_size_string of var
  | E_size_bytes of var
  | E_isnat of var
  | E_int_of_nat of var
  | E_chain_id
  | E_lambda of adt_typ * var * stmt
  | E_exec of var * var
  | E_dup of var
  | E_nil of adt_typ
  | E_empty_set of adt_typ
  | E_empty_map of adt_typ * adt_typ
  | E_empty_big_map of adt_typ * adt_typ
  | E_apply of var * var
  | E_list_append of var * var
  | E_special_empty_list of ttyp
  | E_special_empty_map of ttyp * ttyp
  | E_create_account_address of var * var * var * var
  | E_voting_power of var
  | E_keccak of var
  | E_sha3 of var
  | E_total_voting_power
  | E_pairing_check of var
  | E_sapling_verify_update of var * var
  | E_sapling_empty_state of Bigint.t
  | E_ticket of var * var
  | E_read_ticket_pair of var
  | E_read_ticket_ticket of var
  | E_split_ticket of var * var
  | E_join_ticket of var
  | E_self_address
  | E_level
  | E_open_chest of var * var * var
  | E_get_and_update_val of var * var * var
  | E_get_and_update_map of var * var * var
  | E_dup_n of Bigint.t * var
  | E_get_n of Bigint.t * var
  | E_update_n of Bigint.t * var * var
[@@deriving sexp]

and expr = expr_t node [@@deriving sexp]

and stmt_t =
  | S_seq of stmt * stmt
  | S_assign of var * expr
  | S_skip
  | S_drop of var list
  | S_swap
  | S_dig of Bigint.t
  | S_dug of Bigint.t
  | S_if of var * stmt * stmt
  | S_if_none of var * stmt * stmt
  | S_if_left of var * stmt * stmt
  | S_if_cons of var * stmt * stmt
  | S_loop of var * stmt
  | S_loop_left of var * stmt
  | S_map_list of var * stmt
  | S_map_map of var * stmt
  | S_iter_set of var * stmt
  | S_iter_list of var * stmt
  | S_iter_map of var * stmt
  | S_failwith of var
  | S_return of var
[@@deriving sexp]

and stmt = stmt_t node [@@deriving sexp]
and program = adt_typ * adt_typ * stmt [@@deriving sexp]

module type Common = sig
  type t'
  type t = t' node

  val create : ?location:Common_adt.Loc.t -> t' -> t
  val to_string : t -> string

  include Sexpable.S with type t := t
end

module Data : Common with type t' = adt_typ * data_t and type t = data
module Expr : Common with type t' = expr_t and type t = expr

module Stmt : sig
  include Common with type t' = stmt_t and type t = stmt

  val simpl : t -> t
end
