open Core_kernel
module Var = Var
module Typ = Typ
module Operation = Operation

type var = Var.t

type typ = Typ.t

type operation = Operation.t

type data =
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
  | D_elt of data * data
  | D_list of data list
  | D_instruction of stmt

and expr =
  | E_var of var
  | E_push of data * typ
  | E_car of var
  | E_cdr of var
  | E_abs of var
  | E_neg of var
  | E_not of var
  | E_add of var * var
  | E_sub of var * var
  | E_mul of var * var
  | E_div of var * var
  | E_shiftL of var * var
  | E_shiftR of var * var
  | E_and of var * var
  | E_or of var * var
  | E_xor of var * var
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
  | E_left of var * typ
  | E_right of var * typ
  | E_some of var
  | E_none of typ
  | E_mem of var * var
  | E_get of var * var
  | E_update of var * var * var
  | E_concat of var * var
  | E_concat_list of var
  | E_slice of var * var * var
  | E_pack of var
  | E_unpack of typ * var
  | E_self
  | E_contract_of_address of typ * var
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
  | E_create_contract_address of
      (Loc.t, Carthage.Adt.annot list) Michelson.Carthage.Adt.program
      * var
      * var
      * var
  | E_unlift_option of var
  | E_unlift_or_left of var
  | E_unlift_or_right of var
  | E_hd of var
  | E_tl of var
  | E_size of var
  | E_isnat of var
  | E_int_of_nat of var
  | E_chain_id
  | E_lambda of typ * typ * var * stmt
  | E_exec of var * var
  | E_dup of var
  | E_nil of typ
  | E_empty_set of typ
  | E_empty_map of typ * typ
  | E_empty_big_map of typ * typ
  | E_apply of var * var
  | E_append of var * var
  | E_special_empty_list of typ
  | E_special_empty_map of typ * typ
[@@deriving ord, sexp]

and stmt_t =
  | S_seq of stmt * stmt
  | S_assign of var * expr
  | S_skip
  | S_drop of var list
  | S_swap
  | S_dig
  | S_dug
  | S_if of var * stmt * stmt
  | S_if_none of var * stmt * stmt
  | S_if_left of var * stmt * stmt
  | S_if_cons of var * stmt * stmt
  | S_loop of var * stmt
  | S_loop_left of var * stmt
  | S_map of var * stmt
  | S_iter of var * stmt
  | S_failwith of var
  | S_return of var

and stmt = { id : int; stm : stmt_t }

and program = typ * typ * stmt

module Data : sig
  include Comparable.S with type t = data

  include Sexpable.S with type t = data

  val to_string : t -> string
end

module Expr : sig
  include Comparable.S with type t = expr

  include Sexpable.S with type t = expr

  val to_string : t -> string
end

module Stmt : sig
  include Comparable.S with type t = stmt

  include Sexpable.S with type t = stmt
end

val create_stmt : stmt_t -> stmt

val simpl : stmt -> stmt
