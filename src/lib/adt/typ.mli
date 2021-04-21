open Core_kernel

type t =
  | T_key
  | T_unit
  | T_signature
  | T_option of t
  | T_list of t
  | T_set of t
  | T_operation
  | T_contract of t
  | T_pair of t * t
  | T_or of t * t
  | T_lambda of t * t
  | T_map of t * t
  | T_big_map of t * t
  | T_chain_id
  | T_int
  | T_nat
  | T_string
  | T_bytes
  | T_mutez
  | T_bool
  | T_key_hash
  | T_timestamp
  | T_address

include Comparable.S with type t := t

include Sexpable.S with type t := t

val to_string : t -> string
