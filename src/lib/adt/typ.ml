open Core_kernel

module T = struct
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
  [@@deriving ord, sexp]
end

include T
include Comparable.Make (T)

let rec to_string = function
  | T_int -> "int"
  | T_nat -> "nat"
  | T_string -> "string"
  | T_bytes -> "bytes"
  | T_mutez -> "mutez"
  | T_bool -> "bool"
  | T_key_hash -> "key_hash"
  | T_timestamp -> "timestamp"
  | T_address -> "address"
  | T_key -> "key"
  | T_unit -> "unit"
  | T_signature -> "signature"
  | T_option t -> [%string "(option %{to_string t})"]
  | T_list t -> [%string "(list %{to_string t})"]
  | T_set t -> [%string "(set %{to_string t})"]
  | T_operation -> "operation"
  | T_contract t -> [%string "(contract %{to_string t})"]
  | T_pair (t_1, t_2) -> [%string "(pair %{to_string t_1} %{to_string t_2})"]
  | T_or (t_1, t_2) -> [%string "(or %{to_string t_1} %{to_string t_2})"]
  | T_lambda (t_1, t_2) ->
      [%string "(lambda %{to_string t_1} %{to_string t_2})"]
  | T_map (t_1, t_2) -> [%string "(map %{to_string t_1} %{to_string t_2})"]
  | T_big_map (t_1, t_2) ->
      [%string "(big_map %{to_string t_1} %{to_string t_2})"]
  | T_chain_id -> "chain_id"
