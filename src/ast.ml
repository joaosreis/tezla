type comparable_type =
  | T_int
  | T_nat
  | T_string
  | T_bytes
  | T_mutez
  | T_bool
  | T_key_hash
  | T_timestamp
  | T_address

type typ =
  | T_comparable of comparable_type
  | T_key
  | T_unit
  | T_signature
  | T_option of typ
  | T_list of typ
  | T_set of comparable_type
  | T_operation
  | T_contract of typ
  | T_pair of typ * typ
  | T_or of typ * typ
  | T_lambda of typ * typ
  | T_map of comparable_type * typ
  | T_big_map of comparable_type * typ

type instruction =
  | I_seq of instruction * instruction
  | I_drop
  | I_dup
  | I_swap
  | I_push of typ * data
  | I_some
  | I_none of typ
  | I_unit
  | I_if_none of instruction * instruction
  | I_pair
  | I_car
  | I_cdr
  | I_left of typ
  | I_right of typ
  | I_if_left of instruction * instruction
  | I_if_right of instruction * instruction
  | I_nil of typ
  | I_cons
  | I_if_cons of instruction * instruction
  | I_size
  | I_empty_set of comparable_type
  | I_empty_map of comparable_type * typ
  | I_map of instruction
  | I_iter of instruction
  | I_mem
  | I_get
  | I_update
  | I_if of instruction * instruction
  | I_loop of instruction
  | I_loop_left of instruction
  | I_lambda of typ * typ * instruction
  | I_exec
  | I_dip of instruction
  | I_failwith of data
  | I_cast
  | I_rename
  | I_concat
  | I_slice
  | I_pack
  | I_unpack
  | I_add
  | I_sub
  | I_mul
  | I_ediv
  | I_abs
  | I_neg
  | I_lsl
  | I_lsr
  | I_or
  | I_and
  | I_xor
  | I_not
  | I_compare
  | I_eq
  | I_neq
  | I_lt
  | I_gt
  | I_le
  | I_ge
  | I_self
  | I_contract of typ
  | I_transfer_tokens
  | I_set_delegate
  | I_create_account
  | I_create_contract of instruction
  | I_implicit_account
  | I_now
  | I_amount
  | I_balance
  | I_check_signature
  | I_blake2b
  | I_sha256
  | I_sha512
  | I_hash_key
  | I_steps_to_quota
  | I_source
  | I_sender
  | I_address

and data =
  | D_int of Z.t
  | D_nat of Z.t
  | D_string of string
  | D_timestamp of string
  | D_signature of string
  | D_key of string
  | D_key_hash of string
  | D_mutez of string
  | D_contract of string
  | D_unit
  | D_bool of bool
  | D_pair of data * data
  | D_left of data
  | D_right of data
  | D_some of data
  | D_none
  | D_list of data list
  | D_set of data list
  | D_map of (data * data) list
  | D_instruction of instruction
