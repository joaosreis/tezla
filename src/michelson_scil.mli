type unop = Fst | Snd | Abs | Neg | Not

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | ShiftL
  | ShiftR
  | And
  | Or
  | Xor
  | Neq
  | Lt
  | Gt
  | Leq
  | Geq
  | Compare

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

type expr =
  | E_unop of unop * expr
  | E_binop of binop * expr * expr
  | E_ident of string
  | E_cons of expr * expr
  | E_int of Z.t
  | E_nat of Z.t
  | E_string of string
  | E_timestamp of string
  | E_signature of string
  | E_key of string
  | E_key_hash of string
  | E_mutez of string
  | E_contract of string
  | E_unit
  | E_bool of bool
  | E_pair of expr * expr
  | E_left of expr
  | E_right of expr
  | E_some of expr
  | E_none
  | E_list of expr list
  | E_set of expr list
  | E_map of (expr * expr) list
  | E_stmt of stmt
  | E_mem of expr * expr
  | E_get of expr * expr
  | E_update of expr * expr * expr
  | E_cast of expr
  | E_concat of expr * expr
  | E_slice of expr * expr * expr
  | E_pack of expr
  | E_unpack of expr
  | E_self
  | E_contract_of_address of expr
  | E_set_delegate of expr
  | E_create_account of expr * expr * expr * expr
  | E_implicit_account of expr
  | E_now
  | E_amount
  | E_balance
  | E_check_signature of expr * expr * expr
  | E_blake2b of expr
  | E_sha256 of expr
  | E_sha512 of expr
  | E_hash_key of expr
  | E_steps_to_quota
  | E_source
  | E_sender
  | E_address_of_contact of expr
  | E_is_none of expr
  | E_lift_option of expr
  | E_is_left of expr
  | E_lift_or of expr
  | E_is_list_empty of expr
  | E_list_hd of expr
  | E_list_tl of expr
  | E_size of expr

and stmt =
  | S_seq of stmt * stmt
  | S_var_decl of string
  | S_assign of string * expr
  | S_skip
  | S_if of expr * stmt * stmt
  | S_while of expr * stmt
  | S_if_cons of stmt * stmt
  | S_size
  | S_empty_set of comparable_type
  | S_empty_map of comparable_type * typ
  | S_map of stmt
  | S_iter of stmt
  | S_mem
  | S_get
  | S_update
  | S_loop of stmt
  | S_loop_left of stmt
  | S_lambda of typ * typ * stmt
  | S_exec
  | S_dip of stmt
  | S_failwith of expr
  | S_cast
  | S_rename
  | S_concat
  | S_slice
  | S_pack
  | S_unpack
  | S_add
  | S_sub
  | S_mul
  | S_ediv
  | S_abs
  | S_neg
  | S_lsl
  | S_lsr
  | S_or
  | S_and
  | S_xor
  | S_not
  | S_compare
  | S_eq
  | S_neq
  | S_lt
  | S_gt
  | S_le
  | S_ge
  | S_self
  | S_contract of typ
  | S_transfer_tokens
  | S_set_delegate
  | S_create_account
  | S_create_contract of stmt
  | S_implicit_account
  | S_now
  | S_amount
  | S_balance
  | S_check_signature
  | S_blake2b
  | S_sha256
  | S_sha512
  | S_hash_key
  | S_steps_to_quota
  | S_source
  | S_sender
  | S_address
  | S_todo
