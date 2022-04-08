open! Core
module Var = Var
module Operation = Operation
module Node = Common_adt.Node

type var = Var.t [@@deriving ord, sexp]
type adt_typ = Edo_adt.Adt.typ [@@deriving ord, sexp]
type ttyp = Edo_adt.Typ.t [@@deriving ord, sexp]
type operation = Operation.t [@@deriving ord, sexp]
type 'a node = 'a Node.t [@@deriving ord, sexp]

module Id () = struct
  let create_id_counter () = ref (-1)
  let id_counter = create_id_counter ()

  let next_id () =
    let () = id_counter := !id_counter + 1 in
    !id_counter
end

module type Common = sig
  type t'
  type t = t' node

  val create : ?location:Common_adt.Loc.t -> t' -> t
  val to_string : t -> string

  include Sexpable.S with type t := t
  include Comparable.S with type t := t
end

module Make_common (T : sig
  type t'
  type t = t' node [@@deriving ord, sexp]

  val to_string : t -> string
end) : Common with type t' = T.t' and type t = T.t = struct
  include T
  include Comparable.Make (T)
  include Id ()

  let create = Node.create (next_id ())
  let to_string = T.to_string
end

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
[@@deriving ord, sexp]

and data = (adt_typ * data_t) node [@@deriving ord, sexp]

and expr_t =
  | E_var of var
  | E_push of data
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
  | E_left of var * adt_typ
  | E_right of var * adt_typ
  | E_some of var
  | E_none of adt_typ
  | E_mem of var * var
  | E_get of var * var
  | E_update of var * var * var
  | E_concat of var * var
  | E_concat_list of var
  | E_slice of var * var * var
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
  | E_lambda of adt_typ * adt_typ * var * stmt
  | E_exec of var * var
  | E_dup of var
  | E_nil of adt_typ
  | E_empty_set of adt_typ
  | E_empty_map of adt_typ * adt_typ
  | E_empty_big_map of adt_typ * adt_typ
  | E_apply of var * var
  | E_append of var * var
  | E_special_empty_list of ttyp
  | E_special_empty_map of ttyp * ttyp
  | E_create_account_operation of var * var * var * var
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
[@@deriving ord, sexp]

and expr = expr_t node [@@deriving ord, sexp]

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
[@@deriving ord, sexp]

and stmt = stmt_t node [@@deriving ord, sexp]
and program = adt_typ * adt_typ * stmt [@@deriving ord, sexp]

module Data = Make_common (struct
  type t' = adt_typ * data_t
  type t = data [@@deriving ord, sexp]

  let rec to_string d =
    match snd d.Node.value with
    | D_int d -> Bigint.to_string d
    | D_string s -> s
    | D_bytes b -> [%string "%{b#Bytes}"]
    | D_left d -> [%string "Left %{to_string d}"]
    | D_right d -> [%string "Right %{to_string d}"]
    | D_some d -> [%string "Some %{to_string d}"]
    | D_none -> "None"
    | D_unit -> "Unit"
    | D_bool b -> ( match b with true -> "True" | false -> "False")
    | D_pair (d_1, d_2) -> [%string "(Pair %{to_string d_1} %{to_string d_2})"]
    | D_list d -> List.to_string ~f:to_string d
    | D_map d ->
        List.to_string
          ~f:(fun (d_1, d_2) ->
            [%string "Elt %{to_string d_1} %{to_string d_2}"])
          d
    | D_instruction _ -> "{ ... }"
end)

module Expr = Make_common (struct
  type t' = expr_t
  type t = expr [@@deriving ord, sexp]

  let to_string e =
    let typ_to_string t =
      Edo_adt.Pp.pp_typ Format.str_formatter t;
      Format.flush_str_formatter ()
    in
    match e.Node.value with
    | E_var v -> Var.to_string v
    | E_push d -> [%string "PUSH %{d#Data}"]
    | E_car e -> [%string "CAR %{e#Var}"]
    | E_cdr e -> [%string "CDR %{e#Var}"]
    | E_abs e -> [%string "ABS %{e#Var}"]
    | E_neg e -> [%string "NEG %{e#Var}"]
    | E_not e -> [%string "NOT %{e#Var}"]
    | E_eq e -> [%string "EQ %{e#Var}"]
    | E_neq e -> [%string "NEQ %{e#Var}"]
    | E_lt e -> [%string "LT %{e#Var}"]
    | E_gt e -> [%string "GT %{e#Var}"]
    | E_leq e -> [%string "LEQ %{e#Var}"]
    | E_geq e -> [%string "GEQ %{e#Var}"]
    | E_left (e, t) -> [%string "LEFT %{typ_to_string t} %{e#Var}"]
    | E_right (e, t) -> [%string "RIGHT %{typ_to_string t} %{e#Var}"]
    | E_some e -> [%string "SOME %{e#Var}"]
    | E_pack e -> [%string "PACK %{e#Var}"]
    | E_implicit_account e -> [%string "IMPLICIT_ACCOUNT %{e#Var}"]
    | E_blake2b e -> [%string "BLAKE2B %{e#Var}"]
    | E_sha256 e -> [%string "SHA256 %{e#Var}"]
    | E_sha512 e -> [%string "SHA512 %{e#Var}"]
    | E_hash_key e -> [%string "HASH_KEY %{e#Var}"]
    | E_unit -> "UNIT"
    | E_none t -> [%string "NONE %{typ_to_string t}"]
    | E_add (v_1, v_2) -> [%string "ADD %{v_1#Var} %{v_2#Var}"]
    | E_sub (v_1, v_2) -> [%string "SUB %{v_1#Var} %{v_2#Var}"]
    | E_mul (v_1, v_2) -> [%string "MUL %{v_1#Var} %{v_2#Var}"]
    | E_div (v_1, v_2) -> [%string "EDIV %{v_1#Var} %{v_2#Var}"]
    | E_shiftL (v_1, v_2) -> [%string "LSL %{v_1#Var} %{v_2#Var}"]
    | E_shiftR (v_1, v_2) -> [%string "LSR %{v_1#Var} %{v_2#Var}"]
    | E_and (v_1, v_2) -> [%string "AND %{v_1#Var} %{v_2#Var}"]
    | E_or (v_1, v_2) -> [%string "OR %{v_1#Var} %{v_2#Var}"]
    | E_xor (v_1, v_2) -> [%string "XOR %{v_1#Var} %{v_2#Var}"]
    | E_compare (v_1, v_2) -> [%string "COMPARE %{v_1#Var} %{v_2#Var}"]
    | E_cons (v_1, v_2) -> [%string "CONS %{v_1#Var} %{v_2#Var}"]
    | E_pair (v_1, v_2) -> [%string "PAIR %{v_1#Var} %{v_2#Var}"]
    | E_mem (v_1, v_2) -> [%string "MEM %{v_1#Var} %{v_2#Var}"]
    | E_get (v_1, v_2) -> [%string "GET %{v_1#Var} %{v_2#Var}"]
    | E_concat (v_1, v_2) -> [%string "CONCAT %{v_1#Var} %{v_2#Var}"]
    | E_concat_list e -> [%string "CONCAT %{e#Var}"]
    | E_update (v_1, v_2, v_3) ->
        [%string "UPDATE %{v_1#Var} %{v_2#Var} %{v_3#Var}"]
    | E_slice (v_1, v_2, v_3) ->
        [%string "SLICE %{v_1#Var} %{v_2#Var} %{v_3#Var}"]
    | E_check_signature (v_1, v_2, v_3) ->
        [%string "CHECK_SIGNATURE %{v_1#Var} %{v_2#Var} %{v_3#Var}"]
    | E_unpack (t, v) -> [%string "UNPACK %{typ_to_string  t} %{v#Var}"]
    | E_self -> [%string "SELF"]
    | E_now -> [%string "NOW"]
    | E_amount -> [%string "AMOUNT"]
    | E_balance -> [%string "BALANCE"]
    | E_source -> [%string "SOURCE"]
    | E_sender -> [%string "SENDER"]
    | E_address_of_contract e -> [%string "ADDRESS %{e#Var}"]
    | E_unlift_option e -> [%string "unlift_option %{e#Var}"]
    | E_unlift_or_left e -> [%string "unlift_or_left %{e#Var}"]
    | E_unlift_or_right e -> [%string "unlift_or_right %{e#Var}"]
    | E_hd e -> [%string "hd %{e#Var}"]
    | E_tl e -> [%string "tl %{e#Var}"]
    | E_isnat e -> [%string "ISNAT %{e#Var}"]
    | E_int_of_nat e -> [%string "INT %{e#Var}"]
    | E_chain_id -> "CHAIN_ID"
    | E_lambda (t_1, t_2, v, _) ->
        [%string
          "LAMBDA %{typ_to_string  t_1} %{typ_to_string t_2} (%{v#Var} => { \
           ... })"]
    | E_exec (v_1, v_2) -> [%string "EXEC %{v_1#Var} %{v_2#Var}"]
    | E_contract_of_address (t, v) ->
        [%string "CONTRACT %{typ_to_string  t} %{v#Var}"]
    | E_create_contract_address (_, v_1, v_2, v_3) ->
        [%string "CREATE_CONTRACT {...} %{v_1#Var} %{v_2#Var} %{v_3#Var}"]
    | E_operation o -> Operation.to_string o
    | E_dup v -> [%string "DUP %{v#Var}"]
    | E_nil t -> [%string "NIL %{typ_to_string t}"]
    | E_empty_set t -> [%string "EMPTY_SET %{typ_to_string t}"]
    | E_empty_map (t_k, t_v) ->
        [%string "EMPTY_MAP %{typ_to_string t_k} %{typ_to_string t_v}"]
    | E_empty_big_map (t_k, t_v) ->
        [%string "EMPTY_BIG_MAP %{typ_to_string t_k} %{typ_to_string t_v}"]
    | E_apply (v_1, v_2) -> [%string "APPLY %{v_1#Var} %{v_2#Var}"]
    | E_append (v_1, v_2) -> [%string "append(%{v_1#Var}, %{v_2#Var})"]
    | E_special_empty_list _ -> "{  }"
    | E_special_empty_map _ -> "{  }"
    | _ -> (*TODO: *) assert false
end)

module Stmt = struct
  module T = struct
    type t' = stmt_t
    type t = stmt [@@deriving ord, sexp]

    let to_string s = Int.to_string s.Node.id
  end

  include Make_common (T)

  let rec simpl s =
    match s.Node.value with
    | S_seq ({ value = S_skip; _ }, s) | S_seq (s, { value = S_skip; _ }) ->
        simpl s
    | S_seq (s_1, s_2) -> { s with value = S_seq (simpl s_1, simpl s_2) }
    | S_if (c, s_1, s_2) -> { s with value = S_if (c, simpl s_1, simpl s_2) }
    | S_if_cons (c, s_1, s_2) ->
        { s with value = S_if_cons (c, simpl s_1, simpl s_2) }
    | S_if_left (c, s_1, s_2) ->
        { s with value = S_if_left (c, simpl s_1, simpl s_2) }
    | S_if_none (c, s_1, s_2) ->
        { s with value = S_if_none (c, simpl s_1, simpl s_2) }
    | S_loop (c, s) -> { s with value = S_loop (c, simpl s) }
    | S_loop_left (c, s) -> { s with value = S_loop_left (c, simpl s) }
    | S_iter (c, s) -> { s with value = S_iter (c, simpl s) }
    | S_map (x, s) -> { s with value = S_map (x, simpl s) }
    | S_skip | S_swap | S_dig | S_dug | S_assign _ | S_drop _ | S_failwith _
    | S_return _ ->
        s
end
