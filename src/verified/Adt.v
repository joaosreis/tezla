From Coq Require Import ZArith.
From Coq Require Import Bool.Bool.
From Coq Require Import Strings.String.

From Tezla Require Import Edo_adt.
From Tezla Require Import Common_adt.
From Tezla Require Import Var.
From Tezla Require Import Typ.

Definition var (A: Typ.t) : Type := Var.t A.
Definition adt_typ : Type := Adt.typ.
Definition ttyp : Type := Typ.t.

Inductive data : Typ.t -> Type :=
| D_int : Z -> data int
| D_string : String.string -> data string
(* | D_bytes : Bytes.t *)
| D_unit : data unit
| D_bool : Datatypes.bool -> data bool
| D_pair {x y} : data x -> data y -> data (pair x y).

Inductive expr {self_type} : forall (X: Typ.t), Type :=
| E_var {x} : var x -> expr x
| E_push {x} : data x -> expr x
| E_car {x y} : var (pair x y) -> expr x
| E_cdr {x y} : var (pair x y) -> expr y
| E_abs : var nat -> expr int
| E_neg_nat : var nat -> expr int
| E_neg_int : var int -> expr int
| E_neg_bls12_381_g1 : var bls12_381_g1 -> expr bls12_381_g1
| E_neg_bls12_381_g2 : var bls12_381_g2 -> expr bls12_381_g2
| E_neg_bls12_381_fr : var bls12_381_fr -> expr bls12_381_fr
| E_not_bool : var bool -> expr bool
| E_not_nat : var nat -> expr int
| E_not_int : var int -> expr int
| E_add_nat : var nat -> var nat -> expr nat
| E_add_nat_int : var nat -> var int -> expr int
| E_add_int_nat : var int -> var nat -> expr int
| E_add_int : var int -> var int -> expr int
| E_add_timestamp_int : var timestamp -> var int -> expr timestamp
| E_add_int_timestamp : var int -> var timestamp -> expr timestamp
| E_add_mutez : var mutez -> var mutez -> expr mutez
| E_add_bls12_381_g1 : var bls12_381_g1 -> var bls12_381_g1 -> expr bls12_381_g1
| E_add_bls12_381_g2 : var bls12_381_g2 -> var bls12_381_g2 -> expr bls12_381_g2
| E_add_bls12_381_fr : var bls12_381_fr -> var bls12_381_fr -> expr bls12_381_fr
| E_sub_nat : var nat -> var nat -> expr int
| E_sub_nat_int : var nat -> var int -> expr int
| E_sub_int_nat : var int -> var nat -> expr int
| E_sub_int : var int -> var int -> expr int
| E_sub_timestamp_int : var timestamp -> var int -> expr timestamp
| E_sub_timestamp : var timestamp -> var timestamp -> expr int
| E_sub_mutez : var mutez -> var mutez -> expr mutez
| E_mul_nat : var nat -> var nat -> expr nat
| E_mul_nat_int : var nat -> var int -> expr int
| E_mul_int_nat : var int -> var nat -> expr int
| E_mul_int : var int -> var int -> expr int
| E_mul_mutez_nat : var mutez -> var nat -> expr mutez
| E_mul_nat_mutez : var nat -> var mutez -> expr mutez
| E_mul_bls12_381_g1_bls12_381_fr : var bls12_381_g1 -> var bls12_381_fr -> expr bls12_381_g1
| E_mul_bls12_381_g2_bls12_381_fr : var bls12_381_g2 -> var bls12_381_fr -> expr bls12_381_g2
| E_mul_bls12_381_fr_bls12_381_fr : var bls12_381_fr -> var bls12_381_fr -> expr bls12_381_fr
| E_mul_nat_bls12_381_fr : var nat -> var bls12_381_fr -> expr bls12_381_fr
| E_mul_int_bls12_381_fr : var int -> var bls12_381_fr -> expr bls12_381_fr
| E_mul_bls12_381_fr_nat : var bls12_381_fr -> var nat -> expr bls12_381_fr
| E_mul_bls12_381_fr_int : var bls12_381_fr -> var int -> expr bls12_381_fr
| E_ediv_nat : var nat -> var nat -> expr (option (pair int nat))
| E_ediv_nat_int : var nat -> var int -> expr (option (pair int nat))
| E_ediv_int_nat : var int -> var nat -> expr (option (pair int nat))
| E_ediv_int : var int -> var int -> expr (option (pair int nat))
| E_ediv_mutez_nat : var mutez -> var nat -> expr (option (pair mutez mutez))
| E_ediv_mutez : var mutez -> var mutez -> expr (option (pair nat mutez))
| E_lsl : var nat -> var nat -> expr nat
| E_lsr : var nat -> var nat -> expr nat
| E_and_bool : var bool -> var bool -> expr bool
| E_and_nat : var nat -> var nat -> expr nat
| E_and_int_nat : var int -> var nat -> expr nat
| E_or_bool : var bool -> var bool -> expr bool
| E_or_nat : var nat -> var nat -> expr nat
| E_xor_bool : var bool -> var bool -> expr bool
| E_xor_nat : var nat -> var nat -> expr nat
| E_eq : var int -> expr bool
| E_neq : var int -> expr bool
| E_lt : var int -> expr bool
| E_gt : var int -> expr bool
| E_leq : var int -> expr bool
| E_geq : var int -> expr bool
(* | E_compare : var -> var -> expr TODO: *)
| E_cons {x} : var x -> var (list x) -> expr (list x)
| E_create_contract_operation {x} : program x -> var (option key_hash) -> var (mutez) -> var x -> expr operation
| E_create_contract_address {x} : program x -> var (option key_hash) -> var (mutez) -> var x -> expr address
| E_transfer_tokens {x} : var x -> var mutez -> var (contract x) -> expr operation
| E_set_delegate : var (option key_hash) -> expr operation
| E_create_account_operation : var key_hash -> var (option key_hash) -> var bool -> var mutez -> expr operation
| E_create_account_address : var key_hash -> var (option key_hash) -> var bool -> var mutez -> expr address
| E_unit : expr unit
| E_pair {x y} : var x -> var y -> expr (pair x y)
(* | E_pair_n : list var -> expr *)
| E_left {x} (y : Typ.t) : var x -> expr (or x y)
| E_right {x} (y : Typ.t) : var y -> expr (or y x)
| E_some {x} : var x -> expr (option x)
| E_none (x : Typ.t) : expr (option x)
| E_mem_set {x : comparable_type} : var x -> var (set x) -> expr bool
| E_mem_map {x : comparable_type} {y} : var x -> var (map x y) -> expr bool
| E_mem_big_map {x : comparable_type} {y} : var x -> var (big_map x y) -> expr bool
| E_get_map {x : comparable_type} {y} : var x -> var (map x y) -> expr y
| E_get_big_map {x : comparable_type} {y} : var x -> var (big_map x y) -> expr y
| E_update_set {x : comparable_type} : var x -> var bool -> var (set x) -> expr (set x)
| E_update_map {x : comparable_type} {y} : var x -> var (option y) -> var (map x y) -> expr (map x y)
| E_update_big_map {x : comparable_type} {y} : var x -> var (option y) -> var (map x y) -> expr (map x y)
| E_concat_string : var string -> var string -> expr string
| E_concat_bytes : var bytes -> var bytes -> expr bytes
| E_concat_list_string : var (list string) -> expr string
| E_concat_list_bytes : var (list bytes) -> expr bytes
| E_slice_string : var nat -> var nat -> var string -> expr (option string)
| E_slice_bytes : var nat -> var nat -> var bytes -> expr (option bytes)
| E_pack {x} : var x -> expr bytes
| E_unpack x : var bytes -> expr (option x)
| E_self : expr self_type
| E_contract_of_address x : var address -> expr (option (contract x))
| E_implicit_account : var key_hash -> expr (contract unit)
| E_now : expr timestamp
| E_amount : expr mutez
| E_balance : expr mutez
| E_check_signature : var key -> var signature -> var bytes -> expr bool
| E_blake2b : var bytes -> expr bytes
| E_sha256 : var bytes -> expr bytes
| E_sha512 : var bytes -> expr bytes
| E_hash_key : var key -> expr key_hash
| E_source : expr address
| E_sender : expr address
| E_address_of_contract {x} : var (contract x) -> expr address
| E_unlift_option {x} : var (option x) -> expr x
| E_unlift_or_left {x y} : var (or x y) -> expr x
| E_unlift_or_right {x y} : var (or x y) -> expr y
| E_hd {x} : var (list x) -> expr x
| E_tl {x} : var (list x) -> expr (list x)
| E_size_list {x} : var (list x) -> expr nat
| E_size_set {x : comparable_type} : var (set x) -> expr nat
| E_size_map {x : comparable_type} {y} : var (map x y) -> expr nat
| E_size_string : var string -> expr nat
| E_size_bytes : var bytes -> expr nat
| E_isnat : var int-> expr (option nat)
| E_int_of_nat : var nat -> expr int
| E_chain_id : expr chain_id
| E_lambda x y : var x -> stmt -> expr (lambda x y)
| E_exec {x y} : var x -> var (lambda x y) -> expr y
| E_dup {x} : var x -> expr x
| E_nil x : expr (list x)
| E_empty_set x : expr (set x)
| E_empty_map x y : expr (map x y)
| E_empty_big_map x y : expr (big_map x y)
| E_apply {x y z} : var x -> var (lambda (pair x y) z) -> expr (lambda y z)
| E_list_append {x} : var (list x) -> var (list x) -> expr (list x)
| E_special_empty_list x : expr (list x)
| E_special_empty_map x y : expr (map x y)
| E_voting_power : var key_hash -> expr nat
| E_keccak : var bytes -> expr bytes
| E_sha3 : var bytes -> expr bytes
| E_total_voting_power : expr nat
| E_pairing_check : var (list (pair bls12_381_g1 bls12_381_g2)) -> expr bool
| E_sapling_verify_update {ms} : var (sapling_transaction ms)  -> var (sapling_state ms) -> expr (option (pair int (sapling_state ms)))
| E_sapling_empty_state ms : expr (sapling_state ms)
| E_ticket {x : comparable_type} : var x -> var nat -> expr (ticket x)
| E_read_ticket_pair {x} : var (ticket x) -> expr (pair address (pair x nat))
| E_read_ticket_ticket {x} : var (ticket x) -> expr (ticket x)
| E_split_ticket {x} : var (ticket x) -> var (pair nat nat) -> expr (option (pair (ticket x) (ticket x)))
| E_join_ticket {x} : var (pair (ticket x) (ticket x)) -> expr (option (ticket x))
| E_self_address : expr address
| E_level : expr nat
| E_get_and_update_val {k : comparable_type} {v} : var k -> var (option v) -> var (map k v) -> expr (option v)
| E_get_and_update_map {k : comparable_type} {v} : var k -> var (option v) -> var (map k v) -> expr (map k v)
| E_dup_n {x} (n : Datatypes.nat) : var x -> expr x
| E_get_zero {x} : var x -> expr x
(* | E_get_n {x} n : var (pair ) -> expr *)
(* | E_update_n : Z -> var -> var -> expr *)

with stmt {self_type} : Type :=
| S_seq : stmt -> stmt -> stmt
| S_assign {x} : String.string -> expr x -> stmt
| S_skip: stmt
| S_drop {x} : Datatypes.list (var x) -> stmt
| S_swap: stmt
| S_dig : Z -> stmt
| S_dug : Z -> stmt
| S_if : var bool -> stmt -> stmt -> stmt
| S_if_none {x} : var (option x) -> stmt -> stmt -> stmt
| S_if_left {x y} : var (or x y) -> stmt -> stmt -> stmt
| S_if_cons {x} : var (list x) -> stmt -> stmt -> stmt
| S_loop : var bool -> stmt -> stmt
| S_loop_left {x y} : var (or x y) -> stmt -> stmt
| S_map_list {x} : var (list x) -> stmt -> stmt
| S_map_map {x y} : var (map x y) -> stmt -> stmt
| S_iter_set {x} : var (set x) -> stmt -> stmt
| S_iter_list {x} : var (list x) -> stmt -> stmt
| S_iter_map {x y} : var (map x y) -> stmt -> stmt
| S_failwith {x} : var x -> stmt
| S_return {x} : var x -> stmt

with program {self_type} : forall (x: Typ.t), Type :=
| Program : stmt -> program self_type.
