From Tezla Require Import Common_adt.

From Coq Require Import Lists.List.
From Coq Require Import Init.Datatypes.
From Coq Require Import ZArith.

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
