From Tezla Require Import Common_adt.
From Coq Require Import Lists.List.
From Coq Require Import Init.Datatypes.
From Coq Require Import ZArith.

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
