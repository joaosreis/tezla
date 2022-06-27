From Coq Require Import ZArith.
From Coq Require Import Bool.Bool.
From Coq Require Import Strings.String.

From Tezla Require Import Edo_adt.

Definition adt_typ : Type := Adt.typ.

Inductive data_t : Type :=
| D_int : Z -> data_t
| D_string : string -> data_t
(* | D_bytes of Bytes.t *)
| D_unit
| D_bool : bool -> data_t
| D_pair : data -> data -> data_t

with data : Type :=
  data_raw : adt_typ -> data_t -> data.
