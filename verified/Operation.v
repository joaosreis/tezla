From Tezla Require Import Var.
From Tezla Require Import Edo_adt.

Definition var : Type := Var.t.

Inductive t : Type :=
| O_create_contract : Typed_adt.program -> var -> var -> var -> var -> t.
