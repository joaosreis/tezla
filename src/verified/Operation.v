From Tezla Require Import Var.
From Tezla Require Import Edo_adt.

Definition var : Type := Var.t.

Inductive t : Type :=
| O_create_contract : Typed_adt.program -> var -> var -> var -> var -> t
| O_transfer_tokens : var -> var -> var -> t
| O_set_delegate : var -> t
| O_create_account : var -> var -> var -> var -> t.
