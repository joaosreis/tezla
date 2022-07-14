From Coq Require Import Strings.String.

From Tezla Require Import Typ.

Inductive t (A : Typ.t) : Type :=
| var (s : String.string) : t A.
