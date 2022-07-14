From Coq Require Import Strings.String.
From Coq Require Import ZArith.

Module Annot.

  Inductive t : Type :=
  | A_type : string -> t
  | A_var : string -> t
  | A_field : string -> t.

End Annot.

Module Node.
  Record t (A: Type) : Type := create { id : Z; value : A; }.
End Node.
