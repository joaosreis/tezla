From Coq Require Import Strings.String.

Module Annot.

  Inductive t : Type :=
  | A_type : string -> t
  | A_var : string -> t
  | A_field : string -> t.

End Annot.
