From Coq Require Import Strings.String.
From Tezla Require Import Edo_adt.

Record t := {
    var_name : string;
    var_type : Typ.t;
  }.

Definition to_string (t: t) : string :=
  var_name t.
