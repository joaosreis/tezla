From Coq Require Import Strings.String.

Record t := {
    var_name : string;
  }.

Definition to_string (t: t) : string :=
  var_name t.
