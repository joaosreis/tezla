open! Containers

type t = {
  var_name : string;
  var_type : Edo_adt.Typ.t; [@compare fun _ _ -> 0] [@equal fun _ _ -> true]
}
[@@deriving ord, eq]

let to_string { var_name; _ } = var_name
