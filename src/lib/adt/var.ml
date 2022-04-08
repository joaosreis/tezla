open! Core

module T = struct
  type t = { var_name : string; var_type : Edo_adt.Typ.t Lazy.t }
  [@@deriving ord, sexp]
end

include T
include Comparable.Make (T)

let to_string { var_name; _ } = var_name
