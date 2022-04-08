open! Core

type t = { var_name : string; var_type : Edo_adt.Typ.t Lazy.t }

include Comparable.S with type t := t
include Sexpable.S with type t := t

val to_string : t -> string
