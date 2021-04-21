open Core_kernel

type t = { var_name : string; var_type : Typ.t }

include Comparable.S with type t := t

include Sexpable.S with type t := t

val to_string : t -> string
