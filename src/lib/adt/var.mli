open! Containers

type t = { var_name : string; var_type : Edo_adt.Typ.t } [@@deriving eq, ord]

include Set.OrderedType with type t := t

val to_string : t -> string
