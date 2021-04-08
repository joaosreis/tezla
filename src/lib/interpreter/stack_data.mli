open Adt

type t_key_hash = Key_hash of string

type t_address =
  | Address : string -> t_address
  | Address_contract : 'a t_contract -> t_address

and 'a t_contract =
  | Contract_addr of t_address
  | Contract_key_hash of t_key_hash

type t_bool = Bool of bool

type t_bytes = Bytes of bytes

type t_chain_id = Chain_id of bytes

type t_int = Int of Bigint.t

type t_key = Key of string

type (_, _) t_lambda = Lambda of stmt

type t_mutez = Mutez of Int64.t

type t_nat = Nat of Bigint.t

type t_operation = Operation of operation

type t_signature = Signature of string

type t_string = String of string

type t_timestamp = Timestamp of Bigint.t

type t_unit = Unit

type ('a, 'b) t_pair = 'a t * 'b t

and _ t =
  | SD_address : t_address -> t_address t
  | SD_big_map : ('a, 'b) t_big_map -> ('a, 'b) t_big_map t
  | SD_bool : t_bool -> t_bool t
  | SD_bytes : t_bytes -> t_bytes t
  | SD_chain_id : t_chain_id -> t_chain_id t
  | SD_contract : 'a t_contract -> 'a t_contract t
  | SD_int : t_int -> t_int t
  | SD_key : t_key -> t_key t
  | SD_key_hash : t_key_hash -> t_key_hash t
  | SD_lambda : ('a, 'b) t_lambda -> ('a, 'b) t_lambda t
  | SD_list : 'a t_list -> 'a t_list t
  | SD_map : ('a, 'b) t_map -> ('a, 'b) t_map t
  | SD_mutez : t_mutez -> t_mutez t
  | SD_nat : t_nat -> t_nat t
  | SD_operation : t_operation -> t_operation t
  | SD_option : 'a t_option -> 'a t_option t
  | SD_or : ('a, 'b) t_or -> ('a, 'b) t_or t
  | SD_pair : ('a, 'b) t_pair -> ('a, 'b) t_pair t
  | SD_set : 'a t_set -> 'a t_set t
  | SD_signature : t_signature -> t_signature t
  | SD_string : t_string -> t_string t
  | SD_timestamp : t_timestamp -> t_timestamp t
  | SD_unit : t_unit t

and (_, _) t_big_map =
  | Big_map_nil : ('a, 'b) t_big_map
  | Big_map_cons : ('a t * 'b t) * ('a, 'b) t_big_map -> ('a, 'b) t_big_map

and _ t_list = L_nil : 'a t_list | L_cons : 'a t * 'a t_list -> 'a t_list

and (_, _) t_map =
  | Map_nil : ('a, 'b) t_map
  | Map_cons : ('a t * 'b t) * ('a, 'b) t_map -> ('a, 'b) t_map

and _ t_option = O_none : 'a t_option | O_some : 'a t -> 'a t_option

and (_, _) t_or =
  | Or_left : 'l t -> ('l, 'r) t_or
  | Or_right : 'r t -> ('l, 'r) t_or

and _ t_set = Set_nil : 'a t_set | Set_cons : 'a t * 'a t_set -> 'a t_set

and _ typ =
  | Address_t : t_address typ
  | Big_map_t : 'a typ * 'b typ -> ('a, 'b) t_big_map typ
  | Bool_t : t_bool typ
  | Bytes_t : t_bytes typ
  | Chain_id_t : t_chain_id typ
  | Contract_t : 'a typ -> 'a t_contract typ
  | Int_t : t_int typ
  | Key_t : t_key typ
  | Key_hash_t : t_key_hash typ
  | Lambda_t : 'a typ * 'b typ -> ('a, 'b) t_lambda typ
  | List_t : 'a typ -> 'a t_list typ
  | Map_t : 'a typ * 'b typ -> ('a, 'b) t_map typ
  | Mutez_t : t_mutez typ
  | Nat_t : t_nat typ
  | Operation_t : t_operation typ
  | Option_t : 'a typ -> 'a t_option typ
  | Or_t : 'a typ * 'b typ -> ('a, 'b) t_or typ
  | Pair_t : 'a typ * 'b typ -> ('a, 'b) t_pair typ
  | Set_t : 'a typ -> 'a t_set typ
  | Signature_t : t_signature typ
  | String_t : t_string typ
  | Timestamp_t : t_timestamp typ
  | Unit_t : t_unit typ

type e_typ = E_T : 'a typ -> e_typ

val typ_from_adt_typ : Adt.typ -> e_typ

val from_adt_data : 'a typ -> Adt.data -> 'a t

val equal : 'a t -> 'a t -> bool

val cast : 'a typ -> 'b t -> 'a t

val cast_map_key : 'a typ -> (_, 'b) t_map -> ('a, 'b) t_map

val cast_big_map_key : 'a typ -> (_, 'b) t_big_map -> ('a, 'b) t_big_map

val to_string : 'a t -> string
