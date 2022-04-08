open! Core

exception Type_error
exception Non_pushable_type

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
type (_, _) t_lambda = Lambda of Adt.stmt
type t_mutez = Mutez of Int64.t
type t_nat = Nat of Bigint.t
type t_operation = Operation of Adt.operation
type t_signature = Signature of string
type t_string = String of string
type t_timestamp = Timestamp of Bigint.t
type t_unit = Unit
type t_never = Never
type t_bls12_381_g1 = Bls12_381_g1 of Bigint.t
type t_bls12_381_g2 = Bls12_381_g2 of Bigint.t
type t_bls12_381_fr = Bls12_381_fr of Bigint.t
type t_chest = Chest
type t_chest_key = Chest_key
type t_sapling_state = Sapling_state
type t_sapling_transaction = Sapling_transaction

type ('a, 'b) t_pair = Pair of 'a t * 'b t

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
  | SD_never : t_never t
  | SD_bls12_381_g1 : t_bls12_381_g1 -> t_bls12_381_g1 t
  | SD_bls12_381_g2 : t_bls12_381_g2 -> t_bls12_381_g2 t
  | SD_bls12_381_fr : t_bls12_381_fr -> t_bls12_381_fr t
  | SD_chest : t_chest t
  | SD_chest_key : t_chest_key t
  | SD_ticket : 'a t -> 'a t_ticket t
  | SD_sapling_state : t_sapling_state -> t_sapling_state t
  | SD_sapling_transaction : t_sapling_transaction t

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
and _ t_ticket = Ticket : 'a t -> 'a t_ticket

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
  | Never_t : t_never typ
  | Bls12_381_g1_t : t_bls12_381_g1 typ
  | Bls12_381_g2_t : t_bls12_381_g2 typ
  | Bls12_381_fr_t : t_bls12_381_fr typ
  | Chest_t : t_chest typ
  | Chest_key_t : t_chest_key typ
  | Ticket_t : 'a typ -> 'a t_ticket typ
  | Sapling_state_t : Bigint.t -> t_sapling_state typ
  | Sapling_transaction_t : Bigint.t -> t_sapling_transaction typ

type e_typ = E_T : 'a typ -> e_typ

let rec typ_from_adt_typ t =
  let open Edo_adt in
  let open Common_adt in
  match fst t.Node.value with
  | Adt.T_address -> E_T Address_t
  | T_key -> E_T Key_t
  | T_unit -> E_T Unit_t
  | T_signature -> E_T Signature_t
  | T_operation -> E_T Operation_t
  | T_chain_id -> E_T Chain_id_t
  | T_int -> E_T Int_t
  | T_nat -> E_T Nat_t
  | T_string -> E_T String_t
  | T_bytes -> E_T Bytes_t
  | T_mutez -> E_T Mutez_t
  | T_bool -> E_T Bool_t
  | T_key_hash -> E_T Key_hash_t
  | T_timestamp -> E_T Timestamp_t
  | T_option t ->
      let (E_T t) = typ_from_adt_typ t in
      E_T (Option_t t)
  | T_list t ->
      let (E_T t) = typ_from_adt_typ t in
      E_T (List_t t)
  | T_set t ->
      let (E_T t) = typ_from_adt_typ t in
      E_T (Set_t t)
  | T_contract t ->
      let (E_T t) = typ_from_adt_typ t in
      E_T (Contract_t t)
  | T_pair (t_1, t_2) ->
      let (E_T t_1) = typ_from_adt_typ t_1 in
      let (E_T t_2) = typ_from_adt_typ t_2 in
      E_T (Pair_t (t_1, t_2))
  | T_or (t_1, t_2) ->
      let (E_T t_1) = typ_from_adt_typ t_1 in
      let (E_T t_2) = typ_from_adt_typ t_2 in
      E_T (Or_t (t_1, t_2))
  | T_lambda (t_1, t_2) ->
      let (E_T t_1) = typ_from_adt_typ t_1 in
      let (E_T t_2) = typ_from_adt_typ t_2 in
      E_T (Lambda_t (t_1, t_2))
  | T_map (t_1, t_2) ->
      let (E_T t_1) = typ_from_adt_typ t_1 in
      let (E_T t_2) = typ_from_adt_typ t_2 in
      E_T (Map_t (t_1, t_2))
  | T_big_map (t_1, t_2) ->
      let (E_T t_1) = typ_from_adt_typ t_1 in
      let (E_T t_2) = typ_from_adt_typ t_2 in
      E_T (Big_map_t (t_1, t_2))
  | T_never -> E_T Never_t
  | T_bls12_381_g1 -> E_T Bls12_381_g1_t
  | T_bls12_381_g2 -> E_T Bls12_381_g2_t
  | T_bls12_381_fr -> E_T Bls12_381_fr_t
  | T_chest -> E_T Chest_t
  | T_chest_key -> E_T Chest_key_t
  | T_ticket t ->
      let (E_T t) = typ_from_adt_typ t in
      E_T (Ticket_t t)
  | T_sapling_transaction n -> E_T (Sapling_transaction_t n)
  | T_sapling_state n -> E_T (Sapling_state_t n)

let rec from_adt_data : type a. a typ -> Adt.data -> a t =
 fun t d ->
  match (t, snd d.value) with
  | Address_t, D_string s -> SD_address (Address s)
  | Bool_t, D_bool b -> SD_bool (Bool b)
  | Bytes_t, D_bytes b -> SD_bytes (Bytes b)
  | Chain_id_t, D_string s -> SD_chain_id (Chain_id (Bytes.of_string s))
  | Chain_id_t, D_bytes b -> SD_chain_id (Chain_id b)
  | Int_t, D_int n -> SD_int (Int n)
  | Key_t, D_string s -> SD_key (Key s)
  | Key_hash_t, D_string s -> SD_key_hash (Key_hash s)
  | Lambda_t (_, _), D_instruction (_, i) -> SD_lambda (Lambda i)
  | List_t t, D_list l ->
      let l = list_from_adt_data t l in
      SD_list l
  | Map_t (t_k, t_v), D_map l ->
      let m = map_from_adt_data t_k t_v l in
      SD_map m
  | Big_map_t (t_k, t_v), D_map l ->
      let m = big_map_from_adt_data t_k t_v l in
      SD_big_map m
  | Mutez_t, D_int n -> SD_mutez (Mutez (Bigint.to_int64_exn n))
  | Nat_t, D_int n -> SD_nat (Nat n)
  | Option_t _, D_none -> SD_option O_none
  | Option_t t, D_some d ->
      let d' = from_adt_data t d in
      SD_option (O_some d')
  | Or_t (t_l, _), D_left d ->
      let d' = from_adt_data t_l d in
      SD_or (Or_left d')
  | Or_t (_, t_r), D_right d ->
      let d' = from_adt_data t_r d in
      SD_or (Or_right d')
  | Pair_t (t_1, t_2), D_pair (d_1, d_2) ->
      let d_1' = from_adt_data t_1 d_1 in
      let d_2' = from_adt_data t_2 d_2 in
      SD_pair (Pair (d_1', d_2'))
  | Set_t t, D_list l ->
      let s = set_from_adt_data t l in
      SD_set s
  | Signature_t, D_string s -> SD_signature (Signature s)
  | String_t, D_string s -> SD_string (String s)
  | Timestamp_t, D_int n -> SD_timestamp (Timestamp n)
  | Timestamp_t, D_string _s ->
      (* TODO: timestamp string to num *)
      SD_timestamp (Timestamp Bigint.zero)
  | Unit_t, D_unit -> SD_unit
  | Bls12_381_g1_t, D_int n -> SD_bls12_381_g1 (Bls12_381_g1 n)
  | Bls12_381_g2_t, D_int n -> SD_bls12_381_g2 (Bls12_381_g2 n)
  | Bls12_381_fr_t, D_int n -> SD_bls12_381_fr (Bls12_381_fr n)
  | ( ( Sapling_state_t _ | Sapling_transaction_t _ | Ticket_t _ | Chest_t
      | Chest_key_t | Never_t | Big_map_t _ | Contract_t _ | Operation_t ),
      _ ) ->
      raise Non_pushable_type
  | ( Address_t,
      ( D_unit | D_none | D_int _ | D_bytes _ | D_bool _
      | D_pair (_, _)
      | D_left _ | D_right _ | D_some _ | D_map _ | D_list _ | D_instruction _
        ) )
  | ( Bool_t,
      ( D_unit | D_none | D_int _ | D_string _ | D_bytes _
      | D_pair (_, _)
      | D_left _ | D_right _ | D_some _ | D_map _ | D_list _ | D_instruction _
        ) )
  | ( Bytes_t,
      ( D_unit | D_none | D_int _ | D_string _ | D_bool _
      | D_pair (_, _)
      | D_left _ | D_right _ | D_some _ | D_map _ | D_list _ | D_instruction _
        ) )
  | ( Chain_id_t,
      ( D_unit | D_none | D_int _ | D_bool _
      | D_pair (_, _)
      | D_left _ | D_right _ | D_some _ | D_map _ | D_list _ | D_instruction _
        ) )
  | ( Int_t,
      ( D_unit | D_none | D_string _ | D_bytes _ | D_bool _
      | D_pair (_, _)
      | D_left _ | D_right _ | D_some _ | D_map _ | D_list _ | D_instruction _
        ) )
  | ( Key_t,
      ( D_unit | D_none | D_int _ | D_bytes _ | D_bool _
      | D_pair (_, _)
      | D_left _ | D_right _ | D_some _ | D_map _ | D_list _ | D_instruction _
        ) )
  | ( Key_hash_t,
      ( D_unit | D_none | D_int _ | D_bytes _ | D_bool _
      | D_pair (_, _)
      | D_left _ | D_right _ | D_some _ | D_map _ | D_list _ | D_instruction _
        ) )
  | ( Lambda_t (_, _),
      ( D_unit | D_none | D_int _ | D_string _ | D_bytes _ | D_bool _
      | D_pair (_, _)
      | D_left _ | D_right _ | D_some _ | D_map _ | D_list _ ) )
  | ( List_t _,
      ( D_unit | D_none | D_int _ | D_string _ | D_bytes _ | D_bool _
      | D_pair (_, _)
      | D_left _ | D_right _ | D_some _ | D_map _ | D_instruction _ ) )
  | ( Map_t (_, _),
      ( D_unit | D_none | D_int _ | D_string _ | D_bytes _ | D_bool _
      | D_pair (_, _)
      | D_left _ | D_right _ | D_some _ | D_list _ | D_instruction _ ) )
  | ( Mutez_t,
      ( D_unit | D_none | D_string _ | D_bytes _ | D_bool _
      | D_pair (_, _)
      | D_left _ | D_right _ | D_some _ | D_map _ | D_list _ | D_instruction _
        ) )
  | ( Nat_t,
      ( D_unit | D_none | D_string _ | D_bytes _ | D_bool _
      | D_pair (_, _)
      | D_left _ | D_right _ | D_some _ | D_map _ | D_list _ | D_instruction _
        ) )
  | ( Option_t _,
      ( D_unit | D_int _ | D_string _ | D_bytes _ | D_bool _
      | D_pair (_, _)
      | D_left _ | D_right _ | D_map _ | D_list _ | D_instruction _ ) )
  | ( Or_t (_, _),
      ( D_unit | D_none | D_int _ | D_string _ | D_bytes _ | D_bool _
      | D_pair (_, _)
      | D_some _ | D_map _ | D_list _ | D_instruction _ ) )
  | ( Pair_t (_, _),
      ( D_unit | D_none | D_int _ | D_string _ | D_bytes _ | D_bool _ | D_left _
      | D_right _ | D_some _ | D_map _ | D_list _ | D_instruction _ ) )
  | ( Set_t _,
      ( D_unit | D_none | D_int _ | D_string _ | D_bytes _ | D_bool _
      | D_pair (_, _)
      | D_left _ | D_right _ | D_some _ | D_map _ | D_instruction _ ) )
  | ( Signature_t,
      ( D_unit | D_none | D_int _ | D_bytes _ | D_bool _
      | D_pair (_, _)
      | D_left _ | D_right _ | D_some _ | D_map _ | D_list _ | D_instruction _
        ) )
  | ( String_t,
      ( D_unit | D_none | D_int _ | D_bytes _ | D_bool _
      | D_pair (_, _)
      | D_left _ | D_right _ | D_some _ | D_map _ | D_list _ | D_instruction _
        ) )
  | ( Timestamp_t,
      ( D_unit | D_none | D_bytes _ | D_bool _
      | D_pair (_, _)
      | D_left _ | D_right _ | D_some _ | D_map _ | D_list _ | D_instruction _
        ) )
  | ( Unit_t,
      ( D_none | D_int _ | D_string _ | D_bytes _ | D_bool _
      | D_pair (_, _)
      | D_left _ | D_right _ | D_some _ | D_map _ | D_list _ | D_instruction _
        ) )
  | ( Bls12_381_g1_t,
      ( D_unit | D_none | D_string _ | D_bytes _ | D_bool _
      | D_pair (_, _)
      | D_left _ | D_right _ | D_some _ | D_map _ | D_list _
      | D_instruction (_, _) ) )
  | ( Bls12_381_g2_t,
      ( D_unit | D_none | D_string _ | D_bytes _ | D_bool _
      | D_pair (_, _)
      | D_left _ | D_right _ | D_some _ | D_map _ | D_list _
      | D_instruction (_, _) ) )
  | ( Bls12_381_fr_t,
      ( D_unit | D_none | D_string _ | D_bytes _ | D_bool _
      | D_pair (_, _)
      | D_left _ | D_right _ | D_some _ | D_map _ | D_list _
      | D_instruction (_, _) ) ) ->
      raise Type_error

and list_from_adt_data : type a. a typ -> Adt.data list -> a t_list =
 fun t -> function
  | [] -> L_nil
  | d :: tl ->
      let d' = from_adt_data t d in
      let tl' = list_from_adt_data t tl in
      L_cons (d', tl')

and set_from_adt_data : type a. a typ -> Adt.data list -> a t_set =
 fun t -> function
  | [] -> Set_nil
  | d :: tl ->
      let d' = from_adt_data t d in
      let tl' = set_from_adt_data t tl in
      Set_cons (d', tl')

and map_from_adt_data :
    type k v. k typ -> v typ -> (Adt.data * Adt.data) list -> (k, v) t_map =
 fun t_k t_v d_l ->
  match d_l with
  | [] -> Map_nil
  | (d_k, d_v) :: tl ->
      let d_k' = from_adt_data t_k d_k in
      let d_v' = from_adt_data t_v d_v in
      let tl' = map_from_adt_data t_k t_v tl in
      Map_cons ((d_k', d_v'), tl')

and big_map_from_adt_data :
    type k v. k typ -> v typ -> (Adt.data * Adt.data) list -> (k, v) t_big_map =
 fun t_k t_v d_l ->
  match d_l with
  | [] -> Big_map_nil
  | (d_k, d_v) :: tl ->
      let d_k' = from_adt_data t_k d_k in
      let d_v' = from_adt_data t_v d_v in
      let tl' = big_map_from_adt_data t_k t_v tl in
      Big_map_cons ((d_k', d_v'), tl')

exception Not_comparable

let rec equal : type a. a t -> a t -> bool =
 fun d_1 d_2 ->
  match (d_1, d_2) with
  | SD_address (Address s_1), SD_address (Address s_2)
  | SD_key (Key s_1), SD_key (Key s_2)
  | SD_key_hash (Key_hash s_1), SD_key_hash (Key_hash s_2)
  | SD_signature (Signature s_1), SD_signature (Signature s_2)
  | SD_string (String s_1), SD_string (String s_2) ->
      String.equal s_1 s_2
  | SD_address _, SD_address _ -> false
  | SD_bool (Bool b_1), SD_bool (Bool b_2) -> Bool.equal b_1 b_2
  | SD_bytes (Bytes b_1), SD_bytes (Bytes b_2)
  | SD_chain_id (Chain_id b_1), SD_chain_id (Chain_id b_2) ->
      Bytes.equal b_1 b_2
  | SD_int (Int n_1), SD_int (Int n_2)
  | SD_nat (Nat n_1), SD_nat (Nat n_2)
  | SD_timestamp (Timestamp n_1), SD_timestamp (Timestamp n_2) ->
      Bigint.equal n_1 n_2
  | SD_mutez (Mutez n_1), SD_mutez (Mutez n_2) -> Int64.equal n_1 n_2
  | SD_option d_1, SD_option d_2 -> (
      match (d_1, d_2) with
      | O_none, O_none -> true
      | O_some d_1, O_some d_2 -> equal d_1 d_2
      | O_some _, O_none | O_none, O_some _ -> false)
  | SD_or d_1, SD_or d_2 -> (
      match (d_1, d_2) with
      | Or_left _, Or_right _ | Or_right _, Or_left _ -> false
      | Or_left d_1, Or_left d_2 -> equal d_1 d_2
      | Or_right d_1, Or_right d_2 -> equal d_1 d_2)
  | SD_pair (Pair (d_1, d_2)), SD_pair (Pair (d_3, d_4)) ->
      if equal d_1 d_3 then equal d_2 d_4 else false
  | SD_unit, SD_unit -> true
  | SD_never, SD_never -> true
  | SD_big_map _, SD_big_map _
  | SD_contract _, SD_contract _
  | SD_lambda _, SD_lambda _
  | SD_list _, SD_list _
  | SD_map _, SD_map _
  | SD_operation _, SD_operation _
  | SD_set _, SD_set _
  | SD_chest, SD_chest
  | SD_chest_key, SD_chest_key
  | SD_sapling_state _, SD_sapling_state _
  | SD_sapling_transaction, SD_sapling_transaction
  | SD_bls12_381_g1 _, SD_bls12_381_g1 _
  | SD_bls12_381_g2 _, SD_bls12_381_g2 _
  | SD_bls12_381_fr _, SD_bls12_381_fr _
  | SD_ticket _, SD_ticket _ ->
      raise Not_comparable

exception Cast_error

let rec cast : type a b. a typ -> b t -> a t =
 fun t d ->
  match (t, d) with
  | Address_t, SD_address _ -> d
  | Big_map_t (t_1, t_2), SD_big_map bm -> SD_big_map (cast_big_map t_1 t_2 bm)
  | Bool_t, SD_bool _ -> d
  | Bytes_t, SD_bytes _ -> d
  | Chain_id_t, SD_chain_id _ -> d
  | Contract_t _, SD_contract (Contract_addr a) -> SD_contract (Contract_addr a)
  | Contract_t _, SD_contract (Contract_key_hash k) ->
      SD_contract (Contract_key_hash k)
  | Int_t, SD_int _ -> d
  | Key_t, SD_key _ -> d
  | Key_hash_t, SD_key_hash _ -> d
  | Lambda_t _, SD_lambda (Lambda i) -> SD_lambda (Lambda i)
  | List_t t, SD_list l -> SD_list (cast_list t l)
  | Map_t (t_1, t_2), SD_map m -> SD_map (cast_map t_1 t_2 m)
  | Mutez_t, SD_mutez _ -> d
  | Nat_t, SD_nat _ -> d
  | Operation_t, SD_operation _ -> d
  | Option_t _, SD_option O_none -> SD_option O_none
  | Option_t t, SD_option (O_some d) -> SD_option (O_some (cast t d))
  | Or_t (t, _), SD_or (Or_left d) -> SD_or (Or_left (cast t d))
  | Or_t (_, t), SD_or (Or_right d) -> SD_or (Or_right (cast t d))
  | Pair_t (t_1, t_2), SD_pair (Pair (d_1, d_2)) ->
      SD_pair (Pair (cast t_1 d_1, cast t_2 d_2))
  | Set_t t, SD_set s -> SD_set (cast_set t s)
  | Signature_t, SD_signature _ -> d
  | String_t, SD_string _ -> d
  | Timestamp_t, SD_timestamp _ -> d
  | Unit_t, SD_unit -> d
  | _ -> raise Cast_error

and cast_big_map :
    type a b c d. a typ -> b typ -> (c, d) t_big_map -> (a, b) t_big_map =
 fun t_k t_v -> function
  | Big_map_nil -> Big_map_nil
  | Big_map_cons ((k, v), tl) ->
      Big_map_cons ((cast t_k k, cast t_v v), cast_big_map t_k t_v tl)

and cast_list : type a b. a typ -> b t_list -> a t_list =
 fun t -> function
  | L_nil -> L_nil
  | L_cons (d, tl) -> L_cons (cast t d, cast_list t tl)

and cast_map : type a b c d. a typ -> b typ -> (c, d) t_map -> (a, b) t_map =
 fun t_k t_v -> function
  | Map_nil -> Map_nil
  | Map_cons ((k, v), tl) ->
      Map_cons ((cast t_k k, cast t_v v), cast_map t_k t_v tl)

and cast_set : type a b. a typ -> b t_set -> a t_set =
 fun t -> function
  | Set_nil -> Set_nil
  | Set_cons (d, tl) -> Set_cons (cast t d, cast_set t tl)

and cast_map_key : type a b c. a typ -> (b, c) t_map -> (a, c) t_map =
 fun t -> function
  | Map_nil -> Map_nil
  | Map_cons ((k, v), tl) -> Map_cons ((cast t k, v), cast_map_key t tl)

and cast_big_map_key : type a b c. a typ -> (b, c) t_big_map -> (a, c) t_big_map
    =
 fun t -> function
  | Big_map_nil -> Big_map_nil
  | Big_map_cons ((k, v), tl) ->
      Big_map_cons ((cast t k, v), cast_big_map_key t tl)

let rec to_string : type a. a t -> string = function
  | SD_signature (Signature s)
  | SD_string (String s)
  | SD_key (Key s)
  | SD_key_hash (Key_hash s)
  | SD_address (Address s) ->
      s
  | SD_address (Address_contract c) -> to_string (SD_contract c)
  | SD_unit -> "Unit"
  | SD_big_map _ | SD_bool (Bool true) -> "true"
  | SD_bool (Bool false) -> "false"
  | SD_timestamp (Timestamp n)
  | SD_int (Int n)
  | SD_nat (Nat n)
  | SD_bls12_381_g1 (Bls12_381_g1 n)
  | SD_bls12_381_g2 (Bls12_381_g2 n)
  | SD_bls12_381_fr (Bls12_381_fr n) ->
      Bigint.to_string n
  | SD_chain_id (Chain_id b) | SD_bytes (Bytes b) -> Bytes.to_string b
  | SD_option O_none -> "None"
  | SD_option (O_some d) -> [%string "Some %{to_string d}"]
  | SD_mutez (Mutez n) -> Int64.to_string n
  | SD_pair (Pair (d_1, d_2)) ->
      [%string "(%{to_string d_1}, %{to_string d_2})"]
  | SD_operation (Operation o) -> Adt.Operation.to_string o
  | SD_or (Or_left d) -> [%string "Left %{to_string d}"]
  | SD_or (Or_right d) -> [%string "Right %{to_string d}"]
  | SD_contract _ -> "contract"
  | SD_lambda _ -> "lambda"
  | SD_list l -> list_to_string l
  | SD_set s -> set_to_string s
  | SD_map m -> map_to_string m
  | SD_never -> "never"
  | SD_chest -> "<chest>"
  | SD_chest_key -> "<chest_key>"
  | SD_sapling_transaction -> "<sapling transaction>"
  | SD_sapling_state _ -> "<sapling state>"
  | SD_ticket _ -> "<ticket>"

and list_to_string : type a. a t_list -> string =
 fun l ->
  let rec aux acc = function
    | L_nil -> acc
    | L_cons (h, t) -> aux [%string "%{acc}; %{to_string h}"] t
  in
  [%string "{ %{aux \"\" l} }"]

and set_to_string : type a. a t_set -> string =
 fun l ->
  let rec aux acc = function
    | Set_nil -> acc
    | Set_cons (h, t) -> aux [%string "%{acc}; %{to_string h}"] t
  in
  [%string "{ %{aux \"\" l} }"]

and map_to_string : type a b. (a, b) t_map -> string =
 fun l ->
  let rec aux acc = function
    | Map_nil -> acc
    | Map_cons ((k, v), t) ->
        aux [%string "%{acc}; Elt %{to_string k} %{to_string v}"] t
  in
  [%string "{ %{aux \"\" l} }"]
