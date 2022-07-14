From Coq Require Import Datatypes.
From Coq Require Import ZArith.
Require Import
  Coq.FSets.FMapList
  Coq.Structures.OrderedTypeEx.

From Tezla Require Import Common_adt.
From Tezla Require Import Adt.
From Tezla Require Import Var.
From Tezla Require Import Typ.

Module Import M := FMapList.Make(String_as_OT).

Definition var (A : Typ.t) : Type := Var.t A.
Inductive val : Typ.t -> Type :=
| V_bool : Datatypes.bool -> val bool
| V_none (x : Typ.t) : val (option x)
| V_some {x} (v : val x) : val (option x)
| V_default {x} : val x
| V_unit : val unit
| V_int : Z -> val int
| V_nat : Z -> val nat
| V_string : String.string -> val string
| V_left (y : Typ.t) {x} : val x -> val (or x y)
| V_right (x : Typ.t) {y} : val y -> val (or x y)
| V_pair {x y} : val x -> val y -> val (pair x y)
| V_list {x} : Datatypes.list (val x) -> val (list x)
| V_set {x : comparable_type} : Datatypes.list (val x) -> val (set x)
| V_map {x : comparable_type} {y} : Datatypes.list (val x * val y) -> val (Typ.map x y).
Inductive some_val : Type :=
| Sval {x} : val x -> some_val.
Definition store : Type := M.t some_val.
Definition oracle : Type := Datatypes.unit.
Record state : Type := mkState { gas : Datatypes.nat; st : store; }.

Definition assign {X : Typ.t} (s : store) (k : var X) (v : val X) :=
    match k with
    | Var.var _ k => M.add k (Sval v) s
    end.

Definition lookup {X : Typ.t} (s : store) (v : var X) : Datatypes.option some_val :=
    match v with
    | Var.var _ v => M.find v s
    end.

Inductive result : Type :=
| R_failed {x} : var x -> state -> result
| R_state : state -> result
| R_return {x} : var x -> state -> result
| R_timeout : result
| R_type_error : result
| R_value : some_val -> state -> result.

Fixpoint val_of_data {X : Typ.t} (d : data X) : val X :=
    match d with
    | D_int n => V_int n
    | D_string s => V_string s
    | D_unit => V_unit
    | D_bool b => V_bool b
    | D_pair d1 d2 => V_pair (val_of_data d1) (val_of_data d2)
    end.

(* Definition eval_expr (self_type : Typ.t) (oracle : oracle) clock state (expr : @expr self_type) :=
    match clock with
    | O => R_timeout
    | S clock =>
        match expr with
        | E_var v =>
            match lookup (st state) v with
            | None => R_type_error
            | Some v => R_value v state
            end
        | E_push d => R_value (Sval (val_of_data d)) state
        | E_car v =>
            match lookup (st state) v with
            | Some (Sval (V_pair d1 _)) =>
                R_value (Sval d1) state
            | _ => R_type_error
            end
        | E_cdr v =>
            match lookup (st state) v with
            | Some (Sval (V_pair _ d2)) =>
                R_value (Sval d2) state
            | _ => R_type_error
            end
        | E_abs v => 
            match lookup (st state) v with
            | Some (Sval (V_int n)) =>
                R_value (Sval (V_nat (Z.abs n))) state
            | _ => R_type_error
            end
        | _ => R_type_error (* TODO: *)
        end
    end. *)

Fixpoint eval_stmt self_type (oracle : oracle) clock state (stmt : @stmt self_type) :=
    match clock with
    | O => R_timeout
    | S clock =>
    match stmt with
    | S_seq s_1 s_2 =>
        match eval_stmt self_type oracle clock state s_1 with
        | R_state state =>            
            eval_stmt self_type oracle clock state s_2
        | r => r
        end
    | S_assign v e => R_state state (* TODO: gas *)
    | S_skip => R_state state
    | S_drop vars => R_state state (* TODO: gas *)
    | S_swap => R_state state (* TODO: gas *)
    | S_dig n => R_state state (* TODO: gas *)
    | S_dug n => R_state state (* TODO: gas *)
    | S_if v i_t i_f =>
        match lookup (st state) v with
        | Some (Sval (V_bool b)) =>
            if b then eval_stmt self_type oracle clock state i_t
            else eval_stmt self_type oracle clock state i_f
        | _ => R_type_error
        end
    | S_if_none v i_t i_f =>
        match lookup (st state) v with
        | Some (Sval (V_none _)) =>
            eval_stmt self_type oracle clock state i_t
        | Some (Sval (V_some _)) =>
            eval_stmt self_type oracle clock state i_f
        | _ => R_type_error
        end
    | S_if_left v i_t i_f =>
        match lookup (st state) v with
        | Some (Sval (V_left _ _)) =>
            eval_stmt self_type oracle clock state i_t
        | Some (Sval (V_right _ _)) =>
            eval_stmt self_type oracle clock state i_f
        | _ => R_type_error
        end
    | S_if_cons v i_t i_f =>
        match lookup (st state) v with
        | Some (Sval (V_list nil)) =>
            eval_stmt self_type oracle clock state i_f
        | Some (Sval (V_list _)) =>
            eval_stmt self_type oracle clock state i_t
        | _ => R_type_error
        end
    | S_loop v i as loop =>
        match lookup (st state) v with
        | Some (Sval (V_bool b)) =>
            if b then
                if clock =? 0 then
                    R_timeout
                else
                    eval_stmt self_type oracle clock state (S_seq i loop)
            else R_state state
        | _ => R_type_error
        end
    | S_loop_left v i as loop =>
        match lookup (st state) v with
        | Some (Sval (V_left _ _)) =>
            if clock =? 0 then
                R_timeout
            else
                eval_stmt self_type oracle clock state (S_seq i loop)
        | Some (Sval (V_right _ _)) =>
            R_state state
        | _ => R_type_error
        end
    | S_map_list v i as loop =>
        match lookup (st state) v with
        | Some (Sval (V_list nil)) =>
            R_state state
        | Some (Sval (V_list _)) =>
            if clock =? 0 then
                R_timeout
            else
                eval_stmt self_type oracle clock state (S_seq i loop)
        | _ => R_type_error
        end
    | S_map_map v i as loop =>
        match lookup (st state) v with
        | Some (Sval (V_list nil)) =>
            R_state state
        | Some (Sval (V_list _)) =>
            if clock =? 0 then
                R_timeout
            else
                eval_stmt self_type oracle clock state (S_seq i loop)
        | _ => R_type_error
        end
    | S_iter_set v i as loop =>
        match lookup (st state) v with
        | Some (Sval (V_set nil)) =>
            R_state state
        | Some (Sval (V_set _)) =>
            if clock =? 0 then
                R_timeout
            else
                eval_stmt self_type oracle clock state (S_seq i loop)
        | _ => R_type_error
        end
    | S_iter_list v i as loop =>
        match lookup (st state) v with
        | Some (Sval (V_list nil)) =>
            R_state state
        | Some (Sval (V_list _)) =>
            if clock =? 0 then
                R_timeout
            else
                eval_stmt self_type oracle clock state (S_seq i loop)
        | _ => R_type_error
        end
    | S_iter_map v i as loop =>
        match lookup (st state) v with
        | Some (Sval (V_map nil)) =>
            R_state state
        | Some (Sval (V_map _)) =>
            if clock =? 0 then
                R_timeout
            else
                eval_stmt self_type oracle clock state (S_seq i loop)
        | _ => R_type_error
        end
    | S_failwith v => R_failed v state
    | S_return v => R_return v state
    end
    end.
