open Core

type var = Var.t [@@deriving ord, sexp]

module T = struct
  type t =
    | O_create_contract of Edo_adt.Typed_adt.program * var * var * var
    | O_transfer_tokens of var * var * var
    | O_set_delegate of var
    | O_create_account of var * var * var * var
  [@@deriving ord, sexp]
end

include T
include Comparable.Make (T)

let to_string = function
  | O_create_account (v_1, v_2, v_3, v_4) ->
      [%string "CREATE_ACCOUNT %{v_1#Var} %{v_2#Var} %{v_3#Var} %{v_4#Var}"]
  | O_create_contract (_, v_1, v_2, v_3) ->
      [%string "CREATE_CONTRACT {...} %{v_1#Var} %{v_2#Var} %{v_3#Var}"]
  | O_set_delegate v -> [%string "SET_DELEGATE %{v#Var}"]
  | O_transfer_tokens (v_1, v_2, v_3) ->
      [%string "TRANSFER_TOKENS %{v_1#Var} %{v_2#Var} %{v_3#Var}"]
