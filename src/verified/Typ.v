
Inductive simple_comparable_type : Type :=
| unit
| never
| bool
| int
| nat
| string
| chain_id
| bytes
| mutez
| key_hash
| key
| signature
| timestamp
| address.

Inductive comparable_type : Type :=
| CSimple (x : simple_comparable_type)
| CPair (x : comparable_type) (y : comparable_type)
| COr (x: comparable_type) (y : comparable_type)
| COption (x : comparable_type).

Inductive t : Type :=
| Comparable (x : simple_comparable_type)
| option (x : t)
| list (x : t)
| set (x : comparable_type)
| operation
| contract (x : t)
| ticket (x : comparable_type)
| pair (x : t) (y : t)
| or (x : t) (y : t)
| lambda (x : t) (y : t)
| map (x : comparable_type) (y : t)
| big_map (x : comparable_type) (y : t)
| bls12_381_g1
| bls12_381_g2
| bls12_381_fr
| sapling_transaction (ms : Datatypes.nat)
| sapling_state (ms : Datatypes.nat).

Fixpoint comparable_type_to_type (c : comparable_type) : t :=
  match c with
  | CSimple a => Comparable a
  | CPair a b => pair (comparable_type_to_type a) (comparable_type_to_type b)
  | COption a => option (comparable_type_to_type a)
  | COr a b =>
    or (comparable_type_to_type a) (comparable_type_to_type b)
  end.

Coercion comparable_type_to_type : comparable_type >-> t.
Coercion CSimple : simple_comparable_type >-> comparable_type.
