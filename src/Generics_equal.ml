module Mapper = struct
  type 'a t = 'a -> 'a -> bool
  type mapper = { map : 'a. 'a Generics.typ -> 'a t }

  let unit _ _ = true
  let int : int -> int -> bool = ( = )
  let char : char -> char -> bool = ( = )
  let bool : bool -> bool -> bool = ( = )
  let int32 : int32 -> int32 -> bool = ( = )
  let int64 : int64 -> int64 -> bool = ( = )
  let string x y = x == y || String.equal x y
  let bytes x y = x == y || Bytes.equal x y
  let float = Float.equal

  let option self t x y =
    x == y
    ||
    match (x, y) with
    | None, None -> true
    | Some x, Some y ->
      let equal = self.map t in
      equal x y
    | _ -> false

  let result self t t_err x y =
    x == y
    ||
    match (x, y) with
    | Error e1, Error e2 ->
      let equal = self.map t_err in
      equal e1 e2
    | Ok v1, Ok v2 ->
      let equal = self.map t in
      equal v1 v2
    | _ -> false

  let record self record_t r1 r2 =
    let fields = Generics.Record.any_field_list record_t in
    List.for_all
      (fun (Generics.Field.Any field) ->
        let ft = Generics.Field.typ field in
        let v1 = Generics.Field.get r1 field in
        let v2 = Generics.Field.get r2 field in
        let eq = self.map ft in
        eq v1 v2)
      fields

  let variant self variant_t variant1 variant2 =
    let (Generics.Constr.Value (c1, args1)) =
      Generics.Variant.value variant_t variant1
    in
    let (Generics.Constr.Value (c2, args2)) =
      Generics.Variant.value variant_t variant2
    in
    let n1 = Generics.Constr.name c1 in
    let n2 = Generics.Constr.name c2 in
    if string n1 n2 then
      match (Generics.Constr.make c1, Generics.Constr.make c2) with
      | Const _, Const _ -> true
      | Args (args1_t, _), Args (args2_t, _) -> (
        match Generics.equal args1_t args2_t with
        | Some Equal -> self.map args1_t args1 args2
        | None -> false)
      | _ -> false
    else false

  let abstract _name _v1 _v2 = false
end

include Generics.Map (Mapper)
