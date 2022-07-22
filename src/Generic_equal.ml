module Mapper = struct
  type 'a t = 'a -> 'a -> bool
  type mapper = { map : 'a. 'a Generic.typ -> 'a t }

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
    let fields = Generic.Record.fields record_t in
    List.for_all
      (fun (Generic.Field.Any field) ->
        let ft = Generic.Field.typ field in
        let v1 = Generic.Field.get r1 field in
        let v2 = Generic.Field.get r2 field in
        let eq = self.map ft in
        eq v1 v2)
      fields

  let variant self variant_t variant1 variant2 =
    let (Generic.Variant.Value (c1, args1)) =
      Generic.Variant.view variant_t variant1
    in
    let (Generic.Variant.Value (c2, args2)) =
      Generic.Variant.view variant_t variant2
    in
    let n1 = Generic.Constr.name c1 in
    let n2 = Generic.Constr.name c2 in
    if string n1 n2 then
      match (Generic.Constr.args c1, Generic.Constr.args c2) with
      | None, None -> true
      | Some t1, Some t2 -> (
        match Generic.equal t1 t2 with
        | Some Equal -> self.map t1 args1 args2
        | None -> false)
      | _ -> false
    else false

  let abstract self _name typ v1 v2 =
    let eq = self.map typ in
    eq v1 v2
end

module M = Generic.Map (Mapper)

let equal = M.map
