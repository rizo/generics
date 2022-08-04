module Mapper = struct
  type 'a t = 'a -> string
  type mapper = { map : 'a. 'a Generics.typ -> 'a t }

  let unit () = "()"
  let int = string_of_int
  let int32 = Int32.to_string
  let int64 = Int64.to_string
  let float = string_of_float
  let bool = string_of_bool
  let char = String.make 1
  let string x = String.concat "" [ "\""; String.escaped x; "\"" ]
  let bytes x = string (Bytes.to_string x)

  let record self record_t r1 =
    let fields =
      record_t
      |> Generics.Record.any_field_list
      |> List.map (fun (Generics.Field.Any field) ->
             let typ = Generics.Field.typ field in
             let show = self.map typ in
             let v = Generics.Field.get r1 field in
             String.concat ""
               [ "  "; Generics.Field.name field; " = "; show v; ";" ])
      |> String.concat "\n"
    in
    String.concat "\n" [ "{ "; fields; "}" ]

  let variant self variant_t variant =
    let (Generics.Constr.Value (constr, args)) =
      Generics.Variant.value variant_t variant
    in
    let constr_name = Generics.Constr.name constr in
    match Generics.Constr.make constr with
    | Const _ -> constr_name
    | Args (args_t, _) ->
      let show_args = self.map args_t args in
      String.concat " " [ constr_name; show_args ]

  let abstract name _ = String.concat "" [ "<"; name; ">" ]
end

include Generics.Map (Mapper)
