module Mapper = struct
  type 'a t = unit -> 'a
  type mapper = { map : 'a. 'a Generic.t -> 'a t }

  let unit () = ()
  let int () = 0
  let int32 () = Int32.of_int 0
  let int64 () = Int64.of_int 0
  let float () = 0.0
  let bool () = false
  let char () = '\000'
  let string () = ""
  let bytes () = Bytes.of_string ""
  let record _self _name _fields () = failwith "TODO"

  let record' self record () =
    let rec loop : type r f. f -> (r, f) Generic.Field.list -> r =
     fun m fs ->
      match fs with
      | Generic.Field.[] -> m
      | f :: fs' ->
        let t = Generic.Field.typ f in
        let x = self.map t () in
        let m' = m x in
        loop m' fs'
    in
    let make = Generic.Record.make record in
    let fields = Generic.Record.fields' record in
    loop make fields

  let variant _self variant_t =
    let constr_list = Generic.Variant.constr_list variant_t in
    let (Generic.Variant.Constr constr) = List.hd constr_list in
    match Generic.Constr.args constr with
    | Some _args_t -> failwith ""
    | None -> failwith ""

  let abstract self _name (t : 'a Generic.t) x =
    let show = self.map t in
    show x
end

module Map = Generic.Map (Mapper)

let show = Map.map
