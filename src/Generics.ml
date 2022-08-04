module Tid = struct
  type 'a t = { name : string; code : int; witness : 'a Witness.t }

  (* Must be in sync with the first non-scalar type in Typ.t *)
  let current_code = ref 0

  let make name =
    incr current_code;
    { code = !current_code; witness = Witness.make (); name }

  let unit : unit t = make "unit"
  let int : int t = make "int"
  let int32 : int32 t = make "int32"
  let int64 : int64 t = make "int64"
  let float : float t = make "float"
  let bool : bool t = make "bool"
  let char : char t = make "char"
  let string : string t = make "string"
  let bytes : bytes t = make "bytes"
end

type 'a tid = 'a Tid.t

module rec Typ : sig
  type _ typ =
    | Unit : unit typ
    | Int : int typ
    | Int32 : int32 typ
    | Int64 : int64 typ
    | Float : float typ
    | Bool : bool typ
    | Char : char typ
    | String : string typ
    | Bytes : bytes typ
    | Record : ('record, 'fields) Typ.Record.t -> 'record typ
    | Variant : 'variant Typ.Variant.t -> 'variant typ
    | Abstract : 'a tid -> 'a typ
    | Alias : 'a typ * ('a, 'b) Witness.equal -> 'b typ

  type any = Any : 'a typ -> any

  val equal : 'a typ -> 'b typ -> ('a, 'b) Witness.equal option

  module Field : sig
    type ('record, 'a) t
    type 'record any = Any : ('record, 'a) t -> 'record any

    type ('record, 'fields) list =
      | [] : ('record, 'record) list
      | ( :: ) :
          ('record, 'a) t * ('record, 'fields) list
          -> ('record, 'a -> 'fields) list

    val name : ('record, 'a) t -> string
    val typ : ('record, 'a) t -> 'a typ
    val get : 'record -> ('record, 'a) t -> 'a
  end

  module Record : sig
    type ('record, 'fields) t

    val name : ('record, 'fields) t -> string
    val any_field_list : ('record, 'fields) t -> 'record Typ.Field.any list
    val field_list : ('record, 'fields) t -> ('record, 'fields) Typ.Field.list
    val make : ('record, 'fields) t -> 'fields
  end

  module Constr : sig
    type ('variant, 'args) t

    type ('variant, 'args) make =
      | Const : 'variant -> ('variant, unit) make
      | Args : 'args typ * ('args -> 'variant) -> ('variant, 'args) make

    type 'variant value =
      | Value : ('variant, 'args) t * 'args -> 'variant value

    type 'variant any = Any : ('variant, 'args) t -> 'variant any

    val name : ('variant, 'args) t -> string
    val make : ('variant, 'args) t -> ('variant, 'args) make
  end

  module Variant : sig
    type 'variant t

    val name : 'variant t -> string
    val value : 'variant t -> 'variant -> 'variant Constr.value
    val constr_list : 'variant t -> 'variant Constr.any list
  end

  val field : string -> 'a typ -> ('record -> 'a) -> ('record, 'a) Field.t
  val record : string -> ('record, 'fields) Field.list -> 'fields -> 'record typ

  val constr :
    string -> ('variant, 'args) Constr.make -> ('variant, 'args) Constr.t

  val variant :
    string ->
    'variant Constr.any list ->
    ('variant -> 'variant Constr.value) ->
    'variant typ

  val abstract : string -> 'a typ
  val tid : 'a typ -> 'a Tid.t
end = struct
  type _ typ =
    | Unit : unit typ
    | Int : int typ
    | Int32 : int32 typ
    | Int64 : int64 typ
    | Float : float typ
    | Bool : bool typ
    | Char : char typ
    | String : string typ
    | Bytes : bytes typ
    | Record : ('record, 'fields) Typ.Record.t -> 'record typ
    | Variant : 'variant Typ.Variant.t -> 'variant typ
    | Abstract : 'a tid -> 'a typ
    | Alias : 'a typ * ('a, 'b) Witness.equal -> 'b typ

  type any = Any : 'a typ -> any

  module Field = struct
    type ('record, 'a) t = { name : string; get : 'record -> 'a; typ : 'a typ }

    type ('record, 'fields) list =
      | [] : ('record, 'record) list
      | ( :: ) :
          ('record, 'a) t * ('record, 'fields) list
          -> ('record, 'a -> 'fields) list

    type 'record any = Any : ('record, 'a) t -> 'record any

    let name t = t.name
    let typ t = t.typ
    let get r (field : ('record, 'a) t) = field.get r

    let rec any_list :
        type record fields. (record, fields) list -> record any List.t =
     fun fields ->
      match fields with
      | [] -> []
      | field :: fields -> Any field :: any_list fields
  end

  module Record = struct
    type ('record, 'fields) t = {
      tid : 'record tid;
      fields : 'record Field.any list;
      fields' : ('record, 'fields) Field.list;
      make : 'fields;
      witness : 'record Witness.t;
    }

    let name t = t.tid.name
    let any_field_list t = t.fields
    let field_list t = t.fields'
    let make t = t.make
  end

  module Constr = struct
    type ('variant, 'args) make =
      | Const : 'variant -> ('variant, unit) make
      | Args : 'args typ * ('args -> 'variant) -> ('variant, 'args) make

    type ('variant, 'args) t = { name : string; make : ('variant, 'args) make }

    type 'variant value =
      | Value : ('variant, 'args) t * 'args -> 'variant value

    type 'variant any = Any : ('variant, 'args) t -> 'variant any

    let name : type variant args. (variant, args) t -> string = fun t -> t.name
    let make t = t.make
  end

  module Variant = struct
    type 'variant t = {
      tid : 'variant tid;
      constr_list : 'variant Constr.any list;
      value : 'variant -> 'variant Constr.value;
      witness : 'variant Witness.t;
    }

    let name t = t.tid.name
    let value t = t.value
    let constr_list variant = variant.constr_list
  end

  let equal : type a b. a typ -> b typ -> (a, b) Witness.equal option =
   fun t1 t2 ->
    match (t1, t2) with
    | Int, Int -> Some Equal
    | Bool, Bool -> Some Equal
    | String, String -> Some Equal
    | Variant t1, Variant t2 ->
      Witness.equal t1.Variant.witness t2.Variant.witness
    | Record t1, Record t2 -> Witness.equal t1.Record.witness t2.Record.witness
    | Abstract a1, Abstract a2 -> (
      match Witness.equal a1.witness a2.witness with
      | Some Equal -> Some Equal
      | None -> None)
    | _ -> None

  let field name typ get = { Field.name; typ; get }

  let record name fields make =
    let any_fields = Field.any_list fields in
    Typ.Record
      {
        Record.tid = Tid.make name;
        fields = any_fields;
        fields' = fields;
        make;
        witness = Witness.make ();
      }

  let constr name make = { Constr.name; make }

  let variant name constr_list value =
    Typ.Variant
      {
        Variant.tid = Tid.make name;
        constr_list;
        value;
        witness = Witness.make ();
      }

  let abstract name = Abstract (Tid.make name)

  let tid : type a. a typ -> a Tid.t =
   fun t ->
    match t with
    | Unit -> Tid.unit
    | Int -> Tid.int
    | Int32 -> Tid.int32
    | Int64 -> Tid.int64
    | Float -> Tid.float
    | Bool -> Tid.bool
    | Char -> Tid.char
    | String -> Tid.string
    | Bytes -> Tid.bytes
    | Record r -> r.Record.tid
    | Variant v -> v.Variant.tid
    | Abstract tid -> tid
    | Alias _ -> failwith ""
end

include Typ

type 'a dyn = 'a typ * 'a

let int = Typ.Int
let int32 = Typ.Int32
let int64 = Typ.Int64
let float = Typ.Float
let unit = Typ.Unit
let bool = Typ.Bool
let char = Typ.Char
let string = Typ.String
let bytes = Typ.Bytes

let option t =
  let none_constr = Typ.constr "None" (Const None) in
  let some_constr = Typ.constr "Some" (Args (t, fun x -> Some x)) in
  let value = function
    | None -> Typ.Constr.Value (none_constr, ())
    | Some x -> Typ.Constr.Value (some_constr, x)
  in
  Typ.variant "option"
    [ Typ.Constr.Any none_constr; Typ.Constr.Any some_constr ]
    value

let result ok_t error_t =
  let ok_constr = Typ.constr "Ok" (Args (ok_t, fun x -> Ok x)) in
  let error_constr = Typ.constr "Some" (Args (error_t, fun x -> Error x)) in
  let value result =
    match result with
    | Ok x -> Typ.Constr.Value (ok_constr, x)
    | Error err -> Typ.Constr.Value (error_constr, err)
  in
  Typ.variant "result"
    [ Typ.Constr.Any ok_constr; Typ.Constr.Any error_constr ]
    value

module Dyn = struct
  type 'a t = 'a dyn
  type any = Any : 'a dyn -> any

  let typ (t, _) = t
  let value (_, v) = v
end

type ('a, 'b) equal = ('a, 'b) Witness.equal = Equal : ('a, 'a) equal

module type Mapper = sig
  type 'a t
  type mapper = { map : 'a. 'a typ -> 'a t }

  val unit : unit t
  val int : int t
  val int32 : int32 t
  val int64 : int64 t
  val float : float t
  val bool : bool t
  val char : char t
  val string : string t
  val bytes : bytes t
  val record : mapper -> ('r, 'fields) Record.t -> 'r t
  val variant : mapper -> 'variant Variant.t -> 'variant t
  val abstract : string -> 'a t
end

module Int_map = Map.Make (Int)

module Map (Mapper : Mapper) : sig
  val map : 'a typ -> 'a Mapper.t
  val register : 'a typ -> 'a Mapper.t -> unit
end = struct
  module type Extension = sig
    type t

    val map : t Mapper.t
  end

  type extension =
    | Extension :
        ('a Witness.t * (module Extension with type t = 'a))
        -> extension

  let mappers : extension Int_map.t ref = ref Int_map.empty

  let rec map : type a. a typ -> a Mapper.t =
   fun typ ->
    match typ with
    | Unit -> Mapper.unit
    | Int -> Mapper.int
    | Int32 -> Mapper.int32
    | Int64 -> Mapper.int64
    | Float -> Mapper.float
    | Bool -> Mapper.bool
    | Char -> Mapper.char
    | String -> Mapper.string
    | Bytes -> Mapper.bytes
    | Record r -> Mapper.record self r
    | Variant v -> Mapper.variant self v
    | Abstract tid -> (
      match Int_map.find_opt tid.code !mappers with
      | None -> failwith "no extension for code"
      | Some (Extension (ext_witness, ext)) -> (
        match Witness.equal tid.witness ext_witness with
        | None -> failwith "type witnesses are not the same"
        | Some Witness.Equal ->
          let module Ext = (val ext : Extension with type t = a) in
          Ext.map))
    | Alias _ -> failwith "no equal"

  and self = { Mapper.map }

  let register (type a) typ map =
    let module Extension = struct
      type t = a

      let map = map
    end in
    let tid = Typ.tid typ in
    let ext =
      Extension (tid.witness, (module Extension : Extension with type t = a))
    in
    mappers := Int_map.add tid.code ext !mappers
end
