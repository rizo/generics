(*

  typ
  dyn
  any

  Field.t
  Field.name
  Field.typ
  Field.get : ('record, 'a) t -> 'record -> 'a
  Field.map : ('record, 'a) t -> ('a -> 'a) -> 'record -> 'record
  Field.set : ('record, 'a) t -> 'a -> 'record -> unit

  Record.t
  Record.name
  Record.fields
  Record.map
  Record.fold
  Record.make

  Constr.t

  Constr.const
  Constr.apply

  constr "None" (Const None)
  constr "Some" (Args (t, fun x -> Some x))

  constr "None" Typ.unit (fun () -> None)
  constr "Some" some_t (fun x -> Some x)

  Variant.t

  Abstract.t


  const
  case0
  tag
  variant
  args
  constructor
  constr
  Case.const
  Case.args

  case (Const Blue)
  case (Args (fun rgb -> Rgb rgb))

  constr (Const Blue)
  constr (Args (int * int * int) (fun rgb -> Rgb rgb))

  Case.const Blue
  Case.apply Typ.int rgb (fun rgb -> Rgb rgb)

  *)

module rec Typ : sig
  type _ t =
    | Unit : unit t
    | Int : int t
    | Int32 : int32 t
    | Int64 : int64 t
    | Float : float t
    | Bool : bool t
    | Char : char t
    | String : string t
    | Bytes : bytes t
    | Record : ('record, 'fields) Typ.Record.t -> 'record t
    | Variant : 'variant Typ.Variant.t -> 'variant t
    | Abstract : 'a Typ.Abstract.t -> 'a t

  type 'a typ = 'a t
  type any = Any : 'a t -> any

  val equal : 'a typ -> 'b typ -> ('a, 'b) Witness.equal option

  module Abstract : sig
    type 'a t

    val name : 'a t -> string
    val typ : 'a t -> 'a typ
  end

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
    val fields : ('record, 'fields) t -> 'record Typ.Field.any list
    val fields' : ('record, 'fields) t -> ('record, 'fields) Typ.Field.list
    val map : ('record Field.any -> 'b) -> ('record, 'fields) t -> 'b list

    val fold :
      ('a -> 'record Field.any -> 'a) -> 'a -> ('record, 'fields) t -> 'a

    val make : ('record, 'fields) t -> 'fields
  end

  module Constr : sig
    type ('variant, 'args) t

    type ('variant, 'args) make =
      | Const : 'variant -> ('variant, unit) make
      | Args : 'args Typ.t * ('args -> 'variant) -> ('variant, 'args) make

    val name : ('variant, 'args) t -> string
    val args : ('variant, 'args) t -> 'args Typ.t option
    val make : ('variant, 'args) t -> ('variant, 'args) make
  end

  module Variant : sig
    type 'variant t

    type 'variant constr =
      | Constr : ('variant, 'args) Constr.t -> 'variant constr

    type 'variant value =
      | Value : ('variant, 'args) Constr.t * 'args -> 'variant value

    val value : ('variant, 'args) Constr.t -> 'args -> 'variant value
    val name : 'variant t -> string
    val view : 'variant t -> 'variant -> 'variant value
    val constr_list : 'variant t -> 'variant constr list
  end

  val field : string -> 'a t -> ('record -> 'a) -> ('record, 'a) Field.t
  val record : string -> ('record, 'fields) Field.list -> 'fields -> 'record t

  val constr :
    string -> ('variant, 'args) Constr.make -> ('variant, 'args) Constr.t

  val variant :
    string ->
    'variant Variant.constr list ->
    ('variant -> 'variant Variant.value) ->
    'variant t

  val abstract : string -> 'a t -> 'a t
end = struct
  type _ t =
    | Unit : unit t
    | Int : int t
    | Int32 : int32 t
    | Int64 : int64 t
    | Float : float t
    | Bool : bool t
    | Char : char t
    | String : string t
    | Bytes : bytes t
    | Record : ('record, 'fields) Typ.Record.t -> 'record t
    | Variant : 'variant Typ.Variant.t -> 'variant t
    | Abstract : 'a Typ.Abstract.t -> 'a t

  type 'a typ = 'a Typ.t
  type any = Any : 'a t -> any

  module Field = struct
    type ('record, 'a) t = {
      name : string;
      get : 'record -> 'a;
      typ : 'a Typ.t;
    }

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
      name : string;
      fields : 'record Field.any list;
      fields' : ('record, 'fields) Field.list;
      make : 'fields;
      witness : 'record Witness.t;
    }

    let name t = t.name
    let fields t = t.fields
    let fields' t = t.fields'
    let map f t = List.map f t.fields
    let fold f init t = List.fold_left f init t.fields
    let make t = t.make
  end

  module Constr = struct
    type ('variant, 'args) make =
      | Const : 'variant -> ('variant, unit) make
      | Args : 'args Typ.t * ('args -> 'variant) -> ('variant, 'args) make

    type ('variant, 'args) t = { name : string; make : ('variant, 'args) make }

    let name : type variant args. (variant, args) t -> string = fun t -> t.name

    let args : type variant args. (variant, args) t -> args Typ.t option =
     fun t ->
      match t.make with
      | Const _ -> None
      | Args (args_t, _make) -> Some args_t

    let make : type variant args. (variant, args) t -> (variant, args) make =
     fun variant_t -> variant_t.make
  end

  module Variant = struct
    type 'variant constr =
      | Constr : ('variant, 'args) Constr.t -> 'variant constr

    type 'variant value =
      | Value : ('variant, 'args) Constr.t * 'args -> 'variant value

    type 'variant t = {
      name : string;
      constr_list : 'variant constr list;
      view : 'variant -> 'variant value;
      witness : 'variant Witness.t;
    }

    let value constr x = Value (constr, x)
    let name t = t.name
    let view t = t.view
    let constr_list variant = variant.constr_list
  end

  module Abstract = struct
    type 'a t = { name : string; typ : 'a Typ.t; witness : 'a Witness.t }

    let name t = t.name
    let typ t = t.typ
  end

  let rec equal : type a b. a typ -> b typ -> (a, b) Witness.equal option =
   fun t1 t2 ->
    match (t1, t2) with
    | Int, Int -> Some Equal
    | Bool, Bool -> Some Equal
    | String, String -> Some Equal
    | Variant t1, Variant t2 ->
      Witness.equal t1.Variant.witness t2.Variant.witness
    | Record t1, Record t2 -> Witness.equal t1.Record.witness t2.Record.witness
    | Abstract a1, Abstract a2 -> (
      match equal a1.Abstract.typ a2.Abstract.typ with
      | Some Equal -> Some Equal
      | None -> None)
    | _ -> None

  let field name typ get = { Field.name; typ; get }

  let record name fields make =
    let any_fields = Field.any_list fields in
    Typ.Record
      {
        Record.name;
        fields = any_fields;
        fields' = fields;
        make;
        witness = Witness.make ();
      }

  let constr name make = { Constr.name; make }

  let variant name constr_list view =
    Typ.Variant { Variant.name; constr_list; view; witness = Witness.make () }

  let abstract name typ =
    Typ.Abstract { Abstract.name; typ; witness = Witness.make () }
end

include Typ

type 'a dyn = 'a t * 'a

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
  let view = function
    | None -> Typ.Variant.Value (none_constr, ())
    | Some x -> Typ.Variant.Value (some_constr, x)
  in
  Typ.variant "option"
    [ Typ.Variant.Constr none_constr; Typ.Variant.Constr some_constr ]
    view

let result ok_t error_t =
  let ok_constr = Typ.constr "Ok" (Args (ok_t, fun x -> Ok x)) in
  let error_constr = Typ.constr "Some" (Args (error_t, fun x -> Error x)) in
  let view result =
    match result with
    | Ok x -> Typ.Variant.Value (ok_constr, x)
    | Error err -> Typ.Variant.Value (error_constr, err)
  in
  Typ.variant "result"
    [ Typ.Variant.Constr ok_constr; Typ.Variant.Constr error_constr ]
    view

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
  val abstract : mapper -> string -> 'a typ -> 'a t
end

module Map (Mapper : Mapper) = struct
  let rec map : type a. a Typ.t -> a Mapper.t =
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
    | Abstract a ->
      let name = Typ.Abstract.name a in
      let typ = Typ.Abstract.typ a in
      Mapper.abstract self name typ

  and self = { Mapper.map }
end
