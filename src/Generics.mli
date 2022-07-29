type 'a typ
(** The type for runtime representation of types. *)

type 'a dyn = 'a typ * 'a
(** The type for dynamic values, i.e., values with a type representation. *)

(** Type representation value with existential variable. *)
type any = Any : 'a typ -> any

(** {1 Basic types} *)

val unit : unit typ
(** Runtime representation for unit values. *)

val int : int typ
(** Runtime representation for integers. *)

val int32 : int32 typ
(** Runtime representation for the 32-bit integer type. *)

val int64 : int64 typ
(** Runtime representation for the 64-bit integer type. *)

val float : float typ
(** Runtime representation for the float type. *)

val bool : bool typ
(** Runtime representation for booleans. *)

val char : char typ
(** Runtime representation for characters. *)

val string : string typ
(** Runtime representation for strings. *)

val bytes : bytes typ
(** Runtime representation for bytes. *)

val option : 'a typ -> 'a option typ
(** Runtime representation for option types. *)

val result : 'a typ -> 'e typ -> ('a, 'e) result typ
(** Runtime representation for result types. *)

(** {1 Records} *)

(** Definitions for runtime field representation. *)
module Field : sig
  type ('record, 'a) t
  (** The runtime representation of fields with values of type ['a] belonging to
      the record of type ['record]. *)

  type 'record any = Any : ('record, 'a) t -> 'record any

  (** The type for field lists that preserves individual field types.

      The type variable ['make] is the function type from record fields to the
      record type. *)
  type ('record, 'make) list =
    | [] : ('record, 'record) list
    | ( :: ) :
        ('record, 'a) t * ('record, 'make) list
        -> ('record, 'a -> 'make) list

  val name : ('record, 'a) t -> string
  (** [name field] is the name of [field]. *)

  val typ : ('record, 'a) t -> 'a typ
  (** [typ field] is the runtime representation of the type of [field]. *)

  val get : 'record -> ('record, 'a) t -> 'a
  (** [get record field] is the value of [field] in [record]. *)
end

val field : string -> 'a typ -> ('record -> 'a) -> ('record, 'a) Field.t
(** [field name field_t get : ('record, 'a) Field.t] is the runtime
    representation of the field with type ['a], represented by [field_t],
    belonging to the record ['record]. See the {!module:Field} module for more
    operations on fields. *)

(** Definitions for runtime record representation. *)
module Record : sig
  type ('record, 'make) t
  (** The runtime representation of records of type ['record].

      The type variable ['make] is the function type from field values to the
      record value. For example the record:

      {[
        type r1 = { foo : int; bar : float list }
      ]}

      ...has the type [(r1, int -> float list -> r1) Record.t]. This can be used
      to dynamically create record values given a list of fields (see
      {!val:Record.make}).*)

  val name : ('record, 'make) t -> string
  (** The name of the record type. *)

  val any_fields : ('record, 'make) t -> 'record Field.any list
  (** [any_fields record] is the list of all [record]'s fields represented as a
      list of {!type:Field.any} to allow for uniform representation of
      individual fields. *)

  val fields : ('record, 'make) t -> ('record, 'make) Field.list
  (** [fields record] is the list of all [record]'s fields represented as
      {!type:Field.list} to preserve the types of individual fields. *)

  val make : ('record, 'make) t -> 'make
  (** [make record] is a smart constructor function for the [record] represented
      as a function from fields to record of type ['record]. *)
end

val record : string -> ('record, 'make) Field.list -> 'make -> 'record typ
(** [record name fields make] is the runtime representation of the record type
    with a given [name] and [fields]. The function [make] can be used to create
    the record value from field values. See the {!module:Record} module for more
    operations on records. *)

(** {1 Variants} *)

(** Definitions for runtime variant constructor representation. *)
module Constr : sig
  (** Constructors can either be constant or take one or more arguments.

      The following example demonstrates how to encode the runtime
      representation of the option type provided in the standard library.

      {[
        type 'a option = None | Some of 'a

        let option t =
          let open Generic in
          let none_constr = constr "None" (Const None) in
          let some_constr = constr "Some" (Args (t, fun x -> Some x)) in
          let value = function
            | None -> Constr.Value (none_constr, ())
            | Some x -> Constr.Value (some_constr, x)
          in
          variant "option"
            [ Constr.Any none_constr; Constr.Any some_constr ]
            value
      ]} *)

  type ('variant, 'args) t
  (** The runtime representation of constructors for the variant type ['variant]
      with arguments of type ['args]. *)

  (** The representation of the constructor values either as an immediate
      constant value or a function from constructor arguments to the variant
      value. *)
  type ('variant, 'args) make =
    | Const : 'variant -> ('variant, unit) make
    | Args : 'args typ * ('args -> 'variant) -> ('variant, 'args) make

  (** The runtime representation of a constructor with arguments. *)
  type 'variant value = Value : ('variant, 'args) t * 'args -> 'variant value

  (** The constructor with the hidden representation of the arguments. *)
  type 'variant any = Any : ('variant, 'args) t -> 'variant any

  val name : ('variant, 'args) t -> string
  (** The name of the constructor. *)

  val make : ('variant, 'args) t -> ('variant, 'args) make
  (** [make constr] is the representation of the value of the constructor.

      The constructor can either be constant or have arguments. *)
end

val constr :
  string -> ('variant, 'args) Constr.make -> ('variant, 'args) Constr.t
(** [constr name make : ('variant, 'args) Constr.t] is the runtime
    representation of the constructor with arguments of type ['args] belonging
    to the variant ['variant].

    The [make] argument is either [Const c], for a constant constructor [c], or
    [Args (args_t, variant_of_args)] that represents the constructor with
    arguments fo type [arg_t], where [variant_of_args] is the function from
    constructor arguments to the variant value.

    See the {!module:Constr} module for more operations on constructors. *)

(** Definitions for runtime variant representation. *)
module Variant : sig
  type 'variant t

  val name : 'variant t -> string
  val value : 'variant t -> 'variant -> 'variant Constr.value
  val constr_list : 'variant t -> 'variant Constr.any list
end

val variant :
  string ->
  'variant Constr.any list ->
  ('variant -> 'variant Constr.value) ->
  'variant typ
(** Variant representation.

    [varinat name constr_list value] is the runtime representation of a variant
    type with constructors described by [constr_list] where [name] is the type
    name of the variant and [value] is a function that maps every variant
    constructor to its runtime representation as a {!type:Constr.value}. *)

(** {1 Abstract types} *)

val abstract : string -> 'a typ

(** {1 Dynamic types} *)

(** Definitions for values with runtime type representation. *)
module Dyn : sig
  type 'a t = 'a dyn
  (** The type for dynamic values, i.e., values with a type representation. *)

  (** The type for dynamic values with existential variable. *)
  type any = Any : 'a dyn -> any

  val typ : 'a dyn -> 'a typ
  (** The runtime representation of the dynamic value. *)

  val value : 'a dyn -> 'a
  (** The value of the dynamic value. *)
end

(** {1 Type equality} *)

(** The type for equality between [typ] values. *)
type ('a, 'b) equal = Equal : ('a, 'a) equal

val equal : 'a typ -> 'b typ -> ('a, 'b) equal option
(** Checks for equality between two type representations. *)

(** {1 Mapper} *)

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

module Map : functor (Mapper : Mapper) -> sig
  val map : 'a typ -> 'a Mapper.t
  val register : 'a typ -> 'a Mapper.t -> unit
end
