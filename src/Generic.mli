type 'a t
(** The type for runtime representation of types. *)

type 'a typ = 'a t
(** Alias for ['a t]. *)

type 'a dyn = 'a t * 'a
(** The type for dynamic values, i.e., values with a type representation. *)

(** Type representation value with existential variable. *)
type any = Any : 'a t -> any

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

val option : 'a t -> 'a option typ
(** Runtime representation for option types. *)

val result : 'a t -> 'e t -> ('a, 'e) result typ
(** Runtime representation for result types. *)

(** {1 Records} *)

module Field : sig
  type ('record, 'a) t
  (** The runtime representation of fields with values of type ['a] belonging to
      the record of type ['record]. *)

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

val field : string -> 'a t -> ('t -> 'a) -> ('t, 'a) Field.t

module Record : sig
  type ('record, 'fields) t
  (** The runtime representation of records of type ['record] with fields of
      type ['fields].

      The type variable ['fields] is the function type from field values to the
      record. *)

  val name : ('record, 'fields) t -> string
  val fields : ('record, 'fields) t -> 'record Field.any list
  val fields' : ('record, 'fields) t -> ('record, 'fields) Field.list
  val map : ('record Field.any -> 'b) -> ('record, 'fields) t -> 'b list
  val fold : ('a -> 'record Field.any -> 'a) -> 'a -> ('record, 'fields) t -> 'a

  val make : ('record, 'fields) t -> 'fields
  (** [make record] is a smart constructor function for [record] represented as
      a function from fields to record of type ['record]. *)
end

val record : string -> ('record, 'fields) Field.list -> 'fields -> 'record typ

(** {1 Variants} *)

module Constr : sig
  type ('variant, 'args) t

  type ('variant, 'args) make =
    | Const : 'variant -> ('variant, unit) make
    | Args : 'args typ * ('args -> 'variant) -> ('variant, 'args) make

  val name : ('variant, 'args) t -> string
  val args : ('variant, 'args) t -> 'args typ option
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

val constr :
  string -> ('variant, 'args) Constr.make -> ('variant, 'args) Constr.t

val variant :
  string ->
  'variant Variant.constr list ->
  ('variant -> 'variant Variant.value) ->
  'variant t
(** Variant representation.

    [varinat name constr_list view] is the runtime representation of a variant
    type with constructors described by [constr_list] where [name] is the type
    name of the variant and [view] is a function that maps every variant
    constructor to its runtime representation as a {!t:Case.t}. *)

(** {1 Abstract types} *)

module Abstract : sig
  type 'a t

  val name : 'a t -> string
  val typ : 'a t -> 'a typ
end

val abstract : string -> 'a typ -> 'a typ

(** {1 Type representation equality} *)

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
  val abstract : mapper -> string -> 'a typ -> 'a t
end

module Map : functor (Mapper : Mapper) -> sig
  val map : 'a typ -> 'a Mapper.t
end
