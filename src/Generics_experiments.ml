module X1 = struct
  type no_args
  type 'args args = Args of 'args

  type ('variant, 'make) case =
    | Const : { name : string; value : 'variant } -> ('variant, 'variant) case
    | Apply : {
        name : string;
        args : 'args Generics.t;
        make : 'args -> 'variant;
      }
        -> ('variant, 'args -> 'variant) case

  let none = Const { name = "None"; value = None }
  let some args = Apply { name = "Some"; args; make = (fun x -> Some x) }

  let make : type variant make. (variant, make) case -> make =
   fun case ->
    match case with
    | Const const -> const.value
    | Apply apply -> apply.make

  let (_ : 'a option) = make none
  let (_ : 'a option) = make (some Generics.int) 42
end

module X2 = struct
  type no_args
  type !'args args

  type ('variant, 'args) case =
    | Const : { name : string; value : 'variant } -> ('variant, no_args) case
    | Apply : {
        name : string;
        args : 'args Generics.t;
        make : 'args -> 'variant;
      }
        -> ('variant, 'args args) case

  let none = Const { name = "None"; value = None }
  let some args = Apply { name = "Some"; args; make = (fun x -> Some x) }

  let make_const : type variant. (variant, no_args) case -> variant =
   fun case ->
    match case with
    | Const const -> const.value

  let make_apply : type variant. (variant, 'args args) case -> 'args -> variant
      =
   fun case ->
    match case with
    | Apply apply -> apply.make

  let (_ : 'a option) = make_const none
  let (_ : 'a option) = make_apply (some Generics.int) 42
end

module X3 = struct
  type no_args
  type !'args args

  type ('variant, 'args) case =
    | Const : { name : string; value : 'variant } -> ('variant, no_args) case
    | Apply : {
        name : string;
        args : 'args Generics.t;
        make : 'args -> 'variant;
      }
        -> ('variant, 'args args) case

  type ('variant, 'make) make =
    | MkConst : ('variant, no_args) case -> ('variant, 'variant) make
    | MkApply :
        ('variant, 'args args) case
        -> ('variant, 'args -> 'variant) make

  let none = Const { name = "None"; value = None }
  let some args = Apply { name = "Some"; args; make = (fun x -> Some x) }

  let make : type variant args. (variant, args) make -> args =
   fun case ->
    match case with
    | MkConst (Const const) -> const.value
    | MkApply (Apply apply) -> apply.make

  let (_ : 'a option) = make (MkConst none)
  let (_ : 'a option) = make (MkApply (some Generics.int)) 42
end

module X4 = struct
  module Constr = struct
    type ('variant, 'args) make =
      | Const : 'variant -> ('variant, unit) make
      | Args : 'args Generics.t * ('args -> 'variant) -> ('variant, 'args) make

    type ('variant, 'args) t = { name : string; make : ('variant, 'args) make }
  end

  module Variant = struct
    type 'variant constr =
      | Constr : ('variant, 'args) Constr.t -> 'variant constr

    type 'variant value =
      | Value : ('varaint, 'args) Constr.t * 'args -> 'variant value

    let value : ('variant, 'args) Constr.t -> 'args -> 'variant value =
     fun constr value -> Value (constr, value)

    type 'variant t = {
      constrs : 'variant constr list;
      view : 'variant -> 'variant value;
    }
  end

  let constr name make : ('variant, 'args) Constr.t = { name; make }
  let variant constrs view = { Variant.constrs; view }

  let option t =
    let none_constr = constr "None" (Const None) in
    let some_constr = constr "Some" (Args (t, fun x -> Some x)) in
    let view variant =
      match variant with
      | None -> Variant.value none_constr ()
      | Some x -> Variant.value some_constr x
    in
    variant [ Variant.Constr none_constr; Variant.Constr some_constr ] view
end

module X5_broken = struct
  module Constr = struct
    type ('variant, 'args) args =
      ('args Generics.t * ('args -> 'variant)) option

    type ('variant, 'args) t = { name : string; args : ('variant, 'args) args }
  end

  module Variant = struct
    type 'variant constr =
      | Constr : ('variant, 'args) Constr.t -> 'variant constr

    type 'variant value =
      | Value : ('varaint, 'args) Constr.t * 'args -> 'variant value

    let value : ('variant, 'args) Constr.t -> 'args -> 'variant value =
     fun constr value -> Value (constr, value)

    type 'variant t = {
      constrs : 'variant constr list;
      view : 'variant -> 'variant value;
    }
  end

  let constr name args : ('variant, 'args) Constr.t = { name; args }
  let variant constrs view = { Variant.constrs; view }

  type color = Red | Green | Blue | Rgb of int

  let color_t =
    let red_constr = constr "Red" None in
    let green_constr = constr "Green" None in
    let blue_constr = constr "Blue" None in
    let rgb_constr = constr "Rgb" (Some (Generics.int, fun x -> Rgb x)) in
    let view variant =
      match variant with
      | Red -> Variant.value red_constr ()
      | Green -> Variant.value green_constr ()
      | Blue -> Variant.value blue_constr ()
      | Rgb x -> Variant.value rgb_constr x
    in
    variant
      [
        Variant.Constr red_constr;
        Variant.Constr green_constr;
        Variant.Constr blue_constr;
        Variant.Constr rgb_constr;
      ]
      view
end

module X6 = struct
  module Constr = struct
    type 'variant const = { name : string; value : 'variant }

    type ('variant, 'args) with_args = {
      name : string;
      args : 'args Generics.t;
      make : 'args -> 'variant;
    }

    type 'variant t =
      | Const : 'variant const -> 'variant t
      | With_args : ('variant, 'args) with_args -> 'variant t

    let const name value = { name; value }
    let with_args name args make = { name; args; make }
  end

  module Variant = struct
    type 'variant value =
      | Const : 'variant Constr.const -> 'variant value
      | With_args : ('variant, 'args) Constr.with_args * 'args -> 'variant value

    type 'variant t = {
      constr_list : 'variant Constr.t list;
      view : 'variant -> 'variant value;
    }
  end

  let variant constr_list view = { Variant.constr_list; view }

  type color = Red | Green | Blue | Rgb of int

  let color_t =
    let red_constr = Constr.const "Red" Red in
    let green_constr = Constr.const "Green" Green in
    let blue_constr = Constr.const "Blue" Blue in
    let rgb_constr = Constr.with_args "Rgb" Generics.int (fun x -> Rgb x) in
    let view variant =
      match variant with
      | Red -> Variant.Const red_constr
      | Green -> Variant.Const green_constr
      | Blue -> Variant.Const blue_constr
      | Rgb x -> Variant.With_args (rgb_constr, x)
    in
    variant
      [
        Constr.Const red_constr;
        Constr.Const green_constr;
        Constr.Const blue_constr;
        Constr.With_args rgb_constr;
      ]
      view
end

module type X7 = sig
  type 'a typ
  type 't variant
  type ('variant, 'args) constr
  type 'a constr_val

  val constr_const : string -> 'variant -> ('variant, unit) constr

  val constr_args :
    string -> 'args typ -> ('args -> 'variant) -> ('variant, 'args) constr

  val constr_const_val : ('variant, unit) constr -> 'variant constr_val
  val constr_args_val : ('variant, 'args) constr -> 'args -> 'variant constr_val
end
