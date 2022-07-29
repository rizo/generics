(* Type identifiers.
   See http://alan.petitepomme.net/cwn/2015.03.24.html#1 *)

module Instances = struct
  type _ t = ..
end

module type Instances = sig
  type t
  type _ Instances.t += Witness : t Instances.t
end

type 'a t = (module Instances with type t = 'a)

let make () (type s) =
  let module M = struct
    type t = s
    type _ Instances.t += Witness : t Instances.t
  end in
  (module M : Instances with type t = s)

let tag : type a. a t -> int =
 fun t ->
  let module W = (val t : Instances with type t = a) in
  Obj.magic W.Witness

type ('a, 'b) equal = Equal : ('a, 'a) equal

let equal : type r s. r t -> s t -> (r, s) equal option =
 fun r s ->
  let module R = (val r : Instances with type t = r) in
  let module S = (val s : Instances with type t = s) in
  match R.Witness with
  | S.Witness -> Some Equal
  | _ -> None
