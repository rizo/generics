type color = Red | Green | Blue | Rgb of int

let color_t =
  let red_constr = Generics.constr "Red" (Const Red) in
  let green_constr = Generics.constr "Green" (Const Green) in
  let blue_constr = Generics.constr "Blue" (Const Blue) in
  let rgb_constr =
    Generics.constr "Rgb" (Args (Generics.int, fun rgb -> Rgb rgb))
  in

  (* let (_ : color) = Generics.Case.make red_constr in
     let (_ : color) = Generics.Case.make rgb_constr 42 in *)
  Generics.variant "color"
    [
      Generics.Constr.Any rgb_constr;
      Generics.Constr.Any red_constr;
      Generics.Constr.Any green_constr;
      Generics.Constr.Any blue_constr;
    ] (fun variant ->
      match variant with
      | Red -> Generics.Constr.Value (red_constr, ())
      | Green -> Generics.Constr.Value (green_constr, ())
      | Blue -> Generics.Constr.Value (blue_constr, ())
      | Rgb rgb -> Generics.Constr.Value (rgb_constr, rgb))

type person = {
  name : string;
  age : int;
  profession : string option;
  favorite_color : color;
}

let alice =
  { name = "Alice"; age = 22; profession = None; favorite_color = Red }

let alice' =
  {
    name = "Alice";
    age = 22;
    profession = Some "Lawyer";
    favorite_color = Blue;
  }

let bob = { name = "Bob"; age = 34; profession = None; favorite_color = Rgb 42 }
let bob2 = { name = "Bob"; age = 42; profession = None; favorite_color = Green }

let person_t =
  let name_field = Generics.field "name" Generics.string (fun p -> p.name) in
  let age_field = Generics.field "age" Generics.int (fun p -> p.age) in
  let profession_field =
    Generics.field "profession" Generics.(option string) (fun p -> p.profession)
  in
  let favorite_color =
    Generics.field "favorite_color" color_t (fun p -> p.favorite_color)
  in
  Generics.record "person"
    [ name_field; age_field; profession_field; favorite_color ]
    (fun name age profession favorite_color ->
      { name; age; profession; favorite_color })

let default_person = Generics_default.map person_t
let in_channel_t : in_channel Generics.typ = Generics.abstract "in_channel"
(* let default_in_channel = Generics_default.make in_channel_t *)
