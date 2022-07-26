type color = Red | Green | Blue | Rgb of int

let color_t =
  let red_constr = Generic.constr "Red" (Const Red) in
  let green_constr = Generic.constr "Green" (Const Green) in
  let blue_constr = Generic.constr "Blue" (Const Blue) in
  let rgb_constr =
    Generic.constr "Rgb" (Args (Generic.int, fun rgb -> Rgb rgb))
  in

  (* let (_ : color) = Generic.Case.make red_constr in
     let (_ : color) = Generic.Case.make rgb_constr 42 in *)
  Generic.variant "color"
    [
      Generic.Constr.Any rgb_constr;
      Generic.Constr.Any red_constr;
      Generic.Constr.Any green_constr;
      Generic.Constr.Any blue_constr;
    ] (fun variant ->
      match variant with
      | Red -> Generic.Constr.Value (red_constr, ())
      | Green -> Generic.Constr.Value (green_constr, ())
      | Blue -> Generic.Constr.Value (blue_constr, ())
      | Rgb rgb -> Generic.Constr.Value (rgb_constr, rgb))

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
  let name_field = Generic.field "name" Generic.string (fun p -> p.name) in
  let age_field = Generic.field "age" Generic.int (fun p -> p.age) in
  let profession_field =
    Generic.field "profession" Generic.(option string) (fun p -> p.profession)
  in
  let favorite_color =
    Generic.field "favorite_color" color_t (fun p -> p.favorite_color)
  in
  Generic.record "person"
    [ name_field; age_field; profession_field; favorite_color ]
    (fun name age profession favorite_color ->
      { name; age; profession; favorite_color })
