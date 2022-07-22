type person = { name : string; age : int; profession : string option }

let alice = { name = "Alice"; age = 22; profession = None }
let alice' = { name = "Alice"; age = 22; profession = Some "Lawyer" }
let bob = { name = "Bob"; age = 34; profession = None }
let bob2 = { name = "Bob"; age = 42; profession = None }

let person_t =
  let name_field = Generic.field "name" Generic.string (fun p -> p.name) in
  let age_field = Generic.field "age" Generic.int (fun p -> p.age) in
  let profession_field =
    Generic.field "profession" Generic.(option string) (fun p -> p.profession)
  in
  Generic.record "person" [ name_field; age_field; profession_field ]
    (fun name age profession -> { name; age; profession })

type color = Red | Green | Blue | Rgb of int

let color_t =
  let red_constr = Generic.Constr.const "Red" Red in
  let green_constr = Generic.Constr.const "Green" Green in
  let blue_constr = Generic.Constr.const "Blue" Blue in
  let rgb_constr =
    Generic.Constr.apply "Rgb" Generic.int (fun rgb -> Rgb rgb)
  in

  (* let (_ : color) = Generic.Case.make red_constr in
     let (_ : color) = Generic.Case.make rgb_constr 42 in *)
  Generic.variant "color"
    [
      Generic.Variant.Constr red_constr;
      Generic.Variant.Constr green_constr;
      Generic.Variant.Constr blue_constr;
      Generic.Variant.Constr rgb_constr;
    ] (fun variant ->
      match variant with
      | Red -> Generic.Variant.Value (red_constr, ())
      | Green -> Generic.Variant.Value (green_constr, ())
      | Blue -> Generic.Variant.Value (blue_constr, ())
      | Rgb rgb -> Generic.Variant.Value (rgb_constr, rgb))
