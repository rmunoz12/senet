open Sast

let rec get_attributes g = match g.extends with
    None -> g.attributes
  | Some(par) ->
      let pa = get_attributes par in
      pa @ g.attributes

let rec order_attrib = function
    [] -> []
  | g :: rest ->
      let a = get_attributes g in
      let new_g = {g with attributes = a} in
      new_g :: order_attrib rest

let group_eval (program : Sast.program) =
  let setup, turns = program in
  let v, f, g = setup in
  let g = order_attrib g in
  let setup = v, f, g in
  let program = setup, turns in
  program

let print_program (program : Sast.program) =
  raise (Sast.SemError "Not Implemented")

