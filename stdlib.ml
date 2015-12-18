open Types

let funcs =
  let s =
    { vname = "print_arg";
      vtype = Str;
      vinit = None}
  in
  let b = { s with vtype = Bool } in
  let i = { s with vtype = Int } in
  let g = { s with vtype = Group("", None) } in
  let print_str =
    { ftype = Void;
      fname = "print";
      formals = [s];
      locals = [];
      body = [];
      turns_func = false;
      group_method = "";
      f_is_built_in = true }
  in
  let print_bool = { print_str with formals = [b] } in
  let print_int = { print_str with formals = [i] } in
  let print_group = { print_str with formals = [g] } in
  [BasicFunc(print_str); BasicFunc(print_bool); BasicFunc(print_int);
   BasicFunc(print_group)]

let vars =
  let init = IntLiteral(0, "") in
  let v = { vname = "PLAYER_ON_MOVE" ; vinit = Some(init, Int) ; vtype = Int } in
  [v]

let grps =
  let stmt = Return(Field(This), Group("Object", None)) in
  let init =
    { ftype = Group("Object", None);
      fname = "__init__";
      formals = [];
      locals = [];
      body = [stmt];
      turns_func = false;
      group_method = "Object";
      f_is_built_in = true } in
  let obj =
    { gname = "Object";
      extends = None;
      par_actuals = None;
      attributes = [];
      methods = [BasicFunc(init)] } in
  [obj]
