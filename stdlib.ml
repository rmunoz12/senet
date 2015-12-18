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

