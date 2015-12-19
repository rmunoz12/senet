open Types

let funcs =
  let s =
    { vname = "print_arg";
      vtype = Str;
      vinit = None;
      vloop = false }
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
  let v = { vname = "PLAYER_ON_MOVE" ; vinit = Some(init, Int) ;
            vtype = Int; vloop = false }
  in
  [v]

let obj =
  let stmt = Return(Field(This), Group("Object", None)) in
  let init =
    { ftype = Group("Object", None);
      fname = "__init__";
      formals = [];
      locals = [];
      body = [stmt];
      turns_func = false;
      group_method = "Object";
      f_is_built_in = true }
  in
  { gname = "Object";
    extends = None;
    par_actuals = None;
    attributes = [];
    methods = [BasicFunc(init)] }

let board =
  let v =
    { vname = "cells"; vtype = List_t(Int); vinit = None; vloop = false }
  in
  let attr = [v] in
  let v = { v with vname = "x"; vtype = Int } in
  let remove =
    { ftype = Void; fname = "remove"; formals = [v]; locals = []; body = [];
      turns_func = false; group_method = "Board"; f_is_built_in = true }
  in
  let owns = { remove with ftype = Int; fname = "owns" } in
  let v = { v with vname = "l"; vtype = List_t(Int) } in
  let toi = { owns with fname = "toi"; formals = [v] } in
  let tol = { owns with ftype = List_t(Int); fname = "tol" } in
  let meth =
    [BasicFunc(remove); BasicFunc(owns); BasicFunc(owns);
     BasicFunc(toi); BasicFunc(tol)]
  in
  { obj with gname = "Board"; extends = Some(obj);
    attributes = attr; methods = meth }

let piece =
  let v = { vname = ""; vtype = Void; vinit = None; vloop = false } in
  let owner = { v with vname = "owner"; vtype = Int } in
  let fixed = { v with vname = "fixed"; vtype = Bool } in
  let b = { v with vname = "b"; vtype = Group("Board", None) } in
  let x = { v with vname = "x"; vtype = Int } in
  let place =
    { ftype = Bool; fname = "place"; formals = [b; x]; locals = []; body = [];
      turns_func = false; group_method = "Piece"; f_is_built_in = true }
  in
  { obj with gname = "Piece"; extends = Some(obj);
    attributes = [owner; fixed]; methods = [BasicFunc(place)] }

let boards_lib =
  let x = { vname = "x"; vtype = Int; vinit = None; vloop = false } in
  let y = { x with vname = "y" } in
  let rect =
    { board with gname = "Rect"; extends = Some(board); attributes = [x; y] }
  in
  let loop = { rect with gname = "Loop"; attributes = [x] } in
  let line = { loop with gname = "Line" } in
  let hex = { loop with gname = "Hex" } in
  [rect; loop; line; hex]

let grps =
  [obj; board; piece] @ boards_lib
