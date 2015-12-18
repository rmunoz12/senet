type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq
        | Mod | And | Or

type bool_lit =
    True
  | False

type t =
    Int
  | Bool
  | Str
  | Void
  | List_t of t
  | Group of string * group_decl option  (* 2nd value filled in by Cast *)

and list_lit =
    Elems of expression list * string
  | EmptyList

and var_decl = {
    vname : string;
    vtype : t;
    vinit : expression option
}

and expr_detail =
    IntLiteral of int * string
  | StrLiteral of string * string
  | ListLiteral of list_lit
  | BoolLiteral of bool_lit * string
  | VoidLiteral
  | Field of field_expr
  | Binop of expression * op * expression
  | Assign of field_expr * expression
  | Call of var_decl option * func_decl * expression list
  | Element of expression * expression
  | Uminus of expression
  | Not of expression
  | Noexpr
  | Remove of field_expr * field_expr * list_lit
  | Place of field_expr * field_expr * list_lit

and expression =
    expr_detail * t

and field_expr =
    Var of var_decl
  | Attrib of var_decl * var_decl
  | Fun of func_decl
  | Method of var_decl * func_decl
  | Grp of group_decl
  | This

and statement =
    Block of symbol_table * statement list
  | Expression of expression
  | Return of expression
  | Break
  | Continue
  | End
  | Pass of func_decl * expression
  | If of expression * statement * statement
  | For of var_decl * expression list * statement
  | While of expression * statement

and basic_func_decl = {
    ftype : t;
    fname : string;
    formals : var_decl list;
    locals : var_decl list;
    body : statement list;
    turns_func : bool;
    group_method : string;
    f_is_built_in : bool
  }

and assert_decl = {
    aname : string;
    aformals : var_decl list;
    alocals : var_decl list;
    abody : statement list;
    a_turns_func : bool;
    a_group_method : string;
    a_is_built_in : bool
  }

and func_decl =
    BasicFunc of basic_func_decl
  | AssertFunc of assert_decl

and group_decl = {
    gname : string;
    extends : group_decl option;
    par_actuals : expression list option;
    attributes : var_decl list;
    methods : func_decl list;
  }

and setup = var_decl list * func_decl list * group_decl list

and turns = func_decl list

and program = setup * turns


and symbol_table = {
  parent : symbol_table option;
  mutable variables : var_decl list;
  mutable functions : func_decl list;
  mutable groups : group_decl list;
  mutable turns : string list;
  mutable ll_count : int;
  mutable elem_count : int
}

type partial_group_table = {
  group_name : string;
  par : group_decl option;
  symbols : symbol_table;
}

type translation_environment = {
  scope : symbol_table;
  return_type : t;
  in_loop : bool;
  partial_group_info : partial_group_table option
}
