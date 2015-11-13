open Ast

exception SemError of string (* * Lexing.lexbuf *)

type t =
    Void
  | Int
  | Struct of string * ((string * t) array) (* name, fields *)
  | Array of t * int                        (* type, size *)
  | Exception of string

type expr_detail =
    IntConst of int
  | Id of Ast.var_decl


type expression = expr_detail * t

(*
type expr =
    IntLiteral of int
  | StrLiteral of string
  | ListLiteral of list_lit
  | BoolLiteral of bool_lit
  | VoidLiteral
  | Field of field_expr
  | Binop of expr * op * expr
  | Assign of field_expr * expr
  | Call of field_expr * expr list
  | Element of expr * expr
  | Uminus of expr
  | Not of expr
  | Noexpr
  | Remove of field_expr * field_expr * list_lit
  | Place of field_expr * field_expr * list_lit *)

let check_expression = function
    IntLiteral(i) -> true
  | StrLiteral(s) -> true
  | ListLiteral(ll) -> true
  | BoolLiteral(b) -> true
  | VoidLiteral -> true
  | Field(fd) -> true
  | Binop(e1, o, e2) -> true
  | Assign(fd, e) -> true
  | Call(fd, el) -> true
  | Element(e1, e2) -> true
  | Uminus(e) -> true
  | Not(e) -> true
  | Noexpr -> true
  | Remove(fd1, fd2, ll) -> true
  | Place(fd1, fd2, ll) -> true

let check_single_statement = function
    Block(slist) -> true
  | Expr(e) -> check_expression(e)
  | Return(e) -> true
  | Break -> true
  | Continue -> true
  | If(e, s1, s2) -> true
  | For(e, elist, s) -> true
  | While(e, s) -> true

let rec check_statments = function
    [] -> true
  | hd :: tl -> ignore(check_single_statement(hd));
                check_statments(tl)

let check_basic_function = function
    BasicFunc(f) -> check_statments(f.body)
  | AssertFunc(f) -> false

let rec check_for_begin(t) = match t with
  | [] -> false
  | hd :: tl ->
        ( match hd with
            BasicFunc(f) -> if f.fname = "begin" then true
                       else check_for_begin(tl)
          | AssertFunc(f) -> check_for_begin(tl))

let check_program (s, t) =
  if check_for_begin(t) = false then raise (SemError ("No begin @turns function"))
  else "Success"

