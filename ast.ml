
(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mul | Div | Mod | Eq | Neq | Less | Greater | Leq | Geq | And | Or | BWAnd | BWOr | Exp

type uop = Not
type typ_const =  Int_const | Bool_const | Char_const | Float_const | Void_const  
type typ = Int | Bool | Char | String | Float | Void | Arr of typ | Const of typ_const


type expr =
  | IntLit of int
  | BoolLit of bool
  | CharLit of char
  | Strlit of string
  | FloatLit of float
  | ArrayLit of expr list
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of assign
  | Subscription of string * expr
  (* function call *)
  | Call of string * expr list
  | Noexpr
  and assign = 
    LongAssign of typ * string * expr
    | ShortAssign of string * expr

type stmt =
  | Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt 
  | While of expr * stmt
  | Continue
  | Break
  (* return *)
  | Return of expr

(* int x: name binding *)
type bind = typ * string
(* int x = 1: full declaration binding *)
type bindplus = typ * string * expr

(* func_def: ret_typ fname formals locals body *)
type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt list;
}

(* type program = bind list * func_def list *)
type program = bindplus list * func_def list

(* Pretty-printing functions *)
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Exp -> "**"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | BWAnd -> "&"
  | BWOr -> "|"

let string_of_uop = function
   Not -> "!"

let  string_of_const_typ = function
  Int_const -> "int"
  | Bool_const -> "bool"
  | Char_const -> "char"
  | Float_const -> "float"
  | Void_const -> "void"

let rec string_of_typ = function
   Int -> "int"
 | Bool -> "bool"
 | Char -> "char"
 | String -> "string"
 | Float -> "float"
 | Arr(t) -> string_of_typ t ^ "[]"
 | Const(t) ->"const " ^ string_of_const_typ t 
 | Void -> "void"

let rec string_of_expr = function
  | IntLit(l) -> string_of_int l
  | FloatLit(l) -> string_of_float l
  | CharLit(l) -> Char.escaped l
  | Strlit(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | ArrayLit(el) -> "[" ^ String.concat "," (List.map string_of_expr el) ^ "]"
  | Id(s) -> s
  | Unop (o, e) -> string_of_uop o ^ string_of_expr e
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  (* | Assign(v, e) -> v ^ " = " ^ string_of_expr e *)
  | Assign(a) -> string_of_assign a
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Subscription(a, e) -> a ^ "[" ^ string_of_expr e ^ "]"
  | Noexpr -> ""
  and string_of_assign = function
    | LongAssign(t, v, e) -> string_of_typ t ^ v ^ " = " ^ string_of_expr e
    | ShortAssign(v, e) -> v ^ " = " ^ string_of_expr e

let rec string_of_stmt = function
  | Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^ 
                      string_of_stmt s1 ^ ( if s2 = Block([]) then "" else "else\n" ^ string_of_stmt s2)
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | For(e1, e2, e3, s) -> "roll (" ^ string_of_expr e1 ^ "; " ^ string_of_expr e2 ^ "; " ^ string_of_expr e3 ^ ") " ^ string_of_stmt s
  | Continue -> "continue;"
  | Break -> "break;"



(* let  string_of_const_typ = function
Int_const -> "int"
| Bool_const -> "bool"
| Char_const -> "char"
| Float_const -> "float"
| Void_const -> "void" *)


(* let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Char -> "char"
  | String -> "string"
  | Float -> "float"
  | Arr(t) -> string_of_typ t ^ "[]"
  | Const(t) ->"const " ^ string_of_const_typ t 
  | Void -> "void" *)





let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_vdeclplus (t, id, e) = string_of_typ t ^ " " ^ id ^ string_of_expr e ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.rtyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  "\n\nParsed program: \n\n" ^
  (* String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^ *)
  String.concat "" (List.map string_of_vdeclplus vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
