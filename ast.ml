
(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mul | Div | Mod | Eq | Neq | Less | Greater | Leq | Geq | And | Or | BWAnd | BWOr | Exp
type uop = Not

type typ_const =  Int_const | Bool_const | Char_const | Float_const | Void_const  
type typ = Int | Bool | Char | Float | Void | Arr of typ | Const of typ_const 

(* int x: name binding *)
(* type bind = Bind of typ * string *)
type bind = typ * string


type expr =
  | IntLit of int
  | BoolLit of bool
  | CharLit of char
  | FloatLit of float
  | ArrayLit of expr list
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Subsription of string * expr
  (* function call *)
  | Call of string * expr list
  | Noexpr

type stmt =
  | VoidStmt
  (* | Func of bind * bind list * stmt *)
  | Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt 
  | While of expr * stmt
  | Continue
  | Break
  (* return *)
  | Return of expr
  (* | ReturnVoid *)


(* func_def: ret_typ fname formals locals body *)
type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt list;
}

type program = bind list * func_def list

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

let rec string_of_typ = function
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | Char -> "char"
  | Void -> "void"
  | Arr(t) ->  string_of_typ t ^ " []"
  | Func -> "func"

(* let string_of_bind = function
  | Bind(t, s) -> s ^ ": " ^ string_of_typ t *)

let rec string_of_expr = function
  | IntLit(l) -> string_of_int l
  | FloatLit(l) -> string_of_float l
  | CharLit(l) -> Char.escaped l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | ArrayLit(el) -> "[" ^ String.concat "," (List.map string_of_expr el) ^ "]"
  | Id(s) -> s
  | Unop (o, e) -> string_of_uop o ^ string_of_expr e
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Subsription(a, e) -> a ^ "[" ^ string_of_expr e ^ "]"
  | Noexpr -> ""

let rec string_of_stmt = function
  | VoidStmt -> "void;"
  (* | Func(b, bl, s) -> 
    string_of_bind b ^ " (" ^ String.concat ", " (List.map string_of_bind bl) ^ ")\n" ^ 
    string_of_stmt s ^ "\n" *)
  | Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
<<<<<<< HEAD
  (* | ReturnVoid -> "return void;\n" *)
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
=======
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^ 
                      string_of_stmt s1 ^ ( if s2 = Block([]) then "" else "else\n" ^ string_of_stmt s2)
>>>>>>> df21846dce052b723fa3df86d5c0887e24beb3e0
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | For(e1, e2, e3, s) -> "roll (" ^ string_of_expr e1 ^ "; " ^ string_of_expr e2 ^ "; " ^ string_of_expr e3 ^ ") " ^ string_of_stmt s
  | Continue -> "continue;"
  | Break -> "break;"



let  string_of_const_typ = function
Int_const -> "int"
| Bool_const -> "bool"
| Char_const -> "char"
| Float_const -> "float"
| Void_const -> "void"


<<<<<<< HEAD
=======
let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Char -> "char"
  | Float -> "float"
  | Arr(t) -> string_of_typ t ^ "[]"
  | Const(t) ->"const " ^ string_of_const_typ t 
  | Void -> "void"





>>>>>>> df21846dce052b723fa3df86d5c0887e24beb3e0
let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.rtyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
