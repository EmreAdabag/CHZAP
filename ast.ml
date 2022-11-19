
(* Abstract Syntax Tree and functions for printing it *)

type op = 
  Add | Sub | Mul | Div | Mod | Eq | Neq | Less | Greater | Leq | Geq | And | Or | BWAnd | BWOr | Exp
type uop = Not

type typ_const =  Int_const | Bool_const | Char_const | Float_const | Void_const  
type func_typ = typ * typ list
and typ = 
  | Int | Bool | Char | Float | Void 
  | Arr of typ 
  | Const of typ_const 
  | Ftyp of func_typ

(* int x: name binding *)
(* type bind = Bind of typ * string *)
type bind = typ * string
(* int f() { ... }: function binding *)
(* and fbind = func_typ * bind list * stmt list * string *)

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
  (* | FuncDef of func_def *)
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
  (* | FuncDef of func_def *)

(* func_def: ret_typ fname formals locals body *)
(* type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt list;
} *)

(* func_def: ftyp fname decls body *)
type func_def = {
  ftyp: func_typ;
  rtyp: typ;
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt list;
}

(* let func_to_def = function
  | FuncTyp(rt, fn, fs, ls, bd) -> { rtyp = rt; fname = fn; formals = fs; locals = ls; body = bd} *)

(* type program = bind list * func_def list *)
type program = bind list * func_def list

(* let stmt_of_fdecl = function
  | fd -> FuncDef(fd) *)

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

let string_of_const_typ = function
  | Int_const -> "int"
  | Bool_const -> "bool"
  | Char_const -> "char"
  | Float_const -> "float"
  | Void_const -> "void"

let rec string_of_typ = function
  | Int -> "int"
  | Bool -> "bool"
  | Char -> "char"
  | Float -> "float"
  | Arr(t) -> string_of_typ t ^ "[]"
  | Const(t) ->"const " ^ string_of_const_typ t 
  | Void -> "void"
  | Ftyp((rt, tl)) -> string_of_typ rt ^ " (" ^ 
    String.concat ", " (List.map string_of_typ tl) ^ ")"

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
    "(" ^ string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2 ^ ")"
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Subsription(a, e) -> a ^ "[" ^ string_of_expr e ^ "]"
  | Noexpr -> ""
  (* | FuncDef(fd) -> string_of_fdecl fd *)

let rec string_of_stmt = function
  | VoidStmt -> "void;"
  (* | Func(b, bl, s) -> 
    string_of_bind b ^ " (" ^ String.concat ", " (List.map string_of_bind bl) ^ ")\n" ^ 
    string_of_stmt s ^ "\n" *)
  | Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^ 
                      string_of_stmt s1 ^ ( if s2 = Block([]) then "" else "else\n" ^ string_of_stmt s2)
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | For(e1, e2, e3, s) -> 
    "roll (" ^ string_of_expr e1 ^ "; " ^ string_of_expr e2 ^ "; " ^ 
    string_of_expr e3 ^ ") " ^ string_of_stmt s
  | Continue -> "continue;"
  | Break -> "break;"

and string_of_bind = function
  | t, s -> string_of_typ t ^ " " ^ s ^ ";\n"

(* and string_of_fbind = function
  | (rt, fs), decls, stmts, s -> string_of_typ rt ^ " " ^ s ^ " (" ^ 
  String.concat ", " (List.map string_of_bind fs) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_bind decls) ^
  String.concat "" (List.map string_of_stmt stmts) ^
  "}\n" *)

(* and string_of_ftyp = function
  | rt, fs -> string_of_typ rt ^ " (" ^
    String.concat ", " (List.map string_of_typ (List.map fst fs)) ^ ")" *)

and string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

and string_of_fdecl fdecl =
  string_of_typ fdecl.rtyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_bind fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_bind vars) ^ "\n" ^
  (* String.concat "\n" (List.map string_of_fdecl funcs) *)
  String.concat "\n" (List.map string_of_fdecl funcs)
