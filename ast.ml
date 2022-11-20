(* Abstract Syntax Tree and functions for printing it *)

type op = 
  Add | Sub | Mul | Div | Mod | Eq | Neq | Less | Greater | Leq | Geq | And | Or | BWAnd | BWOr | Exp
type uop = Not

type typ_const =  Int_const | Bool_const | Char_const | Float_const | Void_const

type typ = 
  | Int | Bool | Float | Char | Void 
  | Arr of typ 
  | Const of typ_const 
  | Ftyp of ftyp
  | Dyn 
and ftyp =
{
  rtyp: typ;
  intypes: typ list;
}


(* int x: name binding *)
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
  | Subscription of string * expr
  (* function call *)
  | Call of string * expr list
  | Noexpr
  (* anonymous *)
  | Afunc of typ * bind list * stmt

and stmt =
  (* consider binding as a separate stmt *)
  | Bstmt of bind
  | Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt 
  | While of expr * stmt
  | Continue
  | Break
  (* return *)
  | Return of expr
  (* func_def *)
  | Func of func_def

and func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt;
}

(* func_def: ret_typ fname formals locals body *)

(* type program = bind list * func_def list *)
type program = stmt list


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
  | Ftyp(t) -> "function (" ^ String.concat ", " 
    (List.map string_of_typ t.intypes) ^ ") -> " ^ string_of_typ t.rtyp
  | Dyn -> "dyn"

let string_of_bind b= 
  "(" ^ string_of_typ (fst b) ^ ": " ^ snd b ^ ")"

let rec string_of_stmt stmt = 
  match stmt with
  | Bstmt(b) -> string_of_bind b ^ "\n"
  | Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | For(e1, e2, e3, s) -> 
    "roll (" ^ string_of_expr e1 ^ "; " ^ string_of_expr e2 ^ "; " ^ 
    string_of_expr e3 ^ ") " ^ string_of_stmt s
  | Continue -> "continue;"
  | Break -> "break;"
  | Func(f) -> string_of_func f
    
    (* "function " ^ string_of_bind b ^ " (" ^ String.concat ", " 
    (List.map string_of_bind bl) ^ ")\n" ^ string_of_stmt s ^ "\n" *)

and string_of_expr = function
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
  | Subscription(a, e) -> a ^ "[" ^ string_of_expr e ^ "]"
  | Noexpr -> ""
  | Afunc(t, bl, s) -> "lambda: " ^ string_of_typ t ^ " (" ^ String.concat ", " 
    (List.map string_of_bind bl) ^ ")\n" ^ string_of_stmt s ^ "\n"

and string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

and string_of_func func =
  string_of_typ func.rtyp ^ " " ^
  func.fname ^ "(" ^ String.concat ", " (List.map string_of_bind func.formals) ^
  ")\n"
  ^ (string_of_stmt func.body) ^
  "\n"

(* let string_of_program (vars, funcs) =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) *)
let string_of_program stmts =
  "\n\nParsed program: \n\n" ^ 
  String.concat "\n" (List.map string_of_stmt stmts)
