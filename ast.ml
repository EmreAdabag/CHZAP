(* Abstract Syntax Tree and functions for printing it *)

type op = 
  Add | Sub | Mul | Div | Mod | Eq | Neq | Less | Greater | Leq | Geq | And | Or | BWAnd | BWOr
type uop = Not

(* type ftyp = typ * typ list *)
type typ = 
  | Int | Bool | Float | Char | String | Void 
  | Arr of typ * int
  | Darr of typ
  | Const of typ 
  | Ftyp of typ * typ list

type infer_typ = 
  | Auto 
  | Const_auto

(* int x: name binding *)
type bind = Bind of typ * string

type expr =
  | IntLit of int
  | BoolLit of bool
  | CharLit of char
  | StringLit of string
  | FloatLit of float
  | ArrayLit of expr list
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | CAssign of string * expr
  | Subscription of string * expr
  (* function call *)
  | Call of string * expr list
  | Noexpr
  (* anonymous *)
  | Afunc of typ * bind list * stmt

and stmt =
  (* consider binding as a separate stmt *)
  | Bstmt of bind
  | BAstmt of bind * expr
  | BAIstmt of infer_typ * string * expr
  | Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | For of stmt * expr * expr * stmt 
  | While of expr * stmt
  | Continue
  | Break
  | Assert of expr
  (* return *)
  | Return of expr
  (* func_def *)
  | Func of bind * bind list * stmt

(* 
(* func_def: ret_typ fname formals locals body *)
type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt list;
} *)

(* type program = bind list * func_def list *)
type program = stmt list


(* Pretty-printing functions *)
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
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
  | Bool -> "bool"
  | Char -> "char"
  | String -> "string"
  | Float -> "float"
  | Arr(t, s) -> string_of_typ t ^ "[" ^ (string_of_int s) ^ "]"
  | Darr(t) -> string_of_typ t ^ "[]"
  | Const(t) ->"const " ^ string_of_typ t 
  | Void -> "void"
  | Ftyp(t, tl) -> "function (" ^ String.concat ", " 
    (List.map string_of_typ tl) ^ ") -> " ^ string_of_typ t

let rec string_of_infer_typ = function
  | Auto -> "auto"
  | Const_auto -> "const auto"

let string_of_bind (b : bind) = match b with
  | Bind(t, s) -> s ^ ": " ^ string_of_typ t

let rec string_of_expr = function
  | IntLit(l) -> string_of_int l
  | FloatLit(l) -> string_of_float l
  | CharLit(l) -> Char.escaped l
  | StringLit(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | ArrayLit(el) -> "[" ^ String.concat "," (List.map string_of_expr el) ^ "]"
  | Id(s) -> s
  | Unop (o, e) -> string_of_uop o ^ string_of_expr e
  | Binop(e1, o, e2) ->
    "(" ^ string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2 ^ ")"
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | CAssign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Subscription(a, e) -> a ^ "[" ^ string_of_expr e ^ "]"
  | Noexpr -> ""
  | Afunc(t, bl, s) -> "lambda: " ^ string_of_typ t ^ " (" ^ String.concat ", " 
    (List.map string_of_bind bl) ^ ") {\n" ^ string_of_stmt s ^ "}"

and string_of_stmt stmt = 

  match stmt with
  | Bstmt(b) -> string_of_bind b ^ "\n"
  | BAstmt(Bind(t, n), e) -> 
    string_of_bind (Bind(t, n)) ^ " " ^ string_of_expr (Assign(n, e)) ^ "\n"
  | BAIstmt(t, n, e) -> 
    n ^ ": " ^ string_of_infer_typ t ^ string_of_expr (Assign(n, e)) ^ "\n"
  | Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | For(s1, e2, e3, s) -> 
    "roll (" ^ string_of_stmt s1 ^ "; " ^ string_of_expr e2 ^ "; " ^ 
    string_of_expr e3 ^ ") " ^ string_of_stmt s
  | Continue -> "continue;"
  | Break -> "break;"
  | Assert(e) -> "assert " ^ (string_of_expr e) ^ ";\n"
  | Func(b, bl, s) -> "function " ^ string_of_bind b ^ " (" ^ String.concat ", " 
    (List.map string_of_bind bl) ^ ")\n" ^ string_of_stmt s ^ "\n"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

(* let string_of_fdecl fdecl =
  string_of_typ fdecl.rtyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n" *)

(* let string_of_program (vars, funcs) =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) *)
let string_of_program stmts =
  "\n\nParsed program: \n\n" ^ 
  String.concat "\n" (List.map string_of_stmt stmts)
