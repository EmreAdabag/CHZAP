(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
  | SIntLit of int
  | SBoolLit of bool
  | SCharLit of char
  | SFloatLit of float
  | SArrayLit of sexpr list
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SAssign of string * sexpr
  | SSubscription of string * sexpr
  (*S function call *)
  | SCall of string * sexpr list
  | SNoexpr
  (*S anonymous *)
  | SAfunc of typ * bind list * sstmt

and sstmt =
  (* consider binding as a separate stmt *)
  | SBstmt of bind
  | SBAstmt of bind * sx
  | SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sstmt * sexpr * sexpr * sstmt 
  | SWhile of sexpr * sstmt
  | SContinue
  | SBreak
  (*S return *)
  | SReturn of sexpr
  (*S func_def *)
  | SFunc of bind * bind list * sstmt

(* and sfunc_def = {
  srtyp: typ;
  sfname: string;
  sformals: bind list;
  slocals: bind list;
  sbody: sstmt;
} *)

type sprogram = sstmt list



(* Pretty-printing functions *)
let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
      | SIntLit(l) -> string_of_int l
      | SBoolLit(true) -> "true"
      | SBoolLit(false) -> "false"
      | SCharLit(l) -> Char.escaped l
      | SFloatLit(l) -> string_of_float l
      | SArrayLit(l) -> "[" ^ String.concat "," (List.map string_of_sexpr l) ^ "]"
      | SId(s) -> s
      | SUnop(o, e) -> "TODO SUnop"
      | SBinop(e1, o, e2) ->
        string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
      | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
      | SCall(f, el) ->
          f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
      | SSubscription(a, e) -> a ^ "[" ^ string_of_sexpr e ^ "]" 
      | SNoexpr -> ""
      | SAfunc(t, bl, s) -> "lambda: " ^ string_of_typ t ^ " (" ^ String.concat ", " 
        (List.map string_of_bind bl) ^ ") {\n" ^ string_of_sstmt s ^ "}"
  ) ^ ")"

and string_of_sstmt = function
  | SBstmt(b) -> string_of_bind b ^ ";\n"
  | SBAstmt(Bind(t, n), e) -> 
    string_of_bind (Bind(t, n)) ^ " " ^ string_of_sexpr (t, SAssign(n, (t, e))) ^ "\n"
  | SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n"
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n"
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
                       string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SFor(s1, e2, e3, s) -> 
    "for (" ^ string_of_sstmt s1 ^ "; " ^ string_of_sexpr e2 ^ "; " ^ 
    string_of_sexpr e3 ^ ") " ^ string_of_sstmt s
  | SContinue -> "continue;\n"
  | SBreak -> "break;\n"
  (* | SFunc(f) -> string_of_func f *)
  | SFunc(b, bl, s) -> "function " ^ string_of_bind b ^ " (" ^ String.concat ", " 
    (List.map string_of_bind bl) ^ ")\n" ^ string_of_sstmt s ^ "\n"

(* and string_of_func func =
  string_of_typ func.srtyp ^ " " ^
  func.sfname ^ "(" ^ String.concat ", " (List.map string_of_bind func.sformals) ^
  ")\n"
  ^ (string_of_sstmt func.sbody) ^
  "\n" *)

let string_of_sprogram (sprogram) =
  "\n\nSementically checked program: \n\n" ^
  String.concat "" (List.map string_of_sstmt sprogram) ^ "\n"
