(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
    SIntLit of int
  | SBoolLit of bool
  | SCharLit of char
  | SFloatLit of float
  | SId of string
  | SArrLit of sexpr list
  | SUnop of uop * sexpr
  | SBinop of sexpr * op * sexpr
  | SAssign of string * sexpr
  (* call *)
  | SCall of string * sexpr list
  | SSubsription of string * sexpr
  | SNoexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SContinue
  | SBreak
  (* return *)
  | SReturn of sexpr

(* func_def: ret_typ fname formals locals body *)
type sfunc_def = {
  srtyp: typ;
  sfname: string;
  sformals: bind list;
  slocals: bind list;
  sbody: sstmt list;
}

type sprogram = bind list * sfunc_def list



(* Pretty-printing functions *)
let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
        SIntLit(l) -> string_of_int l
      | SBoolLit(true) -> "true"
      | SBoolLit(false) -> "false"
      | SCharLit(l) -> Char.escaped l
      | SFloatLit(l) -> string_of_float l
      | SArrLit(l) -> "[" ^ String.concat "," (List.map string_of_sexpr l) ^ "]"
      | SId(s) -> s
      | SUnop(o, e) -> "TODO SUnop"
      | SBinop(e1, o, e2) ->
        string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
      | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
      | SCall(f, el) ->
          f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
      | SSubsription(a, e) -> a ^ "[" ^ string_of_sexpr e ^ "]" 
      | SNoexpr -> ""
  ) ^ ")"
      


let rec string_of_sstmt = function
    SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n"
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n"
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
                       string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SFor(e1, e2, e3, s) -> "for (" ^ string_of_sexpr e1 ^ "; " ^ string_of_sexpr e2 ^ "; " ^ string_of_sexpr e3 ^ ") " ^ string_of_sstmt s
  | SContinue -> "continue;\n"
  | SBreak -> "break;\n"

let string_of_sfdecl fdecl =
  string_of_typ fdecl.srtyp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.slocals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (vars, funcs) =
  "\n\nSementically checked program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
