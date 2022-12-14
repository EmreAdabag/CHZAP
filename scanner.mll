(* Ocamllex scanner for Chzap *)

{ open Chzapparse }

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let floatnumber = ('-'? digit+ '.' digit+)
let whitespace = [' ' '\r' '\t' '\n']


rule token = parse
whitespace { token lexbuf }
(* | newline { token lexbuf} *)
| "/*" { multi_comment 0 lexbuf }
| "//" { single_comment lexbuf }
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LBRACE }
| '}' { RBRACE }
| '[' { LBRACK }
| ']' { RBRACK }
| "[]" { LRBRACK }
| ';' { SEMI }
| ':' { COLON }
| ',' { COMMA }
| "->" { ARROW }

(* Operators *)
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| '%' { MOD }
| '=' { ASSIGN }
| "==" { EQ }
| "!=" { NEQ }
| '<' { LT }
| "<=" { LEQ }
| '>' { GT }
| ">=" { GEQ }
| "&&" { AND }
| "||" { OR }
| '!' { NOT }
| '&' { BWAND }
| '|' { BWOR }

(* flow control *)
| "if" { IF }
| "else" { ELSE }
| "roll" { FOR }
| "while" { WHILE }
| "continue" { CONTINUE }
| "break" { BREAK }
| "return" { RETURN }
| "assert" { ASSERT }
(* | "true" { TRUE }
| "false" { FALSE } *)


(* types *)
| "bool" { BOOL }
| "char" { CHAR }
| "int" { INT }
| "float" { FLOAT }
| "void" { VOID } 
| "const" { CONST }
| "function" { FUNC }
| "auto"    { AUTO }
| "const auto" { CONST_AUTO }


(* literals *)
| "true" { BOOL_LITERAL(true) }
| "false" { BOOL_LITERAL(false) }
| (alpha) (alpha | digit | '_')* as lit { ID(lit) }
| '-'? digit+ as lit { INT_LITERAL(int_of_string lit) }
| floatnumber as lit { FLOAT_LITERAL(float_of_string lit) }
| "'" ( _ as c ) "'" { CHAR_LITERAL(c) }
| "\"" ( [^'"']* as s ) "\"" { STRING_LITERAL(s) }

| eof    { EOF }
| _ as c { raise (Failure("illegal character " ^ Char.escaped c)) }


and multi_comment level = parse
| "*/" { if level = 0 then token lexbuf else multi_comment (level - 1) lexbuf}
| "/*" {multi_comment (level + 1) lexbuf}
| eof { raise (Failure("Unclosed comment. ")) }
| _ { multi_comment level lexbuf }

and single_comment = parse
| '\n' { token lexbuf }
| eof { EOF }
| _ { single_comment lexbuf }