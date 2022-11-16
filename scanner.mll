(* Ocamllex scanner for MicroC *)

{ open Microcparse }
let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let id = alpha (alpha | digit | '_')*
let char = ''' ( ascii | digit ) '''
let float = (digit+) ['.'] digit+
let int = digit+
let whitespace = [' ' '\t' '\r']
let newline = '\n'

rule token = parse
whitespace { token lexbuf }
| newline { token lexbuf}
| "/*" { opencomment lexbuf }
| "//" { comment lexbuf }
| '(' { LPAREN }
| ')' { RPAREN }
| ';' { SEMI }
| ':' { COLON }
| ',' { COMMA }
| "[" { LBRACK }
| "]" { RBRACK }

(* Operators *)
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| "**" { EXP }
| '/' { DIVIDE }
| '%' { MODULO }
| '=' { ASSIGN }
| "==" { EQ }
| "!=" { NEQ }
| '<' { LT }
| "<=" { LEQ }
| ">" { GT }
| ">=" { GEQ }
| "&&" { AND }
| "||" { OR }
| "!" { NOT }
| "&" { BWAND }
| "|" { BWOR }

(* flow control *)
| "if" { IF }
| "else" { ELSE }
| "roll" { FOR }
| "while" { WHILE }
| "continue" { CONTINUE }
| "break" { BREAK }

(* types *)
| "int" { INT }
| "uint" { UINT }
| "char" { CHAR }
| "const" { CONST }
| "float" { FLOAT }
| "bool" { BOOL }

(* literals *)
| int as lit { INT_LITERAL(int_of_string lit) }
| float as lit { FLOAT_LITERAL(float_of_string lit) }
| char as lit { CHAR_LITERAL( String.get lit 1 ) }


(* misc *)
| "return" { RETURN }
| "void" { VOID } 
| "true" { TRUE }
| "false" { FALSE }
| eof    { EOF }


and opencomment = parse
newline { opencomment lexbuf }
| "*/" { token lexbuf }
| _ { opencomment lexbuf }

and comment = parse
newline {token lexbuf}
| _ {comment lexbuf}