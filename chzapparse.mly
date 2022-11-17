/* Ocamlyacc parser for MicroC */

%{
open Ast
%}

%token SEMI COLON LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token PLUS MINUS TIMES DIVIDE EXP MODULO ASSIGN
%token EQ NEQ LT LEQ GT GEQ BWAND BWOR NOT AND OR
%token IF ELSE FOR WHILE CONTINUE BREAK
%token INT UINT CHAR CONST FLOAT BOOL
/* return, COMMA token */
%token RETURN VOID TRUE FALSE
%token COMMA LB INDENT DEINDENT
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <char> CHAR_LITERAL
%token <bool> BOOL_LITERAL
%token <string> ID
%token EOF

%start program
%type <Ast.program> program

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left GT GEQ LT LEQ
%left PLUS MINUS
%left TIMES DIVIDE

%%

program:
  | stmt_list EOF { $1 }

stmt_list:
  | /* nothing */ { [] }
  | stmt stmt_list { $1 :: $2 }

stmt:
  | LB /* empty */ { Void }
  | expr { Expr($1) }

expr:
  | INT_LITERAL { IntLit($1) }
  | BOOL_LITERAL { BoolLit($1) }
  | FLOAT_LITERAL { FloatLit($1) }
  | ID { Id($1) }
  | expr PLUS expr { Binop($1, Add, $3) }
  | expr MINUS expr { Binop($1, Sub, $3) }
  | expr TIMES expr { Binop($1, Mul, $3) }
  | expr DIVIDE expr { Binop($1, Div, $3) }
  | ID ASSIGN expr { Assign($1, $3) }

// /* add function declarations*/
// program:
//   decls EOF { $1}

// decls:
//    /* nothing */ { ([], [])               }
//  | vdecl SEMI decls { (($1 :: fst $3), snd $3) }
//  | fdecl decls { (fst $2, ($1 :: snd $2)) }

// vdecl_list:
//   /*nothing*/ { [] }
//   | vdecl SEMI vdecl_list  {  $1 :: $3 }

// /* int x */
// vdecl:
//   typ ID { ($1, $2) }

// typ:
//     INT   { Int   }
//   | BOOL  { Bool  }

// /* fdecl */
// fdecl:
//   vdecl LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
//   {
//     {
//       rtyp=fst $1;
//       fname=snd $1;
//       formals=$3;
//       locals=$6;
//       body=$7
//     }
//   }

// /* formals_opt */
// formals_opt:
//   /*nothing*/ { [] }
//   | formals_list { $1 }

// formals_list:
//   vdecl { [$1] }
//   | vdecl COMMA formals_list { $1::$3 }

// stmt_list:
//   /* nothing */ { [] }
//   | stmt stmt_list  { $1::$2 }

// stmt:
//     expr SEMI                               { Expr $1      }
//   | LBRACE stmt_list RBRACE                 { Block $2 }
//   /* if (condition) { block1} else {block2} */
//   /* if (condition) stmt else stmt */
//   | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
//   | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
//   /* return */
//   | RETURN expr SEMI                        { Return $2      }

// expr:
//     INT_LITERAL      { Literal($1)            }
//   | BOOL_LITERAL     { BoolLit($1)            }
//   | ID               { Id($1)                 }
//   | expr PLUS   expr { Binop($1, Add,   $3)   }
//   | expr MINUS  expr { Binop($1, Sub,   $3)   }
//   | expr EQ     expr { Binop($1, Equal, $3)   }
//   | expr NEQ    expr { Binop($1, Neq, $3)     }
//   | expr LT     expr { Binop($1, Less,  $3)   }
//   | expr AND    expr { Binop($1, And,   $3)   }
//   | expr OR     expr { Binop($1, Or,    $3)   }
//   | ID ASSIGN expr   { Assign($1, $3)         }
//   | LPAREN expr RPAREN { $2                   }
//   /* call */
//   | ID LPAREN args_opt RPAREN { Call ($1, $3)  }

// /* args_opt*/
// args_opt:
//   /*nothing*/ { [] }
//   | args { $1 }

// args:
//   expr  { [$1] }
//   | expr COMMA args { $1::$3 }
