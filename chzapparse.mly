/* Ocamlyacc parser for MicroC */

%{
open Ast
%}

%token SEMI COLON LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token BWAND BWOR PLUS MINUS TIMES DIVIDE EXP MOD NOT
%token ASSIGN
%token EQ NEQ LT LEQ GT GEQ AND OR
%token NOELSE IF ELSE FOR WHILE CONTINUE BREAK
%token INT UINT CHAR CONST FLOAT BOOL
/* return, COMMA token */
%token RETURN VOID TRUE FALSE
%token COMMA LB TAB INDENT DEDENT
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
%left TIMES DIVIDE MOD
%right EXP
%right NOT

%start program
%type <Ast.program> program

%%

/* tokens */
// return a list of tokens
// ref: https://github.com/jacobaustin123/Coral.git

// tokens:
//   | LB { [LB] }
//   | token_list LB { $1 @ [LB] }

// token_list:
//   | token { [$1] }
//   | token token_list { $1 :: $2 }

// token:
//   | COLON { COLON }
//   | INDENT { INDENT }
//   | RETURN { RETURN }
//   | NOT { NOT }
//   | IF { IF }
//   | ELSE { ELSE }
//   | FOR { FOR }
//   | WHILE { WHILE }
//   | COMMA { COMMA }
//   | NEQ { NEQ }
//   | LT { LT }
//   | GT { GT }
//   | LEQ { LEQ }
//   | GEQ { GEQ }
//   | AND { AND }
//   | CONTINUE { CONTINUE }
//   | BREAK { BREAK }
//   | OR { OR }
//   | TRUE { TRUE }
//   | FALSE { FALSE }
//   | PLUS { PLUS }
//   | MINUS { MINUS }
//   | TIMES { TIMES }
//   | DIVIDE { DIVIDE }
//   | EXP { EXP }
//   | LPAREN { LPAREN }
//   | RPAREN { RPAREN }
//   | LBRACK { LBRACK }
//   | RBRACK { RBRACK }
//   | LBRACE { LBRACE }
//   | RBRACE { RBRACE }
//   | EQ { EQ }
//   | BOOL { BOOL }
//   | INT { INT }
//   | FLOAT { FLOAT }
//   | INDENT { INDENT }
//   | DEDENT { DEDENT }
//   | ID { ID($1) }
//   | FLOAT_LITERAL { FLOAT_LITERAL($1) }
//   | INT_LITERAL { INT_LITERAL($1) }
//   | BOOL_LITERAL { BOOL_LITERAL($1) }
//   | CHAR_LITERAL { CHAR_LITERAL($1) }
//   | EOF { EOF }

/* programs */

program:
  decls EOF { $1}

decls:
   /* nothing */ { ([], [])               }
 | vdecl SEMI decls { (($1 :: fst $3), snd $3) }
 | fdecl decls { (fst $2, ($1 :: snd $2)) }

fdecl:
  vdecl LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
  {
    {
      rtyp=fst $1;
      fname=snd $1;
      formals=$3;
      locals=$6;
      body=$7
    }
  }

/* formals_opt */
formals_opt:
  /*nothing*/ { [] }
  | formals_list { $1 }

formals_list:
  vdecl { [$1] }
  | vdecl COMMA formals_list { $1::$3 }

vdecl_list:
  /*nothing*/ { [] }
  | vdecl SEMI vdecl_list  {  $1 :: $3 }

/* int x */
vdecl:
  typ ID { ($1, $2) }

typ:
    INT   { Int  }
  | BOOL  { Bool }
  | CHAR  { Char }
  | FLOAT { Float }
  | VOID  { Void }
  | CHAR  { Char }
  | typ LBRACK RBRACK   { Arr($1) }
  | CONST const_typ { Const($2) }

const_typ:
    INT   { Int_const   }
  | BOOL  { Bool_const  }
  | CHAR  { Char_const }
  | FLOAT { Float_const }
  | VOID  { Void_const }

stmt_list:
  | /* nothing */ { [] }
  | stmt stmt_list { $1 :: $2 }

stmt:
  | expr SEMI { Expr($1) }
  | LBRACE stmt_list RBRACE                 { Block $2 } 
  /* if (condition) { block1} else {block2} */
  /* if (condition) stmt else stmt */
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt 
    { For($3, $5, $7, $9) }
  | FOR LPAREN expr RPAREN stmt 
    { For_1($3, $5) }
  | BREAK SEMI      { Break }
  | CONTINUE SEMI   { Continue }
  /* return */
  | RETURN expr_opt SEMI                        { Return $2      }

expr_opt:
  /* nothing */ { Noexpr }
| expr { $1 }

expr:
//EQ+ NEQ+ LT+ LEQ GT GEQ BWAND BWOR NOT AND OR
  | INT_LITERAL       { IntLit($1) }
  | BOOL_LITERAL      { BoolLit($1) }
  | FLOAT_LITERAL     { FloatLit($1) }
  | CHAR_LITERAL      { CharLit($1) }
  | LBRACK args_opt RBRACK { ArrayLit($2) }
  | ID LBRACK expr RBRACK  { Subsription($1, $3) }
  | ID                { Id($1) }
  | expr PLUS expr    { Binop($1, Add, $3) }
  | expr MINUS expr   { Binop($1, Sub, $3) }
  | expr TIMES expr   { Binop($1, Mul, $3) }
  | expr DIVIDE expr  { Binop($1, Div, $3) }
  | expr MOD expr     { Binop($1, Mod, $3) }
  | expr EQ expr      { Binop($1, Eq, $3) }
  | expr NEQ expr     { Binop($1, Neq, $3) }
  | expr LT expr      { Binop($1, Less, $3) }
  | expr LEQ expr     { Binop($1, Leq, $3) }
  | expr GT expr      { Binop($1, Greater, $3) }
  | expr GEQ expr     { Binop($1, Geq, $3) }
  | expr AND expr     { Binop($1, And, $3) }
  | expr OR expr      { Binop($1, Or, $3) }
  | NOT expr          { Unop(Not, $2) }
  | ID ASSIGN expr    { Assign($1, $3) }
   /* call */
  | ID LPAREN args_opt RPAREN { Call ($1, $3)  }
  | LPAREN expr RPAREN          { $2 }

// possible fix of the args_opt issue?
/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }
