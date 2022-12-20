/* Ocamlyacc parser for MicroC */

%{
open Ast
%}

%token SEMI COLON LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK LRBRACK ARROW
%token BWAND BWOR PLUS MINUS TIMES DIVIDE MOD NOT ASSIGN
%token EQ NEQ LT LEQ GT GEQ AND OR ASSERT
%token NOELSE IF ELSE FOR WHILE CONTINUE BREAK
%token INT CHAR CONST FLOAT BOOL FUNC AUTO CONST_AUTO
%token RETURN VOID TRUE FALSE
%token COMMA LB TAB INDENT DEDENT
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <char> CHAR_LITERAL
%token <string> STRING_LITERAL
%token <bool> BOOL_LITERAL
%token <string> ID
%token EOF


%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE

%right ASSIGN
%left OR
%left AND
%left BWOR
%left BWAND
%left EQ NEQ
%left GT GEQ LT LEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT

%nonassoc LPAREN LBRACK LBRACE
%nonassoc RPAREN RBRACK RBRACE
%left LRBRACK

%%


/* programs */

program:
  | stmt_list EOF { $1 }

basic_typ:
  | INT   { Int   }
  | BOOL  { Bool  }
  | CHAR  { Char }
  | FLOAT { Float }

typ:
  | basic_typ       { $1 }
  | CONST basic_typ { Const($2) }
  /* arrays are only allowed on basic types */
  | basic_typ LBRACK INT_LITERAL RBRACK { Arr($1, $3) }
  | basic_typ LRBRACK { Darr($1) }
  | FUNC LPAREN typ_list RPAREN ARROW typ { Ftyp($6, $3) }

/* type list is forced to be non-empty (use "void" as placeholder) */
typ_list:
  | VOID               { [] }
  | typ                { [$1] }
  | typ COMMA typ_list { $1 :: $3 }

auto_typ:
  | AUTO  { Auto }
  | CONST_AUTO { Const_auto }

stmt_list:
  | /* nothing */ { [] }
  | stmt stmt_list { $1 :: $2 }

bind:
  | typ ID { Bind($1, $2) }

formals_list:
  | /* empty */ { [] }
  | bind { [$1] }
  | bind COMMA formals_list { $1 :: $3 }

stmt:
  // empty stmt
  | SEMI { Expr(Noexpr) }
  | expr SEMI { Expr($1) }
  | LBRACE stmt_list RBRACE { Block($2) }
  // decls
  | bind SEMI { Bstmt($1) }
  | bind ASSIGN expr SEMI { BAstmt($1, $3) }
  | auto_typ ID ASSIGN expr SEMI { BAIstmt($1, $2, $4) }
  // control flows
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
  | FOR LPAREN stmt expr SEMI expr_opt RPAREN stmt { For($3, $4, $6, $8) }
  | FOR LPAREN expr RPAREN stmt { For(Expr(Noexpr), $3, Noexpr, $5) }
  | BREAK SEMI      { Break }
  | CONTINUE SEMI   { Continue }
  | ASSERT expr SEMI { Assert($2) }
  | RETURN expr_opt SEMI { Return($2) }
  // func def
  | typ ID LPAREN formals_list RPAREN stmt { Func(Bind($1, $2), $4, $6) }
  | ID LPAREN formals_list RPAREN stmt { Func(Bind(Void, $1), $3, $5) }


expr_opt:
  | /* nothing */ { Noexpr }
  | expr { $1 }

expr:
  | INT_LITERAL       { IntLit($1) }
  | BOOL_LITERAL      { BoolLit($1) }
  | FLOAT_LITERAL     { FloatLit($1) }
  | CHAR_LITERAL      { CharLit($1) }
  | STRING_LITERAL    { StringLit($1) }
  | LBRACK args_opt RBRACK { ArrayLit($2) }
  | ID LBRACK expr RBRACK  { Subscription($1, $3) }
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
  | expr BWAND expr   { Binop($1, BWAnd, $3)}
  | expr BWOR expr   { Binop($1, BWOr, $3)}
  | NOT expr          { Unop(Not, $2) }
  | LPAREN expr RPAREN          { $2 }
  | ID ASSIGN expr    { Assign($1, $3) }
   /* call */
  | ID LPAREN args_opt RPAREN { Call ($1, $3) }
  /* anonymous */
  | FUNC LPAREN formals_list RPAREN ARROW typ stmt { Afunc($6, $3, $7) }
  | FUNC LPAREN formals_list RPAREN stmt { Afunc(Void, $3, $5) }

args_opt:
  | /*nothing*/ { [] }
  | args { $1 }

args:
  | expr  { [$1] }
  | expr COMMA args { $1::$3 }
