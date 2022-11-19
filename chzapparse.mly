/* Ocamlyacc parser for MicroC */

%{
open Ast
%}

%token SEMI COLON LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK ARROW
%token BWAND BWOR PLUS MINUS TIMES DIVIDE EXP MOD NOT ASSIGN
%token EQ NEQ LT LEQ GT GEQ AND OR
%token NOELSE IF ELSE FOR WHILE CONTINUE BREAK
%token INT UINT CHAR CONST FLOAT BOOL FUNC
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

%nonassoc NOELSE
%nonassoc ELSE

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left GT GEQ LT LEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right EXP
%right NOT
%nonassoc LPAREN RPAREN

%start program
%type <Ast.program> program

%%

/* programs */

program:
  decls EOF { $1 }

decls:
   /* nothing */ { ([], [])               }
 | vdecl SEMI decls { (($1 :: fst $3), snd $3) }
 | fdecl decls { (fst $2, ($1 :: snd $2)) }

fdecl:
  | vdecl LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
    {{
        ftyp = (fst $1, List.map fst $3); 
        rtyp = fst $1;
        fname = snd $1; 
        formals = $3; 
        locals = $6; 
        body = $7;
    }}
  // | vdecl LPAREN formals_opt RPAREN LBRACE decls RBRACE
  //   {{
  //       ftyp = (fst $1, List.map fst $3); 
  //       rtyp = fst $1;
  //       fname = snd $1; 
  //       formals = $3; 
  //       locals = fst $6; 
  //       body = List.map stmt_of_fdecl (snd $6);
  //   }}

/* formals_opt */
formals_opt:
  | /*nothing*/ { [] }
  // | VOID { [] }
  | formals_list { $1 }

formals_list:
  | vdecl { [$1] }
  | vdecl COMMA formals_list { $1::$3 }

vdecl_list:
  | /*nothing*/ { [] }
  // | vdecl_no_empty { $1 }
  | vdecl SEMI vdecl_list  { $1 :: $3 }

// vdecl_no_empty:
//   | vdecl SEMI vdecl_list  { $1 :: $3 }

/* int x */
vdecl:
  | typ ID { ($1, $2) }
//  | typ ID ASSIGN expr { ($1, $2) }

typ_no_arr:
  | INT   { Int  }
  | BOOL  { Bool }
  | CHAR  { Char }
  | FLOAT { Float }
  | VOID  { Void }
  | CONST const_typ { Const($2) }
  // | ftyp { Func($1) }

typ:
  | typ_no_arr { $1 }
  | typ LBRACK RBRACK   { Arr($1) }
  // | typ FUNC LPAREN typ_list RPAREN { ($1, $4) }
  // | FUNC LPAREN RPAREN ARROW typ_no_arr { Func($5, []) }
  // | FUNC LPAREN typ_no_arr RPAREN ARROW typ_no_arr { Func($6, [$3]) }
  | FUNC LPAREN typ_list RPAREN ARROW typ_no_arr { Ftyp($6, $3) }

typ_list:
  // | /* empty list */ { [] }
  // | { [] }
  | typ_no_arr { [$1] }
  | typ_no_arr COMMA typ_list { $1 :: $3 }

const_typ:
  | INT   { Int_const   }
  | BOOL  { Bool_const  }
  | CHAR  { Char_const }
  | FLOAT { Float_const }
  | VOID  { Void_const }

// typ:
//   | INT { Int }
//   | BOOL { Bool }
//   | FLOAT { Float }
//   | CHAR { Char }
//   // | FUNC { Func }
//   | VOID { Void }

stmt_list:
  | /* nothing */ { [] }
  | stmt stmt_list { $1 :: $2 }

// bind_opt:
//   | ID { Bind(Void, $1) }
//   | typ ID { Bind($1, $2) }

// formals_list:
//   | /* empty */ { [] }
//   | bind_opt { [$1] }
//   | bind_opt COMMA formals_list { $1 :: $3 }

// stmt_block:
//   | LBRACE stmt_list RBRACE { Block($2) }

stmt:
  | expr SEMI { Expr($1) }
  | LBRACE stmt_list RBRACE                 { Block($2) } 
  /* if (condition) { block1} else {block2} */
  /* if (condition) stmt else stmt */
  // | typ ID LPAREN formals_list RPAREN stmt_block { Func(Bind($1, $2), $4, $6) }
  // | FUNC ID LPAREN formals_list RPAREN stmt_block { Func(Bind(Void, $2), $4, $6) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt 
    { For($3, $5, $7, $9) }
  | FOR LPAREN expr RPAREN stmt 
    { For(Noexpr, $3, Noexpr, $5) }
  | BREAK SEMI      { Break }
  | CONTINUE SEMI   { Continue }
  /* return */
  | RETURN expr_opt SEMI                        { Return $2      }

expr_opt:
  | /* nothing */ { Noexpr }
  | expr { $1 }

expr:
//EQ+ NEQ+ LT+ LEQ GT GEQ BWAND BWOR NOT AND OR
  | INT_LITERAL       { IntLit($1) }
  | BOOL_LITERAL      { BoolLit($1) }
  | FLOAT_LITERAL     { FloatLit($1) }
  | CHAR_LITERAL      { CharLit($1) }
  | LBRACK args_opt RBRACK { ArrayLit($2) }
  | ID LBRACK expr RBRACK  { Subscription($1, $3) }
  | ID                { Id($1) }
  | expr PLUS expr    { Binop($1, Add, $3) }
  | expr MINUS expr   { Binop($1, Sub, $3) }
  | expr TIMES expr   { Binop($1, Mul, $3) }
  | expr EXP expr     { Binop($1, Exp, $3) }
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
  | LPAREN expr RPAREN          { $2 }
  | ID ASSIGN expr    { Assign($1, $3) }
   /* call */
  | ID LPAREN args_opt RPAREN { Call ($1, $3)  }
  /* anonymous function definition */
  // | FUNC LPAREN formals_opt RPAREN ARROW typ LBRACE vdecl_list stmt_list RBRACE
  //   { FuncDef({
  //       ftyp = ($6, List.map fst $3); 
  //       rtyp = $6; 
  //       fname = "_anonymous"; 
  //       formals = $3; 
  //       locals = $8; 
  //       body = $9;
  //   }) }

// possible fix of the args_opt issue?
/* args_opt*/
args_opt:
  | /*nothing*/ { [] }
  | args { $1 }

args:
  | expr  { [$1] }
  | expr COMMA args { $1::$3 }
