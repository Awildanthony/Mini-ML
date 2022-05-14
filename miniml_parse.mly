(*
                         CS 51 Final Project
                           MiniML -- Parser
*)
                  
%{
  open Expr ;;
%}

%token EOF
%token OPEN CLOSE
%token LET DOT IN REC
%token NEGATE_I 
%token NEGATE_F
%token NOT
%token PLUS_I MINUS_I
%token PLUS_F MINUS_F
%token TIMES_I DIVIDE_I
%token TIMES_F DIVIDE_F
%token POWER_I
%token POWER_F
%token UNEQUAL GREATEROREQUAL LESSOREQUAL GREATERTHAN LESSTHAN EQUALS
%token IF THEN ELSE 
%token FUNCTION
%token RAISE
%token <string> ID
%token <int> INT 
%token <float> FLOAT
%token TRUE FALSE

%nonassoc IF
%left UNEQUAL GREATEROREQUAL LESSOREQUAL GREATERTHAN LESSTHAN EQUALS
%left PLUS_I MINUS_I
%left PLUS_F MINUS_F
%left TIMES_I DIVIDE_I
%left TIMES_F DIVIDE_F
%left POWER_I
%left POWER_F
%nonassoc NEGATE_I 
%nonassoc NEGATE_F
%nonassoc NOT

%start input
%type <Expr.expr> input

(* Grammar follows *)
%%
input:  exp EOF                 { $1 }

exp:    exp expnoapp            { App($1, $2) }
        | expnoapp              { $1 }

expnoapp: 
        | INT                   { Num $1 }
        | FLOAT                 { Float $1 }
        | TRUE                  { Bool true }
        | FALSE                 { Bool false }
        | ID                    { Var $1 }
        | exp PLUS_I exp          { Binop(Plus_i, $1, $3) }
        | exp PLUS_F exp          { Binop(Plus_f, $1, $3) }
        | exp MINUS_I exp         { Binop(Minus_i, $1, $3) }
        | exp MINUS_F exp         { Binop(Minus_f, $1, $3) }
        | exp TIMES_I exp         { Binop(Times_i, $1, $3) }
        | exp TIMES_F exp         { Binop(Times_f, $1, $3) }
        | exp DIVIDE_I exp        { Binop(Divide_i, $1, $3) }
        | exp DIVIDE_F exp        { Binop(Divide_f, $1, $3) }
        | exp POWER_I exp         { Binop(Power_i, $1, $3) }
        | exp POWER_F exp         { Binop(Power_f, $1, $3) }
        | exp EQUALS exp          { Binop(Equals, $1, $3) }
        | exp LESSTHAN exp        { Binop(LessThan, $1, $3) }
        | exp GREATERTHAN exp     { Binop(GreaterThan, $1, $3) }
        | exp LESSOREQUAL exp     { Binop(LessOrEqual, $1, $3) }
        | exp GREATEROREQUAL exp  { Binop(GreaterOrEqual, $1, $3) }
        | exp UNEQUAL exp         { Binop(Unequal, $1, $3) }
        | NEGATE_I exp               { Unop(Negate_i, $2) }
        | NEGATE_F exp               { Unop(Negate_f, $2) }
        | NOT exp                    { Unop(Not, $2) }
        | IF exp THEN exp ELSE exp      { Conditional($2, $4, $6) }
        | LET ID EQUALS exp IN exp      { Let($2, $4, $6) }
        | LET REC ID EQUALS exp IN exp  { Letrec($3, $5, $7) }
        | FUNCTION ID DOT exp   { Fun($2, $4) } 
        | RAISE                 { Raise }
        | OPEN exp CLOSE        { $2 }
;

%%
