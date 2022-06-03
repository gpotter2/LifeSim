%{
  open Ast;;

  let rec body params expr = match params with
  | [] -> expr
  | p :: prms -> Fun(p, body prms expr)
  ;;
%}

%token <int> INT
%token <float> FLOAT
%token <string> IDENT
%token TRUE FALSE
%token <string> STRING
%token PLUS MINUS MULT DIV EQUAL GREATER SMALLER POWER GREATEREQUAL SMALLEREQUAL
%token LPAR RPAR LBRACK RBRACK LBRACE RBRACE SEMI COLON COMA POINT
%token LET REC LETREC IN FUN ARROW
%token IF THEN ELSE
%token STRUCT
%token DOUBLARRO DO
%left EQUAL GREATER SMALLER GREATEREQUAL SMALLEREQUAL
%left PLUS MINUS
%left MULT DIV

%start main
%type <Ast.expr> main

%%

main: main_expr { $1 };

main_expr:
  expr
    { $1 }
| IDENT IDENT DOUBLARRO IDENT IDENT DO LBRACE expr RBRACE
    { Comportement($1, $2, $4, $5, Bool(true), $8) }
| IDENT IDENT DOUBLARRO IDENT IDENT IF expr DO LBRACE expr RBRACE
    { Comportement($1, $2, $4, $5, $7, $10) }
| STRUCT IDENT LBRACE entite_expr RBRACE
    { Entite($2, $4) }
;

expr:
  LETREC IDENT IDENT seqident EQUAL expr IN main_expr    { Letrec($2, $3, (body $4 $6), $8) }
| LET REC IDENT IDENT seqident EQUAL expr IN main_expr   { Letrec($3, $4, (body $5 $7), $9) }
| LET IDENT seqident EQUAL expr IN main_expr  { Let($2, (body $3 $5) , $7) }
| FUN IDENT ARROW main_expr                   { Fun($2, $4) }
| IF expr THEN expr ELSE expr                 { If($2, $4, $6) }
| arith_expr                                  { $1 }
;

entite_expr:
  entite_expr SEMI entite_expr                  { $1 @ $3 }
  | entite_expr SEMI                            { $1 }
  | IDENT COLON expr                            { [Attribut($1, $3)] }
;

arith_expr:
  application                        { $1 }
| arith_expr EQUAL arith_expr        { Binop("=", $1, $3) }
| arith_expr GREATER arith_expr      { Binop(">", $1, $3) }
| arith_expr GREATEREQUAL arith_expr { Binop(">=", $1, $3) }
| arith_expr SMALLER arith_expr      { Binop("<", $1, $3) }
| arith_expr SMALLEREQUAL arith_expr { Binop("<=", $1, $3) }
| arith_expr PLUS arith_expr         { Binop("+", $1, $3) }
| arith_expr MINUS arith_expr        { Binop("-", $1, $3) }
| arith_expr MULT arith_expr         { Binop("*", $1, $3) }
| arith_expr DIV arith_expr          { Binop("/", $1, $3) }
| arith_expr POWER arith_expr        { Binop("^", $1, $3) }
;

/* Attention : on considère ci-dessous que MINUS atom est dans la categorie
 * des applications. Cela permet de traiter
 *                        n - 1
 *  comme une soustraction binaire, et 
 *                        f (- 1)
 * comme l'application de f a l'oppose de 1.
 */

application:
  atom                { $1 }
| MINUS atom          { Monop("-", $2) }
| application atom    { App($1, $2) }
;

atom:
  INT            { Int($1) }
| FLOAT          { Float($1) }
| TRUE           { Bool(true) }
| FALSE          { Bool(false) }
| STRING         { String($1) }
| IDENT LBRACE RBRACE
    { EntiteVal($1, []) }
| IDENT LBRACE entite_expr RBRACE
    { EntiteVal($1, $3) }
| IDENT POINT IDENT
    { EntiteAccess($1, $3) }
| IDENT          { Ident($1) }
| LBRACK lst_expr RBRACK { Liste($2) }
| LPAR expr RPAR { $2 }
;

lst_expr:
  lst_expr atom    { $1 @ [$2] }
  | atom COMA           { [$1] }
;

seqident:
  IDENT seqident  { $1 :: $2 }
| /* rien */      { [ ] }
;