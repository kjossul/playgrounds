%{
#define YYSTYPE double
#include <math.h>
%}
%token NUM
%token OP_PLUS
%token OP_MINUS
%token OP_MUL
%token OP_DIV
%token OP_EXP
%token UN_MINUS
%token NEWLINE
%token NEG


/* operator precedence */
%left OP_PLUS OP_MINUS
%left OP_MUL OP_DIV
%left NEG
%right OP_EXP

%%
input: /* empty */
| input line
;

line: NEWLINE
| exp NEWLINE { printf ("\t%.10g\n", $1); }
;

exp: NUM { $$ = $1; }
| exp OP_PLUS exp { $$ = $1 + $3; }
| exp OP_MINUS exp { $$ = $1 - $3; }
| exp OP_MUL exp { $$ = $1 * $3; }
| exp OP_DIV exp { $$ = $1 / $3; }
| OP_MINUS exp %prec NEG { $$ = -$2; }
| exp OP_EXP exp { $$ = pow($1,$3); }
| '(' exp ')' { $$ = $2; }
;
%%

int main(){
  yyparse();
}

int yyerror(char * s){
  printf("%s\n",s);
}
