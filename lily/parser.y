//Andrew Guttman
//asguttma
//Xiaoli Tang
//xtang2

%{
//parser for scanner project.

#include "lyutils.h"
#include "astree.h"

%}

%debug
%defines
%error-verbose
%token-table
%verbose

%token TOK_VOID TOK_BOOL TOK_CHAR TOK_INT TOK_STRING
%token TOK_IF TOK_ELSE TOK_WHILE TOK_RETURN TOK_STRUCT
%token TOK_FALSE TOK_TRUE TOK_NULL TOK_NEW TOK_ARRAY
%token TOK_EQ TOK_NE TOK_LT TOK_LE TOK_GT TOK_GE
%token TOK_IDENT TOK_INTCON TOK_CHARCON TOK_STRINGCON

%token TOK_BLOCK TOK_CALL TOK_IFELSE TOK_INITDECL
%token TOK_POS TOK_NEG TOK_NEWARRAY TOK_TYPEID TOK_FIELD
%token TOK_ORD TOK_CHR TOK_ROOT

%right     TOK_IF TOK_ELSE
%right     '='
%left      TOK_EQ TOK_NE TOK_LT TOK_LE TOK_GT TOK_GE
%left      '+' '-'
%left      '*' '/' '%'
%right     

%start  start


%%

start   : program               { yyparse_astree = $1; }
        ;

program : program structdef     { $$ = adopt1 ($1, $2); }
        | program function      { $$ = adopt1 ($1, $2); }
        | program statement     { $$ = adopt1 ($1, $2);}
        | program error '}'     { $$ = $1; }
        | program error ';'     { $$ = $1; }
        |                       { $$ = new_parseroot(); }
        ;

structdef : TOK_STRUCT TOK_IDENT '{' fielddecls '}' {$$ = TOK_TYPEID
          ; TOK_STRUCT TOK_IDENT '{' '}'            {$$ =
          
fielddecls : fielddecls ';' fielddecl    {$$ = 
           ; 

fielddecl : basetype '[]' TOK_IDENT     {$$ = TOK_FIELD
          | basetype  TOK_IDENT         {$$ =
          ;


basetype : TOK_VOID             {
           TOK_BOOL             {
           TOK_CHAR             {
           TOK_INT              {
           TOK_STRING           {
           TOK_IDENT            { $$ TOK_TYPEID
           

function : function identdecl param ')' block  { $$ = adopt2(adopt1sym($1, $2, TOK_FUNCTION), $3, $5); }
         | function identdecl param ')' ';'    { $$ = adopt2(changesym($1, TOK_PROTOTYPE), $2, $3); }
         ;
         

param      : '(' param ',' identdecl   { $$ = adopt2(changesym($1, TOK_PARAMLIST), $2, $4); }
           | '(' identdecl             { $$ = adopt1sym($1, $2, TOK_PARAMLIST); }
           | '('                       { $$ = changesym($1, TOK_PARAMLIST); }
           ;


identdecl : basetype '[]' TOK_IDENT    {
          | basetype TOK_IDENT {
          ;


statement : block               { $$ = $1; }
          | vardecl             { $$ = $1; }
          | while               { $$ = $1; }
          | ifelse              { $$ = $1; }
          | return              { $$ = $1; }
          | expr ';'            { free_ast($2); $$ = $1; }
          ;
 
block     : '{' statements '}'     { free_ast($3); $$ = adopt1sym ($1, $2, TOK_BLOCK); }
          | '{' '}'                { free_ast($2); $$ = $1;}
          |  ';'                   { $$ = $1; }
          ;
 
statements : statements statement  { $$ = adopt1($1, $2); }
 		   ;
 			      
vardecl : identdecl '=' expr ';'    { free_ast($3); adopt2(changesym($2,TOK_VALDECL), $1, $3); }
        ;

while   : TOK_WHILE '(' expr ')' statement {$$ = adopt2($1, $3, $5); }
        ;         


ifelse    : TOK_IF '(' expr ')' statement  { free_ast2 ($2, $4); $$ = adopt2 ($1, $3, $5); }
          | TOK_IF '(' expr ')' statement TOK_ELSE statement { free_ast2 ($2, $4); free_ast($6);
           $$ = adopt2 (adopt1sym ($1, $3, TOK_IFELSE), $5, $7);
            }
          ;


return      : TOK_RETURN ';'        {$$ = adopt1sym($1,$2,TOK_RETURNVOID); }
            | TOK_RETURN expr ';'   { free_ast($3); $$ = adopt1($1, $2) ;}
            ;

expr        : binop                   { $$ = $1 ; }
            | unop                    { $$ = $1 ; }
            | allocator               { $$ = $1 ; }
            | call                    { $$ = $1 ; }
            | '(' expr ')'            { free_ast2($1,$3); $$ = $2;}
            | variable                { $$ = $1 ; }
            | constant                { $$ = $1 ; }
            ;
            
binop	    :  expr '=' expr          { $$ = adopt2 ($2, $1, $3); }
            |  expr TOK_EQ expr       { $$ = adopt2 ($2, $1, $3); }
            |  expr TOK_NE expr       { $$ = adopt2 ($2, $1, $3); }
            |  expr TOK_LT expr       { $$ = adopt2 ($2, $1, $3); }
            |  expr TOK_GT expr       { $$ = adopt2 ($2, $1, $3); }
            |  expr TOK_LE expr       { $$ = adopt2 ($2, $1, $3); }
            |  expr TOK_GE expr       { $$ = adopt2 ($2, $1, $3); }
            |  expr '+' expr          { $$ = adopt2 ($2, $1, $3); }
            |  expr '-' expr          { $$ = adopt2 ($2, $1, $3); }
            |  expr '*' expr          { $$ = adopt2 ($2, $1, $3); } 
            |  expr '/' expr          { $$ = adopt2 ($2, $1, $3); }
            |  expr '%' expr          { $$ = adopt2 ($2, $1, $3); }
            ;

unop	    :  '+' expr %prec TOK_POS { $$ = adopt1sym ($1, $2, TOK_POS); }
            |  '-' expr %prec TOK_NEG { $$ = adopt1sym ($1, $2, TOK_NEG); }
            |  '!' expr               { $$ = adopt1 ($1, $2); }
            |  TOK_ORD expr           { $$ = adopt1 ($1, $2); }
            |  TOK_CHR expr           { $$ = adopt1 ($1, $2); }
            ;           

%%


const char *get_yytname (int symbol) {
   return yytname [YYTRANSLATE (symbol)];
}


bool is_defined_token (int symbol) {
   return YYTRANSLATE (symbol) > YYUNDEFTOK;
}

//static void* yycalloc (size_t size) {
//   void* result = calloc (1, size);
//   assert (result != NULL);
//   return result;
//}

