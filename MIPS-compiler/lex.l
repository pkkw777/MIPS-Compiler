%{
#include <stdlib.h>
#include <string.h>
#include "def.tab.hh"
#define INFILE_ERROR 1
#define OUTFILE_ERROR 2
extern int yylineno;
int yyerror(const char *msg,...);
%}
%%
\+                      							{ return '+';}
\*                     								{ return '*';}
\(                     								{ return '(';}
\)                     								{ return ')';}
\/                    								{ return '/';}
\-                    								{ return '-';}
\^                    								{ return '^';}
\[                      							{ return '[';}
\]                     								{ return ']';}
\{                     								{ return '{';}
\}                     								{ return '}';}
\;                     								{ return ';';}
\=                    							 	{ return '=';}
\=\=                    							{ return EQ;}
\!\=                   								{ return NE;}
\<                     								{ return LT;}
\>                      							{ return GT;}
\>\=                    							{ return GE;}
\<\=                   								{ return LE;}
\&\&                    							{ return AND;}
\|\|                    							{ return OR;}
int                     							{ return INT;}
float                 								{ return FLOAT;}
if                   							    	{ return IF;}
else                  								{ return ELSE;}
while										{ return WHILE;}
printi              								{ return PRINTI;}
printf             								{ return PRINTF;}
prints										{ return PRINTS;}
readi										{ return READI;}
readf										{ return READF;}							
0|[1-9][0-9]*      								{ yylval.ival	= atoi(yytext); 		return LC;}
[0-9]*[.][0-9]*									{ yylval.fval	= atof(yytext);  		return LZ;}
[A-Za-z_][A-Za-z0-9_]*  							{ yylval.text = strdup(yytext);		return ID;}
\"[^\"]*\" 									{ yylval.text = strdup(yytext); 
																										return STRING;}
\ |\t          									{;}
\n         									{yylineno++ ;}
.           									{yyerror("Blad leksykalny\n");}
%%
int yyerror(const char *msg,...)
{
    printf("%d: %s", yylineno, msg);
    exit(1);
}
