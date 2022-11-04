%{
#include "string.h"
#include "def.h"

extern int yycolumn;

#define YYSTYPE yylval
extern "C"
{	
	int yywrap(void);
	int yylex(void);
}

void flexout(std::string name,std::string value){
  std::cout<<name<<"\t:"<<value<<std::endl;
}
//flex为bison提供行号信息
%}

oct (0([1-7][0-7]*))|0
dec ([1-9][0-9]*)|0
hex (0x[1-9a-fA-F][0-9a-fA-F]*)|0
floatvar ([0-9]*\.[0-9]+|[0-9]+\.)((E|e)[\+\-]?[0-9]+)?
%option yylineno 

%%

"int"        {flexout("TYPE","int");}
"float"      {flexout("TYPE","float");}
"char"      {flexout("TYPE","char");}
{oct}|{dec}|{hex} {flexout("INT",yytext);}
{floatvar} {flexout("FLOAT",yytext);}
[;] {flexout("SEMI",yytext);}
[,] {flexout("COMMA",yytext);}
"=" {flexout("ASSIGNOP",yytext);}
"==" {flexout("RELOP",yytext);}
[\+] {flexout("PLUS",yytext);}
[\-] {flexout("MINUS",yytext);}
[\*] {flexout("STAR",yytext);}
[\/] {flexout("DIV",yytext);}
"&&" {flexout("AND",yytext);}
"||" {flexout("OR",yytext);}
[.] {flexout("DOT",yytext);}
[!] {flexout("NOT",yytext);}

(\+\+) {flexout("PLUSPLUS",yytext);}
(\-\-) {flexout("MINUSMINUS",yytext);}
(\+\=) {flexout("PLUSASS",yytext);}
(\-\=) {flexout("MINUSASS",yytext);}

[\<] {flexout("LT",yytext);}
[\>] {flexout("GT",yytext);}
[\(] {flexout("LP",yytext);}
[\)] {flexout("RP",yytext);}
[\{] {flexout("LC",yytext);}
[\}] {flexout("RC",yytext);}
[\[] {flexout("LB",yytext);}
[\]] {flexout("RB",yytext);}
"struct" {flexout("STRUCT",yytext);}
"return" {flexout("RETURN",yytext);}
"if" {flexout("IF",yytext);}
"else" {flexout("ELSE",yytext);}
"while" {flexout("WHILE",yytext);}
[A-Za-z][A-Za-z0-9]* {flexout("ID",yytext);}


[\n]		{yycolumn=1;}
[ \r\t]		{/*printf("过滤空格等字符\n");*/}


[\-]?(0([1-9][0-9]*)) {
	printf("Error type A at Line %d: Illegal octal number \'%s\'\n", yylineno, yytext);
}
[\-]?(0x[1-9a-zA-z][0-9a-zA-Z]*) {
	printf("Error type A at Line %d: Illegal hexadecimal number \'%s\'\n", yylineno, yytext);
}


. {
	printf("Error type A at Line %d: Mysterious characters \'%s\'\n", yylineno, yytext);
}
%%

int yywrap()
{
    return 1;
}