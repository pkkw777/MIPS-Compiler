%{
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <stack>
#include <string>
#include <map>
extern "C" int yylex();
extern "C" int yyerror(const char *msg, ...);
using namespace std;
ofstream rpn("rpn.txt");
ofstream threes("trojki.txt");
ofstream code_out("code.asm");

const int NONE_TYPE = 0;
const int INT_TYPE = 1;
const int FLOAT_TYPE = 2;
const int INT_ARRAY = 4;
const int FLOAT_ARRAY = 5;
const int STRING_TYPE = 6;
static int label_counter = 0;
static int while_counter = 0;
static int counter = 0;
static int float_counter = 0;

class Element
{
	public:
				Element(int type, string value):type(type), value(value) {;}
				int type;
				string value;
};

class Symbol
{
	public:
				Symbol(int type, int size, string value):type(type), size(size), value(value) {;}
				int type;
				int size;
				string value;
	~Symbol();
};

stack <Element> argstack;
stack<string> labels;
vector <string> code;
map <string, Symbol *> symbols;

Symbol::~Symbol()
{
	for(map<string, Symbol*>::iterator itr = symbols.begin(); itr != symbols.end(); itr++)
	{
		delete itr->second;
		symbols.erase(itr);
	}
}


void make_op(char, string);
void insert_symbol(int  type, string id, int size, string value);
void make_tmp_float(float v);
string get_load_line(Element e, int reg_no);
void print_int(string name);
void print_float(string name);
int get_type(Element e);
void check_predicate(string cond);
void print_string(string name);
void gen_string_code(string name);
void gen_if_code();
void input_integer(string name);
void input_float(string name);
string get_conv(int reg_no, int e1, int e2);
void print_tabi(string test);
void print_tabf(string test);
%}

%union
{
    char 	*text;
    int		ival;
    double 	fval;
};

%type <text> wyr
%token <text> ID
%token <text> STRING
%token <ival> LC
%token <fval> LZ
%token EQ NE LT GT GE LE
%token AND OR
%token INT FLOAT
%token IF ELSE
%token WHILE
%token READI READF
%token PRINTI PRINTF PRINTS				
%start linialinia
%%

linialinia 		: 	linialinia linia
					|	linia
					;
					
linia				:	przypisanie   ';'					{rpn << "\n";}
					|	deklaracja	  ';'				{;}
					|	wyswietlanie ';'				{;}
					|	pobranie ';'					{;}
					|	warunek_if	  				{;}
					|	warunek_while					{;}
					;
					
wyswietlanie : 
					PRINTI  '('ID')' 					{print_int($3);}
					|					
					PRINTI '('ID tab_wyr')'
					{
						code.push_back("\n#PRINT TAB INT");
						string test = $3;
						print_tabi(test);
						print_int($3);
						;}
														
					|
					PRINTF '('ID tab_wyr')'
					{
						code.push_back("\n#PRINT TAB FLOAT");
						string test = $3;
						print_tabf(test);
						print_float($3);
					;}
															
					|
					PRINTF '('ID')' 					{print_float($3);}
					|
					PRINTS '('STRING')' {print_string($3);}
					;

pobranie	:
					ID	'='	READI					{input_integer($1);}
					|
					ID	'='	READF					{input_float($1);}
					;
					
deklaracja : INT ID
					{		
							if(symbols.find($2) != symbols.end())
							{
								cout << "ERROR. This symbol: "<< $2 << "\nExists  \n" ;
								exit(-1);
							}
							insert_symbol(INT_TYPE, $2, 1, "0");
					}
					| FLOAT ID
					{
							if(symbols.find($2) != symbols.end())
							{
								cout << "ERROR. This symbol: "<< $2 << "\nExists  \n" ;
								exit(-1);
							}
							insert_symbol(FLOAT_TYPE, $2, 1, "0.0");
					}
					| INT ID '[' LC ']' 
					{
							insert_symbol(INT_ARRAY, $2, $4, "0:" + to_string($4));
					}
					| FLOAT ID '[' LC ']'
					{
							insert_symbol(FLOAT_ARRAY, $2, $4, "0:" + to_string($4));
					}
					;
					
warunek_while : warunek_while_start '(' warunek ')' '{' linialinia '}'
					{
						stringstream s;
						string label1 = labels.top();
						labels.pop();
						string label2 = labels.top();
						labels.pop();
						s << "j" << " " << label2 + "\n";
						code.push_back(s.str());
						code.push_back(label1 + ": ");
					}
					;

warunek_while_start : WHILE 
					{ 
						string label = "WHILE_LABEL" + to_string(while_counter);
						while_counter++;
						labels.push(label);
						code.push_back(label + ": ");
					}
					;
								
warunek_if	:	IF '(' warunek ') '  '{' linialinia '}'
					{
						gen_if_code();
					}
					|	IF '(' warunek ') '  '{' linialinia '}' warunek_else '{ ' linialinia '}'
					{
						gen_if_code();
					}
					;

warunek_else		:	ELSE
					{
						string label = labels.top();
						string newlabel = "LABEL" + to_string(label_counter);
						code.push_back("b " + newlabel);
						code.push_back(label + ":");
						labels.pop();
						label = "LABEL" + to_string(label_counter);
						labels.push(label);
						label_counter++;
					}
					;

warunek		:	wyr EQ wyr 						{check_predicate("bne");}
					|	wyr NE wyr 			{check_predicate("beq");}
					|	wyr LT wyr 			{check_predicate("bge");}
					|	wyr GT wyr 			{check_predicate("ble");}
					|	wyr LE wyr 			{check_predicate("bgt");}
					|	wyr GE wyr 			{check_predicate("blt");}
					;
					
przypisanie	: ID '=' wyr
					{
						rpn << " = " << $1; 
						Element e(ID, $1);
						argstack.push(e);
						make_op('=', "sw");
					}
					|	ID '[' wyr ']' '=' wyr
					{
						auto symbol = symbols.find($1);
						string test = $1;
						code.push_back("#TABLICA " + test);
						code.push_back("la $t4, " + test);

						Element e1 = argstack.top();
						argstack.pop();
						Element e2 = argstack.top();
						argstack.pop();				

						code.push_back(get_load_line(e1, 0));
						code.push_back(get_load_line(e2, 5));

						if(symbol->second->type == INT_ARRAY)
						{
							code.push_back("mul $t5, $t5, 4");
							code.push_back("add $t4, $t4, $t5");
							code.push_back("sw $t0, ($t4)");
						}
						else
						{
							code.push_back("mul $t5, $t5, 4");
							code.push_back("add $t4, $t4, $t5");
							code.push_back("s.s $f0, ($t4)");
						}
					;}
					; 		

wyr
	:wyr '+' skladnik 	   	{rpn << " + "; make_op('+', "add");}
	|wyr '-' skladnik	   	{rpn << " - "; make_op('-', "sub");}
	|skladnik		      	{rpn << " ";}
	;
skladnik
	:skladnik '*' czynnik		{rpn << " * "; make_op('*', "mul");}
	|skladnik '/' czynnik		{rpn << " / "; make_op('/', "div");}
	|czynnik		        {rpn << " ";}
	;
czynnik
	:ID			        {
						rpn <<  $1; 
						Element e(ID, $1);
						argstack.push(e);
					}
	|
	ID tab_wyr	{

					auto symbol = symbols.find($1);
					string test = $1;
					Element e1 = argstack.top();
					argstack.pop();
					code.push_back("la $t4, " + test);
					if(e1.type == ID)
						code.push_back("lw $t5, " + e1.value);
					else
						code.push_back("li $t5, " + e1.value);

					if(symbol->second->type == INT_ARRAY)
					{
						code.push_back("mul $t5, $t5, 4");
						code.push_back("add $t4, $t4, $t5");
						code.push_back("lw $t0, ($t4)");

						counter++;
						string tmp_name = "tmpr" + to_string(counter);
						code.push_back("#TAB int " + tmp_name);

						insert_symbol(INT_TYPE, tmp_name, 1, "0");
						Element e(ID, tmp_name);
						argstack.push(e);

						code.push_back("sw $t0, " + tmp_name);

					}
					else
					{
						code.push_back("mul $t5, $t5, 4");
						code.push_back("add $t4, $t4, $t5");
						code.push_back("l.s $f0, ($t4)");

						string tmp_name = "tmp_float_" + to_string(float_counter);
						code.push_back("#TAB float " + tmp_name);
						make_tmp_float(0.0);
						code.push_back("s.s $f0, " + tmp_name);
					}

				;}
	|
	|LC			             
				{
					rpn << $1;
					Element e(LC, to_string($1));
					argstack.push(e);
				}
	|LZ			{
					make_tmp_float($1);
				}
	|STRING			{;}
	|'(' wyr ')'		{;}
	;
tab_wyr
	:'[' wyr ']'		{}
	;
%%

int main(int argc, char *argv[])
{
	if(argc > 1)
	{
		;
	}
	else
	{
		;
	}
	;
	yyparse();
	
	code_out << ".data \n";
	for(auto symbol : symbols)
	{
			code_out << symbol.first << ":";
			if(symbol.second->type == INT_TYPE || symbol.second->type == INT_ARRAY)
			{
				code_out << " .word " << symbol.second->value;
			}
			else if(symbol.second->type == FLOAT_TYPE || symbol.second->type == FLOAT_ARRAY)
			{
				code_out << " .float " << symbol.second->value;
			}
			else if(symbol.second->type == STRING_TYPE)
			{
				code_out << " .asciiz " << symbol.second->value;
			}
			else
			{
				code_out << "ERROR";
			}
			code_out << " \n";
	}
	
	
	code_out << ".text:\n";
	
	for(auto line : code)
	{
		code_out << line << endl;
	}
	return 0;
}

void make_tmp_float(float v)
{
	string tmp_name = "tmp_float_" + to_string(float_counter);
	Element e(ID, tmp_name);
	insert_symbol(FLOAT_TYPE, tmp_name, 1, to_string(v));
	argstack.push(e);
	float_counter++;
}

void check_predicate(string cond)
{
	Element e2 = argstack.top();
	argstack.pop();
	Element e1 = argstack.top();
	argstack.pop();
	
	if(e1.type == FLOAT_TYPE || e2.type == FLOAT_TYPE)
	{
		cout << "Illegal number\n"; exit(-1);
	}
	
	string a1, a2, label;
	a1 = get_load_line(e1, 0);
	a2 = get_load_line(e2, 1);
	label = "LABEL" + to_string(label_counter);
	
	labels.push(label);
	code.push_back(a1);
	code.push_back(a2);
	code.push_back(cond +" $t0, $t1, " + label);
	
	label_counter++;
}

int get_type(Element e)
{
	int etype = NONE_TYPE;
	
	if(e.type == ID)
	{
		auto symbol = symbols.find(e.value);
		if(symbol->second->type == INT_TYPE)
		{
			return etype = INT_TYPE;
		}
		else if(symbol->second->type == FLOAT_TYPE)
		{
			return etype = FLOAT_TYPE;
		}
		else if(symbol->second->type == INT_ARRAY)
		{
			return etype = INT_ARRAY;
		}
		else
		{
			return etype = FLOAT_ARRAY;
		}
	}
	else
	{
		return etype = INT_TYPE;
	}

	return etype;
}

void insert_symbol(int  type, string id, int size, string value)
{
	if(symbols.find(id)==symbols.end())
	{
		symbols[id]= new Symbol(type, size, value);
	}
}

string get_conv(int reg_no, int e1type, int e2type)
{
	stringstream s;

	if (!(e2type == FLOAT_TYPE && e1type == INT_TYPE))
	{
		return {};
	}
	
	s << "mtc1 $t" << reg_no << ", " << "$f" << reg_no << "\n";
	s << "cvt.s.w $f" << reg_no << ", " << "$f" << reg_no;
	return s.str();
}

//Int czy flloat
string get_load_line(Element e, int reg_no)
{
	string regname = "$t";
	stringstream s;
	s << 'l';
	if(e.type == ID)
	{
		auto symbol = symbols.find(e.value);
		if(symbol->second->type == INT_TYPE)
		{
			s << "w ";
		}
		else
		{
			s << ".s ";
			regname = "$f";
		}
	}
	else //e.type == LC
	{
		s << "i ";
	}
	s << regname << reg_no << "," << e.value;
	return s.str();
}

void make_op(char op, string mnemo_op)
{
	Element e2 = argstack.top();
	argstack.pop();
	Element e1 = argstack.top();
	argstack.pop();
	string tmp_name = "tmpr";
	tmp_name += to_string(counter);
	
	threes << tmp_name << " <= ";
	
	int e1type = get_type(e1);
	int e2type = get_type(e2);
	
	if(op == '=')
	{
		threes << e2.value << op << e1.value << endl;
		string line1, line2;
		if(e2type == INT_TYPE)
		{
			line1 = get_load_line(e1, 0);
			line2 = "sw $t0, " + e2.value;
			insert_symbol(INT_TYPE, e2.value, 1, "0");
		}
		else
		{
			line1 = get_load_line(e1, 0);
			line2 = "s.s $f0, " + e2.value;
			insert_symbol(FLOAT_TYPE, e2.value, 1, "0");			
		}
				
		code.push_back(line1);
		
		if(e2type == INT_TYPE && e1type == FLOAT_TYPE)
		{
			code.push_back("\n#PRZYPISANIE FLOAT DO INT");
			code.push_back("lwc1 $f0, " + e1.value);
			code.push_back("trunc.w.s $f0, $f0");
			code.push_back("mfc1 $t0, $f0");
		}
		else if(e2type == FLOAT_TYPE && e1type == INT_TYPE)
		{
			code.push_back("\n#PRZYPISANIE INT DO FLOAT");
			code.push_back("mtc1 $t0, $f0");
			code.push_back("cvt.s.w $f0, $f0");
			code.push_back("swc1 $f0, "+ e2.value);
		}
		
		code.push_back(line2);
	}
	else
	{
		stringstream s;
		s << e1.value << op << e2.value;
		threes << s.str() << endl;
		
		int e_tmp_type = INT_TYPE;
		if(e1type == FLOAT_TYPE || e2type == FLOAT_TYPE)
		{
			e_tmp_type = FLOAT_TYPE;
		}
			
		code.push_back("\n#" + s.str());
		code.push_back(get_load_line(e1, 0));
		code.push_back(get_conv(0, e1type, e2type));
		code.push_back(get_load_line(e2, 1));
		code.push_back(get_conv(1, e2type, e1type));
		
		if(e1type == FLOAT_TYPE || e2type == FLOAT_TYPE)
		{
			code.push_back(mnemo_op + ".s $f0, $f0, $f1");
			code.push_back("swc1 $f0, " + tmp_name);
		}
		else
		{
			code.push_back(mnemo_op + " $t0, $t0, $t1");
			code.push_back("sw $t0, " + tmp_name);
		}
		
		insert_symbol(e_tmp_type, tmp_name, 1 ,"0");
		Element e(ID, tmp_name);
		argstack.push(e);
		counter++;	
	}
}

void print_int(string name)
{
	code.push_back("\n#PRINTI " + name);
	code.push_back("li $v0, 1");
	code.push_back("lw $a0, " + name);
	code.push_back("syscall");
}

void print_float(string name)
{
	code.push_back("\n#PRINTF " + name);
	code.push_back("li $v0, 2");
	code.push_back("lwc1 $f12, " + name);
	code.push_back("syscall");
}

void gen_string_code(string name)
{
	code.push_back("\n#PRINTS " + name);
	code.push_back("li $v0, 4");
	code.push_back("la $a0, " + name);
	code.push_back("syscall");
}

void gen_if_code()
{
	string label = labels.top();
	code.push_back(label + ":");
	labels.pop();
	label_counter++;
}

void print_string(string name)
{
	static int string_counter = 0;
	string tmp_name = "tmp_string_" + to_string(string_counter);
	Element e(ID, tmp_name);
	insert_symbol(STRING_TYPE, tmp_name, 1, name);
	argstack.push(e);
	string_counter++;
	gen_string_code(tmp_name);
}

void input_integer(string name)
{
	code.push_back("\n#INPUT_INT " + name);
	code.push_back("li $v0, 5");
	code.push_back("syscall");
	code.push_back("sw $v0, " + name);
}

void input_float(string name)
{
	code.push_back("\n#INPUT_FLOAT " + name);
	code.push_back("li $v0, 6");
	code.push_back("syscall");
	code.push_back("s.s $f0, " + name);
}

void print_tabi(string test)
{
	Element var1 = argstack.top();
	argstack.pop();
	string a1 = get_load_line(var1, 0);
	code.push_back("la $t4, " + test);
	if(var1.type == ID)
	code.push_back("lw $t5, " + var1.value);
	else
	code.push_back("li $t5, " + var1.value);

	code.push_back("mul $t5, $t5, 4");
	code.push_back("add $t4, $t4, $t5");
	code.push_back("lw $t0, ($t4)");
	code.push_back("sw $t0, " + test);
}

void print_tabf(string test)
{
	Element var1 = argstack.top();
	argstack.pop();
	string a1 = get_load_line(var1, 0);
	code.push_back("la $t4, " + test);
	if(var1.type == ID)
	code.push_back("lw $t5, " + var1.value);
	else
	code.push_back("li $t5, " + var1.value);

	code.push_back("mul $t5, $t5, 4");
	code.push_back("add $t4, $t4, $t5");
	code.push_back("l.s $f0, ($t4)");
	code.push_back("s.s $f0, " + test);
}
