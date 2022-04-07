/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex

    /*#define codegen(...) \
        do { \
            for (int i = 0; i < INDENT; i++) { \
                fprintf(fout, "\t"); \
            } \
            fprintf(fout, __VA_ARGS__); \
        } while (0)*/    

    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;
   
    /* Other global variables */
    FILE *fout = NULL;
    bool HAS_ERROR = false;
    //int INDENT = 0;
    char *bytecode_filename = "hw3.j";
    void yyerror (){
        printf("error:%d: ", yylineno + 1);
    }
    FILE *FileOp() {
	return fopen(bytecode_filename, "a");
    }

    /* Symbol table function - you can add new function if needed. */
	struct table {
		int scope;
		int length;
		struct data *head;
		struct table *prev;
	};
	struct data {
		int index;
		char *name;
		char *type;
		int address;
		int lineno;
		char *elementType;
		struct data *next;
	};
	struct nest {
		int label;
		int end;
		struct nest *prev;
	};
	struct table *head;
	struct nest *layer;
	int scope = 0;
	int address = 0;
	int label = 0;
	int endLabel = 0;
	int nowEndLabel = 0;
	bool assignFlag = true;
	char *thisType = NULL;
	char *storeType = NULL;
	char *nothing = NULL;
	static void create_symbol();
	static void insert_symbol(char *, char *, char *);
	static char *lookup_symbol(char *);
	static void dump_symbol();
	char *getType(char *);
	static char *findType(char *);
	static void assign_symbol(char *);
  
%}

%error-verbose

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 */
%union {
    char *id;
    int i_val;
    float f_val;
    char *s_val;
    char *type;
    char *operator;
}

/* Token without return */
%token SEMICOLON
%token TRUE FALSE
%token INC DEC
%token GEQ LEQ EQL NEQ
%token ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN QUO_ASSIGN REM_ASSIGN
%token AND OR
%token PRINT IF ELSE FOR WHILE


/* Token with return, which need to sepcify type */
%token <id> IDENT
%token <i_val> INT_LIT
%token <f_val> FLOAT_LIT
%token <s_val> STRING_LIT
%token <type> INT FLOAT BOOL STRING

/* Nonterminal with return, which need to sepcify type */
%type <type> Type TypeName Expression UnaryExpr PrimaryExpr Operand Literal IndexExpr ConversionExpr Condition SubExpr AndExpr AddExpr CmpExpr  MulExpr IfCond BlockElse WhileCond
%type <operator> unary_op cmp_op add_op mul_op assign_op or_op and_op
 

/* Yacc will start at this nonterminal */
%start Program

/* Grammar section */
%%

Program
    : StatementList{
	FILE *file = FileOp();
	fprintf(file, "\treturn\n");
	fprintf(file, ".end method\n");
	fclose(file);
    }//{printf("pro\n");}
;

Type
    : TypeName { $$ = $1; }//printf("type\n"); }
;

TypeName
    : INT  { $$ = "int"; }//printf("typename int\n");}
    | FLOAT  { $$ = "float"; }//printf("typename float\n");}
    | STRING  { $$ = "string"; }// printf("typename string\n");}
    | BOOL  { $$ = "bool"; }
;


Literal
    : INT_LIT 
	{ $$ = "INT_LIT"; 
	  printf("INT_LIT %d\n", $<i_val>1); 
	  FILE *file = FileOp();
	  fprintf(file, "\tldc %d\n", $<i_val>1);
	  fclose(file);	
	}//printf("INT\n"); }
    | FLOAT_LIT 
	{ $$ ="FLOAT_LIT";
	  printf("FLOAT_LIT %f\n", $<f_val>1); 
	  FILE *file = FileOp();
	  fprintf(file, "\tldc %.6f\n", $<f_val>1);
	  fclose(file);
	}//printf("FLOAT\n"); }
    | TRUE
	{ $$ = "BOOL_LIT"; 
          printf("TRUE\n"); 
	  FILE *file = FileOp();
	  fprintf(file, "\ticonst_1\n");
	  fclose(file);
	}
    | FALSE
	{ $$ = "BOOL_LIT"; 
	  printf("FALSE\n");
	  FILE *file = FileOp();
	  fprintf(file, "\ticonst_0\n");
	  fclose(file);
	} 
    | '"' STRING_LIT '"'
	{ $$ = "STRING_LIT"; 
	  printf("STRING_LIT %s\n", $<s_val>2); 
	  FILE *file = FileOp();
	  fprintf(file, "\tldc \"%s\"\n", $<s_val>2);
          fclose(file);
	}
;

IndexExpr 
    : PrimaryExpr '[' Expression ']'
  	{ //printf("INDEX\n"); 
	  FILE *file = FileOp();
	  if (assignFlag==false){
		//printf("%s\n",(findType(thisType)));
		if (strcmp(findType(thisType), "int") == 0){
			fprintf(file, "\tiaload\n");		
		}
		else if(strcmp(findType(thisType), "float") == 0){
          		fprintf(file, "\tfaload\n");
		}
	  }
	  fclose(file);
	  $$ = $1;
	}
;

ConversionExpr 
    : '(' Type ')' Expression
	{
		printf("%s\n",$2);
		if (strcmp(getType($2), "float") == 0) {
			printf("I");
			FILE *file = FileOp();
			fprintf(file, "\ti2f\n");
			fclose(file);
		} 
		else  if(strcmp(getType($2), "int") == 0) {
			printf("F");
			FILE *file = FileOp();
			fprintf(file, "\tf2i\n");
			fclose(file);
		}
		printf(" to ");
		if (strcmp(getType($4), "float") == 0) {
			printf("I");
		} 
		else if (strcmp(getType($4), "int") == 0) {
			printf("F");
		}
		printf("\n");
	}
;

Expression 
    : SubExpr 
		{ $$ = $1; }//printf("subexpr\n");}
;

SubExpr
	: AndExpr or_op AndExpr
		{ 
			FILE *file = FileOp();
			fprintf(file, "\tior\n");
			fclose(file);			
			if (strcmp(getType($1), "bool") == 0 &&strcmp(getType($3), "bool") == 0) {
				$$ = "BOOL_LIT";
			} 
			else {
				char *type;
				if (strcmp(getType($1), "bool") != 0) {
					type = getType($<type>1);
				} 
				else if (strcmp(getType($1), "bool") == 0){
					type = getType($<type>3);
				}
				yyerror();
				printf("invalid operation: (operator %s not defined on %s)", $<operator>2,type);
			}
		}
	| AndExpr
		{ $$ = $1;}
;
AndExpr
	: CmpExpr and_op CmpExpr
		{ printf("%s\n", $<operator>2); 
		  FILE *file = FileOp();
		  fprintf(file, "\tiand\n");
		  fclose(file);
		  if (strcmp(getType($1), "bool") == 0 &&strcmp(getType($3), "bool") == 0) {
			$$ = "BOOL_LIT";
		  } 
		  else {
			char *type;
			if (strcmp(getType($1), "bool") != 0) {
				type = getType($<type>1);
			} 
			else if (strcmp(getType($1), "bool") == 0){
				type = getType($<type>3);
			}
			yyerror();
			printf("invalid operation: (operator %s not defined on %s)", $<operator>2,type);
		  }
		}
	| CmpExpr
		{ $$ = $1; }//printf("CMPEXPR\n");} 
;
CmpExpr
	: AddExpr cmp_op AddExpr
		{ printf("%s\n", $<operator>2); 
		  FILE *file = FileOp();
		  char *type = findType(thisType);
		  if (strcmp(type, "int") == 0) {
			fprintf(file, "\tisub\n");
		  } 
		  else if (strcmp(type, "float") == 0){
			fprintf(file, "\tfcmpl\n");
		  }
		  if (strcmp($<operator>2, "EQL") == 0) {
			fprintf(file, "\tifeq L_cmp_%d\n", label);
		  }
		  if (strcmp($<operator>2, "NEQ") == 0) {
			fprintf(file, "\tifne L_cmp_%d\n", label);
		  }
		  if (strcmp($<operator>2, "LSS") == 0) {
			fprintf(file, "\tiflt L_cmp_%d\n", label);
		  }
		  if (strcmp($<operator>2, "LEQ") == 0) {
			fprintf(file, "\tifle L_cmp_%d\n", label);
		  }
		  if (strcmp($<operator>2, "GTR") == 0) {
			fprintf(file, "\tifgt L_cmp_%d\n", label);
		  }
		  if (strcmp($<operator>2, "GEQ") == 0) {
			fprintf(file, "\tifge L_cmp_%d\n", label);
	  	  }
		  fprintf(file, "\ticonst_0\n");
		  fprintf(file, "\tgoto L_cmp_%d\n", label+1);
		  //label++;
		  fprintf(file, "L_cmp_%d:\n", label);
		  label++;
		  fprintf(file, "\ticonst_1\nL_cmp_%d:\n",label);
		  fclose(file);
		  $$ = "bool";
		   
		}
	| AddExpr //{ printf("aaaaaa\n"); }
;
AddExpr
	: MulExpr add_op MulExpr
		{ printf("%s\n", $<operator>2); 
		  FILE *file = FileOp();
		  $$ = $1;
		  
		  if (strcmp($<operator>2, "ADD") == 0) {
			if (strcmp(getType($<type>1), "int") == 0) {
				fprintf(file, "\tiadd\n");
			} 
			else if (strcmp(getType($<type>1), "float") == 0){
				fprintf(file, "\tfadd\n");
			}
		  } 
		  else if (strcmp($<operator>2, "SUB") == 0){
			if (strcmp(getType($<type>1), "int") == 0) {
				fprintf(file, "\tisub\n");
			} 
			else if (strcmp(getType($<type>1), "float") == 0){
				fprintf(file, "\tfsub\n");
			}
		  }
		  fclose(file);
		}
	| AddExpr add_op MulExpr
		{ printf("%s\n", $<operator>2); 
		  FILE *file = FileOp();
		  $$ = $1;
		  
		  if (strcmp($<operator>2, "ADD") == 0) {
			if (strcmp(getType($<type>1), "int") == 0) {
				fprintf(file, "\tiadd\n");
			} 
			else if (strcmp(getType($<type>1), "float") == 0){
				fprintf(file, "\tfadd\n");
			}
		  } 
		  else if (strcmp($<operator>2, "SUB") == 0){
			if (strcmp(getType($<type>1), "int") == 0) {
				fprintf(file, "\tisub\n");
			} 
			else if(strcmp(getType($<type>1), "float") == 0){
				fprintf(file, "\tfsub\n");
			}
		  }
		  fclose(file);

		}
	| MulExpr
		//{ printf("mmmmm\n"); }
;
MulExpr
	: UnaryExpr mul_op UnaryExpr
		{  
		  printf("%s\n", $<operator>2);
		  FILE *file = FileOp(); 
		  if ((strcmp(getType($1), "float") == 0 ||strcmp(getType($3), "float") == 0)&&(strcmp($2, "REM") == 0)) {
			yyerror();
			printf("invalid operation: (operator %s not defined on float)",$<operator>2);
		  } 
		  else {
			$$ = $1;
  		  }
		  if (strcmp($<operator>2, "MUL") == 0) {
			if (strcmp(getType($<type>3), "int") == 0) {
				fprintf(file, "\timul\n");
			}
		  	else if (strcmp(getType($<type>3), "float") == 0){
				fprintf(file, "\tfmul\n");
		  	}
		  } 
		  else if (strcmp($<operator>2, "QUO") == 0) {
			if (strcmp(getType($<type>3), "int") == 0) {
				fprintf(file, "\tidiv\n");
			} 
			else if (strcmp(getType($<type>3), "float") == 0){
				fprintf(file, "\tfdiv\n");
			}
 		  } 
		  else if (strcmp($<operator>2, "REM") == 0) {
			fprintf(file, "\tirem\n");
		  }
		  fclose(file);
		}
	| UnaryExpr 
		{ $$ = $1; }//  printf("unary\n");}
;

/*BinaryExpr
    : Expression binary_op Expression  
		{ 
			printf("%s\n", $<operator>2);
		}
   
; */

UnaryExpr 
    : PrimaryExpr 
		{ $$ = $1; }//printf("pripri\n");}
    | unary_op UnaryExpr
		{ 
		  printf("%s\n", $<operator>1); 
		  FILE *file = FileOp();
		  if (strcmp($<operator>1, "POS") == 0) {
		 	fprintf(file, "\t \n");
		  }
		  if (strcmp($<operator>1, "NEG") == 0) {
			if (strcmp(findType(thisType), "int") == 0) {
				fprintf(file, "\tineg\n");
			} 
			else if (strcmp(findType(thisType), "float") == 0){
				fprintf(file, "\tfneg\n");
			}
		  }
		  if (strcmp($<operator>1, "NOT") == 0) {
			fprintf(file, "\ticonst_1\n\tixor\n");
		  }
		  fclose(file);
		  $$ = $2;
		}
;

and_op
    : AND  { $$ = "AND"; }
;
or_op
    :OR    { $$ = "OR"; }
;

cmp_op 
    : EQL  { $$ = "EQL"; }
    | NEQ  { $$ = "NEQ"; }
    | '<'  { $$ = "LSS"; }
    | LEQ  { $$ = "LEQ"; }
    | '>'  { $$ = "GTR"; }
    | GEQ  { $$ = "GEQ"; }
;

add_op 
    : '+'  { $$ = "ADD"; }
    | '-'  { $$ = "SUB"; }
;

mul_op 
    : '*'  { $$ = "MUL"; }
    | '/'  { $$ = "QUO"; }
    | '%'  { $$ = "REM"; }
;

unary_op 
    : '+'  { $$ = "POS"; }
    | '-'  { $$ = "NEG"; }
    | '!'  { $$ = "NOT"; }
;

PrimaryExpr 
    : Operand 	{ $$ = $1; }// printf("OPRRRR FUCK\n");}
    | IndexExpr  { $$ = $1; }//printf("inininpri\n");} 
    | ConversionExpr  { $$ = $1; }//printf("conver"); }
;

Operand 
    : Literal  { $$ = $1; }//printf("LITERAL\n");}
    | IDENT { $$ = lookup_symbol($<id>1); }
    | '(' Expression ')'  { $$ = $2; }
;

Statement
    : DeclarationStmt //{printf("1\n");}
    | AssignmentStmt //{printf("2\n");}
    | IncDecStmt //{printf("3\n");}
    | Block //{printf("4\n");}
    | IfStmt //{printf("5\n");}
    | WhileStmt //{printf("6\n");}
    | ForStmt //{printf("7\n");}
    | PrintStmt //{printf("8\n");}
    | ExpressionStmt //{printf("9\n");}
;

DeclarationStmt
    : Type IDENT '=' Expression SEMICOLON
	{
		FILE *file = FileOp();
		if (strcmp(getType($<type>1), "int") == 0) {
			fprintf(file, "\tistore %d\n", address);
		}
		if (strcmp(getType($<type>1), "float") == 0) {
			fprintf(file, "\tfstore %d\n", address);
		}
		if (strcmp(getType($<type>1), "string") == 0) {
			fprintf(file, "\tastore %d\n", address);
		}
		fclose(file);
		insert_symbol($<id>2, getType($<type>1), "-");
		
	}
    | Type IDENT '[' Expression ']' SEMICOLON
	{	
		FILE *file = FileOp();		
		if (strcmp(getType($<type>1), "int") == 0) {
			fprintf(file, "\tnewarray int\n\tastore %d\n",address);
		}
		if (strcmp(getType($<type>1), "float") == 0) {
			fprintf(file, "\tnewarray float\n\tastore %d\n",address);
		}
		fclose(file);
		insert_symbol($<id>2, "array" , getType($<type>1));
		//printf("declarestmt\n");
	}
    | Type IDENT SEMICOLON
	{	
		FILE *file = FileOp();
		if (strcmp(getType($<type>1), "int") == 0) {
			fprintf(file, "\tldc 0\n\tistore %d\n", address);
		}
		if (strcmp(getType($<type>1), "float") == 0) {
			fprintf(file, "\tldc 0.0\n\tfstore %d\n", address);
		}
		if (strcmp(getType($<type>1), "string") == 0) {
			fprintf(file, "\tldc \"\"\n\tastore %d\n", address);
		}
		fclose(file);
		insert_symbol($<id>2, getType($<type>1), "-");
	} 
;

AssignmentExpr
    : Expression {storeType = thisType;assignFlag = false;} 
      assign_op Expression 
	{		
		/*if(strcmp($1,"INT_LIT")==0&&strcmp($3,"int")==0){
			yyerror();
			printf("cannot assign to %s\n", $3);

		}*/
		//printf("%s",$<type>4);
		FILE *file = FileOp();
		if (strcmp($<operator>3, "ADD_ASSIGN") == 0) {
			if(strcmp(getType($<type>4),"int") == 0){
				fprintf(file, "\tiadd\n");
			}
			else if(strcmp(getType($<type>4),"float") == 0){
				fprintf(file, "\tfadd\n");
			}
		}
		if (strcmp($<operator>3, "SUB_ASSIGN") == 0) {
			if(strcmp(getType($<type>4),"int") == 0){
				fprintf(file, "\tisub\n");
			}
			else if(strcmp(getType($<type>4),"float") == 0){
				fprintf(file, "\tfsub\n");
			}
		}
		if (strcmp($<operator>3, "MUL_ASSIGN") == 0) {
			if(strcmp(getType($<type>4),"int") == 0){
				fprintf(file, "\timul\n");
			}
			else if(strcmp(getType($<type>4),"float") == 0){
				fprintf(file, "\tfmul\n");
			}
		}
		if (strcmp($<operator>3, "QUO_ASSIGN") == 0) {
			if(strcmp(getType($<type>4),"int") == 0){
				fprintf(file, "\tidiv\n");
			}
			else if(strcmp(getType($<type>4),"float") == 0){
				fprintf(file, "\tfdiv\n");
			}
		}
		if (strcmp($<operator>3, "REM_ASSIGN") == 0) {
			if(strcmp(getType($<type>4),"int") == 0){
				fprintf(file, "\tirem\n");
			}
			else if(strcmp(getType($<type>4),"float") == 0){
				fprintf(file, "\tfrem\n");
			}
		}
		fclose(file);
		assign_symbol(storeType);
		assignFlag = true;
		printf("%s\n", $<operator>3);
	}
;

AssignmentStmt
    : AssignmentExpr SEMICOLON
;

assign_op
    : '='  { $$ = "ASSIGN"; }
    | ADD_ASSIGN  { $$ = "ADD_ASSIGN"; }
    | SUB_ASSIGN  { $$ = "SUB_ASSIGN"; }
    | MUL_ASSIGN  { $$ = "MUL_ASSIGN"; }
    | QUO_ASSIGN  { $$ = "QUO_ASSIGN"; }
    | REM_ASSIGN  { $$ = "REM_ASSIGN"; }
;

IncDecExpr
    : Expression INC  { //printf("INC\n"); 
		FILE *file = FileOp();
		//printf("%s\n",getType($<type>1));
		if (strcmp(getType($<type>1), "int") == 0) {
			fprintf(file, "\tldc 1\n\tiadd\n");
			//fprintf(file, "\tistore 0\n");
		} 
		else if (strcmp(getType($<type>1), "float") == 0){
			fprintf(file, "\tldc 1.0\n\tfadd\n");
			//fprintf(file, "\tfstore 2\n");
		}
		fclose(file);
		assign_symbol(thisType);
	}
    | Expression DEC  { //printf("DEC\n"); 
		FILE *file = FileOp();
		//printf("%s\n",getType($<type>1));
		if (strcmp(getType($<type>1), "int") == 0) {
			fprintf(file, "\tldc 1\n\tisub\n");
			//fprintf(file, "\tistore 0\n");
		} 
		else if(strcmp(getType($<type>1), "float") == 0){
			fprintf(file, "\tldc 1.0\n\tfsub\n");
			//fprintf(file, "\tfstore 2\n");
		}
		fclose(file);
		assign_symbol(thisType);
	}
;
        	
IncDecStmt 
    : IncDecExpr SEMICOLON
;			

ExpressionStmt 
    : Expression SEMICOLON//{printf("exprstmt\n");}
;

Block 
    : '{'  { create_symbol(); }
        StatementList 
      '}'  { dump_symbol(); }
;

StatementList
    : StatementList Statement// {printf("statementlist\n");}
    | Statement// {printf("statementlist\n");}
;

IfStmt 
    : IfCond Block
		{
			FILE *file = FileOp();
			fprintf(file, "L_cmp_%d:\n", layer->label);
			fprintf(file, "OUT_%d:\n", layer->end);
			fclose(file);
			struct nest *temp = layer->prev;
			if(temp==NULL){
				nowEndLabel = endLabel; 
			}
			else if(temp!=NULL){
				nowEndLabel = temp->end;
			}
			free(layer);
			layer = temp;
		}
	| IfCond BlockElse IfStmt
	| IfCond BlockElse Block
		{
			FILE *file = FileOp();
			fprintf(file, "OUT_%d:\n", nowEndLabel);
			fclose(file);
			nowEndLabel = endLabel;
		}
;
IfCond
	: IF Condition
		{
			struct nest *newLayer = (struct nest *)malloc(sizeof(struct nest));
			if (layer!=NULL) {
				newLayer->end = endLabel++;
			} else {
				newLayer->end = nowEndLabel;
			}
			newLayer->label = label;
			newLayer->prev = layer;
			layer = newLayer;
			FILE *file = FileOp();
			fprintf(file, "\tifeq L_cmp_%d\n", label++);
			fclose(file);
		}
;
BlockElse
	: Block ELSE
		{
			FILE *file = FileOp();
			fprintf(file, "\tgoto OUT_%d\n", layer->end);
			fprintf(file, "L_cmp_%d:\n", layer->label);
			fclose(file);
			struct nest *temp = layer->prev;
			nowEndLabel = layer->end;
			free(layer);
			layer = temp;
		}
;

Condition 
    : Expression 
;

WhileStmt 
    : WhileCond Block{
			FILE *file = FileOp();
			fprintf(file, "\tgoto While_%d\n", layer->label);
			fprintf(file, "OUT_%d:\n", layer->end);
			fclose(file);
			struct nest *temp = layer->prev;
			if(temp==NULL){
				nowEndLabel = endLabel; 
			}
			else if(temp!=NULL){
				nowEndLabel = temp->end;
			}
			free(layer);
			layer = temp;
		}
;
WhileCond
    :WHILE {
			struct nest *newLayer = (struct nest *)malloc(sizeof(struct nest));
			newLayer->end = endLabel++;
			newLayer->label = label;
			newLayer->prev = layer;
			layer = newLayer;
			FILE *file = FileOp();
			fprintf(file, "While_%d:\n", layer->label);
			label++;
			fclose(file);
	}
     '('Condition ')'{
			FILE *file = FileOp();
			fprintf(file, "\tifeq OUT_%d\n", layer->end);
			fclose(file);
	}
;
ForStmt 
    : FOR {
			FILE *file = FileOp();
			label++;
			fprintf(file, "L_cmp_%d:\n",label);
			fclose(file);
		}
      '(' ForClause ')' Block {
			FILE *file = FileOp();
			fprintf(file, "\tgoto L_cmp_%d\n", label);
			fprintf(file, "OUT_%d:\n", endLabel);
			fclose(file);
		}
;
ForClause 
    : InitStmt SEMICOLON {
			FILE *file = FileOp();
			//label++;
			fprintf(file, "L_cmp_%d:\n", label);
			fclose(file);
			label++;
		}
    Condition SEMICOLON {
			FILE *file = FileOp();
			label++;
			fprintf(file, "\tgoto L_cmp_%d\n", label+1);
			fprintf(file, "L_cmp_%d:\n", label);
			fclose(file);
		}

    PostStmt {
			FILE *file = FileOp();
			fprintf(file, "\tgoto L_cmp_1\n");
			label++;
			fprintf(file, "L_cmp_%d:\n", label);
			label--;
			fprintf(file, "\tifeq OUT_%d\n", endLabel);
			fclose(file);
		}
;

InitStmt
    : SimpleExpr
;

PostStmt
    : SimpleExpr
;

SimpleExpr 
    : AssignmentExpr 
    | Expression 
    | IncDecExpr
;

PrintStmt 
    : PRINT { assignFlag = false; } 
    '(' Expression ')' SEMICOLON{ printf("PRINT %s\n", getType($<type>4));
	  assignFlag = true;
	  FILE *file = FileOp();
	  if (strcmp(getType($<type>4), "bool") == 0) {
		fprintf(file, "\tifne L_cmp_%d\n\tldc \"false\"\n", label);
		fprintf(file, "\tgoto L_cmp_%d\n", label+1);
		fprintf(file, "L_cmp_%d:\n", label++);
		fprintf(file, "\tldc \"true\"\nL_cmp_%d:\n", label++);
	  }
	  fprintf(file, "\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n\tswap\n");
	  fprintf(file, "\tinvokevirtual java/io/PrintStream/print");
	  if (strcmp(getType($<type>4), "int") == 0) {
		fprintf(file, "(I)V\n");
	  } 
	  else if (strcmp(getType($<type>4), "float") == 0) {
		fprintf(file, "(F)V\n");
	  } 
	  else {
		fprintf(file, "(Ljava/lang/String;)V\n");
	  }
	  fclose(file);
	}// printf("PRINTD\n");}
;


%%

/* C code section */
int main(int argc, char *argv[])
{
    if (argc == 2) {
        yyin = fopen(argv[1], "r");
    } 
    else {
        yyin = stdin;
    }
    
    /*fout = fopen(bytecode_filename, "w");
    codegen(".source hw3.j\n");
    codegen(".class public Main\n");
    codegen(".super java/lang/Object\n");
    codegen(".method public static main([Ljava/lang/String;)V\n");
    codegen(".limit stack 100\n");
    codegen(".limit locals 100;\n");
    //INDENT++;*/

    struct table *global = (struct table *)malloc(sizeof(struct table));
	global->scope = scope;
	global->length = 0;
	global->head = NULL;
	global->prev = NULL;
	head = global;
    remove(bytecode_filename);
    FILE *file = FileOp();
    fprintf(file, ".source hw3.j\n");
    fprintf(file, ".class public Main\n");
    fprintf(file, ".super java/lang/Object\n");
    fprintf(file, ".method public static main([Ljava/lang/String;)V\n");
    fprintf(file, ".limit stack 100\n");
    fprintf(file, ".limit locals 100\n");
    fclose(file); 

    yylineno = 0;
    yyparse();
    dump_symbol();    
    printf("Total lines: %d\n", yylineno);

    /* Codegen end */
    /*codegen("return\n");
    //INDENT--;
    codegen(".end method\n");
    fclose(fout);*/
    fclose(yyin);

    if (HAS_ERROR == true) {
        remove(bytecode_filename);
    }
    
    return 0;
}

char *getType(char *target) {
    if (strcmp(target, "INT_LIT") == 0) return "int";
    if (strcmp(target, "FLOAT_LIT") == 0) return "float";
    if (strcmp(target, "BOOL_LIT") == 0) return "bool";
    if (strcmp(target, "STRING_LIT") == 0) return "string";
    return target;
}

static void create_symbol() {
    struct table *newTable = (struct table *)malloc(sizeof(struct table));
    newTable->scope = scope++;
    newTable->length = 0;
    newTable->head = NULL;
    newTable->prev = head;
    head = newTable;
}

static void insert_symbol(char *id, char *type, char *elementType) {
    struct data *checkData = head->head;
    while (checkData!=NULL) 
    {
        if (strcmp(checkData->name, id) == 0) {
		char lineno[256];
		sprintf(lineno, "%d", checkData->lineno);
		yyerror();
		printf("%s redeclared in this block. previous declaration at line %s\n", id,lineno);
		return;
        }
        checkData = checkData->next;
    }
    struct data *newData = (struct data *)malloc(sizeof(struct data));
    newData->index = head->length;
    newData->name = id;
    newData->type = type;
    newData->address = address;
    newData->lineno = yylineno + 1;
    newData->elementType = elementType;
    newData->next = NULL;
    head->length = head->length + 1;
    address++;
    struct data *lastData = head->head;
     if (lastData == NULL) {
          head->head = newData;
     } 
     else {
          while (lastData->next){ 
              lastData = lastData->next;
          }
          lastData->next = newData;
    }
    //printf("> Insert {%s} into symbol table (scope level: %d)\n", id, scope);
}

static char *lookup_symbol(char *id) {
	struct table *findTable = head;
	struct data *findData;
	while (findTable!=NULL) {
		findData = findTable->head;
		while (findData!=NULL) {
			if (strcmp(findData->name, id) == 0) {
				//printf("IDENT (name=%s, address=%d)\n", findData->name, findData->address);
				FILE *file = FileOp();
				if (strcmp(findData->type, "int") == 0) {
					fprintf(file, "\tiload %d\n", findData->address);
				} else if (strcmp(findData->type, "float") == 0) {
					fprintf(file, "\tfload %d\n", findData->address);
				} else if (strcmp(findData->type, "string") == 0) {
					fprintf(file, "\taload %d\n", findData->address);
				}
				if (strcmp(findData->type, "array") == 0) {
					fprintf(file, "\taload %d\n", findData->address);
				}
				fclose(file);
				break;
			}
			findData = findData->next;
		}
		if (findData==NULL) {
			findTable = findTable->prev;
		} 
		else{
			break;
		}
	}
	thisType = id;
	if (findData == NULL) {
		yyerror();
		printf("undefined: %s\n",id);
		return NULL;
	}
	if (strcmp(findData->type, "array") == 0) {
		return findData->elementType;
	} 
        else {
		return findData->type;
	}
}

static void dump_symbol() {
    struct table *dump = head;
    //printf("> Dump symbol table (scope level: %d)\n", scope);
    //printf("%-10s%-10s%-10s%-10s%-10s%s\n","Index", "Name", "Type", "Address", "Lineno", "Element type");
    struct data *target = dump->head;
    while (target!=NULL) {
    	//printf("%-10d%-10s%-10s%-10d%-10d%s\n", target->index, target->name, target->type, target->address, target->lineno, target->elementType);
    	struct data *temp = target->next;
    	free(target);
    	target = temp;
    }
    struct table *temp = head->prev;
    free(head);
    head = temp;
    scope--;
}
static char *findType(char *id) {
	if (id == NULL) return nothing;
	struct table *findTable = head;
	struct data *findData;
	while (findTable != NULL) {
		findData = findTable->head;
		while (findData!=NULL) {
			if (strcmp(findData->name, id) == 0) {
				break;
			}
			findData = findData->next;
		}
		if (findData == NULL) {
			findTable = findTable->prev;
		} 
		else {
			break;
		}
	}
	if (findData == NULL) return "-";
	if (strcmp(findData->type, "array") == 0) {
		return findData->elementType;
	} else {
		return findData->type;
	}
}
static void assign_symbol(char *id) {
	struct table *findTable = head;
	struct data *findData;
	while (findTable!=NULL) {
		findData = findTable->head;
		while (findData!=NULL) {
			if (strcmp(findData->name, id) == 0) {
				FILE *file = FileOp();
				if (strcmp(findData->type, "int") == 0) {
					fprintf(file, "\tistore %d\n", findData->address);
				} else if (strcmp(findData->type, "float") == 0) {
					fprintf(file, "\tfstore %d\n", findData->address);
				} else if (strcmp(findData->type, "string") == 0) {
					fprintf(file, "\tastore %d\n", findData->address);
				} else if (strcmp(findData->type, "array") == 0) {
					fprintf(file, "\t%castore\n", findData->elementType[0]);
					storeType = NULL;
				}
				fclose(file);
				break;
			}
			findData = findData->next;
		}
		if (findData == NULL) {
			findTable = findTable->prev;
		} else {
			break;
		}
	}
}


