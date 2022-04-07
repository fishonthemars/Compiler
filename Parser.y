/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex
    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;

    void yyerror ()
    {
        printf("error:%d: ", yylineno + 1);
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
	struct table *head;
	int scope = 0;
	int address = 0;
	static void create_symbol();
	static void insert_symbol(char *, char *, char *);
	static char *lookup_symbol(char *);
	static void dump_symbol();
	char *getType(char *);
  
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
%type <type> Type TypeName Expression UnaryExpr PrimaryExpr Operand Literal IndexExpr ConversionExpr Condition SubExpr AndExpr AddExpr CmpExpr  MulExpr  
%type <operator> unary_op cmp_op add_op mul_op assign_op or_op and_op
 

/* Yacc will start at this nonterminal */
%start Program

/* Grammar section */
%%

Program
    : StatementList//{printf("pro\n");}
;

Type
    : TypeName { $$ = $1; }//printf("type\n"); }
;

TypeName
    : INT  { $$ = "int";  }//printf("typename int\n");}
    | FLOAT  { $$ = "float"; }//printf("typename float\n");}
    | STRING  { $$ = "string"; }// printf("typename string\n");}
    | BOOL  { $$ = "bool"; }
;


Literal
    : INT_LIT 
	{ $$ = "INT_LIT"; printf("INT_LIT %d\n", $<i_val>1); }//printf("INT\n"); }
    | FLOAT_LIT 
	{ $$ ="FLOAT_LIT";printf("FLOAT_LIT %f\n", $<f_val>1); }//printf("FLOAT\n"); }
    | TRUE
	{ $$ = "BOOL_LIT"; printf("TRUE\n"); }
    | FALSE
	{ $$ = "BOOL_LIT"; printf("FALSE\n"); } 
    | '"' STRING_LIT '"'
	{ $$ = "STRING_LIT"; printf("STRING_LIT %s\n", $<s_val>2); }
;

IndexExpr 
    : PrimaryExpr '[' Expression ']'//  { printf("INDEX\n"); }
;

ConversionExpr 
    : '(' Type ')' Expression
	{
		//printf("%s\n",$2);
		if (strcmp(getType($2), "float") == 0) {
			printf("I");
		} 
		else  if(strcmp(getType($2), "int") == 0) {
			printf("F");
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
		{ printf("%s\n", $<operator>2); }
	| AndExpr
		{ $$ = $1;}
;
 	AndExpr
	: CmpExpr and_op CmpExpr
		{ printf("%s\n", $<operator>2); }
	| CmpExpr
		{ $$ = $1; }//printf("CMPEXPR\n");} 
;
CmpExpr
	: AddExpr cmp_op AddExpr
		{ printf("%s\n", $<operator>2); $$ = "bool"; }
	| AddExpr //{ printf("aaaaaa\n"); }
;
AddExpr
	: MulExpr add_op MulExpr
		{ printf("%s\n", $<operator>2); }
	| AddExpr add_op MulExpr
		{ printf("%s\n", $<operator>2); }
	| MulExpr
		//{ printf("mmmmm\n"); }
;
MulExpr
	: UnaryExpr mul_op UnaryExpr
		{  $$ = $1; printf("%s\n", $<operator>2); }
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
		{ $$ = $2; printf("%s\n", $<operator>1); }
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
    | ConversionExpr
;

Operand 
    : Literal  { $$ = $1; }//printf("LITERAL\n");}
    | IDENT 
	{ 
		$$ = lookup_symbol($<id>1);
	}
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
		insert_symbol($<id>2, getType($<type>1), "-");
		
	}
    | Type IDENT '[' Expression ']' SEMICOLON
	{	
		
		insert_symbol($<id>2, "array" , getType($<type>1));
		//printf("declarestmt\n");
	}
    | Type IDENT SEMICOLON
	{	
		insert_symbol($<id>2, getType($<type>1), "-");
	} 
;

AssignmentExpr
    : Expression assign_op Expression 
	{		
		if(strcmp($1,"INT_LIT")==0&&strcmp($3,"int")==0){
			yyerror();
			printf("cannot assign to %s\n", $3);

		}
		
		printf("%s\n", $<operator>2);
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
    : Expression INC  { printf("INC\n"); }
    | Expression DEC  { printf("DEC\n"); }
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
    : IF Condition Block 
    | IF Condition Block ELSE IfStmt
    | IF Condition Block ELSE Block
;

Condition 
    : Expression
;

WhileStmt 
    : WHILE '(' Condition ')' Block
;

ForStmt 
    : FOR '(' ForClause ')' Block
;

ForClause 
    : InitStmt SEMICOLON Condition SEMICOLON PostStmt
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
    : PRINT '(' Expression ')' SEMICOLON
	{ printf("PRINT %s\n", getType($<type>3));}// printf("PRINTD\n");}
;


%%

/* C code section */
int main(int argc, char *argv[])
{
    if (argc == 2) {
        yyin = fopen(argv[1], "r");
    } else {
        yyin = stdin;
    }
    struct table *global = (struct table *)malloc(sizeof(struct table));
	global->scope = scope;
	global->length = 0;
	global->head = NULL;
	global->prev = NULL;
	head = global;
 
    yylineno = 0;
    yyparse();
    dump_symbol();    
    printf("Total lines: %d\n", yylineno);
    fclose(yyin);
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
    newTable->scope = ++scope;
    newTable->length = 0;
    newTable->head = NULL;
    newTable->prev = head;
    head = newTable;
}

static void insert_symbol(char *id, char *type, char *elementType) {
    struct data *checkData = head->head;
    while (checkData) 
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
    printf("> Insert {%s} into symbol table (scope level: %d)\n", id, scope);
}

static char *lookup_symbol(char *id) {
	struct table *findTable = head;
	struct data *findData;
	while (findTable) {
		findData = findTable->head;
		while (findData) {
			if (strcmp(findData->name, id) == 0) {
				printf("IDENT (name=%s, address=%d)\n", findData->name, findData->address);
				break;
			}
			findData = findData->next;
		}
		if (!findData) {
			findTable = findTable->prev;
		} 
		else{
			break;
		}
	}
	if (!findData) {
		yyerror();
		printf("undefined: %s\n",id);
		return "no";
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
    printf("> Dump symbol table (scope level: %d)\n", scope);
    printf("%-10s%-10s%-10s%-10s%-10s%s\n","Index", "Name", "Type", "Address", "Lineno", "Element type");
    struct data *target = dump->head;
    while (target) {
    	printf("%-10d%-10s%-10s%-10d%-10d%s\n", target->index, target->name, target->type, target->address, target->lineno, target->elementType);
    struct data *temp = target->next;
		free(target);
		target = temp;
    }
    struct table *temp = head->prev;
    free(head);
    head = temp;
    scope--;
}

