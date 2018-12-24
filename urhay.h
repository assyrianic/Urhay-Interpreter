#ifndef Urhay_HEADER
	#define Urhay_HEADER

#include "libharbol/harbol.h"

typedef int32_t rune;
struct UrhayInterp;

enum UrhayToken {
	TokenInvalid=0,
	
	/* id 123 12.34 "string" 'char' */
	TokenIdentifier, TokenIntLiteral, /*TokenFloatLiteral, TokenStringLiteral, TokenCharLiteral,*/
	
	/* if else for return var hook */
	TokenIf, TokenElse, TokenFor, TokenReturn, TokenVar, TokenHook,
	
	/* () [] {} */
	TokenLeftParen, TokenRightParen, TokenLeftSqBracket, TokenRightSqBracket, TokenLeftCurlyBracket, TokenRightCurlyBracket,
	
	/* , ? : ; */
	TokenComma, TokenQuestion, TokenColon, TokenSemicolon,
	
	/* + - * / = ! & | ^ ~ */
	TokenAdd, TokenSub, TokenStar, TokenSlash, TokenAssign, TokenUnaryNot, TokenAmpersand, TokenVertBar, TokenCarot, TokenCompl,
	
	/* += -= *= /= << >> */
	TokenAddAssign, TokenSubAssign, TokenMulAssign, TokenLeftSh, TokenRightSh,
	
	/* && || != == */
	TokenLogicAnd, TokenLogicOr, TokenNotEqual, TokenEqual,
	
	/* < > <= >= */
	TokenLess, TokenGreater, TokenLessEq, TokenGreaterEq,
	
	/* @ */
	TokenAt,
};

void urhay_lexer_init(struct UrhayInterp *, char *);
void urhay_lexer_preprocess(struct UrhayInterp *);
enum UrhayToken urhay_lexer_get_token(struct UrhayInterp *);


/* Grammar
 * module = <function>* ;
 * function = <id> '(' <id>* ([',' <id>])* ')' <statement> ;
 * statement = <cmpnd_stmt> | <if_stmt> | <iter_stmt> | <return_stmt> | <var_decl> | <main_expr> ;
 * cmpnd_stmt = '{' <statement>* '}' ;
 * if_stmt = 'if' '(' <expr> ')' <statement> ;
 * iter_stmt = 'for' '(' <expr> ')' <statement> ;
 * return_stmt = 'return' <expr> ';' ;
 * var_decl = 'var' <var_decl_list> ';' ;
 * var_decl_list = <var_assign> [',' <var_decl_list>] ;
 * var_assign = <id> ['=' <assignment_exp>] ;
 * main_expr = <comma_expr> ';' ;
 * comma_expr = <assignment_exp> [',' <comma_expr>] ';' ;
 * assignment_exp = <conditional_exp> | <unary_exp> <assignment_operator> <assignment_exp> ;
 * assignment_operator = '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|=' ;
 * conditional_exp = <logical_or_exp> ['?' <expr> ':' <conditional_exp>] ;
 * logical_or_exp = <logical_and_exp> ['||' <logical_or_exp>] ;
 * logical_and_exp = <or_exp> ['&&' <logical_and_exp>] ;
 * or_exp = <xor_exp> ['|' <or_exp>] ;
 * xor_exp = <and_exp> ['^' <xor_exp>] ;
 * and_exp = <equality_exp> ['&' <and_exp>] ;
 * equality_exp = <relational_exp> [('==' | '!=') <equality_exp>] ;
 * relational_exp = <shift_exp> [('>=' | '<=' | '<' | '>') <relational_exp>] ;
 * shift_exp = <additive_exp> [('<<' | '>>') <shift_exp>] ;
 * additive_exp = <mult_exp> [('+' | '-') <additive_exp>] ;
 * mult_exp = <cast_exp> [('/' | '*' | '%') <mult_exp>] ;
 * cast_exp = <unary_exp> | '(' <type_name> ')' <cast_exp> ;
 * unary_exp = <postfix_exp> | <unary_operator> <cast_exp> | 'sizeof' ['('] <unary_exp> [')'] | ;
 * unary_operator = '&' | '*' | '+' | '-' | '~' | '!' ;
 * postfix_exp = <primary_exp> [ '[' <expr> ']' | '(' <arg_expr_list> ')' | '(' ')' | '.' <id> ] ;
 * arg_expr_list = <main_expr> [',' <arg_expr_list>] ;
 * primary_exp = <id> | <int> | <float> | <str> ;
 */

struct UrhayNode;

enum UrhayType {
	TypeInvalid,
	TypeInt,	// integer expression.
	TypeFunc,	// function "pointer".
	TypePtr,	// ptr/ref expr.
	/*
	TypeString,
	TypeFloat,
	TypeStruct,
	TypeMap,
	TypeArray,
	*/
};


struct UrhayVar {
	union {
		int64_t I64;
		/*
		double Dbl;
		struct HarbolString *Str;
		*/
	};
	size_t
		//Bytes,
		//Offset, /* for structs/arrays. */
		Scope
	;
	enum UrhayType VarType;
};

enum UrhayNodeType {
	ASTInvalid,
	ASTModule, ASTFuncDecl,
	ASTCmpndStmt, ASTIfStmt, ASTForLoop, ASTReturnStmt, ASTVarDecl, ASTVarInit,
	ASTAssign, ASTTernaryOp,
	ASTLogicalOr, ASTLogicalAnd, ASTBitOr, ASTBitXor, ASTBitAnd,
	ASTEqual, ASTNotEqual,
	ASTLessThan, ASTGreaterThan, ASTLessEq, ASTGreaterEq,
	ASTBitShiftLeft, ASTBitShiftRight,
	ASTAdd, ASTSub, ASTMul,
	ASTRef, ASTDeref, ASTFuncCall, ASTFuncPtrCall, ASTNot, ASTBitNot, ASTUnaryPlus, ASTUnaryMinus,
	ASTVar, ASTIntLit, /*ASTFloatLit, ASTStrLit,*/
};


struct UrhayNode {
	union {
		struct HarbolString Iden; // for use in an expression.
		int64_t I64Lit;
		struct /* binary expr */ {
			struct UrhayNode *BinaryLeft, *BinaryRight;
		};
		struct UrhayNode *Unary;
		struct /* if stmt or ternary operator */ {
			struct UrhayNode *Cond, *Then, *Else;
		};
		struct /* for stmt */ {
			struct UrhayNode *ForCond, *LoopStmt;
		};
		struct HarbolVector Stmts;
		struct HarbolLinkMap *VarDecls;
		struct /* assignment */ {
			struct HarbolString Varname;
			struct UrhayNode *Value;
		};
		struct UrhayNode *Return;
		struct UrhayNode *Decls;
		struct /* func decl */ {
			struct HarbolString FuncName;
			struct HarbolLinkMap *Params, *LocalVars;
			struct UrhayNode *FuncBody;
		};
		struct /* func call */ {
			struct UrhayNode *Caller;
			struct HarbolVector *Args;
		};
		struct HarbolLinkMap *Module;
	};
	enum UrhayNodeType NodeTag;
};

void urhay_ast_print(const struct UrhayNode *);


struct UrhayInterp {
	struct HarbolLinkMap
		SymTable, // global symtable that will hold functions.
		KeyWords // reserved keywords here!
	;
	struct /* UrhayLexer */ {
		struct HarbolString Lexeme;
		char *Src, *Iter;
		size_t Line;
		enum UrhayToken CurrToken;
	};
	struct UrhayNode *ModuleNode;
	//struct HarbolLinkMap *Modules; // linkmap of module nodes.
};

bool urhay_parse_file(struct UrhayInterp *);
void urhay_err_out(struct UrhayInterp *, const char [], ...);
bool urhay_interpret(struct UrhayInterp *);


#endif /* Urhay_HEADER */
