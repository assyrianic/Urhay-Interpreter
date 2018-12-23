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
	
	/* + - * = ! & | ^ */
	TokenAdd, TokenSub, TokenStar, TokenAssign, TokenUnaryNot, TokenAmpersand, TokenVertBar, TokenCarot,
	
	/* += -= ++ -- */
	TokenAddAssign, TokenSubAssign, /*TokenIncr, TokenDecr,*/
	
	/* && || != == */
	TokenLogicAnd, TokenLogicOr, TokenNotEqual, TokenEqual,
	
	/* < > <= >= */
	TokenLess, TokenGreater,
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

enum UrhayExprType {
	IntExpr, /*StringExpr, FloatExpr, PtrExpr,*/
};

enum UrhayType {
	TypeInvalid, TypeInt, /*TypeString, TypeFloat, TypePtr,*/
};

struct UrhayVar {
	union {
		int64_t I64;
		/*
		double Dbl; struct HarbolString *Str;
		struct UrhayVar *Ref;*/
	};
	size_t
		//Bytes,
		//Offset, /* for structs/arrays. */
		Scope
	;
	//enum UrhayType Tag;
};

enum NodeType {
	ASTInvalid,
	ASTFuncDecl,
	ASTCmpndStmt, ASTIfStmt, ASTForLoop, ASTReturnStmt, ASTVarDecl, ASTVarInit,
	ASTAssign, ASTTernaryOp,
	ASTLogicalOr, ASTLogicalAnd, ASTBitOr, ASTBitXor, ASTBitAnd,
	ASTEqual, ASTNotEqual,
	ASTLessThan, ASTGreaterThan,
	ASTAdd, ASTSub, ASTMul,
	ASTRef, ASTDeref, ASTFuncCall, ASTNot, ASTBitNot,
	ASTVar, ASTIntLit, /*ASTFloatLit, ASTStrLit,*/
};

struct UrhayNode {
	union {
		struct HarbolString Iden; // for use in an expression.
		int64_t I64Lit;
		struct /* binary expr */ {
			struct UrhayNode *BinaryLeft, *BinaryRight;
			//enum UrhayExprType BinaryTag;
		};
		struct /* unary expr */ {
			struct UrhayNode *Unary;
			//enum UrhayExprType UnaryTag;
		};
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
		struct UrhayNode *PtrOp;
	};
	enum NodeType NodeTag;
};

void urhay_ast_print(const struct UrhayNode *);


struct UrhayInterp {
	struct HarbolLinkMap
		SymTable, KeyWords
	;
	struct /* UrhayLexer */ {
		struct HarbolString Lexeme;
		char *Src, *Iter;
		size_t Line;
		enum UrhayToken CurrToken;
	};
};

struct HarbolLinkMap *urhay_parse_module(struct UrhayInterp *);
void urhay_err_out(struct UrhayInterp *, const char [], ...);
bool urhay_interp(struct UrhayInterp *);


#endif /* Urhay_HEADER */
