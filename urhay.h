#ifndef Urhay_HEADER
	#define Urhay_HEADER

#include "libharbol/harbol.h"

typedef int32_t rune;
struct UrhayInterp;

enum UrhayToken {
	TokenInvalid=0,
	
	/* id 123 12.34 "string" 'char' */
	TokenIdentifier, TokenIntLiteral, TokenFloatLiteral, /*TokenStringLiteral, TokenCharLiteral,*/
	
	/* if else for return var hook null */
	TokenIf, TokenElse, TokenFor, TokenReturn, TokenVar, TokenHook, TokenNull,
	
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


struct UrhayNode;

enum UrhayType {
	TypeInvalid,
	TypeInt,	// integer expression.
	TypeFunc,	// function "pointer".
	TypePtr,	// ptr/ref expr.
	TypeFloat,	// float expr.
	/*
	TypeString,
	TypeStruct,
	TypeMap,
	TypeArray,
	*/
};

struct UrhayVar {
	struct HarbolVariant Var;
	size_t
		//Bytes,
		//Offset, /* for structs/arrays. */
		Scope
	;
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
	ASTVar, ASTIntLit, ASTFloatLit, /*ASTStrLit,*/
};


struct UrhayNode {
	union {
		struct HarbolString Iden; // for use in an expression.
		struct HarbolVariant LitVal; // literal value.
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
