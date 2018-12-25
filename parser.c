#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "urhay.h"

void urhay_err_out(struct UrhayInterp *const restrict interp, const char err[restrict], ...)
{
	va_list args;
	va_start(args, err);
	printf("Urhay Interpreter Error: **** ");
	vprintf(err, args);
	printf(" **** on line %zu\n", interp->Line);
	va_end(args);
	exit(-1);
}

static const char *_token_to_cstr(const enum UrhayToken token)
{
	switch( token ) {
		case TokenInvalid: return "invalid token";
		case TokenIdentifier: return "identifier";
		case TokenIntLiteral: return "decimal literal";
		case TokenFloatLiteral: return "float literal";
		//case TokenStringLiteral: return "string literal";
		//case TokenCharLiteral: return "char literal";
		case TokenIf: return "if";
		case TokenFor: return "for";
		case TokenReturn: return "return";
		case TokenVar: return "var";
		case TokenLeftParen: return "(";
		case TokenRightParen: return ")";
		case TokenLeftSqBracket: return "[";
		case TokenRightSqBracket: return "]";
		case TokenLeftCurlyBracket: return "{";
		case TokenRightCurlyBracket: return "}";
		case TokenComma: return ",";
		case TokenQuestion: return "?";
		case TokenColon: return ":";
		case TokenSemicolon: return ";";
		case TokenAdd: return "+";
		case TokenSub: return "-";
		case TokenStar: return "*";
		case TokenAssign: return "=";
		case TokenUnaryNot: return "!";
		case TokenAmpersand: return "&";
		case TokenVertBar: return "|";
		case TokenCarot: return "^";
		case TokenAddAssign: return "+=";
		case TokenSubAssign: return "-=";
		//case TokenIncr: return "++";
		//case TokenDecr: return "--";
		case TokenLogicAnd: return "&&";
		case TokenLogicOr: return "||";
		case TokenNotEqual: return "!=";
		case TokenEqual: return "==";
		default: return "unknown token";
	}
}

static void _parse_expect(struct UrhayInterp *const lexer, const enum UrhayToken token)
{
	if( lexer->CurrToken != token ) {
		urhay_err_out(lexer, "expected '%s' but got '%s'", _token_to_cstr(token), _token_to_cstr(lexer->CurrToken));
	}
}
/*
static bool _next_token(struct UrhayInterp *const lexer, const enum UrhayToken token)
{
	struct UrhayInterp state; memcpy(&state, lexer, sizeof state);
	const enum UrhayToken res = urhay_lexer_get_token(&state);
	harbol_string_del(&state.Lexeme);
	return res==token;
}
*/

typedef struct UrhayNode *ASTFunction(struct UrhayInterp *);

struct HarbolLinkMap *urhay_parse_var_decl_list(struct UrhayInterp *);
struct HarbolVector *urhay_parse_arg_expr_list(struct UrhayInterp *);

ASTFunction
	urhay_parse_function,
	urhay_parse_statement,
	urhay_parse_compound_stmt,
	urhay_parse_if_stmt, urhay_parse_iter_stmt, urhay_parse_ret_stmt,
	urhay_parse_var_decl, urhay_parse_var_assign,
	urhay_parse_main_expr,
	urhay_parse_comma_expr, urhay_parse_assign_expr,
	urhay_parse_conditional_expr, urhay_parse_logical_or_expr, urhay_parse_logical_and_expr,
	urhay_parse_or_expr, urhay_parse_xor_expr, urhay_parse_and_expr,
	urhay_parse_equality_expr, urhay_parse_relational_expr,
	urhay_parse_shift_expr, urhay_parse_add_expr, urhay_parse_mul_expr, urhay_parse_cast_expr, urhay_parse_unary_expr, urhay_parse_postfix_expr, urhay_parse_primary_expr
;

// module = (<function> | <func_decl>)* ;
struct UrhayNode *urhay_parse_module(struct UrhayInterp *const lexer)
{
	if( !lexer )
		return NULL;
	
	// reserve our reserved words.
	harbol_linkmap_insert(&lexer->KeyWords, "var", (union HarbolValue){0});
	harbol_linkmap_insert(&lexer->KeyWords, "if", (union HarbolValue){0});
	harbol_linkmap_insert(&lexer->KeyWords, "else", (union HarbolValue){0});
	harbol_linkmap_insert(&lexer->KeyWords, "for", (union HarbolValue){0});
	harbol_linkmap_insert(&lexer->KeyWords, "return", (union HarbolValue){0});
	harbol_linkmap_insert(&lexer->KeyWords, "hook", (union HarbolValue){0});
	
	// setup module node.
	struct UrhayNode *node = calloc(1, sizeof *node);
	node->NodeTag = ASTModule;
	node->Module = harbol_linkmap_new();
	
	// advance lexer to function identifier.
	urhay_lexer_get_token(lexer);
	while( lexer->CurrToken != TokenInvalid ) {
		struct UrhayNode *func = urhay_parse_function(lexer);
		//urhay_ast_print(func);
		const bool exists = harbol_linkmap_has_key(node->Module, func->FuncName.CStr);
		
		/* if our function doesn't exist in the symbol table, put it in regardless if func's body is defined. */
		if( !exists ) {
			harbol_linkmap_insert(node->Module, func->FuncName.CStr, (union HarbolValue){ .Ptr=func });
		}
		/* if we already have the entry but the funcbody is now defined, overwrite. */
		else if( exists && func->FuncBody ) {
			struct UrhayNode *check = harbol_linkmap_get(node->Module, func->FuncName.CStr).Ptr;
			if( check && check->FuncBody ) {
				urhay_err_out(lexer, "redefinition of function '%s'", func->FuncName.CStr);
			}
			else harbol_linkmap_set(node->Module, func->FuncName.CStr, (union HarbolValue){ .Ptr=func });
		}
	}
	/*
	const HarbolValue *const end = harbol_linkmap_get_iter_end_count(&lexer->SymTable);
	for( const HarbolValue *iter = harbol_linkmap_get_iter(&lexer->SymTable) ; iter && iter<end ; iter++ ) {
		printf("function name: '%s'\n", iter->KvPairPtr->KeyName.CStr);
	}
	*/
	return node;
}

// function = <id> '(' <id>* ([',' <id>])* ')' <statement> ;
struct UrhayNode *urhay_parse_function(struct UrhayInterp *const lexer)
{
	if( !lexer )
		return NULL;
	
	// assert that the current lexeme is a function
	_parse_expect(lexer, TokenIdentifier);
	struct UrhayNode *node = calloc(1, sizeof *node);
	node->NodeTag = ASTFuncDecl;
	harbol_string_copy_str(&node->FuncName, &lexer->Lexeme);
	const enum UrhayToken *const token = &lexer->CurrToken;
	
	// advance laxer and assert we have a left (
	urhay_lexer_get_token(lexer);
	_parse_expect(lexer, TokenLeftParen);
	
	// (a,b,c,...,z)
	// save the params for now, we'll determine their type from their initial usage.
	urhay_lexer_get_token(lexer);
	if( *token==TokenIdentifier )
		node->Params = urhay_parse_var_decl_list(lexer);
	// make sure we hit a ) and then advance the lexer
	_parse_expect(lexer, TokenRightParen);
	urhay_lexer_get_token(lexer);
	
	if( *token != TokenSemicolon ) {
		node->FuncBody = urhay_parse_compound_stmt(lexer);
	} else {
		urhay_lexer_get_token(lexer);
	}
	return node;
}

// cmpnd_stmt = '{' <statement>* '}' ;
struct UrhayNode *urhay_parse_compound_stmt(struct UrhayInterp *const lexer)
{
	if( !lexer )
		return NULL;
	
	struct UrhayNode *node = calloc(1, sizeof *node);
	node->NodeTag = ASTCmpndStmt;
	// assert we hit a { and advance lexer.
	_parse_expect(lexer, TokenLeftCurlyBracket);
	urhay_lexer_get_token(lexer);
	
	const enum UrhayToken *const token = &lexer->CurrToken;
	while( *token && *token != TokenRightCurlyBracket )
		harbol_vector_insert(&node->Stmts, (union HarbolValue){.Ptr = urhay_parse_statement(lexer)});
	
	// assert we hit } and advance lexer.
	_parse_expect(lexer, TokenRightCurlyBracket);
	urhay_lexer_get_token(lexer);
	return node;
}

// statement = <cmpnd_stmt> | <if_stmt> | <iter_stmt> | <return_stmt> | <var_decl> | <expr> ;
struct UrhayNode *urhay_parse_statement(struct UrhayInterp *const lexer)
{
	if( !lexer )
		return NULL;
	
	const enum UrhayToken *const token = &lexer->CurrToken;
	switch( *token ) {
		case TokenLeftCurlyBracket:	return urhay_parse_compound_stmt(lexer);
		case TokenIf:				return urhay_parse_if_stmt(lexer);
		case TokenFor:				return urhay_parse_iter_stmt(lexer);
		case TokenReturn:			return urhay_parse_ret_stmt(lexer);
		case TokenVar:				return urhay_parse_var_decl(lexer);
		case TokenElse: urhay_err_out(lexer, "lone 'else' without 'if' stmt!");
		default:					return urhay_parse_main_expr(lexer);
	}
}

// if_stmt = 'if' <expr> <statement> ;
struct UrhayNode *urhay_parse_if_stmt(struct UrhayInterp *const lexer)
{
	if( !lexer )
		return NULL;
	
	// make sure we have the 'if' keyword and advance lexer
	const enum UrhayToken *const token = &lexer->CurrToken;
	_parse_expect(lexer, TokenIf);
	urhay_lexer_get_token(lexer);
	
	// make sure we have ( and advance lexer
	//_parse_expect(lexer, TokenLeftParen);
	//urhay_lexer_get_token(lexer);
	
	struct UrhayNode *node = calloc(1, sizeof *node);
	node->NodeTag = ASTIfStmt;
	
	node->Cond = urhay_parse_comma_expr(lexer);
	
	// make sure we have ) and advance lexer
	//_parse_expect(lexer, TokenRightParen);
	//urhay_lexer_get_token(lexer);
	
	node->Then = urhay_parse_statement(lexer);
	if( *token==TokenElse ) {
		urhay_lexer_get_token(lexer);
		node->Else = urhay_parse_statement(lexer);
	}
	return node;
}

// iter_stmt = 'for' <expr> <statement> ;
struct UrhayNode *urhay_parse_iter_stmt(struct UrhayInterp *const lexer)
{
	if( !lexer )
		return NULL;
	
	// make sure we have the 'for' keyword and advance lexer
	//const enum UrhayToken *const token = &lexer->CurrToken;
	_parse_expect(lexer, TokenFor);
	urhay_lexer_get_token(lexer);
	
	// make sure we have ( and advance lexer
	//_parse_expect(lexer, TokenLeftParen);
	//urhay_lexer_get_token(lexer);
	
	struct UrhayNode *node = calloc(1, sizeof *node);
	node->NodeTag = ASTForLoop;
	
	node->ForCond = urhay_parse_comma_expr(lexer);
	
	// make sure we have ) and advance lexer
	//_parse_expect(lexer, TokenRightParen);
	//urhay_lexer_get_token(lexer);
	
	node->LoopStmt = urhay_parse_statement(lexer);
	return node;
}

// return_stmt = 'return' <expr> ';' ;
struct UrhayNode *urhay_parse_ret_stmt(struct UrhayInterp *const lexer)
{
	if( !lexer )
		return NULL;
	
	// make sure we have the 'return' keyword and advance lexer
	_parse_expect(lexer, TokenReturn);
	urhay_lexer_get_token(lexer);
	
	struct UrhayNode *node = calloc(1, sizeof *node);
	node->NodeTag = ASTReturnStmt;
	
	node->Return = urhay_parse_comma_expr(lexer);
	
	_parse_expect(lexer, TokenSemicolon);
	urhay_lexer_get_token(lexer);
	return node;
}

// var_decl = 'var' <var_decl_list> ';' ;
struct UrhayNode *urhay_parse_var_decl(struct UrhayInterp *const lexer)
{
	if( !lexer )
		return NULL;
	
	// make sure we have the 'var' keyword and advance lexer
	_parse_expect(lexer, TokenVar);
	urhay_lexer_get_token(lexer);
	
	struct UrhayNode *node = calloc(1, sizeof *node);
	node->NodeTag = ASTVarDecl;
	node->VarDecls = urhay_parse_var_decl_list(lexer);
	
	_parse_expect(lexer, TokenSemicolon);
	urhay_lexer_get_token(lexer);
	return node;
}

// var_decl_list = <var_assign> [',' <var_decl_list>] ;
struct HarbolLinkMap *urhay_parse_var_decl_list(struct UrhayInterp *const lexer)
{
	if( !lexer )
		return NULL;
	
	const enum UrhayToken *const token = &lexer->CurrToken;
	struct HarbolLinkMap *const declmap = harbol_linkmap_new();
	do {
		struct UrhayNode *const var = urhay_parse_var_assign(lexer);
		harbol_linkmap_insert(declmap, var->Varname.CStr, (union HarbolValue){.Ptr = var});
	} while( *token && *token==TokenComma && urhay_lexer_get_token(lexer) );
	return declmap;
}

// var_assign = <id> ['=' <assignment_exp>] ;
struct UrhayNode *urhay_parse_var_assign(struct UrhayInterp *const lexer)
{
	if( !lexer )
		return NULL;
	
	_parse_expect(lexer, TokenIdentifier);
	
	struct UrhayNode *node = calloc(1, sizeof *node);
	node->NodeTag = ASTVarInit;
	
	_parse_expect(lexer, TokenIdentifier);
	harbol_string_copy_str(&node->Varname, &lexer->Lexeme);
	urhay_lexer_get_token(lexer);
	
	const enum UrhayToken *const token = &lexer->CurrToken;
	if( *token==TokenAssign ) {
		urhay_lexer_get_token(lexer);
		node->Value = urhay_parse_assign_expr(lexer);
	}
	return node;
}

// main_expr = <comma_expr> ';' ;
struct UrhayNode *urhay_parse_main_expr(struct UrhayInterp *const lexer)
{
	//printf("\n%s start\n", __func__);
	if( !lexer )
		return NULL;
	
	struct UrhayNode *const node = urhay_parse_comma_expr(lexer);
	_parse_expect(lexer, TokenSemicolon);
	urhay_lexer_get_token(lexer);
	return node;
}

// comma_expr = <assignment_exp> [',' <main_expr>] ';' ;
struct UrhayNode *urhay_parse_comma_expr(struct UrhayInterp *const lexer)
{
	if( !lexer )
		return NULL;
	
	const enum UrhayToken *const token = &lexer->CurrToken;
	struct UrhayNode *node = urhay_parse_assign_expr(lexer);
	while( *token && *token==TokenComma ) {
		urhay_lexer_get_token(lexer);
		node = urhay_parse_assign_expr(lexer);
	}
	return node;
}

// assignment_exp = <conditional_exp> | <unary_exp> <assignment_operator> <assignment_exp> ;
// assignment_operator = '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|=' ;
struct UrhayNode *urhay_parse_assign_expr(struct UrhayInterp *const lexer)
{
	if( !lexer )
		return NULL;
	
	const enum UrhayToken *const token = &lexer->CurrToken;
	struct UrhayNode *node = urhay_parse_conditional_expr(lexer);
	if( *token==TokenQuestion ) {
		urhay_lexer_get_token(lexer);
		// node here is the cond
		struct UrhayNode *tern = calloc(1, sizeof *tern);
		tern->NodeTag = ASTTernaryOp;
		tern->Cond = node;
		tern->Then = urhay_parse_comma_expr(lexer);
		_parse_expect(lexer, TokenColon);
		urhay_lexer_get_token(lexer);
		tern->Else = urhay_parse_conditional_expr(lexer);
		return tern;
	} else if( *token==TokenAssign || *token==TokenAddAssign || *token==TokenSubAssign || *token==TokenMulAssign ) {
		const enum UrhayToken assigner = *token;
		const bool compound_assign = assigner != TokenAssign;
		urhay_lexer_get_token(lexer);
		/* compound assignments split into two binary operations:
		 * 'a += b' == 'a = a + b' ...
		 * First node is 'a = <expr>'. Binary Assign node
		 * Second node is 'a + b'. Binary Addition node, so 3 nodes total.
		 */
		struct UrhayNode *right = NULL;
		struct UrhayNode *value = urhay_parse_assign_expr(lexer);
		if( compound_assign ) {
			struct UrhayNode *binary = calloc(1, sizeof *binary);
			binary->NodeTag = assigner==TokenAddAssign ? ASTAdd : assigner==TokenSubAssign ? ASTSub : assigner==TokenMulAssign ? ASTMul : ASTInvalid;
			binary->BinaryLeft = node;
			binary->BinaryRight = value;
			right = binary;
		} else {
			right = value;
		}
		struct UrhayNode *assign = calloc(1, sizeof *assign);
		assign->NodeTag = ASTAssign;
		assign->BinaryLeft = node;
		assign->BinaryRight = right;
		return assign;
	}
	return node;
}

// conditional_exp = <logical_or_exp> ['?' <main_expr> ':' <conditional_exp>] ;
struct UrhayNode *urhay_parse_conditional_expr(struct UrhayInterp *const lexer)
{
	if( !lexer )
		return NULL;
	
	const enum UrhayToken *const token = &lexer->CurrToken;
	struct UrhayNode *node = urhay_parse_logical_or_expr(lexer);
	if( *token==TokenQuestion ) {
		urhay_lexer_get_token(lexer);
		struct UrhayNode *tern = calloc(1, sizeof *tern);
		tern->NodeTag = ASTTernaryOp;
		tern->Cond = node;
		tern->Then = urhay_parse_comma_expr(lexer);
		_parse_expect(lexer, TokenColon);
		urhay_lexer_get_token(lexer);
		tern->Else = urhay_parse_conditional_expr(lexer);
		return tern;
	}
	return node;
}

// logical_or_exp = <logical_and_exp> ['||' <logical_or_exp>] ;
struct UrhayNode *urhay_parse_logical_or_expr(struct UrhayInterp *const lexer)
{
	if( !lexer )
		return NULL;
	
	const enum UrhayToken *const token = &lexer->CurrToken;
	struct UrhayNode *node = urhay_parse_logical_and_expr(lexer);
	if( *token==TokenLogicOr ) {
		urhay_lexer_get_token(lexer);
		struct UrhayNode *bin = calloc(1, sizeof *bin);
		bin->NodeTag = ASTLogicalOr;
		bin->BinaryLeft = node;
		bin->BinaryRight = urhay_parse_logical_or_expr(lexer);
		return bin;
	}
	return node;
}

// logical_and_exp = <or_exp> ['&&' <logical_and_exp>] ;
struct UrhayNode *urhay_parse_logical_and_expr(struct UrhayInterp *const lexer)
{
	if( !lexer )
		return NULL;
	
	const enum UrhayToken *const token = &lexer->CurrToken;
	struct UrhayNode *node = urhay_parse_or_expr(lexer);
	if( *token==TokenLogicAnd ) {
		urhay_lexer_get_token(lexer);
		struct UrhayNode *bin = calloc(1, sizeof *bin);
		bin->NodeTag = ASTLogicalAnd;
		bin->BinaryLeft = node;
		bin->BinaryRight = urhay_parse_logical_and_expr(lexer);
		return bin;
	}
	return node;
}

// or_exp = <xor_exp> ['|' <or_exp>] ;
struct UrhayNode *urhay_parse_or_expr(struct UrhayInterp *const lexer)
{
	if( !lexer )
		return NULL;
	
	const enum UrhayToken *const token = &lexer->CurrToken;
	struct UrhayNode *node = urhay_parse_xor_expr(lexer);
	if( *token==TokenVertBar ) {
		urhay_lexer_get_token(lexer);
		struct UrhayNode *bin = calloc(1, sizeof *bin);
		bin->NodeTag = ASTBitOr;
		bin->BinaryLeft = node;
		bin->BinaryRight = urhay_parse_or_expr(lexer);
		return bin;
	}
	return node;
}

// xor_exp = <and_exp> ['^' <xor_exp>] ;
struct UrhayNode *urhay_parse_xor_expr(struct UrhayInterp *const lexer)
{
	if( !lexer )
		return NULL;
	
	const enum UrhayToken *const token = &lexer->CurrToken;
	struct UrhayNode *node = urhay_parse_and_expr(lexer);
	if( *token==TokenCarot ) {
		urhay_lexer_get_token(lexer);
		struct UrhayNode *bin = calloc(1, sizeof *bin);
		bin->NodeTag = ASTBitXor;
		bin->BinaryLeft = node;
		bin->BinaryRight = urhay_parse_xor_expr(lexer);
		return bin;
	}
	return node;
}

// and_exp = <equality_exp> ['&' <and_exp>] ;
struct UrhayNode *urhay_parse_and_expr(struct UrhayInterp *const lexer)
{
	if( !lexer )
		return NULL;
	
	const enum UrhayToken *const token = &lexer->CurrToken;
	struct UrhayNode *node = urhay_parse_equality_expr(lexer);
	if( *token==TokenAmpersand ) {
		urhay_lexer_get_token(lexer);
		struct UrhayNode *bin = calloc(1, sizeof *bin);
		bin->NodeTag = ASTBitAnd;
		bin->BinaryLeft = node;
		bin->BinaryRight = urhay_parse_and_expr(lexer);
		return bin;
	}
	return node;
}

// equality_exp = <relational_exp> [('==' | '!=') <equality_exp>] ;
struct UrhayNode *urhay_parse_equality_expr(struct UrhayInterp *const lexer)
{
	if( !lexer )
		return NULL;
	
	const enum UrhayToken *const token = &lexer->CurrToken;
	struct UrhayNode *node = urhay_parse_relational_expr(lexer);
	if( *token==TokenNotEqual || *token==TokenEqual ) {
		const enum UrhayToken eql = *token;
		urhay_lexer_get_token(lexer);
		struct UrhayNode *bin = calloc(1, sizeof *bin);
		bin->NodeTag = eql==TokenEqual ? ASTEqual : eql==TokenNotEqual ? ASTNotEqual : ASTInvalid;
		bin->BinaryLeft = node;
		bin->BinaryRight = urhay_parse_equality_expr(lexer);
		return bin;
	}
	return node;
}

// relational_exp = <shift_exp> [('>=' | '<=' | '<' | '>') <relational_exp>] ;
struct UrhayNode *urhay_parse_relational_expr(struct UrhayInterp *const lexer)
{
	if( !lexer )
		return NULL;
	
	const enum UrhayToken *const token = &lexer->CurrToken;
	struct UrhayNode *node = urhay_parse_shift_expr(lexer);
	if( *token==TokenLess ) {
		urhay_lexer_get_token(lexer);
		struct UrhayNode *bin = calloc(1, sizeof *bin);
		bin->NodeTag = ASTLessThan;
		bin->BinaryLeft = node;
		bin->BinaryRight = urhay_parse_relational_expr(lexer);
		return bin;
	} else if( *token==TokenGreater ) {
		urhay_lexer_get_token(lexer);
		struct UrhayNode *bin = calloc(1, sizeof *bin);
		bin->NodeTag = ASTGreaterThan;
		bin->BinaryLeft = node;
		bin->BinaryRight = urhay_parse_relational_expr(lexer);
		return bin;
	} else if( *token==TokenLessEq ) {
		urhay_lexer_get_token(lexer);
		struct UrhayNode *bin = calloc(1, sizeof *bin);
		bin->NodeTag = ASTLessEq;
		bin->BinaryLeft = node;
		bin->BinaryRight = urhay_parse_relational_expr(lexer);
		return bin;
	} else if( *token==TokenGreaterEq ) {
		urhay_lexer_get_token(lexer);
		struct UrhayNode *bin = calloc(1, sizeof *bin);
		bin->NodeTag = ASTGreaterEq;
		bin->BinaryLeft = node;
		bin->BinaryRight = urhay_parse_relational_expr(lexer);
		return bin;
	}
	return node;
}

// shift_exp = <additive_exp> [('<<' | '>>') <shift_exp>] ;
struct UrhayNode *urhay_parse_shift_expr(struct UrhayInterp *const lexer)
{
	if( !lexer )
		return NULL;
	
	const enum UrhayToken *const token = &lexer->CurrToken;
	struct UrhayNode *node = urhay_parse_add_expr(lexer);
	if( *token==TokenLeftSh || *token==TokenRightSh ) {
		struct UrhayNode *bin = calloc(1, sizeof *bin);
		bin->NodeTag = *token==TokenLeftSh ? ASTBitShiftLeft : *token==TokenRightSh ? ASTBitShiftRight : ASTInvalid;
		urhay_lexer_get_token(lexer);
		bin->BinaryLeft = node;
		bin->BinaryRight = urhay_parse_relational_expr(lexer);
		return bin;
	}
	return node;
}

// additive_exp = <mult_exp> [('+' | '-') <additive_exp>] ;
struct UrhayNode *urhay_parse_add_expr(struct UrhayInterp *const lexer)
{
	if( !lexer )
		return NULL;
	
	const enum UrhayToken *const token = &lexer->CurrToken;
	struct UrhayNode *node = urhay_parse_mul_expr(lexer);
	if( *token==TokenAdd || *token==TokenSub ) {
		const enum UrhayToken t = *token;
		urhay_lexer_get_token(lexer);
		struct UrhayNode *bin = calloc(1, sizeof *bin);
		bin->NodeTag = t==TokenAdd ? ASTAdd : t==TokenSub ? ASTSub : ASTInvalid;
		bin->BinaryLeft = node;
		bin->BinaryRight = urhay_parse_add_expr(lexer);
		return bin;
	}
	return node;
}

// mult_exp = <cast_exp> [('/' | '*' | '%') <mult_exp>] ;
struct UrhayNode *urhay_parse_mul_expr(struct UrhayInterp *const lexer)
{
	if( !lexer )
		return NULL;
	
	const enum UrhayToken *const token = &lexer->CurrToken;
	struct UrhayNode *node = urhay_parse_cast_expr(lexer);
	if( *token==TokenStar ) {
		urhay_lexer_get_token(lexer);
		struct UrhayNode *bin = calloc(1, sizeof *bin);
		bin->NodeTag = ASTMul;
		bin->BinaryLeft = node;
		bin->BinaryRight = urhay_parse_mul_expr(lexer);
		return bin;
	}
	return node;
}

// cast_exp = <unary_exp> | '(' <type_name> ')' <cast_exp> ;
struct UrhayNode *urhay_parse_cast_expr(struct UrhayInterp *const lexer)
{
	if( !lexer )
		return NULL;
	/*
	const enum UrhayToken *const token = &lexer->CurrToken;
	if( *token==TokenLeftParen ) {
		urhay_lexer_get_token(lexer);
		// read cast type
		urhay_parse_cast_expr(lexer);
		return true;
	}
	*/
	return urhay_parse_unary_expr(lexer);
}

// unary_exp = <postfix_exp> | <unary_operator> <cast_exp> | 'sizeof' ['('] <unary_exp> [')'] | ;
// unary_operator = '&' | '@' | '+' | '-' | '~' | '!' ;
struct UrhayNode *urhay_parse_unary_expr(struct UrhayInterp *const lexer)
{
	if( !lexer )
		return NULL;
	
	const enum UrhayToken *const token = &lexer->CurrToken;
	// add code for sizeof and alignof
	if( *token==TokenAt ) {
		urhay_lexer_get_token(lexer);
		struct UrhayNode *node = calloc(1, sizeof *node);
		node->NodeTag = ASTDeref;
		node->Unary = urhay_parse_cast_expr(lexer);
		return node;
	} else if( *token==TokenAmpersand ) {
		urhay_lexer_get_token(lexer);
		struct UrhayNode *node = calloc(1, sizeof *node);
		node->NodeTag = ASTRef;
		node->Unary = urhay_parse_cast_expr(lexer);
		return node;
	} else if( *token==TokenUnaryNot ) {
		urhay_lexer_get_token(lexer);
		struct UrhayNode *node = calloc(1, sizeof *node);
		node->NodeTag = ASTNot;
		node->Unary = urhay_parse_cast_expr(lexer);
		return node;
	} else if( *token==TokenAdd ) {
		urhay_lexer_get_token(lexer);
		struct UrhayNode *node = calloc(1, sizeof *node);
		node->NodeTag = ASTUnaryPlus;
		node->Unary = urhay_parse_cast_expr(lexer);
		return node;
	} else if( *token==TokenSub ) {
		urhay_lexer_get_token(lexer);
		struct UrhayNode *node = calloc(1, sizeof *node);
		node->NodeTag = ASTUnaryMinus;
		node->Unary = urhay_parse_cast_expr(lexer);
		return node;
	} else if( *token==TokenCompl ) {
		urhay_lexer_get_token(lexer);
		struct UrhayNode *node = calloc(1, sizeof *node);
		node->NodeTag = ASTBitNot;
		node->Unary = urhay_parse_cast_expr(lexer);
		return node;
	}
	return urhay_parse_postfix_expr(lexer);
}

// postfix_exp = <primary_exp> [ '[' <expr> ']' | '(' arg_expr_list ')' | '(' ')' | '.' <id> ] ;
struct UrhayNode *urhay_parse_postfix_expr(struct UrhayInterp *const lexer)
{
	if( !lexer )
		return NULL;
	
	const enum UrhayToken *const token = &lexer->CurrToken;
	struct UrhayNode *node = urhay_parse_primary_expr(lexer);
	/*
	if( *token==TokenLeftSqBracket ) {
		urhay_lexer_get_token(lexer);
		struct UrhayNode *expr_node = urhay_parse_assign_expr(lexer);
		_parse_expect(lexer, TokenRightSqBracket);
		urhay_lexer_get_token(lexer);
	} else*/ if( *token==TokenLeftParen ) {
		urhay_lexer_get_token(lexer);
		struct UrhayNode *fcall = calloc(1, sizeof *node);
		fcall->NodeTag = ASTFuncCall;
		fcall->Caller = node;
		fcall->Args = urhay_parse_arg_expr_list(lexer);
		_parse_expect(lexer, TokenRightParen);
		urhay_lexer_get_token(lexer);
		return fcall;
	}
	return node;
}

// arg_expr_list = <assign_expr> [',' <arg_expr_list>] ;
struct HarbolVector *urhay_parse_arg_expr_list(struct UrhayInterp *const lexer)
{
	if( !lexer )
		return NULL;
	
	const enum UrhayToken *const token = &lexer->CurrToken;
	struct HarbolVector *const argvec = harbol_vector_new();
	do {
		struct UrhayNode *const arg = urhay_parse_assign_expr(lexer);
		harbol_vector_insert(argvec, (union HarbolValue){.Ptr = arg});
	} while( *token && *token==TokenComma && urhay_lexer_get_token(lexer) );
	return argvec;
}

// primary_exp = <id> | <int> | <float> | <str> ;
struct UrhayNode *urhay_parse_primary_expr(struct UrhayInterp *const lexer)
{
	if( !lexer )
		return NULL;
	
	const enum UrhayToken *const token = &lexer->CurrToken;
	if( *token==TokenIdentifier ) {
		struct UrhayNode *node = calloc(1, sizeof *node);
		node->NodeTag = ASTVar;
		harbol_string_copy_str(&node->Iden, &lexer->Lexeme);
		urhay_lexer_get_token(lexer);
		return node;
	} else if( *token==TokenIntLiteral ) {
		struct UrhayNode *const node = calloc(1, sizeof *node);
		node->NodeTag = ASTIntLit;
		node->LitVal.Val.Int64 = strtoll(lexer->Lexeme.CStr, NULL, 0);
		node->LitVal.TypeTag = TypeInt;
		urhay_lexer_get_token(lexer);
		return node;
	} else if( *token==TokenFloatLiteral ) {
		struct UrhayNode *const node = calloc(1, sizeof *node);
		node->NodeTag = ASTFloatLit;
		node->LitVal.Val.Double = strtod(lexer->Lexeme.CStr, NULL);
		node->LitVal.TypeTag = TypeFloat;
		urhay_lexer_get_token(lexer);
		return node;
	} else if( *token==TokenNull ) {
		struct UrhayNode *const node = calloc(1, sizeof *node);
		node->NodeTag = ASTIntLit;
		node->LitVal.Val.Int64 = 0;
		node->LitVal.TypeTag = TypePtr;
		urhay_lexer_get_token(lexer);
		return node;
	} else if( *token==TokenLeftParen ) {
		urhay_lexer_get_token(lexer);
		struct UrhayNode *const node = urhay_parse_comma_expr(lexer);
		_parse_expect(lexer, TokenRightParen);
		urhay_lexer_get_token(lexer);
		return node;
	}
	return NULL;
}


bool urhay_parse_file(struct UrhayInterp *const lexer)
{
	if( !lexer )
		return false;
	lexer->ModuleNode = urhay_parse_module(lexer);
	return lexer->ModuleNode != NULL;
}
