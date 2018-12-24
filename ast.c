#include <stdio.h>
#include <stdlib.h>

#include "urhay.h"


int64_t urhay_walk(struct UrhayInterp *, struct UrhayNode *, struct HarbolLinkMap *, struct HarbolLinkMap *, size_t, bool *);
struct HarbolLinkMap *urhay_setup_func_params(struct UrhayInterp *, struct HarbolLinkMap *, struct HarbolLinkMap *);

FILE *g_debug_print;

bool urhay_var_free(void *v)
{
	struct UrhayVar **pvar = v;
	free(*pvar), *pvar=NULL;
	return true;
}


void urhay_ast_print(const struct UrhayNode *const node)
{
	if( !node )
		return;
	
	switch( node->NodeTag ) {
		case ASTModule: {
			puts("\nAST Module Start");
			puts("AST Module Funcs");
			const union HarbolValue *const end = harbol_linkmap_get_iter_end_count(node->Module);
			for( const union HarbolValue *iter = harbol_linkmap_get_iter(node->Module) ; iter && iter<end ; iter++ ) {
				urhay_ast_print(iter->KvPairPtr->Data.Ptr);
			}
			puts("AST Module End\n");
			break;
		}
		case ASTFuncDecl: {
			puts("\nAST Func Decl Start");
			printf("AST Func Name '%s'\n", node->FuncName.CStr);
			puts("AST Func Params");
			const union HarbolValue *const end = harbol_linkmap_get_iter_end_count(node->Params);
			for( const union HarbolValue *iter = harbol_linkmap_get_iter(node->Params) ; iter && iter<end ; iter++ ) {
				urhay_ast_print(iter->KvPairPtr->Data.Ptr);
			}
			puts("AST Func Body");
			urhay_ast_print(node->FuncBody);
			puts("AST Func Decl End\n");
			break;
		}
		case ASTTernaryOp:
			puts("AST Ternary Stmt Start");
		case ASTIfStmt: {
			puts("AST If Stmt Start");
			puts("AST If Expr");
			urhay_ast_print(node->Cond);
			puts("AST If Statements");
			urhay_ast_print(node->Then);
			puts("AST If Else Statements");
			urhay_ast_print(node->Else);
			puts("AST If Stmt End");
			break;
		}
		case ASTForLoop: {
			puts("AST For Stmt Start");
			puts("AST For Expr");
			urhay_ast_print(node->ForCond);
			puts("AST For Loop Stmt");
			urhay_ast_print(node->LoopStmt);
			puts("AST For Stmt End");
			break;
		}
		case ASTReturnStmt: {
			puts("AST Return Stmt Start");
			urhay_ast_print(node->Return);
			puts("AST Return Stmt End");
			break;
		}
		case ASTVarDecl: {
			puts("AST Var Decl Start");
			const union HarbolValue *const end = harbol_linkmap_get_iter_end_count(node->VarDecls);
			for( const union HarbolValue *iter = harbol_linkmap_get_iter(node->VarDecls) ; iter && iter<end ; iter++ ) {
				urhay_ast_print(iter->KvPairPtr->Data.Ptr);
			}
			puts("AST Var Decl End");
			break;
		}
		case ASTVarInit: {
			puts("AST Var Init Start");
			printf("AST Var Name: '%s'\n", node->Varname.CStr);
			urhay_ast_print(node->Value);
			puts("AST Var Init End");
			break;
		}
		case ASTCmpndStmt: {
			puts("AST Compound Stmt Start");
			const union HarbolValue *const end = harbol_vector_get_iter_end_count(&node->Stmts);
			for( const union HarbolValue *iter = harbol_vector_get_iter(&node->Stmts) ; iter && iter<end ; iter++ ) {
				urhay_ast_print(iter->Ptr);
			}
			puts("AST Compound Stmt End");
			break;
		}
		case ASTAssign: {
			puts("AST Assignment Expr Start");
			puts("AST Assignment Left");
			urhay_ast_print(node->BinaryLeft);
			puts("AST Assignment Right");
			urhay_ast_print(node->BinaryRight);
			puts("AST Assignment Expr End");
			break;
		}
		case ASTLogicalOr: {
			puts("AST || Expr Start");
			puts("AST || Expr Left");
			urhay_ast_print(node->BinaryLeft);
			puts("AST || Expr Right");
			urhay_ast_print(node->BinaryRight);
			puts("AST || Expr End");
			break;
		}
		case ASTLogicalAnd: {
			puts("AST && Expr Start");
			puts("AST && Expr Left");
			urhay_ast_print(node->BinaryLeft);
			puts("AST && Expr Right");
			urhay_ast_print(node->BinaryRight);
			puts("AST && Expr End");
			break;
		}
		case ASTBitOr: {
			puts("AST | Expr Start");
			puts("AST | Expr Left");
			urhay_ast_print(node->BinaryLeft);
			puts("AST | Expr Right");
			urhay_ast_print(node->BinaryRight);
			puts("AST | Expr End");
			break;
		}
		case ASTBitXor: {
			puts("AST ^ Expr Start");
			puts("AST ^ Expr Left");
			urhay_ast_print(node->BinaryLeft);
			puts("AST ^ Expr Right");
			urhay_ast_print(node->BinaryRight);
			puts("AST ^ Expr End");
			break;
		}
		case ASTBitAnd: {
			puts("AST & Expr Start");
			puts("AST & Expr Left");
			urhay_ast_print(node->BinaryLeft);
			puts("AST & Expr Right");
			urhay_ast_print(node->BinaryRight);
			puts("AST & Expr End");
			break;
		}
		case ASTNotEqual: {
			puts("AST != Expr Start");
			puts("AST != Expr Left");
			urhay_ast_print(node->BinaryLeft);
			puts("AST != Expr Right");
			urhay_ast_print(node->BinaryRight);
			puts("AST != Expr End");
			break;
		}
		case ASTEqual: {
			puts("AST == Expr Start");
			puts("AST == Expr Left");
			urhay_ast_print(node->BinaryLeft);
			puts("AST == Expr Right");
			urhay_ast_print(node->BinaryRight);
			puts("AST == Expr End");
			break;
		}
		case ASTAdd: {
			puts("AST + Expr Start");
			puts("AST + Expr Left");
			urhay_ast_print(node->BinaryLeft);
			puts("AST + Expr Right");
			urhay_ast_print(node->BinaryRight);
			puts("AST + Expr End");
			break;
		}
		case ASTSub: {
			puts("AST - Expr Start");
			puts("AST - Expr Left");
			urhay_ast_print(node->BinaryLeft);
			puts("AST - Expr Right");
			urhay_ast_print(node->BinaryRight);
			puts("AST - Expr End");
			break;
		}
		case ASTMul: {
			puts("AST * Expr Start");
			puts("AST * Expr Left");
			urhay_ast_print(node->BinaryLeft);
			puts("AST * Expr Right");
			urhay_ast_print(node->BinaryRight);
			puts("AST Mul Expr End");
			break;
		}
		case ASTFuncCall: {
			puts("AST Func Call Start");
			puts("AST Func Call Args");
			const union HarbolValue *const end = harbol_vector_get_iter_end_count(node->Args);
			for( const union HarbolValue *iter = harbol_vector_get_iter(node->Args) ; iter && iter<end ; iter++ ) {
				urhay_ast_print(iter->Ptr);
			}
			puts("AST Func Call End");
			break;
		}
		case ASTVar: {
			puts("AST Variable Expr Start");
			printf("AST Variable Name: '%s'\n", node->Iden.CStr);
			puts("AST Variable Expr End");
			break;
		}
		case ASTIntLit: {
			puts("AST Integer Literal Expr Start");
			printf("AST Integer Literal: '%" PRIi64 "'\n", node->I64Lit);
			puts("AST Integer Literal Expr End");
			break;
		}
		case ASTRef: {
			puts("AST &i Expr Start");
			urhay_ast_print(node->Unary);
			puts("AST &i Expr End");
			break;
		}
		case ASTDeref: {
			puts("AST @p Expr Start");
			urhay_ast_print(node->Unary);
			puts("AST @p Expr End");
			break;
		}
		case ASTBitShiftLeft: {
			puts("AST << Expr Start");
			urhay_ast_print(node->BinaryLeft);
			urhay_ast_print(node->BinaryRight);
			puts("AST << Expr End");
			break;
		}
		case ASTBitShiftRight: {
			puts("AST >> Expr Start");
			urhay_ast_print(node->BinaryLeft);
			urhay_ast_print(node->BinaryRight);
			puts("AST >> Expr End");
			break;
		}
		case ASTInvalid:
		default:
			break;
	}
}

int64_t urhay_walk(struct UrhayInterp *const interp, struct UrhayNode *const node, struct HarbolLinkMap *const restrict params, struct HarbolLinkMap *const restrict localvars, const size_t scope, bool *const do_ret)
{
	if( !node )
		return 0;
	//fprintf(g_debug_print, "do_ret valid?: '%s' | do_ret value: '%u'\n", do_ret ? "yes" : "no", do_ret ? *do_ret : 0xff);
	switch( node->NodeTag ) {
		case ASTIntLit: {
			//fprintf(g_debug_print, "ASTIntLit '%" PRIi64 "'\n", node->I64Lit);
			return node->I64Lit;
		}
		case ASTVar: {
			//if( node->Type==TypeFunc )
			struct UrhayVar *const v = harbol_linkmap_has_key(params, node->Iden.CStr)
				? harbol_linkmap_get(params, node->Iden.CStr).Ptr
				: harbol_linkmap_get(localvars, node->Iden.CStr).Ptr;
			if( !v ) {
				printf("Urhay Interpreter Error: **** ASTVar: undefined variable '%s'! ****\n", node->Iden.CStr);
				exit(-1);
			} else if( v->Scope > scope ) {
				printf("Urhay Interpreter Error: **** ASTVar: variable '%s' is out of scope! ****\n", node->Iden.CStr);
				exit(-1);
			}
			//fprintf(g_debug_print, "ASTVar '%s': '%p'\n", node->Iden.CStr, v);
			return v->I64;
		}
		case ASTMul: {
			//fputs("ASTMul\n", g_debug_print);
			return urhay_walk(interp, node->BinaryLeft, params, localvars, scope, do_ret) * urhay_walk(interp, node->BinaryRight, params, localvars, scope, do_ret);
		}
		case ASTSub: {
			//fputs("ASTSub\n", g_debug_print);
			const int64_t val = urhay_walk(interp, node->BinaryRight, params, localvars, scope, do_ret);
			return urhay_walk(interp, node->BinaryLeft, params, localvars, scope, do_ret) - val;
		}
		case ASTAdd: {
			//fputs("ASTAdd\n", g_debug_print);
			return urhay_walk(interp, node->BinaryLeft, params, localvars, scope, do_ret) + urhay_walk(interp, node->BinaryRight, params, localvars, scope, do_ret);
		}
		case ASTBitShiftLeft: {
			//fputs("ASTAdd\n", g_debug_print);
			return urhay_walk(interp, node->BinaryLeft, params, localvars, scope, do_ret) << urhay_walk(interp, node->BinaryRight, params, localvars, scope, do_ret);
		}
		case ASTBitShiftRight: {
			//fputs("ASTAdd\n", g_debug_print);
			return urhay_walk(interp, node->BinaryLeft, params, localvars, scope, do_ret) >> urhay_walk(interp, node->BinaryRight, params, localvars, scope, do_ret);
		}
		case ASTLessThan: {
			//fputs("ASTLessThan\n", g_debug_print);
			return urhay_walk(interp, node->BinaryLeft, params, localvars, scope, do_ret) < urhay_walk(interp, node->BinaryRight, params, localvars, scope, do_ret);
		}
		case ASTGreaterThan: {
			//fputs("ASTGreaterThan\n", g_debug_print);
			return urhay_walk(interp, node->BinaryLeft, params, localvars, scope, do_ret) > urhay_walk(interp, node->BinaryRight, params, localvars, scope, do_ret);
		}
		case ASTLessEq: {
			//fputs("ASTLessEq\n", g_debug_print);
			return urhay_walk(interp, node->BinaryLeft, params, localvars, scope, do_ret) <= urhay_walk(interp, node->BinaryRight, params, localvars, scope, do_ret);
		}
		case ASTGreaterEq: {
			//fputs("ASTGreaterEq\n", g_debug_print);
			return urhay_walk(interp, node->BinaryLeft, params, localvars, scope, do_ret) >= urhay_walk(interp, node->BinaryRight, params, localvars, scope, do_ret);
		}
		case ASTEqual: {
			//fputs("ASTEqual\n", g_debug_print);
			return urhay_walk(interp, node->BinaryLeft, params, localvars, scope, do_ret) == urhay_walk(interp, node->BinaryRight, params, localvars, scope, do_ret);
		}
		case ASTNotEqual: {
			//fputs("ASTNotEqual\n", g_debug_print);
			return urhay_walk(interp, node->BinaryLeft, params, localvars, scope, do_ret) != urhay_walk(interp, node->BinaryRight, params, localvars, scope, do_ret);
		}
		case ASTBitAnd: {
			//fputs("ASTBitAnd\n", g_debug_print);
			return urhay_walk(interp, node->BinaryLeft, params, localvars, scope, do_ret) & urhay_walk(interp, node->BinaryRight, params, localvars, scope, do_ret);
		}
		case ASTBitXor: {
			//fputs("ASTBitXor\n", g_debug_print);
			return urhay_walk(interp, node->BinaryLeft, params, localvars, scope, do_ret) ^ urhay_walk(interp, node->BinaryRight, params, localvars, scope, do_ret);
		}
		case ASTBitOr: {
			//fputs("ASTBitOr\n", g_debug_print);
			return urhay_walk(interp, node->BinaryLeft, params, localvars, scope, do_ret) | urhay_walk(interp, node->BinaryRight, params, localvars, scope, do_ret);
		}
		case ASTLogicalAnd: {
			//fputs("ASTLogicalAnd\n", g_debug_print);
			return urhay_walk(interp, node->BinaryLeft, params, localvars, scope, do_ret) && urhay_walk(interp, node->BinaryRight, params, localvars, scope, do_ret);
		}
		case ASTLogicalOr: {
			//fputs("ASTLogicalOr\n", g_debug_print);
			return urhay_walk(interp, node->BinaryLeft, params, localvars, scope, do_ret) || urhay_walk(interp, node->BinaryRight, params, localvars, scope, do_ret);
		}
		case ASTNot: {
			//fputs("ASTNot\n", g_debug_print);
			return !urhay_walk(interp, node->Unary, params, localvars, scope, do_ret);
		}
		case ASTUnaryPlus: {
			//fputs("ASTUnaryPlus\n", g_debug_print);
			return +urhay_walk(interp, node->Unary, params, localvars, scope, do_ret);
		}
		case ASTUnaryMinus: {
			//fputs("ASTUnaryMinus\n", g_debug_print);
			return -urhay_walk(interp, node->Unary, params, localvars, scope, do_ret);
		}
		case ASTBitNot: {
			//fputs("ASTBitNot\n", g_debug_print);
			return ~urhay_walk(interp, node->Unary, params, localvars, scope, do_ret);
		}
		case ASTTernaryOp: {
			//fputs("ASTTernaryOp\n", g_debug_print);
			return urhay_walk(interp, node->Cond, params, localvars, scope, do_ret)
				? urhay_walk(interp, node->Then, params, localvars, scope, do_ret)
				: urhay_walk(interp, node->Else, params, localvars, scope, do_ret);
		}
		case ASTAssign: {
			//fputs("ASTAssign\n", g_debug_print);
			const int32_t tag = node->BinaryLeft->NodeTag;
			struct UrhayNode *const varnode = node->BinaryLeft;
			
			if( tag==ASTVar ) {
				struct UrhayVar *const v = harbol_linkmap_has_key(params, varnode->Iden.CStr) ? harbol_linkmap_get(params, varnode->Iden.CStr).Ptr : harbol_linkmap_get(localvars, varnode->Iden.CStr).Ptr;
				if( !v ) {
					printf("Urhay Interpreter Error: **** ASTAssign: undefined variable '%s'! ****\n", node->BinaryLeft->Iden.CStr);
					exit(-1);
				} else if( v->Scope > scope ) {
					printf("Urhay Interpreter Error: **** ASTAssign: variable '%s' is out of scope! ****\n", varnode->Iden.CStr);
					exit(-1);
				}
				v->I64 = urhay_walk(interp, node->BinaryRight, params, localvars, scope, do_ret);
				return v->I64;
			} else if( tag==ASTDeref ) {
				const int64_t iptr = urhay_walk(interp, varnode->Unary, params, localvars, scope, do_ret);
				struct UrhayVar *const v = (struct UrhayVar *)(intptr_t)iptr;
				v->I64 = urhay_walk(interp, node->BinaryRight, params, localvars, scope, do_ret);
				return v->I64;
			} else {
				return 0;
			}
		}
		case ASTFuncCall: {
			//fputs("ASTFuncCall\n", g_debug_print);
			if( node->Caller->NodeTag != ASTVar ) {
				puts("Urhay Interpreter Error: **** ASTFuncCall: cannot call a function except by name or reference! ****\n");
				exit(-1);
			}
			struct UrhayNode *func_node = harbol_linkmap_get(&interp->SymTable, node->Caller->Iden.CStr).Ptr;
			if( !func_node ) {
				printf("Urhay Interpreter Error: **** ASTFuncCall: function '%s' doesn't exist! ****\n", node->Caller->Iden.CStr);
				exit(-1);
			}
			func_node->LocalVars = harbol_linkmap_new();
			struct HarbolLinkMap *frame_params = urhay_setup_func_params(interp, func_node->Params, func_node->LocalVars);
			
			size_t args_given = 0;
			for( size_t i=0 ; i<harbol_vector_get_count(node->Args) ; i++ ) {
				struct UrhayVar *const v = harbol_linkmap_get_by_index(frame_params, i).Ptr;
				struct UrhayNode *arg_expr = harbol_vector_get(node->Args, i).Ptr;
				if( !arg_expr )
					continue;
				v->I64 = urhay_walk(interp, arg_expr, params, localvars, scope, do_ret);
				args_given++;
				//fprintf(g_debug_print, "call arg %zu: '0x%" PRIx64 "'\n", i, v->I64);
			}
			if( args_given < harbol_linkmap_get_count(func_node->Params) ) {
				printf("Urhay Interpreter Error: **** ASTFuncCall: Not enough params to call '%s' ****\n", node->Caller->Iden.CStr);
				exit(-1);
			}
			bool ret = false;
			const int64_t val = urhay_walk(interp, func_node->FuncBody, frame_params, func_node->LocalVars, scope+1, &ret);
			harbol_linkmap_free(&frame_params, urhay_var_free);
			harbol_linkmap_free(&func_node->LocalVars, urhay_var_free);
			//fprintf(g_debug_print, "func call return val: %" PRIi64 "\n", val);
			return val;
		}
		case ASTCmpndStmt: {
			//fputs("ASTCmpndStmt\n", g_debug_print);
			const union HarbolValue *const end = harbol_vector_get_iter_end_count(&node->Stmts);
			for( const union HarbolValue *iter = harbol_vector_get_iter(&node->Stmts) ; iter && iter<end ; iter++ ) {
				struct UrhayNode *stmt = iter->Ptr;
				const int64_t val = urhay_walk(interp, stmt, params, localvars, scope+1, do_ret);
				if( do_ret && *do_ret )
					return val;
			}
			return 0;
		}
		case ASTIfStmt: {
			//fputs("ASTIfStmt\n", g_debug_print);
			const bool cond = urhay_walk(interp, node->Cond, params, localvars, scope, do_ret);
			//fprintf(g_debug_print, "\t\t\t\tif cond value: '%s'\n", cond ? "true" : "false");
			if( cond ) {
				return urhay_walk(interp, node->Then, params, localvars, scope, do_ret);
			} else if( node->Else ) {
				return urhay_walk(interp, node->Else, params, localvars, scope, do_ret);
			}
			return 0;
		}
		case ASTForLoop: {
			//fputs("ASTForLoop\n", g_debug_print);
			size_t runaway_protect = 100000000;
			while( urhay_walk(interp, node->ForCond, params, localvars, scope, do_ret) && runaway_protect ) {
				const int64_t val = urhay_walk(interp, node->LoopStmt, params, localvars, scope, do_ret);
				runaway_protect--;
				if( do_ret && *do_ret )
					return val;
			}
			return 0;
		}
		case ASTReturnStmt: {
			//fputs("ASTReturnStmt\n", g_debug_print);
			if( do_ret )
				*do_ret = true;
			const int64_t val = urhay_walk(interp, node->Return, params, localvars, scope, do_ret);
			return val;
		}
		case ASTVarDecl: {
			//fputs("ASTVarDecl\n", g_debug_print);
			const union HarbolValue *const end = harbol_linkmap_get_iter_end_count(node->VarDecls);
			for( const union HarbolValue *iter = harbol_linkmap_get_iter(node->VarDecls) ; iter && iter<end ; iter++ ) {
				struct UrhayNode *const varnode = iter->KvPairPtr->Data.Ptr;
				struct UrhayVar *var = calloc(1, sizeof *var);
				if( harbol_linkmap_has_key(localvars, varnode->Varname.CStr) || harbol_linkmap_has_key(params, varnode->Varname.CStr) ) {
					printf("Urhay Interpreter Error: **** ASTVarDecl: redefinition of variable '%s' ****\n", varnode->Varname.CStr);
					exit(-1);
				}
				harbol_linkmap_insert(localvars, varnode->Varname.CStr, (union HarbolValue){ .Ptr=var });
				var->Scope = scope;
				var->I64 = urhay_walk(interp, varnode->Value, params, localvars, scope, do_ret);
			}
			return 0;
		}
		case ASTRef: {
			//fputs("ASTRef\n", g_debug_print);
			if( node->Unary && node->Unary->NodeTag != ASTVar ) {
				puts("Urhay Interpreter Error: **** ASTRef: cannot give reference from non-var. ****");
				exit(-1);
			}
			struct UrhayNode *varnode = node->Unary;
			struct UrhayVar *const v = harbol_linkmap_has_key(params, varnode->Iden.CStr) ? harbol_linkmap_get(params, varnode->Iden.CStr).Ptr : harbol_linkmap_get(localvars, varnode->Iden.CStr).Ptr;
			if( !v ) {
				printf("Urhay Interpreter Error: **** ASTRef: undefined variable '%s'! ****\n", varnode->Iden.CStr);
				exit(-1);
			}
			//fprintf(g_debug_print, "reference: '%" PRIi64 "'\n", (int64_t)(intptr_t)v);
			return (int64_t)(intptr_t)v;
		}
		case ASTDeref: {
			//fputs("ASTDeref\n", g_debug_print);
			if( node->Unary && !(node->Unary->NodeTag == ASTVar || node->Unary->NodeTag == ASTDeref || node->Unary->NodeTag == ASTRef) ) {
				puts("Urhay Interpreter Error: **** ASTDeref: cannot de-reference non-var. ****");
				exit(-1);
			}
			
			struct UrhayVar *const v = (struct UrhayVar *)(intptr_t)urhay_walk(interp, node->Unary, params, localvars, scope, do_ret);
			if( !v ) {
				puts("Urhay Interpreter Error: **** ASTDeref: NULL ptr exception! ****");
				exit(-1);
			}
			//printf("deref %p\n", v);
			return v->I64;
		}
		case ASTModule: {
			//fputs("ASTModule\n", g_debug_print);
			struct UrhayNode *main_node = harbol_linkmap_get(node->Module, "main").Ptr;
			main_node->LocalVars = harbol_linkmap_new();
			bool ret = false;
			struct HarbolLinkMap *main_params = urhay_setup_func_params(interp, main_node->Params, main_node->LocalVars);
			const int64_t result = urhay_walk(interp, main_node->FuncBody, main_params, main_node->LocalVars, 0, &ret);
			harbol_linkmap_free(&main_params, urhay_var_free);
			harbol_linkmap_free(&main_node->LocalVars, urhay_var_free);
			return result;
		}
		default: return 0;
	}
}

struct HarbolLinkMap *urhay_setup_func_params(struct UrhayInterp *const interp, struct HarbolLinkMap *const params, struct HarbolLinkMap *const localvars)
{
	struct HarbolLinkMap *param_args = harbol_linkmap_new();
	const union HarbolValue *const end = harbol_linkmap_get_iter_end_count(params);
	for( const union HarbolValue *iter = harbol_linkmap_get_iter(params) ; iter && iter<end ; iter++ ) {
		struct UrhayNode *const varnode = iter->KvPairPtr->Data.Ptr;
		struct UrhayVar *var = calloc(1, sizeof *var);
		var->Scope = 0;
		var->I64 = urhay_walk(interp, varnode->Value, params, localvars, 0, NULL);
		harbol_linkmap_insert(param_args, varnode->Varname.CStr, (union HarbolValue){ .Ptr=var });
	}
	return param_args;
}

bool urhay_interpret(struct UrhayInterp *const interp)
{
	if( !interp || !interp->ModuleNode )
		return false;
	/*
	g_debug_print = fopen("urhay_debug_print.txt", "w+");
	if( !g_debug_print )
		return false;
	*/
	const union HarbolValue *const end = harbol_linkmap_get_iter_end_count(interp->ModuleNode->Module);
	for( const union HarbolValue *iter = harbol_linkmap_get_iter(interp->ModuleNode->Module) ; iter && iter<end ; iter++ ) {
		struct UrhayNode *const func = iter->KvPairPtr->Data.Ptr;
		harbol_linkmap_insert(&interp->SymTable, func->FuncName.CStr, (union HarbolValue){ .Ptr=func });
	}
	
	const int64_t result = urhay_walk(interp, interp->ModuleNode, NULL, NULL, 0, NULL);
	printf("final result == %" PRIi64 "\n", result);
	//fclose(g_debug_print), g_debug_print=NULL;
	return (bool)result;
}
