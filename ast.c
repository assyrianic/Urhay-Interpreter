#include <stdio.h>
#include <stdlib.h>

#include "urhay.h"


int64_t urhay_walk(struct UrhayInterp *, struct UrhayNode *, struct HarbolLinkMap *, struct HarbolLinkMap *, size_t, bool *);
//struct UrhayType *urhay_walk(struct UrhayInterp *, struct UrhayNode *, struct HarbolLinkMap *, struct HarbolLinkMap *, size_t, bool *);
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
			puts("AST Logical Or Expr Start");
			puts("AST Logical Or Expr Left");
			urhay_ast_print(node->BinaryLeft);
			puts("AST Logical Or Expr Right");
			urhay_ast_print(node->BinaryRight);
			puts("AST Logical Or Expr End");
			break;
		}
		case ASTLogicalAnd: {
			puts("AST Logical And Expr Start");
			puts("AST Logical And Expr Left");
			urhay_ast_print(node->BinaryLeft);
			puts("AST Logical And Expr Right");
			urhay_ast_print(node->BinaryRight);
			puts("AST Logical And Expr End");
			break;
		}
		case ASTBitOr: {
			puts("AST Bitwise Or Expr Start");
			puts("AST Bitwise Or Expr Left");
			urhay_ast_print(node->BinaryLeft);
			puts("AST Bitwise Or Expr Right");
			urhay_ast_print(node->BinaryRight);
			puts("AST Bitwise Or Expr End");
			break;
		}
		case ASTBitXor: {
			puts("AST Bitwise Xor Expr Start");
			puts("AST Bitwise Xor Expr Left");
			urhay_ast_print(node->BinaryLeft);
			puts("AST Bitwise Xor Expr Right");
			urhay_ast_print(node->BinaryRight);
			puts("AST Bitwise Xor Expr End");
			break;
		}
		case ASTBitAnd: {
			puts("AST Bitwise And Expr Start");
			puts("AST Bitwise And Expr Left");
			urhay_ast_print(node->BinaryLeft);
			puts("AST Bitwise And Expr Right");
			urhay_ast_print(node->BinaryRight);
			puts("AST Bitwise And Expr End");
			break;
		}
		case ASTNotEqual: {
			puts("AST UnEquality Expr Start");
			puts("AST UnEquality Expr Left");
			urhay_ast_print(node->BinaryLeft);
			puts("AST UnEquality Expr Right");
			urhay_ast_print(node->BinaryRight);
			puts("AST UnEquality Expr End");
			break;
		}
		case ASTEqual: {
			puts("AST Equality Expr Start");
			puts("AST Equality Expr Left");
			urhay_ast_print(node->BinaryLeft);
			puts("AST Equality Expr Right");
			urhay_ast_print(node->BinaryRight);
			puts("AST Equality Expr End");
			break;
		}
		case ASTAdd: {
			puts("AST Add Expr Start");
			puts("AST Add Expr Left");
			urhay_ast_print(node->BinaryLeft);
			puts("AST Add Expr Right");
			urhay_ast_print(node->BinaryRight);
			puts("AST Add Expr End");
			break;
		}
		case ASTSub: {
			puts("AST Sub Expr Start");
			puts("AST Sub Expr Left");
			urhay_ast_print(node->BinaryLeft);
			puts("AST Sub Expr Right");
			urhay_ast_print(node->BinaryRight);
			puts("AST Sub Expr End");
			break;
		}
		case ASTMul: {
			puts("AST Mul Expr Start");
			puts("AST Mul Expr Left");
			urhay_ast_print(node->BinaryLeft);
			puts("AST Mul Expr Right");
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
			puts("AST Reference Expr Start");
			urhay_ast_print(node->PtrOp);
			puts("AST Reference Expr End");
			break;
		}
		case ASTDeref: {
			puts("AST Deref Expr Start");
			urhay_ast_print(node->PtrOp);
			puts("AST Deref Expr End");
			break;
		}
		case ASTInvalid:
		default:
			break;
	}
}

int64_t urhay_walk(struct UrhayInterp *const interp, struct UrhayNode *const node, struct HarbolLinkMap *const params, struct HarbolLinkMap *const localvars, const size_t scope, bool *const do_ret)
{
	if( !node )
		return 0;
	//fprintf(g_debug_print, "do_ret valid?: '%s' | do_ret value: '%u'\n", do_ret ? "yes" : "no", do_ret ? *do_ret : 0xff);
	switch( node->NodeTag ) {
		case ASTIntLit:
			//fprintf(g_debug_print, "ASTIntLit '%" PRIi64 "'\n", node->I64Lit);
			return node->I64Lit;
		case ASTVar: {
			struct UrhayVar *const v = harbol_linkmap_has_key(params, node->Iden.CStr) ? harbol_linkmap_get(params, node->Iden.CStr).Ptr : harbol_linkmap_get(localvars, node->Iden.CStr).Ptr;
			if( !v ) {
				printf("Urhay Interpreter Error: **** ASTVar: undefined variable '%s'! ****\n", node->Iden.CStr);
				exit(-1);
			} else if( v->Scope > scope ) {
				printf("Urhay Interpreter Error: **** ASTVar: variable '%s' is out of scope! ****\n", node->Iden.CStr);
				exit(-1);
			}
			//fprintf(g_debug_print, "ASTVar '%s': '%" PRIi64 "'\n", node->Iden.CStr, v->I64);
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
		case ASTLessThan: {
			//fputs("ASTLessThan\n", g_debug_print);
			return urhay_walk(interp, node->BinaryLeft, params, localvars, scope, do_ret) < urhay_walk(interp, node->BinaryRight, params, localvars, scope, do_ret);
		}
		case ASTGreaterThan: {
			//fputs("ASTGreaterThan\n", g_debug_print);
			return urhay_walk(interp, node->BinaryLeft, params, localvars, scope, do_ret) > urhay_walk(interp, node->BinaryRight, params, localvars, scope, do_ret);
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
				struct UrhayVar *const v = (harbol_linkmap_has_key(params, varnode->Iden.CStr) ? harbol_linkmap_get(params, varnode->Iden.CStr).Ptr : harbol_linkmap_get(localvars, varnode->Iden.CStr).Ptr);
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
				const int64_t iptr = urhay_walk(interp, varnode->PtrOp, params, localvars, scope, do_ret);
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
				puts("Urhay Interpreter Error: **** ASTFuncCall: cannot call a function except by name! ****\n");
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
				//fprintf(g_debug_print, "call arg %zu: '%" PRIi64 "'\n", i, v->I64);
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
			if( node->PtrOp && node->PtrOp->NodeTag != ASTVar ) {
				puts("Urhay Interpreter Error: **** ASTRef: cannot give reference from non-var. ****");
				exit(-1);
			}
			struct UrhayNode *varnode = node->PtrOp;
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
			if( node->PtrOp && !(node->PtrOp->NodeTag == ASTVar || node->PtrOp->NodeTag == ASTDeref) ) {
				puts("Urhay Interpreter Error: **** ASTDeref: cannot de-reference non-var. ****");
				exit(-1);
			}
			const int64_t iptr = urhay_walk(interp, node->PtrOp, params, localvars, scope, do_ret);
			struct UrhayVar *const v = (struct UrhayVar *)(intptr_t)iptr;
			
			if( !v ) {
				puts("Urhay Interpreter Error: **** ASTDeref: NULL ptr exception! ****");
				exit(-1);
			}
			//printf("deref %p\n", v);
			return v->I64;
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

bool urhay_interp(struct UrhayInterp *const interp)
{
	if( !interp )
		return false;
	
	bool ret = false;
	struct UrhayNode *node = harbol_linkmap_get(&interp->SymTable, "main").Ptr;
	g_debug_print = fopen("urhay_debug_print.txt", "w+");
	if( !g_debug_print )
		return false;
	
	node->LocalVars = harbol_linkmap_new();
	struct HarbolLinkMap *main_params = urhay_setup_func_params(interp, node->Params, node->LocalVars);
	const int64_t result = urhay_walk(interp, node->FuncBody, main_params, node->LocalVars, 0, &ret);
	harbol_linkmap_free(&main_params, urhay_var_free);
	harbol_linkmap_free(&node->LocalVars, urhay_var_free);
	printf("final result == %" PRIi64 "\n", result);
	fclose(g_debug_print), g_debug_print=NULL;
	return (bool)result;
}
