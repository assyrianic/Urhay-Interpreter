#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#include "urhay.h"


static inline bool is_space(const rune c)
{
	return( c == ' ' || c == '\t' || c == '\r' || c == '\v' || c == '\f' );
}

static inline bool is_decimal(const rune c)
{
	return( c >= '0' && c <= '9' );
}

static inline bool is_hex(const rune c)
{
	return( (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') || is_decimal(c) );
}

static inline bool is_binary(const rune c)
{
	return( c=='0' || c=='1' );
}

static inline bool is_octal(const rune c)
{
	return( c >= '0' && c <= '7' );
}

static inline bool is_possible_ident(const rune c)
{
	return( (c >= 'a' && c <= 'z')
		|| (c >= 'A' && c <= 'Z')
		|| c == '_'
		|| (c >= '0' && c <= '9')
		|| c < -1 );
}

static inline bool is_alphabetic(const rune c)
{
	return( (c >= 'a' && c <= 'z')
		|| (c >= 'A' && c <= 'Z')
		|| c == '_'
		|| c < -1 );
}

static inline bool is_syriac(const rune c)
{
	return( c>=0x0710 || c<=0x072F );
}



void urhay_lexer_init(struct UrhayInterp *const restrict lexer, char *code)
{
	if( !lexer )
		return;
	
	memset(lexer, 0, sizeof *lexer);
	lexer->Src = lexer->Iter = code;
	lexer->Line++;
}

void urhay_lexer_preprocess(struct UrhayInterp *const lexer)
{
	if( !lexer || !lexer->Src )
		return;
	
	char *restrict iter = lexer->Src;
	while( *iter ) {
		if( *iter=='/' && iter[1]=='/' ) {
			while( *iter != '\n')
				*iter++ = ' ';
		} else if( *iter=='/' && iter[1]=='*' ) {
			*iter++ = ' '; *iter++ = ' ';
			while( !(*iter=='*' && iter[1]=='/') ) {
				if( !*iter || !iter[1] )
					break;
				else if( *iter=='\n' ) {
					iter++;
					continue;
				}
				*iter++ = ' ';
			}
			if( *iter=='*' && iter[1]=='/' ) {
				*iter++ = ' '; *iter++ = ' ';
			}
			continue;
		}
		iter++;
	}
}

enum UrhayToken urhay_lexer_get_token(struct UrhayInterp *const lexer)
{
	if( !lexer || !lexer->Src )
		return TokenInvalid;
	
	struct HarbolString *lexeme = &lexer->Lexeme;
	harbol_string_del(lexeme);
	
	while( *lexer->Iter ) {
		if( is_space(*lexer->Iter) ) {
		}
		else if( *lexer->Iter=='\n' ) {
			lexer->Line++;
		}
		/* lex number */
		else if( is_decimal(*lexer->Iter) ) {
			/* possible octal, hex, binary, or float? */
			if( *lexer->Iter=='0' ) {
				harbol_string_add_char(lexeme, *lexer->Iter++);
				/* lex hexadecimal number */
				if( *lexer->Iter=='x' || *lexer->Iter=='X' ) {
					rune c = *lexer->Iter++;
					harbol_string_add_char(lexeme, c);
					/* TODO: error recover bad hex */
					while( *lexer->Iter && isalnum(*lexer->Iter) ) {
						const rune x = *lexer->Iter++;
						if( !is_hex(x) ) {
							// throw error here.
						}
						harbol_string_add_char(lexeme, x);
					}
					lexer->CurrToken = TokenIntLiteral;
					return TokenIntLiteral;
				} /*else if( *lexer->Iter=='b' || *lexer->Iter=='B' ) {
					rune c = *lexer->Iter++;
					harbol_string_add_char(lexeme, c);
					// TODO: error recover bad binary
					while( *lexer->Iter && isalnum(*lexer->Iter) ) {
						const rune x = *lexer->Iter++;
						if( !is_binary(x) ) {
							// throw error here.
						}
						harbol_string_add_char(lexeme, x);
					}
					lexer->CurrToken = TokenIntLiteral;
					return TokenIntLiteral;
				}*/ else if( *lexer->Iter=='.' ) { // found a float!
					harbol_string_add_char(lexeme, *lexer->Iter++);
					// TODO: error recover bad floating point
					while( *lexer->Iter && isalnum(*lexer->Iter) ) {
						const rune x = *lexer->Iter++;
						if( !is_decimal(x) && (x != 'e' || x != 'E') ) {
							// throw error here.
						}
						harbol_string_add_char(lexeme, x);
					}
					lexer->CurrToken = TokenFloatLiteral;
					return TokenFloatLiteral;
				} else {
					/* TODO: error recover bad octal */
					while( *lexer->Iter && isalnum(*lexer->Iter) ) {
						const rune x = *lexer->Iter++;
						if( !is_octal(x) ) {
							// throw error here.
						}
						harbol_string_add_char(lexeme, x);
					}
					lexer->CurrToken = TokenIntLiteral;
					return TokenIntLiteral;
				}
			} else { /* is regular decimal or float. */
				bool is_float = false;
				harbol_string_add_char(lexeme, *lexer->Iter++);
				while( *lexer->Iter && (isalnum(*lexer->Iter) || *lexer->Iter=='.') ) {
					const rune x = *lexer->Iter++;
					if( x=='.' ) {
						harbol_string_add_char(lexeme, x);
						is_float=true;
						continue;
					} else if( !is_decimal(x) ) {
						if( is_float && (x=='e' || x=='E') ) {
							harbol_string_add_char(lexeme, x);
							continue;
						} else {
							// throw error here.
						}
					}
					harbol_string_add_char(lexeme, x);
				}
				lexer->CurrToken = is_float ? TokenFloatLiteral : TokenIntLiteral;
				return is_float ? TokenFloatLiteral : TokenIntLiteral;
			}
		}
		else if( *lexer->Iter=='.' && is_decimal(lexer->Iter[1]) ) {
			harbol_string_add_char(lexeme, *lexer->Iter++);
			while( *lexer->Iter && isalnum(*lexer->Iter) ) {
				const rune x = *lexer->Iter++;
				if( !is_decimal(x) ) {
					if( x=='e' || x=='E' ) {
						harbol_string_add_char(lexeme, x);
						continue;
					} else {
						// throw error here.
					}
				}
				harbol_string_add_char(lexeme, x);
			}
			lexer->CurrToken = TokenFloatLiteral;
			return TokenFloatLiteral;
		}
		/* lex words! */
		else if( is_alphabetic(*lexer->Iter) ) {
			while( is_possible_ident(*lexer->Iter) )
				harbol_string_add_char(lexeme, *lexer->Iter++);
			
			if( !harbol_string_cmpcstr(lexeme, "if") ) {
				lexer->CurrToken = TokenIf;
				return TokenIf;
			} else if( !harbol_string_cmpcstr(lexeme, "else") ) {
				lexer->CurrToken = TokenElse;
				return TokenElse;
			} else if( !harbol_string_cmpcstr(lexeme, "for") ) {
				lexer->CurrToken = TokenFor;
				return TokenFor;
			} else if( !harbol_string_cmpcstr(lexeme, "return") ) {
				lexer->CurrToken = TokenReturn;
				return TokenReturn;
			} else if( !harbol_string_cmpcstr(lexeme, "var") ) {
				lexer->CurrToken = TokenVar;
				return TokenVar;
			} else if( !harbol_string_cmpcstr(lexeme, "null") ) {
				lexer->CurrToken = TokenNull;
				return TokenNull;
			} else {
				lexer->CurrToken = TokenIdentifier;
				return TokenIdentifier;
			}
		}
		/* lex individual tokens! */
		else {
			switch( *lexer->Iter++ ) {
				case '(': {
					harbol_string_copy_cstr(lexeme, "(");
					lexer->CurrToken = TokenLeftParen;
					return TokenLeftParen;
				}
				case ')': {
					harbol_string_copy_cstr(lexeme, ")");
					lexer->CurrToken = TokenRightParen;
					return TokenRightParen;
				}
				case '[': {
					harbol_string_copy_cstr(lexeme, "[");
					lexer->CurrToken = TokenLeftSqBracket;
					return TokenLeftSqBracket;
				}
				case ']': {
					harbol_string_copy_cstr(lexeme, "]");
					lexer->CurrToken = TokenRightSqBracket;
					return TokenRightSqBracket;
				}
				case '{': {
					harbol_string_copy_cstr(lexeme, "{");
					lexer->CurrToken = TokenLeftCurlyBracket;
					return TokenLeftCurlyBracket;
				}
				case '}': {
					harbol_string_copy_cstr(lexeme, "}");
					lexer->CurrToken = TokenRightCurlyBracket;
					return TokenRightCurlyBracket;
				}
				case ',': {
					harbol_string_copy_cstr(lexeme, ",");
					lexer->CurrToken = TokenComma;
					return TokenComma;
				}
				case '?': {
					harbol_string_copy_cstr(lexeme, "?");
					lexer->CurrToken = TokenQuestion;
					return TokenQuestion;
				}
				case ':': {
					harbol_string_copy_cstr(lexeme, ":");
					lexer->CurrToken = TokenColon;
					return TokenColon;
				}
				case ';': {
					harbol_string_copy_cstr(lexeme, ";");
					lexer->CurrToken = TokenSemicolon;
					return TokenSemicolon;
				}
				case '+': {
					if( *lexer->Iter=='=' ) {
						lexer->Iter++;
						harbol_string_copy_cstr(lexeme, "+=");
						lexer->CurrToken = TokenAddAssign;
						return TokenAddAssign;
					} /*else if( *lexer->Iter=='+' ) {
						lexer->Iter++;
						harbol_string_copy_cstr(lexeme, "++");
						lexer->CurrToken = TokenIncr;
						return TokenIncr;
					}*/ else {
						harbol_string_copy_cstr(lexeme, "+");
						lexer->CurrToken = TokenAdd;
						return TokenAdd;
					}
				}
				case '-': {
					if( *lexer->Iter=='=' ) {
						lexer->Iter++;
						harbol_string_copy_cstr(lexeme, "-=");
						lexer->CurrToken = TokenSubAssign;
						return TokenSubAssign;
					} /*else if( *lexer->Iter=='-' ) {
						lexer->Iter++;
						harbol_string_copy_cstr(lexeme, "--");
						lexer->CurrToken = TokenDecr;
						return TokenDecr;
					}*/ else {
						harbol_string_copy_cstr(lexeme, "-");
						lexer->CurrToken = TokenSub;
						return TokenSub;
					}
				}
				case '*': {
					if( *lexer->Iter=='=' ) {
						harbol_string_copy_cstr(lexeme, "*=");
						lexer->Iter++;
						lexer->CurrToken = TokenMulAssign;
						return TokenMulAssign;
					} else {
						harbol_string_copy_cstr(lexeme, "*");
						lexer->CurrToken = TokenStar;
						return TokenStar;
					}
				}
				case '/': {
					harbol_string_copy_cstr(lexeme, "/");
					lexer->CurrToken = TokenSlash;
					return TokenSlash;
				}
				case '=': {
					if( *lexer->Iter=='=' ) {
						harbol_string_copy_cstr(lexeme, "==");
						lexer->Iter++;
						lexer->CurrToken = TokenEqual;
						return TokenEqual;
					} else {
						harbol_string_copy_cstr(lexeme, "=");
						lexer->CurrToken = TokenAssign;
						return TokenAssign;
					}
				}
				case '!': {
					if( *lexer->Iter=='=' ) {
						harbol_string_copy_cstr(lexeme, "!=");
						lexer->Iter++;
						lexer->CurrToken = TokenNotEqual;
						return TokenNotEqual;
					} else {
						harbol_string_copy_cstr(lexeme, "!");
						lexer->CurrToken = TokenUnaryNot;
						return TokenUnaryNot;
					}
				}
				case '&': {
					if( *lexer->Iter=='&' ) {
						harbol_string_copy_cstr(lexeme, "&&");
						lexer->Iter++;
						lexer->CurrToken = TokenLogicAnd;
						return TokenLogicAnd;
					} else {
						harbol_string_copy_cstr(lexeme, "&");
						lexer->CurrToken = TokenAmpersand;
						return TokenAmpersand;
					}
				}
				case '|': {
					if( *lexer->Iter=='|' ) {
						harbol_string_copy_cstr(lexeme, "||");
						lexer->Iter++;
						lexer->CurrToken = TokenLogicOr;
						return TokenLogicOr;
					} else {
						harbol_string_copy_cstr(lexeme, "|");
						lexer->CurrToken = TokenVertBar;
						return TokenVertBar;
					}
				}
				case '^': {
					harbol_string_copy_cstr(lexeme, "^");
					lexer->CurrToken = TokenCarot;
					return TokenCarot;
				}
				case '<': {
					if( *lexer->Iter=='=' ) {
						harbol_string_copy_cstr(lexeme, "<=");
						lexer->Iter++;
						lexer->CurrToken = TokenLessEq;
						return TokenLessEq;
					} else if( *lexer->Iter=='<' ) {
						harbol_string_copy_cstr(lexeme, "<<");
						lexer->Iter++;
						lexer->CurrToken = TokenLeftSh;
						return TokenLeftSh;
					} else {
						harbol_string_copy_cstr(lexeme, "<");
						lexer->CurrToken = TokenLess;
						return TokenLess;
					}
				}
				case '>': {
					if( *lexer->Iter=='=' ) {
						harbol_string_copy_cstr(lexeme, ">=");
						lexer->Iter++;
						lexer->CurrToken = TokenGreaterEq;
						return TokenGreaterEq;
					} else if( *lexer->Iter=='>' ) {
						harbol_string_copy_cstr(lexeme, ">>");
						lexer->Iter++;
						lexer->CurrToken = TokenRightSh;
						return TokenRightSh;
					} else {
						harbol_string_copy_cstr(lexeme, ">");
						lexer->CurrToken = TokenGreater;
						return TokenGreater;
					}
				}
				case '@': {
					harbol_string_copy_cstr(lexeme, "@");
					lexer->CurrToken = TokenAt;
					return TokenAt;
				}
				case '~': {
					harbol_string_copy_cstr(lexeme, "~");
					lexer->CurrToken = TokenCompl;
					return TokenCompl;
				}
			}
		}
		lexer->Iter++;
	}
	lexer->CurrToken = TokenInvalid;
	return TokenInvalid;
}
