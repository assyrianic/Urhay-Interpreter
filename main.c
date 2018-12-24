#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "urhay.h"

char *_code_from_file(const char filename[restrict])
{
	if( !filename )
		return NULL;
	
	FILE *restrict cfgfile = fopen(filename, "r");
	if( !cfgfile ) {
		fputs("Urhay Interpreter :: unable to find file.\n", stderr);
		return NULL;
	}
	fseek(cfgfile, 0, SEEK_END);
	const int64_t filesize = ftell(cfgfile);
	if( filesize <= -1 ) {
		fprintf(stderr, "Urhay Interpreter :: size of file (%" PRIi64 ") is negative!\n", filesize);
		fclose(cfgfile), cfgfile=NULL;
		return NULL;
	}
	rewind(cfgfile);
	
	char *cfgcode = calloc(filesize+1, sizeof *cfgcode);
	if( !cfgcode ) {
		fputs("Urhay Interpreter :: unable to allocate buffer for file.\n", stderr);
		fclose(cfgfile), cfgfile=NULL;
		return NULL;
	}
	const size_t val = fread(cfgcode, sizeof *cfgcode, filesize, cfgfile);
	fclose(cfgfile), cfgfile=NULL;
	if( val != filesize ) {
		fprintf(stderr, "Urhay Interpreter :: filesize (%" PRIi64 ") does not match 'fread' return value! (%zu)\n", filesize, val);
		free(cfgcode), cfgcode=NULL;
		return NULL;
	}
	return cfgcode;
}

int32_t main(const int argc, char *argv[restrict static argc+1])
{
	if( !argv[1] )
		return -1;
	
	struct UrhayInterp interp;
	urhay_lexer_init(&interp, _code_from_file(argv[1]));
	urhay_lexer_preprocess(&interp);
	printf("code:\n\n%s\ncode end\n", interp.Src);
	
	//enum UrhayToken i = 0;
	//while( (i=urhay_lexer_get_token(&interp)) != 0 )
	//	printf("token:\t%i\t|\t'%s'\n", i, interp.Lexeme.CStr);
	urhay_parse_file(&interp);
	const clock_t start = clock();
	urhay_interpret(&interp);
	const double prof_time = (clock()-start)/(double)CLOCKS_PER_SEC;
	printf("profiling time: '%f'\n", prof_time);
}
