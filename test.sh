#!/bin/bash
cd "$(dirname "$0")"

# -fsanitize=address -fsanitize=undefined -fstrict-aliasing
# -funroll-loops -finline-functions -ffast-math -fexpensive-optimizations


gcc -Wextra -Wall -std=c99 -g -O0 libharbol/stringobj.c libharbol/vector.c libharbol/hashmap.c libharbol/mempool.c libharbol/linkmap.c lexer.c ast.c parser.c main.c -o urhay_interp

./urhay_interp 'code.u'
