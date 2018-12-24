# Urhay Interpreter
a small, educational AST interpreter that implements a C-like language.

## Example Code:
```javascript
main()
{
	var i = 5, x = 6;
	ptr_test(&x);
	if( x<10 )
		i *= x;
	else if( x>10 && x<50 )
		i += x;
	return fib(34);
}

ptr_test(ref)
{
	@ref += 2;
	@ref = @ref << 1;
}

fib(n)
{
	return (n<2) ? n : fib(n-1) + fib(n-2);
}
```

## Features:
* Basic Integer Math (division not implemented yet).
* Function Calls (recursive is supported!).
* Pointers and pointer operations for passing variable by reference.
* `if-else` conditional statements.
* `while` loops (implemented as `for`).
* `return` statements.
* Basic arithmetic ops like `+ - *`.
* Basic bitwise ops like `& | ^ << >>`.
* Basic logical ops like `&& ||`
* Basic comparison ops like `!= == < > <= >=`.
* Basic variables which are untyped and can be used with both integer and pointer operations alike.
* Ignores C++ style `//` and C style `/*  */` comments.
* ternary operators
* Compound assignments like `+= -= *=`
