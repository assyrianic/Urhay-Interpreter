# Urhay Interpreter
a small, educational AST interpreter that implements a C-like language.

## Example Code:
```javascript
main()
{
	return fib(34);
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
* Basic arithmetic ops like `+ - *`.
* Basic bitwise ops like `& | ^`.
* Basic logical ops like `&& ||`
* Basic comparison ops like `!= == < >`.
* Basic variables which are untyped and can be used with both integer and pointer operations alike.
* Ignores C++ style `//` and C style `/*  */` comments.
