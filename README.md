# Urhay Interpreter
a small, educational AST interpreter that implements a C-like language.

## Example Code:
```javascript
fib();
func3();

main1()
{
	return fib(34);
}

fib(n)
{
	if(n<2) {
		return n;
	} else {
		return fib(n-1) + fib(n-2);
	}
}

func3(x, y, z)
{
	return x * y + z;
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
