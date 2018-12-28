# Urhay Interpreter
a small, educational AST interpreter that implements a dynamically-typed, C-like language.

## Example Code:
```javascript
main()
{
	var i = 5, x = 8;
	func1(ptr_test, &x);
	if i==5 {
		i+=2;
	}
	return i;
}

func1(f, x)
{
	f(x);
}

ptr_test(ref)
{
	@ref = 10;
}

fib(n)
{
	return n<2 ? n : fib(n-1) + fib(n-2);
}
```

## Features:
* Basic Integer & Float Math (division not implemented yet).
* Function Calls (recursive is supported!).
* Pointers and pointer operations for passing variable by reference.
* Function Pointers.
* `if-else` conditional statements.
* `while` loops (implemented as `for`).
* `return` statements.
* Basic arithmetic ops like `+ - *`.
* Basic bitwise ops like `& | ^ << >>`.
* Basic logical ops like `&& ||`
* Basic comparison ops like `!= == < > <= >=`.
* Basic variables which are dynamically typed and can be used with both integer, float, and pointer operations alike.
* Ignores C++ style `//` and C style `/* */` comments.
* ternary operators
* Compound assignments like `+= -= *=`
* Mixed integer and float expressions resolve to float
