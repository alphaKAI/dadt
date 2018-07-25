# DADT -- Algebraic Data Type for D

## About
Algebraic Data Type For D.  
Parsing ADT declare expression and generate D code in compile time.  

## Example

### Option

```d
import dadt.parse;

enum option_code = `
type Option(T) =
  | Some of T
  | None`;

mixin(genCode(cast(TypeDeclare)DADT(option_code).buildAST));
/*
  genCode will generate automatically:
    - interface Option(T) {}
    - class Some(T) : Option!(T) {
        T _0;
        this (T _0) {
          this._0 = _0;
        }
      }
    - class None(T) : Option!(T) {}
    // helper functions
    - Some!(T) some(T)(T _0) {
        return new Some!T(_0);
      }
    - None!(T) none(T)() {
        return new None!(T);
      }
    // pattern matching function
    - _RETURN_TYPE_OF_MATCH_WITH_Option matchWithOption!(
      _RETURN_TYPE_OF_MATCH_WITH_Option,
      T,
      choices...)(Option!(T) arg) {}
*/

// If you define bind(>>= in Haskell) function...

auto bind(T, U)(Option!(T) arg, U function(T) proc) {
	import std.traits;

	static if (is(U == void)) {
		alias R = void;
	} else static if (__traits(isSame, TemplateOf!(U), Option)
			|| __traits(isSame, TemplateOf!(U), Some) || __traits(isSame, TemplateOf!(U), None)) {
		alias R = U;
	} else {
		alias R = Option!U;
	}

	if ((cast(Some!T)arg) !is null) {
		T val = (cast(Some!T)arg)._0;

		static if (is(R == void)) {
			proc(val);
			return;
		}
		static if (is(R == U)) {
			static if (__traits(isSame, TemplateOf!(U), Option) || __traits(isSame,
					TemplateOf!(U), Some) || __traits(isSame, TemplateOf!(U), None)) {
				return proc(val);
			}
		} else {
			return some(proc(val));
		}
	} else {
		static if (is(R == void)) {
			return;
		}
		static if (is(R == U)) {
			return cast(Option!(TemplateArgsOf!U))none!(TemplateArgsOf!U);
		} else {
			static if (is(R == None!U)) {
				return none!U;
			} else {
				throw new Error("<1>Error in bind! <incompatible type was given>");
			}
		}
	}
}

void testForOption() {
  Option!int opt = some(100);

  opt.matchWithOption!(Option!int, int,
      (Some!int _) => (int x) => x % 2 == 0 ? some(x) : none!(int),
      (None!int _) => none!(int))
    .matchWithOption!(void, int,
      (Some!int _) => (int x) => writeln("x is Some of ", x),
      (None!int _) => writeln("x is None"));

  Option!int ret = matchWithOption!(Option!int, int,
      (Some!int _) => (int x) => some(x * x),
      (None!int _) => none!(int))(opt);

  ret.matchWithOption!(void, int,
  		(Some!int _) => (int x) => writeln("x is ", x),
      (None!int _) => writeln("None!"));

	opt.matchWithOption!(Option!int, int,
      (Some!int _) => (int x) => x % 2 == 0 ? some(10) : none!int,
			(None!int _) => _)
     .matchWithOption!(void, int,
         (Some!int _) => writeln("Some"),
         (None!int _) => writeln("None"));

  opt.matchWithOption!(Option!string, int,
      (Some!int _) => (int x) => x % 2 == 0 ? some("x % 2 == 0 !!") : none!string,
			(None!int _) => none!string)
      .bind((string x) => writeln(x));
}
```

### Binary Tree

```d
enum binaryTree_code = `
type Tree(T) =
  | Node of Tree!(T) * Tree!(T)
  | Leaf of T
`;

mixin(genCode(cast(TypeDeclare)DADT(binaryTree_code).buildAST));

string indent(size_t n) {
	string ret;
	foreach (i; 0 .. n) {
		if (i % 2) {
			ret ~= "|   ";
		} else {
			ret ~= "||  ";
		}
	}
	return ret;
}

string toString(T)(Tree!T tree, size_t depth = 0) {
	import std.string;

	string indent_str = indent(depth);
	// dfmt off
	return tree.matchWithTree!(string, T,
			(Node!(int) _) => (Tree!int l, Tree!int r) => `
%s|---<left>%s
%s|
%s|---<right>%s`.format(indent_str, toString!(T)(l, depth + 1),
												indent_str,
												indent_str, toString!(T)(r, depth + 1)),
			(Leaf!(int) _) => (int v) => "%s".format(v));
	// dfmt on
}

void testForBinaryTree() {
	Tree!int ti = node(node(leaf(1), leaf(2)), node(leaf(3), leaf(4)));
	toString!int(ti).writeln;
}
```

### Either

```d
enum codeEither = `
type Either(T, U) =
	| Right of T
	| Left of U
`;

mixin(genCode(cast(TypeDeclare)DADT(codeEither).buildAST));

Option!(T) isEitherRight(T, U)(Either!(T, U) arg) {
	if ((cast(Right!(T, U))arg) !is null) {
		T val = (cast(Right!(T, U))arg)._0;
		return some(val);
	} else {
		return none!T;
	}
}

auto then(T, U)(Option!(T) arg, U function(T) proc) {
	static if (is(U == void)) {
		alias R = void;
	} else {
		alias R = Option!U;
	}

	if ((cast(Some!T)arg) !is null) {
		T val = (cast(Some!T)arg)._0;
		proc(val);
	}
}

void testForEither() {
	Either!(int, string) funcEither(int x) {
		if (x % 2 == 0) {
			return right!(int, string)(x * x);
		} else {
			return left!(int, string)("error");
		}
	}

	Either!(int, string) e1 = funcEither(2), e2 = funcEither(3);

	e1.isEitherRight!(int, string).then((int x) => writeln(x));
}
```

### Deriving show
You can use `deriving` keyword.  

```d
enum option_code = `
type Option(T) =
  | Some of T
  | None
	[@@deriving show]
`;

mixin(genCode(cast(TypeDeclare)DADT(option_code).buildAST));
// genCode will generate...
    - interface Option(T) {}
    - class Some(T) : Option!(T) {
        T _0;
        this (T _0) {
          this._0 = _0;
        }
      }
    - class None(T) : Option!(T) {}
    // helper functions
    - Some!(T) some(T)(T _0) {
        return new Some!T(_0);
      }
    - None!(T) none(T)() {
        return new None!(T);
      }
    // pattern matching function
    - _RETURN_TYPE_OF_MATCH_WITH_Option matchWithOption!(
      _RETURN_TYPE_OF_MATCH_WITH_Option,
      T,
      choices...)(Option!(T) arg) {}
    // Stringify Function!!!
    - string show_Option(T)(Option!(T) arg, string function(Option!(T)) optionalPrinter = null);

void testForOption2() {
  Option!int opt = some(100);
	writeln(opt.show_Option);
}
```

## Syntax
DADT using [pegged](https://github.com/PhilippeSigaud/Pegged), and the below PEG is written in extend PEG grammar of pegged.(copied from `source/dadt/parse.d`)  

```
DADT:
  TypeDeclare < "type" BaseConstructor "=" ConstructorList Deriving?

  BaseConstructor < TypeNameWithArgs / TypeNameWithoutArgs

  TypeName <~ !Keyword [A-Z_] [a-zA-Z0-9_]*
  
  Field < FieldOfArray / FieldWithArgs / FieldName
  FieldArgs < "()" / :"(" Field ("," Field)* :")"
  FieldWithArgs < FieldName "!" FieldArgs
  FieldOfArray < (FieldWithArgs / FieldName) ArrayBracket+
  FieldName <~ !Keyword [a-zA-Z_] [a-zA-Z0-9_]*

  ArrayBracket < UnsizedBracket / SizedBracket

  UnsizedBracket < "[]"
  SizedBracket < "[" ArraySize "]"
  ArraySize <~ [a-zA-Z0-9_]*

  TypeNameWithoutArgs < TypeName
  TypeNameWithArgs < TypeName ParameterList
  ParameterList < "()" / :"(" TypeName ("," TypeName)* :")"

  ConstructorWithField < "|" TypeName "of" Field ("*" Field)*
  Constructor <  "|" TypeName
  ConstructorDeclare < ConstructorWithField / Constructor
  ConstructorList < ConstructorDeclare+

  Deriving < "[@@deriving" DerivingArgs "]"
  DerivingArgs < DerivingArg ("," DerivingArg)* 
  DerivingArg <~ !Keyword [a-zA-Z_] [a-zA-Z0-9_]*

  Keyword <~ "of"
  Integer <~ digit+
```

## LICENSE
DADT is released under the MIT License.  
Please see `LICENSE` for details.  
Copyright (C) 2018 Akihiro Shoji