# DADT -- Algebraic Data Type for D

## About
Algebraic Data Type For D.  
Parsing ADT declare expression and generate D code in compile time.  

## Example

### Option

```d
import dadt.parse;

enum OptionDefinition = `
type Option(T) =
  | Some of T
  | None`;

mixin(genCodeFromSource(OptionDefinition));
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
enum BinaryTreeDefinition = `
type Tree(T) =
  | Node of Tree!(T) * Tree!(T)
  | Leaf of T
`;

mixin(genCodeFromSource(BinaryTreeDefinition));
```

### Either

```d
enum EitherDefinition = `
type Either(T, U) =
  | Right of T
  | Left of U
`;

mixin(genCodeFromSource(EitherDefinition));
```

### Deriving show
You can use `deriving` keyword.  

```d
enum OptionDefinition = `
type Option(T) =
  | Some of T
  | None
  [@@deriving show]
`;

mixin(genCodeFromSource(OptionDefinition));
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

  Field < FieldOfDelegate / FieldOfFunction / FieldOfArray / FieldWithArgs / FieldName
  FieldArgs < "()" / :"(" Field ("," Field)* :")"
  FieldWithArgs < FieldName "!" FieldArgs
  FieldOfArray < (FieldWithArgs / FieldName) ArrayBracket+
  FieldOfDelegate < "[" Field ("->" Field)+ "]"
  FieldOfFunction < "<" Field ("->" Field)+ ">"
  FieldName <~ !Keyword [a-zA-Z_] [a-zA-Z0-9_]*

  ArrayBracket < UnsizedBracket / SizedBracket

  UnsizedBracket < "[]"
  SizedBracket < "[" ArraySize "]"
  ArraySize <~ [a-zA-Z0-9_]*

  TypeNameWithoutArgs < TypeName
  TypeNameWithArgs < TypeName ParameterList
  ParameterList < "()" / :"(" TypeName ("," TypeName)* :")"

  RecordField < FieldName ":" Field
  RecordFields < (RecordField ";"?)+
  Record < "{" RecordFields "}"

  ConstructorWithRecord< "|"? TypeName "of" Record
  ConstructorWithField < "|"? TypeName "of" Field ("*" Field)*
  Constructor <  "|"? TypeName
  ConstructorDeclare < ConstructorWithRecord / ConstructorWithField / Constructor
  ConstructorList < ConstructorDeclare+

  Deriving < "[@@deriving" DerivingArgs "]"
  DerivingArgs < DerivingArg ("," DerivingArg)*
  DerivingArg <~ !Keyword [a-zA-Z_] [a-zA-Z0-9_]*

  Keyword <~ "of"
  Integer <~ digit+

```

## Advanced Example

You can use pattern matching in D with [DPMATCH](https://github.com/alphaKAI/dpmatch).  

### Option
```d
import std.stdio;
import dpmatch;
import dadt;

mixin(genCodeFromSource(`
type Option(T) =
| Some of T
| None
[@@deriving show, eq]
`));

import std.traits;

Option!U opt_map(F, T = Parameters!F[0], U = ReturnType!F)(Option!T v_opt, F f)
    if (isCallable!F && arity!F == 1) {
  mixin(patternMatchADTReturn!(v_opt, OptionType, q{
    | Some (x) -> <{ return some(f(x)); }>
    | None -> <{ return none!T(); }>
  }));
}

Option!U opt_bind(F, T = Parameters!F[0], RET = ReturnType!F, U = TemplateArgsOf!RET[0])(
    Option!T v_opt, F f) if (isCallable!F && arity!F == 1) {
  mixin(patternMatchADTReturn!(v_opt, OptionType, q{
    | Some (x) -> <{ return f(x); }>
    | None -> <{ return none!U; }>
  }));
}

T opt_default(T)(Option!T v_opt, T _default) {
  mixin(patternMatchADTReturn!(v_opt, OptionType, q{
    | Some (x) -> <{ return x; }>
    | None -> <{ return _default; }>
  }));
}

void opt_may(F, T = Parameters!F[0])(Option!T v_opt, F f)
    if (isCallable!F && arity!F == 1) {
  mixin(patternMatchADTReturn!(v_opt, OptionType, q{
    | Some (x) -> <{ f(x); }>
    | None -> <{ }>
  }));
}

void main() {
  writeln("opt_default(some(10), 0): ", opt_default(some(10), 0));
  writeln("opt_default(none!int, 0): ", opt_default(none!int, 0));

  Option!int v = some(100);
  mixin(patternMatchADTBind!(v, OptionType, q{
    | Some (x) -> <{ return x; }>
    | None -> <{ return 200; }>
  }, "ret"));

  writeln(ret);

  v = none!int();
  mixin(patternMatchADT!(v, OptionType, q{
    | Some (x) -> <{ writeln("Some with ", x); }>
    | None -> <{ writeln("None"); }>
  }));

  int i1 = 10;
  int i2 = 0;
  Option!int opt_ans = (i2 == 0 ? none!int : some(i2)).opt_map((int d) => i1 / d);
  writeln(show_Option(opt_ans)); // None!(int)

  i2 = 2;

  opt_ans = (i2 == 0 ? none!int : some(i2)).opt_map((int d) => i1 / d);
  writeln(show_Option(opt_ans)); // Some!(int)(5)
}
```

## LICENSE
DADT is released under the MIT License.  
Please see `LICENSE` for details.  
Copyright (C) 2018-2019 Akihiro Shoji