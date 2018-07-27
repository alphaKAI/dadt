module dadt.example;
import std.stdio;
import dadt.parser;

enum code = `
type Option(T) =
  | Some of T
  | None
  [@@deriving show, ord];
`;

mixin(genCode(cast(TypeDeclare)DADT(code).buildAST));

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

  // dfmt off
  opt.matchWithOption!(Option!int, int,
      (Some!int _) => (int x) => x % 2 == 0 ? some(x) : none!(int),
      (None!int _) => none!(int))
    .bind((int x) => some("x % 2 == 0!!"))
    .matchWithOption!(void, string,
      (Some!string _) => (string x) => writeln(x));

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
  ret.show_Option.writeln;
  //mixin(genCode(cast(TypeDeclare)DADT(code).buildAST));
  // dfmt on
  Option!int a = some(1), b = some(2), c = some(3), d = none!int;
  assert(compare_Option(a, a) == 0);
  assert(compare_Option(a, b) == -1);
  assert(compare_Option(c, b) == 1);
  assert(compare_Option(a, d) == -1);
  assert(compare_Option(d, d) == 0);
}

enum code1 = `
type Tree(T) =
  | Node of Tree!(T) * Tree!(T)
  | Leaf of T
`;

mixin(genCode(cast(TypeDeclare)DADT(code1).buildAST));

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

void main() {
  testForOption;
  testForBinaryTree;
  testForEither;

  test;
}

void test() {
  enum code = `
type Option(T) =
  | Some of T
  | None
  [@@deriving show, ord];
`;
  writeln(code);
  writeln("compile to ↓");
  TypeDeclare td = cast(TypeDeclare)DADT(code).buildAST;
  //(genCode(td)).writeln;
}
