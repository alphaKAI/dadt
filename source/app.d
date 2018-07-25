import std.stdio;
import dadt.parse;

enum code = `
type Option(T) =
  | Some of T
  | None
`;

mixin(genCode(cast(TypeDeclare)DADT(code).buildAST));

Option!R bind(T, R)(Option!T arg, Option!(R) function(T) proc) {
	if ((cast(Some!T)arg) !is null) {
		T val = (cast(Some!T)arg)._0;
		return proc(val);
	}
	else {
		return none!(R);
	}
}

void testForOption() {
	Option!int opt = some(100);

	// dfmt off
  opt.matchWithOption!(Option!int, int,
      (Some!int _) => (int x) => cast(Option!int)(x % 2 == 0 ? some(x) : none!(int)),
      (None!int _) => none!(int))
    .bind!(int, string)((int x) => some("x % 2 == 0!!"))
    .matchWithOption!(void, string,
      (Some!string _) => (string x) => writeln(x));

  Option!int ret = matchWithOption!(Option!int, int,
      (Some!int _) => (int x) => some(x * x),
      (None!int _) => none!(int))(opt);

  ret.matchWithOption!(void, int,
  (Some!int _) => (int x) => writeln("x is ", x),
      (None!int _) => writeln("None!"));

  opt.matchWithOption!(Option!int, int
      (Some!int _) => (int x) => some(x * x),
      (None!int _) => none!(int))
    .bind!(int, int)((int x) { writeln("x : ", x); return some(x); });

  //mixin(genCode(cast(TypeDeclare)DADT(code).buildAST));
  // dfmt on
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
		}
		else {
			ret ~= "||  ";
		}
	}
	return ret;
}

string toString(T)(Tree!T tree, size_t depth = 0) {
	import std.string;

	string indent_str = indent(depth);
	return tree.matchWithTree!(string, T, (Node!(int) _) => (Tree!int l,
			Tree!int r) => `
%s|---<left>%s
%s|
%s|---<right>%s`.format(indent_str, toString!(T)(l,
			depth + 1), indent_str, indent_str, toString!(T)(r, depth + 1)),
			(Leaf!(int) _) => (int v) => "%s".format(v));
}

void testForBinaryTree() {
	Tree!int ti = node(node(leaf(1), leaf(2)), node(leaf(3), leaf(4)));
	toString!int(ti).writeln;
}

void main() {
	testForOption;
	testForBinaryTree;
}

void test() {
	enum code = `
type Option(T) =
  | Some of T
  | None
`;
	writeln(code);
	writeln("compile to ↓");
	TypeDeclare td = cast(TypeDeclare)DADT(code).buildAST;
	(genCode(td)).writeln;

}
