import std.stdio;
import std.traits;
import std.algorithm;

void main() {
	/*
	Option!int opt = some(100);

	Option!int ret = matchWithOption!(Option!int, int,
			(Some!int _) => (int x) => some(x * x), (None!int _) => none!(int))(opt);

	ret.matchWithOption!(void, int, (Some!int _) => (int x) => writeln("x is ", x),
			(None!int _) => writeln("None!"));
*/
	import parse;

	enum code = `
type Option(T) =
	| Some of int[N]
	| None
`;

	writeln(code);
	writeln("compile to ↓");
	TypeDeclare td = cast(TypeDeclare)DADT(code).buildAST;
	genCode(td).writeln;
}
