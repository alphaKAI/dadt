module dadt.stdadts.either;
import dadt.parser;

private enum EitherDef = `
type Either(T, U) =
| Right of T
| Left of U
[@@deriving show, eq, ord]
`;

mixin(genCodeFromSource(EitherDef));
