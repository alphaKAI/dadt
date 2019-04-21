module dadt.stdadts.option;
import dadt.parser;

private enum OptionDef = `
type Option(T) =
| Some of T
| None
[@@deriving show, eq, ord]
`;

mixin(genCodeFromSource(OptionDef));
