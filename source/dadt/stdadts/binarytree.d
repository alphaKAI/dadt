module dadt.stdadts.binarytree;
import dadt;

private enum BinaryTreeDef = `
type BinaryTree(T) =
| Node of BinaryTree!(T) * BinaryTree!(T)
| Leaf
[@@deriving show, eq, ord]
`;

mixin(genCodeFromSource(BinaryTreeDef));
