module source.dadt.stdadts.binarytree;
enum BinaryTreeType {
  Node,
  Leaf
}

interface BinaryTree(T) {
  BinaryTreeType type();
}

class Node(T) : BinaryTree!(T) {
  BinaryTree!(T) _0;
  BinaryTree!(T) _1;

  this(BinaryTree!(T) _0, BinaryTree!(T) _1) {
    this._0 = _0;
    this._1 = _1;
  }

  BinaryTreeType type() {
    return BinaryTreeType.Node;
  }

}

Node!(T) node(T)(BinaryTree!(T) _0, BinaryTree!(T) _1) {
  return new Node!(T)(_0, _1);
}

class Leaf(T) : BinaryTree!(T) {
  T _0;

  this(T _0) {
    this._0 = _0;
  }

  BinaryTreeType type() {
    return BinaryTreeType.Leaf;
  }

}

Leaf!(T) leaf(T)(T _0) {
  return new Leaf!(T)(_0);
}

_RETURN_TYPE_OF_MATCH_WITH_BinaryTree matchWithBinaryTree(
    _RETURN_TYPE_OF_MATCH_WITH_BinaryTree, T, choices...)(BinaryTree!(T) arg) {
  import std.traits;

  _RETURN_TYPE_OF_MATCH_WITH_BinaryTree delegate() otherwise = null;

  foreach (choice; choices) {
    alias params = Parameters!choice;
    static if (params.length < 1) {
      otherwise = () => choice();
    }

    if (cast(params[0])(arg) !is null) {

      static if (is(Node!(T) == params[0])) {
        Node!(T) x = cast(Node!(T))arg;

        static if (is(ReturnType!(choice) == _RETURN_TYPE_OF_MATCH_WITH_BinaryTree)) {
          static if (is(_RETURN_TYPE_OF_MATCH_WITH_BinaryTree == void)) {
            choice(x);
          } else {
            return choice(x);
          }
        } else {
          static if (isCallable!(ReturnType!(choice))) {
            return cast(_RETURN_TYPE_OF_MATCH_WITH_BinaryTree)choice(x)(x._0, x._1);
          } else {
            return cast(_RETURN_TYPE_OF_MATCH_WITH_BinaryTree)choice(x);
          }
        }
      }

      static if (is(Leaf!(T) == params[0])) {
        Leaf!(T) x = cast(Leaf!(T))arg;

        static if (is(ReturnType!(choice) == _RETURN_TYPE_OF_MATCH_WITH_BinaryTree)) {
          static if (is(_RETURN_TYPE_OF_MATCH_WITH_BinaryTree == void)) {
            choice(x);
          } else {
            return choice(x);
          }
        } else {
          static if (isCallable!(ReturnType!(choice))) {
            return cast(_RETURN_TYPE_OF_MATCH_WITH_BinaryTree)choice(x)(x._0);
          } else {
            return cast(_RETURN_TYPE_OF_MATCH_WITH_BinaryTree)choice(x);
          }
        }
      }

    }
  }

  if (otherwise !is null) {
    static if (is(_RETURN_TYPE_OF_MATCH_WITH_BinaryTree == void)) {
      otherwise();
      return;
    } else {
      return otherwise();
    }
  }

  static if (!is(_RETURN_TYPE_OF_MATCH_WITH_BinaryTree == void)) {
    return null;
  }
}
