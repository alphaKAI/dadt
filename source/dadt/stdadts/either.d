module source.dadt.stdadts.either;
enum EitherType {
  Right,
  Left
}

interface Either(T, U) {
  EitherType type();
}

class Right(T, U) : Either!(T, U) {
  T _0;

  this(T _0) {
    this._0 = _0;
  }

  EitherType type() {
    return EitherType.Right;
  }

}

Right!(T, U) right(T, U)(T _0) {
  return new Right!(T, U)(_0);
}

class Left(T, U) : Either!(T, U) {
  U _0;

  this(U _0) {
    this._0 = _0;
  }

  EitherType type() {
    return EitherType.Left;
  }

}

Left!(T, U) left(T, U)(U _0) {
  return new Left!(T, U)(_0);
}

_RETURN_TYPE_OF_MATCH_WITH_Either matchWithEither(
    _RETURN_TYPE_OF_MATCH_WITH_Either, T, U, choices...)(Either!(T, U) arg) {
  import std.traits;

  _RETURN_TYPE_OF_MATCH_WITH_Either delegate() otherwise = null;

  foreach (choice; choices) {
    alias params = Parameters!choice;
    static if (params.length < 1) {
      otherwise = () => choice();
    }

    if (cast(params[0])(arg) !is null) {

      static if (is(Right!(T, U) == params[0])) {
        Right!(T, U) x = cast(Right!(T, U))arg;

        static if (is(ReturnType!(choice) == _RETURN_TYPE_OF_MATCH_WITH_Either)) {
          static if (is(_RETURN_TYPE_OF_MATCH_WITH_Either == void)) {
            choice(x);
          } else {
            return choice(x);
          }
        } else {
          static if (isCallable!(ReturnType!(choice))) {
            return cast(_RETURN_TYPE_OF_MATCH_WITH_Either)choice(x)(x._0);
          } else {
            return cast(_RETURN_TYPE_OF_MATCH_WITH_Either)choice(x);
          }
        }
      }

      static if (is(Left!(T, U) == params[0])) {
        Left!(T, U) x = cast(Left!(T, U))arg;

        static if (is(ReturnType!(choice) == _RETURN_TYPE_OF_MATCH_WITH_Either)) {
          static if (is(_RETURN_TYPE_OF_MATCH_WITH_Either == void)) {
            choice(x);
          } else {
            return choice(x);
          }
        } else {
          static if (isCallable!(ReturnType!(choice))) {
            return cast(_RETURN_TYPE_OF_MATCH_WITH_Either)choice(x)(x._0);
          } else {
            return cast(_RETURN_TYPE_OF_MATCH_WITH_Either)choice(x);
          }
        }
      }

    }
  }

  if (otherwise !is null) {
    static if (is(_RETURN_TYPE_OF_MATCH_WITH_Either == void)) {
      otherwise();
      return;
    } else {
      return otherwise();
    }
  }

  static if (!is(_RETURN_TYPE_OF_MATCH_WITH_Either == void)) {
    return null;
  }
}

string show_Either(T, U)(Either!(T, U) arg, string function(Either!(T, U)) conv = null) {
  if (conv !is null) {
    return conv(arg);
  }
  import std.string;

  string[] interface_args = [T.stringof, U.stringof];
  string constructor_arg;
  if (interface_args.length) {
    constructor_arg = "!(" ~ interface_args.join(", ") ~ ")";
  }

  final switch (arg.type) {
  case EitherType.Right:
    Right!(T, U) x = cast(Right!(T, U))arg;

    return "Right" ~ constructor_arg ~ "(%s)".format(x._0);
  case EitherType.Left:
    Left!(T, U) x = cast(Left!(T, U))arg;

    return "Left" ~ constructor_arg ~ "(%s)".format(x._0);
  }
}

int compare_Either(T, U)(Either!(T, U) _lhs, Either!(T, U) _rhs) {

  EitherType lhs_type = _lhs.type, rhs_type = _rhs.type;

  if (lhs_type < rhs_type) {
    return -1;
  }
  if (lhs_type > rhs_type) {
    return 1;
  }

  final switch (lhs_type) {
  case EitherType.Right:
    Right!(T, U) lhs = cast(Right!(T, U))_lhs, rhs = cast(Right!(T, U))_rhs;

    if (lhs._0 < rhs._0) {
      return -1;
    }
    if (lhs._0 > rhs._0) {
      return 1;
    }

    return 0;
  case EitherType.Left:
    Left!(T, U) lhs = cast(Left!(T, U))_lhs, rhs = cast(Left!(T, U))_rhs;

    if (lhs._0 < rhs._0) {
      return -1;
    }
    if (lhs._0 > rhs._0) {
      return 1;
    }

    return 0;
  }
}

bool equal_Either(T, U)(Either!(T, U) _lhs, Either!(T, U) _rhs) {
  return compare_Either!(T, U)(_lhs, _rhs) == 0;
}
