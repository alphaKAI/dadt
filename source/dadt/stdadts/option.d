module dadt.stdadts.option;
enum OptionType {
  Some,
  None
}

interface Option(T) {
  OptionType type();
}

class Some(T) : Option!(T) {
  T _0;

  this(T _0) {
    this._0 = _0;
  }

  OptionType type() {
    return OptionType.Some;
  }

}

Some!(T) some(T)(T _0) {
  return new Some!(T)(_0);
}

class None(T) : Option!(T) {

  this() {

  }

  OptionType type() {
    return OptionType.None;
  }

}

None!(T) none(T)() {
  return new None!(T)();
}

_RETURN_TYPE_OF_MATCH_WITH_Option matchWithOption(_RETURN_TYPE_OF_MATCH_WITH_Option, T, choices...)(
    Option!(T) arg) {
  import std.traits;

  _RETURN_TYPE_OF_MATCH_WITH_Option delegate() otherwise = null;

  foreach (choice; choices) {
    alias params = Parameters!choice;
    static if (params.length < 1) {
      otherwise = () => choice();
    }

    if (cast(params[0])(arg) !is null) {

      static if (is(Some!(T) == params[0])) {
        Some!(T) x = cast(Some!(T))arg;

        static if (is(ReturnType!(choice) == _RETURN_TYPE_OF_MATCH_WITH_Option)) {
          static if (is(_RETURN_TYPE_OF_MATCH_WITH_Option == void)) {
            choice(x);
          } else {
            return choice(x);
          }
        } else {
          static if (isCallable!(ReturnType!(choice))) {
            return cast(_RETURN_TYPE_OF_MATCH_WITH_Option)choice(x)(x._0);
          } else {
            return cast(_RETURN_TYPE_OF_MATCH_WITH_Option)choice(x);
          }
        }
      }

      static if (is(None!(T) == params[0])) {
        None!(T) x = cast(None!(T))arg;

        static if (is(ReturnType!(choice) == _RETURN_TYPE_OF_MATCH_WITH_Option)) {
          static if (is(_RETURN_TYPE_OF_MATCH_WITH_Option == void)) {
            choice(x);
          } else {
            return choice(x);
          }
        } else {
          static if (isCallable!(ReturnType!(choice))) {
            return cast(_RETURN_TYPE_OF_MATCH_WITH_Option)choice(x)();
          } else {
            return cast(_RETURN_TYPE_OF_MATCH_WITH_Option)choice(x);
          }
        }
      }

    }
  }

  if (otherwise !is null) {
    static if (is(_RETURN_TYPE_OF_MATCH_WITH_Option == void)) {
      otherwise();
      return;
    } else {
      return otherwise();
    }
  }

  static if (!is(_RETURN_TYPE_OF_MATCH_WITH_Option == void)) {
    return null;
  }
}

string show_Option(T)(Option!(T) arg, string function(Option!(T)) conv = null) {
  if (conv !is null) {
    return conv(arg);
  }
  import std.string;

  string[] interface_args = [T.stringof];
  string constructor_arg;
  if (interface_args.length) {
    constructor_arg = "!(" ~ interface_args.join(", ") ~ ")";
  }

  final switch (arg.type) {
  case OptionType.Some:
    Some!(T) x = cast(Some!(T))arg;

    return "Some" ~ constructor_arg ~ "(%s)".format(x._0);
  case OptionType.None:
    None!(T) x = cast(None!(T))arg;

    return "None" ~ constructor_arg;
  }
}

int compare_Option(T)(Option!(T) _lhs, Option!(T) _rhs) {

  OptionType lhs_type = _lhs.type, rhs_type = _rhs.type;

  if (lhs_type < rhs_type) {
    return -1;
  }
  if (lhs_type > rhs_type) {
    return 1;
  }

  final switch (lhs_type) {
  case OptionType.Some:
    Some!(T) lhs = cast(Some!(T))_lhs, rhs = cast(Some!(T))_rhs;

    if (lhs._0 < rhs._0) {
      return -1;
    }
    if (lhs._0 > rhs._0) {
      return 1;
    }

    return 0;
  case OptionType.None:
    None!(T) lhs = cast(None!(T))_lhs, rhs = cast(None!(T))_rhs;

    return 0;
  }
}

bool equal_Option(T)(Option!(T) _lhs, Option!(T) _rhs) {
  return compare_Option!(T)(_lhs, _rhs) == 0;
}

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
