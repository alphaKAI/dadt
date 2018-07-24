module dadt.option;
import std.algorithm, std.traits;

/*
	type Option(T) =
		| Some of T
		| None

	↓ Compile to
*/

interface Option(T) {
}

class Some(T) : Option!T {
  T value;

  this(T value) {
    this.value = value;
  }
}

Some!(T) some(T)(T val) {
  return new Some!(T)(val);
}

class None(T) : Option!T {
  this() {
  }
}

None!T none(T)() {
  return new None!T;
}

R matchWithOption(R, E, choices...)(Option!(E) arg) {
  foreach (choice; choices) {
    alias params = Parameters!choice;
    if (params.length < 1) {
      continue;
    }

    if (cast(params[0])(arg) !is null) {
      static if (is(Some!(E) == params[0])) {
        Some!E x = cast(Some!E)arg;

        static if (is(ReturnType!(choice) == R)) {
          static if (is(R == void)) {
            choice(x);
          }
          else {
            return choice(x);
          }
        }
        else {
          return choice(x)(x.value);
        }
      }
      else {
        None!E x = cast(None!E)arg;

        static if (is(R == void)) {
          choice(x);
        }
        else {
          return choice(x);
        }
      }
    }
  }

  static if (!is(R == void)) {
    return null;
  }
}
