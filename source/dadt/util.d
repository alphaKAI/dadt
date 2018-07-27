module dadt.util;
import std.string;

string patternReplaceWithTable(string code, string[string] table) {
  string[] codes;
  size_t[][string] targets;
  bool in_bracket;
  string buf;

  for (size_t i = 0; i < code.length; i++) {
    char ch = code[i];

    if (!in_bracket) {
      if (ch == '#') {
        if (i + 1 < code.length && code[i + 1] == '{') {
          in_bracket = true;
          i++;

          codes ~= buf;
          buf = "";
          continue;
        } else {
          throw new Error("Syntax Error");
        }
      } else {
        buf ~= ch;
      }
    } else {
      if (ch == '}') {
        if (i + 1 < code.length && code[i + 1] == '#') {
          in_bracket = false;
          i++;

          codes ~= buf;
          targets[buf] ~= codes.length - 1;
          buf = "";
          continue;
        } else {
          buf ~= ch;
        }
      } else {
        buf ~= ch;
      }
    }
  }

  if (buf.length) {
    codes ~= buf;
  }

  foreach (key, value; table) {
    size_t[] idxes = targets[key];
    foreach (idx; idxes) {
      codes[idx] = value;
    }
  }

  return codes.join;
}
