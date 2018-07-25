module dadt.parse;
import pegged.grammar;
import std.algorithm;
import std.format;
import std.string;
import std.stdio;
import std.array;
import std.ascii;

mixin(grammar(`
DADT:
  TypeDeclare < "type" BaseConstructor "=" ConstructorList

  BaseConstructor < TypeNameWithArgs / TypeNameWithoutArgs

  TypeName <~ !Keyword [A-Z_] [a-zA-Z0-9_]*
  
  Field < FieldOfArray / FieldWithArgs / FieldName
  FieldArgs < "()" / :"(" Field ("," Field)* :")"
  FieldWithArgs < FieldName "!" FieldArgs
  FieldOfArray < (FieldWithArgs / FieldName) ArrayBracket+
  FieldName <~ !Keyword [a-zA-Z_] [a-zA-Z0-9_]*

  ArrayBracket < UnsizedBracket / SizedBracket

  UnsizedBracket < "[]"
  SizedBracket < "[" ArraySize "]"
  ArraySize <~ [a-zA-Z0-9_]*

  TypeNameWithoutArgs < TypeName
  TypeNameWithArgs < TypeName ParameterList
  ParameterList < "()" / :"(" TypeName ("," TypeName)* :")"

  ConstructorWithField < "|" TypeName "of" Field ("*" Field)*
  Constructor <  "|" TypeName
  ConstructorDeclare < ConstructorWithField / Constructor
  ConstructorList < ConstructorDeclare+

  Keyword <~ "of"
  Integer <~ digit+
`));

interface ASTElement {
}

class BaseConstructor : ASTElement {
  TypeName baseName;
  ParameterList parameterList;

  this(TypeName baseName) {
    this.baseName = baseName;
    this.parameterList = new ParameterList([]);
  }

  this(TypeName baseName, ParameterList parameterList) {
    this.baseName = baseName;
    this.parameterList = parameterList;
  }
}

class TypeName : ASTElement {
  string name;

  this(string name) {
    this.name = name;
  }
}

interface Field : ASTElement {
  const string typeString();
}

class FieldName : Field {
  string fieldName;

  this(string fieldName) {
    this.fieldName = fieldName;
  }

  override const string typeString() {
    return fieldName;
  }
}

class FieldArgs : ASTElement {
  Field[] fields;

  this(Field[] fields) {
    this.fields = fields;
  }
}

class FieldWithArgs : Field {
  FieldName fieldName;
  FieldArgs fieldArgs;

  this(FieldName fieldName, FieldArgs fieldArgs) {
    this.fieldName = fieldName;
    this.fieldArgs = fieldArgs;
  }

  override const string typeString() {
    const string baseType = fieldName.typeString;
    string[] fields;
    string argsStr;
    foreach (field; fieldArgs.fields) {
      fields ~= field.typeString;
    }
    if (fields.length) {
      argsStr = "!(" ~ fields.join(", ") ~ ")";

      return baseType ~ argsStr;
    } else {
      return baseType;
    }
  }
}

interface ArrayBracket : ASTElement {
  const bool isSized();
}

class ArraySize : ASTElement {
  string size;

  this(string size) {
    this.size = size;
  }
}

class SizedBracket : ArrayBracket {
  ArraySize size;

  this(ArraySize size) {
    this.size = size;
  }

  override const bool isSized() {
    return true;
  }
}

class UnsizedBracket : ArrayBracket {
  this() {
  }

  override const bool isSized() {
    return false;
  }
}

class FieldOfArray : Field {
  Field field;
  ArrayBracket bracket;

  this(Field field, ArrayBracket bracket) {
    this.field = field;
    this.bracket = bracket;
  }

  override const string typeString() {
    string baseType = field.typeString;

    if (this.bracket.isSized) {
      SizedBracket sb = cast(SizedBracket)bracket;
      return baseType ~ "[%s]".format(sb.size.size);
    } else {
      return baseType ~ "[]";
    }
  }
}

class ParameterList : ASTElement {
  TypeName[] parameters;

  this(TypeName[] parameters) {
    this.parameters = parameters;
  }
}

class TypeDeclare : ASTElement {
  BaseConstructor baseConstructor;
  ConstructorList constructorList;

  this(BaseConstructor baseConstructor, ConstructorList constructorList) {
    this.baseConstructor = baseConstructor;
    this.constructorList = constructorList;
  }
}

class ConstructorList : ASTElement {
  Constructor[] constructors;

  this(Constructor[] constructors) {
    this.constructors = constructors;
  }
}

class Constructor : ASTElement {
  TypeName typeName;
  Field[] fields;

  this(TypeName typeName) {
    this.typeName = typeName;
  }

  this(TypeName typeName, Field[] fields) {
    this.typeName = typeName;
    this.fields = fields;
  }
}

ASTElement buildAST(ParseTree p) {
  //writeln("p.name : ", p.name);
  final switch (p.name) {
  case "DADT":
    auto e = p.children[0];
    return buildAST(e);
  case "DADT.BaseConstructor":
    auto e = p.children[0];
    return buildAST(e);
  case "DADT.TypeDeclare":
    BaseConstructor baseConstructor = cast(BaseConstructor)buildAST(p.children[0]);

    if (baseConstructor is null) {
      throw new Error("Error in %s!".format(p.name));
    }

    ConstructorList constructorList = cast(ConstructorList)buildAST(p.children[1]);

    return new TypeDeclare(baseConstructor, constructorList);
  case "DADT.TypeName":
    string typeName = p.matches[0];
    return new TypeName(typeName);
  case "DADT.TypeNameWithoutArgs":
    TypeName baseName = cast(TypeName)buildAST(p.children[0]);

    if (baseName is null) {
      throw new Error("Error in %s!".format(p.name));
    }

    return new BaseConstructor(baseName);
  case "DADT.TypeNameWithArgs":
    TypeName baseName = cast(TypeName)buildAST(p.children[0]);
    ParameterList parameterList = cast(ParameterList)buildAST(p.children[1]);
    if (baseName is null || parameterList is null) {
      throw new Error("Error in %s!".format(p.name));
    }

    return new BaseConstructor(baseName, parameterList);

  case "DADT.Field":
    auto e = p.children[0];
    return buildAST(e);
  case "DADT.FieldWithArgs":
    FieldName fieldName = cast(FieldName)buildAST(p.children[0]);
    if (fieldName is null) {
      throw new Error("Error in %s!".format(p.name));
    }

    FieldArgs fieldArgs = cast(FieldArgs)buildAST(p.children[1]);
    if (fieldArgs is null) {
      throw new Error("Error in %s!".format(p.name));
    }

    return new FieldWithArgs(fieldName, fieldArgs);
  case "DADT.FieldArgs":
    Field[] fields;

    foreach (child; p.children) {
      Field field = cast(Field)buildAST(child);
      if (field is null) {
        throw new Error("Error in %s!".format(p.name));
      }
      fields ~= field;
    }

    return new FieldArgs(fields);
  case "DADT.FieldOfArray":
    Field field = cast(Field)buildAST(p.children[0]);
    if (field is null) {
      throw new Error("Error in %s!".format(p.name));
    }

    ArrayBracket ab = cast(ArrayBracket)buildAST(p.children[1]);
    if (ab is null) {
      throw new Error("Error in %s!".format(p.name));
    }

    return new FieldOfArray(field, ab);
  case "DADT.ArrayBracket":
    auto e = p.children[0];
    return buildAST(e);
  case "DADT.ArraySize":
    string size = p.matches[0];
    return new ArraySize(size);
  case "DADT.SizedBracket":
    ArraySize size = cast(ArraySize)buildAST(p.children[0]);
    if (size is null) {
      throw new Error("Error in %s!".format(p.name));
    }

    return new SizedBracket(size);
  case "DADT.UnsizedBracket":
    return new UnsizedBracket;
  case "DADT.FieldName":
    string fieldName = p.matches[0];
    return new FieldName(fieldName);
  case "DADT.ParameterList":
    TypeName[] parameters;
    foreach (child; p.children) {
      TypeName typeName = cast(TypeName)buildAST(child);
      if (typeName is null) {
        throw new Error("Error in %s!".format(p.name));
      }

      parameters ~= typeName;
    }

    return new ParameterList(parameters);
  case "DADT.ConstructorList":
    Constructor[] constructors;
    foreach (child; p.children) {
      Constructor constructor = cast(Constructor)buildAST(child);
      if (constructor is null) {
        throw new Error("Error in %s!".format(p.name));
      }
      constructors ~= constructor;
    }

    return new ConstructorList(constructors);
  case "DADT.ConstructorDeclare":
    auto e = p.children[0];
    return buildAST(e);
  case "DADT.ConstructorWithField":
    TypeName constructorName = cast(TypeName)buildAST(p.children[0]);
    if (constructorName is null) {
      throw new Error("Error in %s!".format(p.name));
    }
    Field[] fields;
    foreach (child; p.children[1 .. $]) {
      Field field = cast(Field)buildAST(child);
      if (field is null) {
        throw new Error("Error in %s!".format(p.name));
      }
      fields ~= field;
    }

    return new Constructor(constructorName, fields);
  case "DADT.Constructor":
    TypeName constructorName = cast(TypeName)buildAST(p.children[0]);
    if (constructorName is null) {
      throw new Error("Error in %s!".format(p.name));
    }
    return new Constructor(constructorName);
  }

}

string genCode(const TypeDeclare td) {
  string code;
  string interface_name = td.baseConstructor.baseName.name;
  const string[] interface_args = td.baseConstructor.parameterList.parameters.map!(
      (const TypeName tn) => tn.name).array;
  string args_str;
  string interface_args_str;
  if (interface_args.length) {
    interface_args_str = "(" ~ interface_args.join(", ") ~ ")";
    args_str = "!" ~ interface_args_str;
  }

  code ~= "interface " ~ interface_name ~ interface_args_str ~ "{}\n";

  foreach (constructor; td.constructorList.constructors) {
    string[] field_names;
    string[string] field_info;
    string field_code;
    string[] field_type_and_names;
    foreach (i, fieldType; constructor.fields) {
      string field_name = "_%d".format(i);
      field_names ~= field_name;
      field_info[field_name] = fieldType.typeString();
    }

    foreach (field_name; field_names) {
      string field_type = field_info[field_name];
      field_type_and_names ~= "%s %s".format(field_type, field_name);
    }
    foreach (field_type_and_name; field_type_and_names) {
      field_code ~= field_type_and_name ~ ";";
    }

    string this_code;
    string this_argument = field_type_and_names.join(", ");
    string initialize_list;
    foreach (field_name; field_names) {
      initialize_list ~= "this.%s = %s;".format(field_name, field_name);
    }

    this_code = `this(%s) {
  %s
}`.format(this_argument, initialize_list);

    code ~= "\n" ~ `class %s%s : %s%s {
  %s

  %s
}`.format(constructor.typeName.name,
        interface_args_str, interface_name, args_str, field_code, this_code);

    string helper_code;
    string helper_returnType = constructor.typeName.name ~ args_str;
    string helper_name = constructor.typeName.name.toLower;
    string helper_typeParameters;

    if (interface_args.length) {
      helper_typeParameters = "(" ~ interface_args.join(", ") ~ ")";
    }

    string[] helper_arguments;
    string[] helper_variables;

    foreach (i, field; constructor.fields) {
      helper_arguments ~= "%s _%d".format(field.typeString(), i);
      helper_variables ~= "_%d".format(i);
    }

    helper_code = `
%s %s%s(%s) {
  return new %s(%s);
}`.format(helper_returnType, helper_name, helper_typeParameters,
        helper_arguments.join(", "), helper_returnType, helper_variables.join(", "));

    code ~= helper_code;
  }

  string match_returnType = "_RETURN_TYPE_OF_MATCH_WITH_%s".format(interface_name);
  string match_header = "%s matchWith%s(%s, %s choices...)(%s%s arg) {".format(
      match_returnType, interface_name, match_returnType,
      interface_args.join(", ") ~ (interface_args.length > 0 ? "," : ""), interface_name, args_str);

  string[] match_static_routers;

  foreach (constructor; td.constructorList.constructors) {
    string type_signature = constructor.typeName.name ~ args_str;
    string[] field_names;
    foreach (i, fieldType; constructor.fields) {
      string field_name = "x._%d".format(i);
      field_names ~= field_name;
    }

    match_static_routers ~= `static if (is(%s == params[0])) {`.format(type_signature) ~ `
        %s x = cast(%s)arg;`.format(type_signature,
        type_signature) ~ `
        static if (is(ReturnType!(choice) == %s)) {
          static if (is(%s == void)) {`.format(match_returnType,
        match_returnType) ~ `
            choice(x);
          }
          else {
            return choice(x);
          }
        }
        else {
          return choice(x)` ~ (field_names.length > 0
        ? `(` ~ field_names.join(", ") ~ `)` : "") ~ ` ;
        }
    }`;
  }

  string match_code = `
  %s
  import std.traits;
  %s delegate() otherwise = null;`.format(match_header,
      match_returnType) ~ `

  foreach (choice; choices) {
    alias params = Parameters!choice;
    static if (params.length < 1) {
      otherwise = () => choice();
    }

    if (cast(params[0])(arg) !is null) {
    ` ~ match_static_routers.join("\n") ~ `
    }
  }

  if (otherwise !is null) {
    static if (is(%s == void)) {
      otherwise();
      return;
    }
    else {
      return otherwise();
    }
  }

  static if (!is(%s == void)) {
    return null;
  }
}`.format(match_returnType,
      match_returnType);

  code ~= match_code;

  return code;
}
