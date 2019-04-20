module dadt.parser;
import pegged.grammar;
import std.algorithm;
import std.format;
import std.string;
import std.stdio;
import std.array;
import std.ascii;
import dadt.util;

mixin(grammar(`
DADT:
  TypeDeclare < "type" BaseConstructor "=" ConstructorList Deriving?

  BaseConstructor < TypeNameWithArgs / TypeNameWithoutArgs

  TypeName <~ !Keyword [A-Z_] [a-zA-Z0-9_]*

  Field < FieldOfDelegate / FieldOfFunction / FieldOfArray / FieldWithArgs / FieldName
  FieldArgs < "()" / :"(" Field ("," Field)* :")"
  FieldWithArgs < FieldName "!" FieldArgs
  FieldOfArray < (FieldWithArgs / FieldName) ArrayBracket+
  FieldOfDelegate < "[" Field ("->" Field)+ "]"
  FieldOfFunction < "<" Field ("->" Field)+ ">"
  FieldName <~ !Keyword [a-zA-Z_] [a-zA-Z0-9_]*

  ArrayBracket < UnsizedBracket / SizedBracket

  UnsizedBracket < "[]"
  SizedBracket < "[" ArraySize "]"
  ArraySize <~ [a-zA-Z0-9_]*

  TypeNameWithoutArgs < TypeName
  TypeNameWithArgs < TypeName ParameterList
  ParameterList < "()" / :"(" TypeName ("," TypeName)* :")"

  RecordField < FieldName ":" Field
  RecordFields < (RecordField ";"?)+
  Record < "{" RecordFields "}"

  ConstructorWithRecord< "|"? TypeName "of" Record
  ConstructorWithField < "|"? TypeName "of" Field ("*" Field)*
  Constructor <  "|"? TypeName
  ConstructorDeclare < ConstructorWithRecord / ConstructorWithField / Constructor
  ConstructorList < ConstructorDeclare+

  Deriving < "[@@deriving" DerivingArgs "]"
  DerivingArgs < DerivingArg ("," DerivingArg)*
  DerivingArg <~ !Keyword [a-zA-Z_] [a-zA-Z0-9_]*

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

class FieldOfDelegate : Field {
  Field retType;
  Field[] args;

  this(Field retType, Field[] args) {
    this.retType = retType;
    this.args = args;
  }

  override const string typeString() {
    string args_str = this.args.map!(arg => arg.typeString()).join(", ");
    if (args_str == "void") {
      args_str = "";
    }
    return "%s delegate(%s)".format(this.retType.typeString(), args_str);
  }
}

class FieldOfFunction : Field {
  Field retType;
  Field[] args;

  this(Field retType, Field[] args) {
    this.retType = retType;
    this.args = args;
  }

  override const string typeString() {
    string args_str = this.args.map!(arg => arg.typeString()).join(", ");
    if (args_str == "void") {
      args_str = "";
    }
    return "%s function(%s)".format(this.retType.typeString(), args_str);
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
  Deriving deriving;

  this(BaseConstructor baseConstructor, ConstructorList constructorList) {
    this.baseConstructor = baseConstructor;
    this.constructorList = constructorList;
  }

  this(BaseConstructor baseConstructor, ConstructorList constructorList, Deriving deriving) {
    this.baseConstructor = baseConstructor;
    this.constructorList = constructorList;
    this.deriving = deriving;
  }
}

class ConstructorList : ASTElement {
  Constructor[] constructors;

  this(Constructor[] constructors) {
    this.constructors = constructors;
  }
}

class RecordField : ASTElement {
  FieldName name;
  Field field;

  this(FieldName name, Field field) {
    this.name = name;
    this.field = field;
  }

  string getFieldTypeString() const {
    return this.field.typeString();
  }

  string getFieldName() const {
    return this.name.fieldName;
  }
}

class RecordFields : ASTElement {
  RecordField[] fields;
  this(RecordField[] fields) {
    this.fields = fields;
  }
}

enum ConstructorType {
  Single,
  Fields,
  Record
}

class Constructor : ASTElement {
  TypeName typeName;
  Field[] fields;
  RecordFields record;
  ConstructorType type;

  this(TypeName typeName) {
    this.typeName = typeName;
    this.type = ConstructorType.Single;
  }

  this(TypeName typeName, Field[] fields) {
    this.typeName = typeName;
    this.fields = fields;
    this.type = ConstructorType.Fields;
  }

  this(TypeName typeName, RecordFields record) {
    this.typeName = typeName;
    this.record = record;
    this.type = ConstructorType.Record;
  }

  string getTypeName() const {
    return this.typeName.name;
  }
}

enum DerivingType {
  Show,
  Ord,
  Eq
}

enum DerivingMap = [
    "show" : DerivingType.Show, "ord" : DerivingType.Ord, "eq" : DerivingType.Eq
  ];

class DerivingArg : ASTElement {
  DerivingType type;

  this(DerivingType type) {
    this.type = type;
  }
}

class DerivingArgs : ASTElement {
  DerivingArg[] args;

  this(DerivingArg[] args) {
    this.args = args;
  }
}

class Deriving : ASTElement {
  DerivingArgs args;

  this(DerivingArgs args) {
    this.args = args;
  }
}

ASTElement buildAST(ParseTree p) {
  /*
  if (!__ctfe) {
    writeln("p.name : ", p.name);
  }
  */

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

    if (constructorList is null) {
      throw new Error("Error in %s!".format(p.name));
    }

    if (p.children.length == 2) {
      return new TypeDeclare(baseConstructor, constructorList);
    } else {
      Deriving deriving = cast(Deriving)buildAST(p.children[2]);

      if (deriving is null) {
        throw new Error("Error in %s!".format(p.name));
      }

      return new TypeDeclare(baseConstructor, constructorList, deriving);
    }
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
  case "DADT.FieldOfDelegate":
    Field[] fields;
    foreach (child; p.children) {
      Field field = cast(Field)buildAST(child);
      if (field is null) {
        throw new Error("Error in %s!".format(p.name));
      }
      fields ~= field;
    }
    Field retType = fields[$ - 1];
    return new FieldOfDelegate(retType, fields[0 .. $ - 1]);
  case "DADT.FieldOfFunction":
    Field[] fields;
    foreach (child; p.children) {
      Field field = cast(Field)buildAST(child);
      if (field is null) {
        throw new Error("Error in %s!".format(p.name));
      }
      fields ~= field;
    }
    Field retType = fields[$ - 1];
    return new FieldOfFunction(retType, fields[0 .. $ - 1]);
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
  case "DADT.RecordField":
    FieldName name = cast(FieldName)buildAST(p.children[0]);
    if (name is null) {
      throw new Error("Error in %s!".format(p.name));
    }
    Field field = cast(Field)buildAST(p.children[1]);
    if (field is null) {
      throw new Error("Error in %s!".format(p.name));
    }

    return new RecordField(name, field);
  case "DADT.RecordFields":
    RecordField[] fields;
    foreach (child; p.children) {
      RecordField field = cast(RecordField)buildAST(child);
      if (field is null) {
        throw new Error("Error in %s!".format(p.name));
      }
      fields ~= field;
    }
    return new RecordFields(fields);
  case "DADT.Record":
    return buildAST(p.children[0]);
  case "DADT.ConstructorWithRecord":
    TypeName constructorName = cast(TypeName)buildAST(p.children[0]);
    if (constructorName is null) {
      throw new Error("Error in %s!".format(p.name));
    }
    // RecordFields
    RecordFields record = cast(RecordFields)buildAST(p.children[1]);
    if (record is null) {
      throw new Error("Error in %s!".format(p.name));
    }
    return new Constructor(constructorName, record);
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
  case "DADT.Deriving":
    DerivingArgs args = cast(DerivingArgs)buildAST(p.children[0]);

    if (args is null) {
      throw new Error("Error in %s!".format(p.name));
    }

    return new Deriving(args);
  case "DADT.DerivingArg":
    string k = p.matches[0].toLower;

    if (k in DerivingMap) {
      return new DerivingArg(DerivingMap[k]);
    } else {
      throw new Error("Unkown deriving target was given : %s".format(p.matches[0]));
    }
  case "DADT.DerivingArgs":
    DerivingArg[] args;

    foreach (child; p.children) {
      DerivingArg arg = cast(DerivingArg)buildAST(child);

      if (arg is null) {
        throw new Error("Error in %s!".format(p.name));
      }

      args ~= arg;
    }

    return new DerivingArgs(args);
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

  string[] enum_elements;
  foreach (i, constructor; td.constructorList.constructors) {
    if (i > 0) {
      enum_elements ~= "  " ~ constructor.getTypeName();
    } else {
      enum_elements ~= constructor.getTypeName();
    }
  }

  // dfmt off
  code ~=
`
enum #{interface_name}#Type {
  #{enum_elements}#
}
`.patternReplaceWithTable([
  "interface_name" : interface_name,
  "enum_elements"  : enum_elements.join(",\n")
]);
  // dfmt on

  // dfmt off
  code ~=
`
interface #{interface_name}##{interface_args_str}# {
  #{interface_name}#Type type();
}
`.patternReplaceWithTable([
      "interface_name"     : interface_name,
      "interface_args_str" : interface_args_str]);
  // dfmt on

  foreach (constructor; td.constructorList.constructors) {
    string constructor_name = constructor.getTypeName();

    string[] field_names;
    string[string] field_info;
    string field_code;
    string[] field_type_and_names;

    if (constructor.type == ConstructorType.Record) {
      foreach (field; constructor.record.fields) {
        string field_name = field.getFieldName();
        field_names ~= field_name;
        field_info[field_name] = field.getFieldTypeString();
      }
    } else {
      foreach (i, fieldType; constructor.fields) {
        string field_name = "_%d".format(i);
        field_names ~= field_name;
        field_info[field_name] = fieldType.typeString();
      }
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

    // dfmt off
    this_code = `
  this(#{this_argument}#) {
    #{initialize_list}#
  }`.patternReplaceWithTable([
        "this_argument"   : this_argument,
        "initialize_list" : initialize_list]);

    string type_code = `
  #{interface_name}#Type type() {
    return #{interface_name}#Type.#{constructor_name}#;
  }
`.patternReplaceWithTable([
  "interface_name"   : interface_name,
  "constructor_name" : constructor_name
]);

    code ~= `
class #{constructor_name}##{interface_args_str}# : #{interface_name}##{args_str}# {
  #{field_code}#
  #{this_code}#
  #{type_code}#
}`.patternReplaceWithTable([
        "constructor_name"   : constructor_name,
        "interface_args_str" : interface_args_str,
        "interface_name"     : interface_name,
        "args_str"           : args_str,
        "field_code"         : field_code,
        "this_code"          : this_code,
        "type_code"          : type_code]);
    // dfmt on

    string helper_code;
    string helper_returnType = constructor_name ~ args_str;
    string helper_name = constructor_name.toLower;
    string helper_typeParameters;

    if (interface_args.length) {
      helper_typeParameters = "(" ~ interface_args.join(", ") ~ ")";
    }

    string[] helper_arguments;
    string[] helper_variables;

    if (constructor.type == ConstructorType.Record) {
      foreach (field; constructor.record.fields) {
        helper_arguments ~= "%s %s".format(field.getFieldTypeString(), field.getFieldName());
        helper_variables ~= "%s".format(field.getFieldName());
      }
    } else {
      foreach (i, field; constructor.fields) {
        helper_arguments ~= "%s _%d".format(field.typeString(), i);
        helper_variables ~= "_%d".format(i);
      }
    }

    // dfmt off
    helper_code = `
#{helper_returnType}# #{helper_name}##{helper_typeParameters}#(#{helper_arguments}#) {
  return new #{helper_returnType}#(#{helper_variables}#);
}
`.patternReplaceWithTable([
      "helper_returnType"     : helper_returnType,
      "helper_name"           : helper_name,
      "helper_typeParameters" : helper_typeParameters,
      "helper_arguments"      : helper_arguments.join(", "),
      "helper_variables"      : helper_variables.join(", ")]);
    // dfmt on

    code ~= helper_code;
  }

  string match_returnType = "_RETURN_TYPE_OF_MATCH_WITH_%s".format(interface_name);
  // dfmt off
  string match_header = "#{match_returnType}# matchWith#{interface_name}#(#{match_returnType}#, #{interface_args}# choices...)(#{interface_name}##{args_str}# arg) {".patternReplaceWithTable([
      "match_returnType" : match_returnType,
      "interface_name"   : interface_name,
      "interface_args"   : interface_args.join(", ") ~ (interface_args.length > 0 ? "," : ""),
      "interface_name"   : interface_name, "args_str":args_str]);
  // dfmt on

  string match_static_routers;

  foreach (constructor; td.constructorList.constructors) {
    string type_signature = constructor.getTypeName() ~ args_str;
    string[] field_names;
    if (constructor.type == ConstructorType.Record) {
      foreach (field; constructor.record.fields) {
        field_names ~= field.getFieldName();
      }
    } else {
      foreach (i, fieldType; constructor.fields) {
        string field_name = "x._%d".format(i);
        field_names ~= field_name;
      }
    }

    // dfmt off
    match_static_routers ~= `
      static if (is(#{type_signature}# == params[0])) {
        #{type_signature}# x = cast(#{type_signature}#)arg;

        static if (is(ReturnType!(choice) == #{match_returnType}#)) {
          static if (is(#{match_returnType}# == void)) {
            choice(x);
          } else {
            return choice(x);
          }
        } else {
          static if (isCallable!(ReturnType!(choice))) {
            return cast(#{match_returnType}#)choice(x)#{field_args}#;
          } else {
            return cast(#{match_returnType}#)choice(x);
          }
        }
      }
`.patternReplaceWithTable([
  "type_signature"   : type_signature,
  "match_returnType" : match_returnType,
  "field_args"       : field_names.length > 0 ? `(` ~ field_names.join(", ") ~ `)` : "()"]);
    // dfmt on
  }

  // dfmt off
  string match_code = `
#{match_header}#
  import std.traits;
  #{match_returnType}# delegate() otherwise = null;

  foreach (choice; choices) {
    alias params = Parameters!choice;
    static if (params.length < 1) {
      otherwise = () => choice();
    }

    if (cast(params[0])(arg) !is null) {
      #{match_static_routers}#
    }
  }

  if (otherwise !is null) {
    static if (is(#{match_returnType}# == void)) {
      otherwise();
      return;
    } else {
      return otherwise();
    }
  }

  static if (!is(#{match_returnType}# == void)) {
    return null;
  }
}
`.patternReplaceWithTable([
  "match_header"         : match_header,
  "match_returnType"     : match_returnType,
  "match_static_routers" : match_static_routers
]);
  // dfmt on
  code ~= match_code;

  if (td.deriving !is null) {
    bool ord_is_generated = false;

    foreach (arg; td.deriving.args.args) {
      string deriving_code;
      final switch (arg.type) with (DerivingType) {
      case Show:
        string show_code;

        // dfmt off
        string show_header = `string show_#{interface_name}##{interface_args_str}#(#{interface_name}##{args_str}# arg, string function(#{interface_name}##{args_str}#) conv = null) {`.patternReplaceWithTable([
          "interface_name"     : interface_name,
          "interface_args_str" : interface_args_str,
          "args_str"           : args_str]);

        string show_middle_header =
`  if (conv !is null) {
    return conv(arg);
  }
  import std.string;
  string[] interface_args = [#{interface_args}#];
  string constructor_arg;
  if (interface_args.length) {
    constructor_arg = "!(" ~ interface_args.join(", ") ~ ")";
  }
`.patternReplaceWithTable(["interface_args" : interface_args.map!(iarg => iarg ~ ".stringof").join(", ")]);
        // dfmt on

        string show_constructors = `
  final switch (arg.type) {`;

        foreach (constructor; td.constructorList.constructors) {
          string constructor_name = constructor.getTypeName();
          string type_signature = constructor.getTypeName() ~ args_str;
          string[] field_names;
          if (constructor.type == ConstructorType.Record) {
            foreach (field; constructor.record.fields) {
              field_names ~= "x." ~ field.getFieldName();
            }
          } else {
            foreach (i, fieldType; constructor.fields) {
              string field_name = "x._%d".format(i);
              field_names ~= field_name;
            }
          }

          string ret_line;

          if (field_names.length) {
            string[] field_formatters;
            foreach (_; field_names) {
              field_formatters ~= "%s";
            }
            // dfmt off
            ret_line = `return "#{constructor_name}#" ~ constructor_arg ~ "(#{field_formatters}#)".format(#{field_names}#);`.patternReplaceWithTable([
              "constructor_name"    : constructor_name,
              "field_formatters" : field_formatters.join(", "),
              "field_names"      : field_names.join(", ")]);
            // dfmt on
          } else {
            ret_line = `return "%s" ~ constructor_arg;`.format(constructor_name);
          }

          // dfmt off
          show_constructors ~= `
    case #{interface_name}#Type.#{constructor_name}#:
      #{type_signature}# x = cast(#{type_signature}#)arg;

      #{ret_line}#`.patternReplaceWithTable([
  "interface_name"   : interface_name,
  "constructor_name" : constructor_name,
  "type_signature"   : type_signature,
  "ret_line"         : ret_line]);
          // dfmt on
        }

        string show_footer = `  }`;
        // dfmt off
        show_code =
`
#{show_header}#
#{show_middle_header}#
#{show_constructors}#
#{show_footer}#
}
`.patternReplaceWithTable([
  "show_header"        : show_header,
  "show_middle_header" : show_middle_header,
  "show_constructors"  : show_constructors,
  "show_footer"        : show_footer
]);
        // dfmt on
        deriving_code ~= show_code;
        break;
      case Ord:
        if (ord_is_generated) {
          break;
        }
        ord_is_generated = true;
        // dfmt off
        string ord_header = `int compare_#{interface_name}##{interface_args_str}#(#{interface_name}##{args_str}# _lhs, #{interface_name}##{args_str}# _rhs) {`.patternReplaceWithTable([
          "interface_name"     : interface_name,
          "interface_args_str" : interface_args_str,
          "args_str"           : args_str]);
        // dfmt on
        string ord_precomp = `
  #{interface_name}#Type lhs_type = _lhs.type,
      rhs_type = _rhs.type;

  if (lhs_type < rhs_type) {
    return -1;
  }
  if (lhs_type > rhs_type) {
    return 1;
  }
`.patternReplaceWithTable([
            "interface_name": interface_name
            ]);

        string ord_compbody = `
  final switch (lhs_type) {`;

        foreach (constructor; td.constructorList.constructors) {
          string constructor_name = constructor.getTypeName();
          string type_signature = constructor.getTypeName() ~ args_str;
          string field_comps;

          if (constructor.type == ConstructorType.Record) {
            foreach (field; constructor.record.fields) {
              string field_name = field.getFieldName();

              field_comps ~= `
      if (lhs.#{field_name}# < rhs.#{field_name}#) {
        return -1;
      }
      if (lhs.#{field_name}# > rhs.#{field_name}#) {
        return 1;
      }`.patternReplaceWithTable([
                  "field_name": field_name
                  ]);
            }
          } else {
            foreach (i, fieldType; constructor.fields) {
              string field_name = "_%d".format(i);

              field_comps ~= `
      if (lhs.#{field_name}# < rhs.#{field_name}#) {
        return -1;
      }
      if (lhs.#{field_name}# > rhs.#{field_name}#) {
        return 1;
      }`.patternReplaceWithTable([
                  "field_name": field_name
                  ]);
            }
          }

          // dfmt off
          ord_compbody ~= `
    case #{interface_name}#Type.#{constructor_name}#:
      #{type_signature}# lhs = cast(#{type_signature}#)_lhs,
              rhs = cast(#{type_signature}#)_rhs;

      #{field_comps}#

      return 0;`.patternReplaceWithTable([
  "interface_name"   : interface_name,
  "constructor_name" : constructor_name,
  "type_signature"   : type_signature,
  "field_comps"      : field_comps
]);
          // dfmt on
        }

        string ord_footer = `  }
}`;
        // dfmt off
        string ord_code = `
#{ord_header}#
#{ord_precomp}#
#{ord_compbody}#
#{ord_footer}#
`.patternReplaceWithTable([
  "ord_header"      : ord_header,
  "ord_precomp"     : ord_precomp,
  "ord_compbody"    : ord_compbody,
  "ord_footer"    : ord_footer
]);
        // dfmt on
        deriving_code ~= ord_code;
        break;
      case Eq:
        // dfmt off
        string eq_header = `bool equal_#{interface_name}##{interface_args_str}#(#{interface_name}##{args_str}# _lhs, #{interface_name}##{args_str}# _rhs) {`.patternReplaceWithTable([
          "interface_name"     : interface_name,
          "interface_args_str" : interface_args_str,
          "args_str"           : args_str]);
        string eq_body = `  return compare_#{interface_name}##{args_str}#(_lhs, _rhs) == 0;`.patternReplaceWithTable([
          "interface_name"     : interface_name,
          "args_str"           : args_str]);
        string eq_footer = "}";
        string eq_code =
`
#{eq_header}#
#{eq_body}#
#{eq_footer}#
`.patternReplaceWithTable([
  "eq_header" : eq_header,
  "eq_body"   : eq_body,
  "eq_footer" : eq_footer]);
        deriving_code ~= eq_code;
        // dfmt on
        if (!ord_is_generated) {
          goto case Ord;
        } else {
          break;
        }
      }
      code ~= deriving_code;
    }
  }

  return code;
}

string genCodeFromSource(string source) {
  auto ast = DADT(source);
  TypeDeclare td = cast(TypeDeclare)buildAST(ast);
  return genCode(td);
}
