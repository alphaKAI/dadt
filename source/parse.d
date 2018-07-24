module parse;
import pegged.grammar;
import std.format;
import std.string;
import std.stdio;
import std.array;
import std.algorithm;

mixin(grammar(`
DADT:
  TypeDeclare < "type" BaseConstructor "=" ConstructorList

  BaseConstructor < TypeNameWithArgs / TypeNameWithoutArgs

  TypeName <~ !Keyword [A-Z_] [a-zA-Z0-9_]*
  
  Field < FieldWithArgs / FieldOfArray / FieldName
  FieldArgs < "()" / :"(" Field ("," Field)* :")"
  FieldWithArgs < FieldName "!" FieldArgs
  FieldOfArray < Field ArrayBracket+
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
    }
    else {
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
  FieldName fieldName;
  ArrayBracket bracket;

  this(FieldName fieldName, ArrayBracket bracket) {
    this.fieldName = fieldName;
    this.bracket = bracket;
  }

  override const string typeString() {
    string baseType = fieldName.typeString;

    if (this.bracket.isSized) {
      SizedBracket sb = cast(SizedBracket)bracket;
      return baseType ~ "[%s]".format(sb.size.size);
    }
    else {
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
  writeln("p.name : ", p.name);
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
    writeln(p.name, " <p.children> : ", p.children);
    FieldName fieldName = cast(FieldName)buildAST(p.children[0]);
    if (fieldName is null) {
      throw new Error("Error in %s!".format(p.name));
    }

    ArrayBracket ab = cast(ArrayBracket)buildAST(p.children[1]);
    if (ab is null) {
      throw new Error("Error in %s!".format(p.name));
    }

    return new FieldOfArray(fieldName, ab);
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
  if (interface_args.length) {
    args_str = "!(" ~ interface_args.join(", ") ~ ")";
  }

  code ~= "interface " ~ interface_name ~ args_str ~ "{}\n";

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
        args_str, interface_name, args_str, field_code, this_code);
  }

  return code;
}
