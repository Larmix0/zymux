
# Zymux grammar documentation
This file defines the grammar (BNF) of the Zymux language.
Executing Zymux code automatically performs the "program" rule.

<br>

**program:** ```declaration*```

---

<br>

# Declaration
Either declares a name that holds some kind of structured data
or acts as a statement if there isn't any name being declared.

---

**declaration:** <br>
```variableDecl``` <br>
```| enumDecl``` <br>
```| funcDecl``` <br>
```| entryDecl``` <br>
```| classDecl``` <br>

**variableDecl:** ```("let" | "const") (name | "[" nameList "]") ("=" expression)? ";"```

**enumDecl:** ```"enum" name "{" nameList? "}"```

**funcDecl:** ```"func" name unnamedFunc```

**entryDecl:** ```"entry" block```

**classDecl:**
```"class" name ("inherits" name)? "{" ("init" unnamedFunc | name unnamedFunc)* "}"```

**unnamedFunc:** ```"(" parameters ")" block```

---

<br>

# Statements
A whole statement which modifies something in the program, but does not declare any new names.

---

**statement:** <br>
```block``` <br>
```| expressionStmt``` <br>
```| ifElseStmt``` <br>
```| matchStmt``` <br>
```| whileStmt``` <br>
```| doWhileStmt``` <br>
```| forStmt``` <br>
```| loopControlStmt``` <br>
```| importStmt``` <br>
```| fromImportStmt``` <br>
```| tryCatchStmt``` <br>
```| raiseStmt``` <br>
```| returnStmt``` <br>
```| exitStmt```

**block:** ```"{" declaration* "}"```

**expressionStmt:** ```expression ";"```

**ifElseStmt:** ```"if" expression block ("else" (block | ifElseStmt))?```

**matchStmt:** ```"match" "{" ("case" expressionList block)* ("default" block)? "}"```

**whileStmt:** ```"while" expression block```

**doWhileStmt:** ```"do" block "while" expression ";"```

**forStmt:** ```"for" (name | "[" nameList "]") "in" expression block```

**loopControlStmt:** ```"break" | "continue"```

**importStmt:** ```"import" importPath ("as" name)? ";"```

**fromImportStmt:** ```"from" importPath "import" (importedNames ";" | "{" importedNames "}")```

**importedNames:** ```(name ("as" name)?)+```

**importPath:** ```"."* (name ".")* name```

**tryCatchStmt:** ```"try" block "catch" ("as" name)? block```

**raiseStmt:** ```"raise" expression ";"```

**returnStmt:** ```"return" (expression)? ";"```

**exitStmt:** ```"exit" expression ";"```

--- 

<br>

# Expressions
expressions are constructs which resolve into some kind of value, and may or may not
change the state of the program. They're typically used inside statements and declarations.

---

**expression:** ```ternary```

**ternary:** ```assignment ("?" expression ":" expression)?```

**assignment:** ```singleAssignment | augmentedAssignment | multiAssignment | binaryLogical```

**multiAssignment:** ```"[" assignmentName ("," assignmentName)* "]" "=" expression```

**singleAssignment:** ```assignmentName "=" expression```

**augmentedAssignment:** ```assignmentName augmentedOperator expression```

**augmentedOperator:**
```"+=" | "-=" | "*=" | "/=" | "%=" | "**= | "<<=" | ">>=" | "|=" | "&=" | "^="```

**assignmentName:** ```(call ".")? name```

**binaryLogical:** ```equality ("||" | "&&" expression)```

**equality:** ```comparison (("==" | "!=") comparison)*```

**comparison:** ```range ((">" | ">=" | "<" | "<=") range)```

**range:** ```binaryBitwise (".." binaryBitwise (".." binaryBitwise)?)?```

**binaryBitwise:** ```term (("<<" | ">>" | "|" | "&" | "^") term)*```

**term:** ```factor (("+" | "-") factor)*```

**factor:** ```exponent (("*" | "/" | "%") exponent)*```

**exponent:** ```binaryDataType ("**" binaryDataType)*```

**binaryDataType:** ```unary (("is" | "as") dataTypeKeyword)*```

**unary:** ```("-" | "!" | "~") unary | call```

**call:** ```map ("(" arguments? ")" | "." name | subscript)*```

**arguments:** ```oneArgument ("," oneArgument)```

**oneArgument:** ```expression | "." name "=" expression```

**subscript:** ```"[" expression "]" ("=" expression)?```

**map:** ```"{" (mapEntry ("," mapEntry)*)? "}" | list```

**mapEntry:** ```expression ":" expression```

**list:** ```"[" expressionList? "]" | primary```

**primary:** <br>
```null``` <br>
```| bool``` <br>
```| int``` <br>
```| float``` <br>
```| string``` <br>
```| name``` <br>
```| dataTypeKeyword``` <br>
```| "this"``` <br>
```| "super" "." name``` <br>
```| "(" expression ")"```

**string:** <br>
```"\"" (anyChar | escape)* "\""``` <br>
```| "#" "\"" anyChar* "\""``` <br>
```| "$" "\"" (anyChar | interpolation | escape)* "\""``` <br>
```| ("#$" | "$#") "\"" (anyChar | interpolation)* "\""```

**interpolation:** ```"{" expression "}"```

**escape:** ```"\" | "n" | "t" | "r" | "b" | "\"```

**int:** ```digit+```

**float:** ```digit+ "." digit+```

**bool:** ```"true" | "false"```

**null:** ```"null"```

---

<br>

# Helper patterns

**expressionList:** ```expression ("," expression)*```

**nameList:** ```name ("," name)?```

**dataTypeKeyword:** ```"int" | "float" | "bool" | "string"```

**name:** ```nameChar (nameChar | digit)*```

**nameChar:** ```"a".."z" | "A".."Z" | "_"```

**digit:** ```"0".."9"```

**anyChar:** ```[Any valid character]```
