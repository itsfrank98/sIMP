# sIMP
sIMP is an imperative programming language that provides an amplied version of the IMP language. It follows the eager evaluation strategy, reached by using call by value.

The two main modules are the **parser** and the **interpreter**. The parser checks for the grammar correctness of the code and, if everything went fine, returns a representation of the program suitable for the parser.
The interpreter works using this representation to actually run the code. The execution of the program can either return an error if some illegal actions were performed (like trying to read from a variable that doesn't exist, or the state of the memory. The error consists of an error message and the action that caused the error. If the execution ended successfully, then the current state of the memory is returned.

<h2> Extended Backus Naur form of the supported grammar </h2>

```lang-none
program ::= com | program

com ::= declareBoolean ";"
    | declareInteger ";"
    | declareArray ";"
    | declareStack ";"
    | assignBoolean ";"
    | assignArrayPosition ";"
    | assignWholeArray ";"
    | assignInteger ";"
    | push ";"
    | pop ";"
    | ifelse
    | whiledo
    | dowhile ";"
    | skip ";"


digit ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
integer ::= {"-"} digit {digit}
letter ::= "A" | "B" | "C" | "D" | "E" | "F" | "G"
         | "H" | "I" | "J" | "K" | "L" | "M" | "N"
         | "O" | "P" | "Q" | "R" | "S" | "T" | "U"
         | "V" | "W" | "X" | "Y" | "Z" | "a" | "b"
         | "c" | "d" | "e" | "f" | "g" | "h" | "i"
         | "j" | "k" | "l" | "m" | "n" | "o" | "p"
         | "q" | "r" | "s" | "t" | "u" | "v" | "w"
         | "x" | "y" | "z"
arithmeticIdentifier ::= letter {letter | digit | "_" }
booleanIdentifier ::= letter {letter | digit | "_" }
constArr ::= "[" {AExpr ","} AExpr "]"

declareBoolean ::= "bool" booleanIdentifier {"=" BExpr}
declareInteger ::= "int" arithmeticIdentifier {"=" AExpr}
declareArray ::= "array[" AExpr "]" arithmeticIdentifier {"=" constArr}
declareStack ::= "stack" arithmeticIdentifier
assignBoolean ::= booleanIdentifier "=" BExp
assignArrayPosition ::= arithmeticIdentifier "[" AExpr "]" "=" AExpr
assignWholeArray ::= arithmeticIdentifier "=" constArr
assignInteger ::= arithmeticIdentifier "=" AExpr
push ::= "push" "(" arithmeticIdentifier "," AExpr ")"
pop ::= "pop" "(" arithmeticIdentifier ")"
ifelse ::= "if" "(" BExp ")" "{" program "}" {"else" "{" program "}"}
whiledo ::= "while" "(" BExp ")" "{" program "}"
dowhile ::= "do" "{" program "}" "while" "(" BExp ")"
skip ::= "skip"

AExpr ::= arithmeticTerm {{"+" | "-"} arithmeticTerm}

arithmeticTerm ::= arithmeticFactor {{"*" | "/" | "^"} arithmeticFactor}
arithmeticFactor ::= "(" AExpr ")"
    | integer
	| constArr
    | "top" "(" arithmeticIdentifier ")"
    | arithmeticIdentifier
    | arithmeticIdentifier "[" AExpr "]"
	
    
BExpr ::= booleanTerm {"or" booleanTerm}
booleanTerm ::= booleanFactor {"and" booleanFactor}
booleanFactor ::= "True"
	| "False"
    | "empty" "(" arithmeticIdentifier ")"
    | booleanIdentifier
    | "(" BExpr ")"
    | "!" BExpr
    | AExpr "<" AExpr
    | AExpr ">" AExpr
    | AExpr "==" AExpr
    | AExpr "<>" AExpr
    | AExpr "<=" AExpr
    | AExpr ">=" AExpr
```

<h2> Example of program written in sIMP (more examples can be found in the test section) </h2>

```
int a = 0; 
int b; 
if(a == 0){ 
	b = 1; 
} else{ 
	b = 2; 
}
```

Chack out the documentation for a more accurate description of the language.

<h2> Current limitations </h2>
At the current state, sIMP has the following limitations (as far as I can tell): <br />
* The user is allowed to use the keywords of the language (if, else and so on) as names for the variables, which is something that should not happen; <br />
* The stack operations can only be done passing the name of the stack. We can't do the following:

```
stack a;
empty(pop(push(a,1)));
```
because we will get a parser error. Moving the push and pop operations in the AExp section should solve this
