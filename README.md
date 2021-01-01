# sIMP
sIMP is an imperative programming language that provides an amplied version of the IMP language. It follows the eager evaluation strategy, reached by using call by value.
The two main modules are the parser, and the interpreter. The parser checks for the grammar correctness of the program and, if everything went fine, returns a representation of the program.
The interpreter works using this representation to actually run the code. The execution of the program can either return an error if some illegal actions were performed (like trying to read from a variable that doesn't exist, or the state of the memory. The error consists of an error message and the action that caused the error. If the execution ended successfully, then the current state of the memory is returned. 
