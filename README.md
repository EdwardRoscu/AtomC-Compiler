# AtomC Compiler 
This project was developed as part of a university course on compilers.
Some initial files and instructions were provided, and modifications were made to meet the project requirements.

Implements a compiler for the AtomC programming language. The compiler includes:

## Lexical Analyzer:
The lexical analyzer is responsible for breaking the input source code into a sequence of tokens.
It scans the input source code and converts it into a sequence of tokens that can be used by the other components of the compiler.

## Syntactic Analyzer:
The syntactic analyzer takes the token sequence generated by the lexical analyzer and checks if the sequence of tokens corresponds to a valid program according to the AtomC programming language grammar.
If the sequence is valid, it generates an abstract syntax tree (AST) that represents the program's structure.

## Semantic Analysis:
The semantic analysis component checks the AST generated by the syntactic analyzer for semantic errors.
This includes checking for variable declarations, function declarations, and type mismatches.

## Type Analysis:
The type analysis component checks the AST for type mismatches and ensures that the types used in the program are consistent with the AtomC programming language's type system.