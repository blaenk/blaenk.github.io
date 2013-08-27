---
title: LLVM
published: July 15, 2013
excerpt: Notes on creating a language with LLVM
---

* toc

I finally found the time to read the excellent [LLVM tutorial](http://llvm.org/docs/tutorial/) in which a toy language -- Kaleidoscope -- is implemented completely from scratch, without the use of an existing lexer (e.g. Lex/Flex) and parser (e.g. Yacc/Bison). A lexer is created to tokenize the input and parsed into an abstract syntax tree (AST). The tree is then recursively descended to generate LLVM IR using the LLVM code-gen builder.

*[AST]: Abstract Syntax Tree
