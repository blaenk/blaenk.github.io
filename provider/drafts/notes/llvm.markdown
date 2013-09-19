---
title: LLVM
published: July 15, 2013
excerpt: Creating an LLVM-backed language from scratch
---

I finally found the time to read the excellent [LLVM tutorial](http://llvm.org/docs/tutorial/) in which a toy language -- Kaleidoscope -- is implemented completely from scratch, without the use of an existing lexer (e.g. Lex/Flex) and parser (e.g. Yacc/Bison). A lexer is created to tokenize the input and parsed into an abstract syntax tree (AST). The tree is then recursively descended to generate LLVM IR using the LLVM code-gen builder.

* [LLVM Conference](http://llvm.org/devmtg/2010-11/)
* [Clang Tutorial](http://amnoid.de/tmp/clangtut/tut.html)
* [Clang Tutorials](https://github.com/loarabia/Clang-tutorial)
* [Parsing C++ with Python and Clang](http://eli.thegreenplace.net/2011/07/03/parsing-c-in-python-with-clang/)

*[AST]: Abstract Syntax Tree
