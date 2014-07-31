---
title: Julia
published: July 23, 2014
excerpt: A high-performance dynamic language for numerical computing
comments: off
toc: left
---

# Types

There are signed and unsigned integers of the form `Intx` and `Uintx` ranging from 8 to 128, as well as `Int` and `Uint` which are aliases for the type representing machine's word size, which itself is available as `WORD_SIZE`. Floating-point types take the form `Floatx` from 16 to 64.

Integer literals take on a type depending on the machine's word size, though if they are too large to fit in 32-bits then they are represented as `Int64`, regardless of the machine's word size.

Unsigned integer literals are written in hexadecimal.

Floating-point literals are inferred as `Float64` values, and `Float32` literals can be entered by using scientific exponent notation with an `f` instead of an `e`.

The `typemin` and `typemax` functions can be used to yield the minimum and maximum bounds for a type.

It's possible to find the distance between two adjacent representable floating-point numbers, a distance known as machine epsilon. The `eps` function can give the distance between 1.0 and the next larger representable floating-point value for a particular type.

The `eps` function can also take a floating-point value, in which case it returns the absolute value of difference between the value and the next representable floating-point value, so that `x + eps(x)` is the next representable floating-point value greater than `x`.

The next or previous representable floating-point number can also be retrieved using the `nextfloat` and `prevfloat` functions.

The `BigInt` and `BigFloat` types can be used to create arbitrary precision integers and floating-point numbers, which are constructed by passing the number as a string.

Variables can be immediately preceded by numeric literals and thereby imply multiplication, simplifying the writing of polynomial expressions. These are known as numeric literal coefficients. The precedence of numeric literal coefficients is the same as unary operators, so they can also precede parentheses and other things.

``` julia
2x^2 - 3x + 1
2(x-1)^2 - 3(x-1) + 1
```

Parenthesized expressions can also be used as coefficients to variables to imply multiplication of the expression by the variable.

``` julia
(x-1)x
```

The `zero` and `one` functions return a literal 0 or 1 depending on the type of the given variable or the provided type. They're useful to avoid overheard from unnecessary type conversions.

The `^` operator can be used to raise a number to a specific power. The `\` operator is the inverse divide operator, which flips the arguments of `/`. The dollar sign `$` is the XOR operator.

In Julia, comparisons can be chained.

``` julia
1 < 2 <= 2 < 3 == 3 > 2 >= 1 == 1 < 3 != 5
```

