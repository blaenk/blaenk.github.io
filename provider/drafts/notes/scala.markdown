---
title: Scala
published: October 12, 2013
excerpt: Promising mixture of OO and FP
comments: off
---

The syntax for a function definition is:

``` scala
def max(a: Int, y: Int): Int = if (x > y) x else y
```

If a function only takes one parameter, it can be called without parentheses. Functions can use special characters such as `+` and `-`, and can transform:

``` scala
1 + 2
(1).+(2)
```

In fact, any function that takes at least two parameters can be called in infix operator notation. If it takes more than two parameters, the right hand side should apply those within parentheses.

A unary operator can be defined by defining a method with `unary_` prepending the name.

If the function ends with `:`, then the code is transformed in reverse:

``` scala
a :: b
b.::(a)
```

A class can be defined as:

``` scala
class MyClass(index: Int, name: String)
```

This also defines a constructor that takes integer and string parameters.

Values can be defined in two ways: `val` are immutable values and `var` are mutable variables.

Arrays are indexed using parentheses, not brackets as in other languages. They're defined as:

``` scala
val strings = new Array[String](3)
strings(0) = "Hello"
strings(1) = "World"

for (i <- 0 to 2)
  print(strings(i))
```

Lambdas are of the form:

``` scala
args.foreach((arg: String) => println(arg))
```

Though Scala allows partial function application, so the above can be shortened to:

``` scala
args.foreach(println)
```

When parentheses are applied to a variable with one or more parameters, Scala transforms the code into an invocation of the `apply` method on that variable:

``` scala
strings(i)
strings.apply(i)
```

On the other hand, when a variable with applied parentheses and arguments is assigned to, the code is transformed to a call to the `update` method:

``` scala
strings(i) = "assignment"
strings.update(i, "assignment")
```

In Scala, Arrays are mutable while Lists are immutable. With Lists, `::` is cons and `:::` is concatenation.

Tuples are declared in the usual form, as in Haskell. However, elements are accessed using `_n` where n is the 1-based number of the element.

The `->` method returns a two-element tuple of the left and right parameters, and is used for example to insert items into a `Map`:

``` scala
val treasureMap = Map[Int, String]()
treasureMap += (1 -> "Go to island")
```

The `Unit` type is equivalent to Haskell's `()` or `void` in C++.

Scala has symbols similar to Ruby's `:symbols` which are defined as:

``` scala
val someKey = 'symbol
```

# Classes and Objects

Objects have members consisting of fields (data) and methods (code). Fields can be declared private.

If you assign an object to a `val`, you can mutate the object but not reassign the `val` to another object. Unit methods can leave off the result type and the equals sign:

``` scala
def add(b: Byte): Unit = sum += b
def add(b: Byte) { sum += b }
```

The semicolon insertion rules state that a semicolon is inserted at the end of a line if:

1. the line ends in a word that would not be legal as the end of a statement (e.g. period or infix operator)
2. next line begins with a word that cannot start a statement
3. line ends while inside parentheses or brackets, because these cannot contain multiple statements anyway

Singleton objects are defined using the `object` keyword and their members are as if `static` members of C++ classes. These cannot be instantiated.

``` scala
object Singleton {
  private val num = 0
}
```

When a singleton object is named after an existing class, it is referred to as the class' **companion object** [^companion_ruby]. They must both be defined in the same source file. They can access each other's private members.

A class is defined using the class keyword, and it can take parameters:

``` scala
class Rational(num: Int, den: Int)
```

Any code inside the class body will be put inside the **primary constructor**.


**Fields**, accessible publicly, are created by defining class-level values:

``` scala
class Rational(num: Int, den: Int) {
  val numer: Int = num
  val denom: Int = den
}
```

**Auxiliary constructors** are additional constructors that simply directly or indirectly delegating object construction to the primary constructor. They are named after `this`, and their first action must be invoking another constructor:

``` scala
class Rational(num: Int, den: Int) {
  def this(n: Int) = this(n, 1)
}
```

Fields and methods can be defined private using the `private` keyword.

**Literal identifiers** are ones that are enclosed in backticks, so that any string that would be accepted by the runtime will result in an identifier.

Implicit conversions can be defined so that values of a certain time are implicitly converted to another type in order for an operation to go through. For example, if there's an addition function for `Rational` that takes a `Rational` and an Integer, we can do `r + 1` but not `1 + r` since that would attempt to invoke the function on the Integer itself. To solve this, we can define an implicit conversion to `Rational` on Integer:

``` scala
implicit def intToRational(x: Int) = new Rational(x)
```

# Functions

## Partial Application

It's not possible to assign methods or nested functions to values, or pass them as arguments to other functions. If this is intended, one can use the explicit partial application syntax:

``` scala
val partial = some.method _
```

This syntax creates an ephemeral class [^cpp_partialapplication] that defines an `apply` method that takes the appropriate amount of arguments. When the partially applied value is called, the arguments are forwarded to the underlying function and its result is returned.

This can also be done for specific arguments, but the type must be stated explicitly, presumably to disambiguate overloads:

``` scala
val partial = func(1, _ : Int, 3)
```

If passing a partially applied function, where all parameters are unapplied, as an argument to another function, partial application can be written implicitly:

``` scala
someNumbers.foreach(println _)
someNumbers.foreach(println)
```

Closures capture values by reference.

[^companion_ruby]: Reminds me of Ruby's 'EigenClasses', but I'm not quite sure yet if it's indeed similar, or if companion objects truly are just a separation for specifying class-wide values/methods.
[^cpp_partialapplication]: Reminds me of `std::bind` in C++11, which does the same thing by creating a [functor](http://en.wikipedia.org/wiki/Function_object#In_C_and_C.2B.2B), or function object --- not to be confused with the category theory [Functor](http://en.wikipedia.org/wiki/Functor) more common in [Haskell](http://www.haskell.org/haskellwiki/Functor).
