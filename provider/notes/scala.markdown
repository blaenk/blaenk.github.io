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

The `yield` keyword can be used in conjunction with `for` loops to generate new collections:

``` scala
val shouts = for (arg <- args) yield arg + "!"
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

Variable arguments can be specified with an asterisk `*`, and are treated as an `Array` inside the function:

``` scala
def echo(args: String*) = for (arg <- args) println(arg)
echo("one", "two", "three")
```

Conversely, an `Array` can be expanded into multiple arguments using the `_*` symbol:

``` scala
val arr = Array("one", "two", "three")
echo(arr: _*)
```

It's possible to pass named parameters to functions:

``` scala
def speed(distance: Float, time: Float): Float = distance / time
speed(time = 10, distance = 100)
```

It's possible to define default parameter values:

``` scala
def printTime(out: java.io.PrintStream  Console.out) =
  out.println("time = " + System.currentTimeMillis())
```

When used in combination with named parameters, it's possible to leave out all parameters except one.

Scala supports tail recursion.

Currying can be made explicit in a function definition by adding multiple parameter lists. This makes it possible to curry functions without having to specify the type of the holes as in partial application above.

``` scala
def curriedSum(x: Int)(y: Int) = x + y
val three  = curriedSum(1)(2)

def equivalentSum(x: Int) = (y: Int) => x = y
val three_ = equivalentSum(1)(2)
```

Explicitly defining multiple parameter lists allows using braces for the last parameter:

``` scala
val braces = curriedSum(1) { 2 }
```

Parameters can be passed "by-name" so that parameters can be passed to a function and not evaluated until explicitly done so within the function. The syntax is simply to prepend the parameter with `=>`, I take this to mean that it's wrapping the parameter in a parameter-less lambda like `()` so that the expression passed as the parameter isn't evaluated until explicitly done so within the function:

``` scala
def byNameAssert(predicate: => Boolean) =
  if (assertionsEnabled && !predicate)
    throw new AssertionError

byNameAssert(5 > 3)
```

# Composition and Inheritance

Abstract classes are defined using the abstract keyword:

``` scala
abstract class Element {
  def contents: Array[String]
  def height: Int = contents.length
  def width: Int = if (height == 0) 0 else contents(0).length
}
```

Methods themselves are abstract if they have no defined implementation.

Parameterless methods are those that take no parameters and their parentheses are omitted. The convention is to omit the parentheses when there are no parameters and the method accesses mutable state only by reading its class' fields. Further, empty parentheses in function calls can be omitted. The convention is to only omit them if they have no side-effects. I/O calls for example should always explicitly contain the parentheses.

Classes can be extended other classes. This allows the sub-class to inherit all non-private members of the parent class, and makes the sub-class a sub-type of the parent type. All classes implicitly extend from `scala.AnyRef` which is equivalent to Java's `java.lang.Object`:

``` scala
class ArrayElement(conts: Array[String]) extends Element {
  def contents: Array[String] = conts
}
```

Class fields and methods belong to the same namespace, which makes it possible for a subclass to override one with the other:

``` scala
class ArrayElement(conts: Array[String]) extends Element {
  val contents: Array[String] = conts
}
```

Parametric fields are fields that are initialized through the primary constructor:

``` scala
class ArrayElement(
  val contents: Array[String]
) extends Element
```

It's possible to invoke superclass constructors by simply supplying the parameter in the extends section:

``` scala
class LineElement(s: String) extends ArrayElement(Array(s)) {
  override def width  = s.length
  override def height = 1
}
```

Overriding must be explicitly denoted.

The `final` keyword can be used to prevent subclass overriding on a particular method:

``` scala
class A extends B {
  final override def method() { println "and that's final" }
}
```

The `final` keyword can also be used to prevent subclassing of a particular class entirely:

``` scala
final class A extends B {
  override def method() { println "final class" }
}
```

# Hierarchy

Every class is a direct or indirect subclass of `Any`, which defines a variety of "universal methods" available to all subclasses. The `==` method is final and simply calls `equals`, which is the method that subclasses should override.

`Any` has two direct subclasses. `AnyVal` is the parent class of every built-in value class in Scala: `Byte`, `Short`, `Char`, `Int`, `Long`, `Float`, `Double`, `Boolean`, and `Unit`. These can't be instantiated with `new` because they are defined as final and abstract. `AnyRef` is the base class of all reference classes, it's just an alias for `java.lang.Object`.

There are two "bottom" types, which are subclasses of every kind of type. `Null` is the type of the `null` reference and it inherits from any class that inherits from `AnyRef`. `Nothing` is a type that simply signifies abnormal termination. For example, the `error` function is of type `Nothing`. Since it's a subclass of any class, the `error` function can be called from within any other function regardless of its type.

# Traits

Traits encapsulate methods and fields which can be mixed into other classes [^ruby_mixins]. Traits are defined as:

``` scala
trait SomeTrait {
  def someMethod() {
    println("printing")
  }
}
```

A trait can subclass a certain class, which essentially specifies a constraint on the trait such that it can only be mixed into classes that are or subclass the extended class.

Traits can be mixed in using either the `extends` or `with` keywords, multiple mixins are simply chained using `with`. They also defined types, so a value of a certain trait type can be set to any object whose class mixes-in the particular trait.

Traits can override methods implemented in the classes they're mixed into. This is accomplished with the `abstract override` directive. This combination of specifiers conveys the fact that it overrides a method that it's mixed into (hence `override`), and therefore that method must be defined in the class it's mixed into (hence `abstract`). Methods with these qualifiers can use `super` to access the class they're mixed into, specifically the same method they're overriding:

``` scala
class BasicIntQueue extends IntQueue {
  private val buf = new ArrayBuffer[Int]
  def get() = buf.remove(0)
  def put(x: Int) { buf += x }
}

trait Doubling extends IntQueue {
  abstract override def put(x: Int) { super.put(2 * x) }
}
```

[^companion_ruby]: Reminds me of Ruby's 'EigenClasses', but I'm not quite sure yet if it's indeed similar, or if companion objects truly are just a separation for specifying class-wide values/methods.
[^cpp_partialapplication]: Reminds me of `std::bind` in C++11, which does the same thing by creating a [functor](http://en.wikipedia.org/wiki/Function_object#In_C_and_C.2B.2B), or function object --- not to be confused with the category theory [Functor](http://en.wikipedia.org/wiki/Functor) more common in [Haskell](http://www.haskell.org/haskellwiki/Functor).
[^ruby_mixins]: Reminds me of Ruby's modules that can be included

