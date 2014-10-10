---
title: Scala
published: October 12, 2013
excerpt: Promising mixture of OO and FP
comments: off
toc: left
---

I've been meaning to learn Scala for some time. Haskell has really left me wanting to program in a very functional style, and it seems like Scala is used more in the industry than Haskell. Scala seems to provide a decent compromise in a language that mixes object-oriented programming with functional programming characteristics. Furthermore, the JVM is battle-tested and probably the most robust virtual machine of any language out there at the moment.

*[JVM]: Java Virtual Machine

* toc

# Basics

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

Raw strings are possible by using three successive double quote delimiters:

``` scala
val rawstr = """\d+"""
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

When a singleton object is named after an existing class, it is referred to as the class' _companion object_ [^companion_ruby]. They must both be defined in the same source file. They can access each other's private members.

[^companion_ruby]: Reminds me of Ruby's 'EigenClasses', but I'm not quite sure yet if it's indeed similar, or if companion objects truly are just a separation for specifying class-wide values/methods.

A class is defined using the class keyword, and it can take parameters:

``` scala
class Rational(num: Int, den: Int)
```

Any code inside the class body will be put inside the _primary constructor_.


_Fields_, accessible publicly, are created by defining class-level values:

``` scala
class Rational(num: Int, den: Int) {
  val numer: Int = num
  val denom: Int = den
}
```

_Auxiliary constructors_ are additional constructors that simply directly or indirectly delegate object construction to the primary constructor. They are named after `this`, and their first action must be invoking another constructor:

``` scala
class Rational(num: Int, den: Int) {
  def this(n: Int) = this(n, 1)
}
```

Fields and methods can be defined private using the `private` keyword.

_Literal identifiers_ are ones that are enclosed in backticks, so that any string that would be accepted by the runtime will result in an identifier.

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

[^cpp_partialapplication]: Reminds me of `std::bind` in C++11, which does the same thing by creating a functor, or [function object] (not to be confused with the category theory [Functor] more common in [Haskell]).

[function object]: http://en.wikipedia.org/wiki/Function_object#In_C_and_C.2B.2B
[Functor]: http://en.wikipedia.org/wiki/Functor
[Haskell]: http://www.haskell.org/haskellwiki/Functor

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
def printTime(out: java.io.PrintStream) =
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

[^ruby_mixins]: Reminds me of Ruby's modules that can be included.

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

Multiple mixins can be "stacked," in which case the same function is called in reverse order of the mixin list. That is, the right-most trait is treated as if a "superclass" of the one to its left, and `super` has that effect.

# Packages

The `package` statement is used as in Java to specify that what follows is to be part of the specified package. However, it can also be used with braces to only insert the contained code within the package.

The `import` statement can be used to import packages and symbols in different ways:

``` scala
def showFruit(fruit: Fruit) {
  import fruit._
  println(name + "s are " + color)
}

// only Apple and Orange
import Fruits.{Apple, Orange}

// everything, but rename Apple to McIntosh
import Fruits.{Apple => McIntosh, _}

// everything except Pear
import Fruits.{Pear => _, _}
```

Every Scala file implicitly imports the following packages:

``` scala
import java.lang._
import scala._
import Predef._
```

# Testing

Assertions can be made with `assert` and enabled in the JVM using the command options `-ea` (enable assertions) and `-da`. An alternative function is `ensuring`, which also takes a predicate. The predicate is a function that takes a so-called "result type" and returns a Boolean. If the predicate returns true, then `ensuring` returns the "result type," but if the predicate returns false, then `ensuring` throws an `AssertionError`.

Unit testing is possible with tools such as [ScalaTest](http://www.scalatest.org) and ScalaCheck. ScalaTest tests are functions with names prefixed by `test` inside classes that extend `Suite`:

``` scala
import org.scalatest.Suite
import Element.elem

class ElementSuite extends Suite {
  def testUniformElement() {
    val ele = elem('x', 2, 3)
    assert(ele.width == 2)
  }
}
```

ScalaTest has another style of testing with the FunSuite package:

``` scala
import org.scalatest.FunSuite
import Element.elem

class ElementSuite extends FunSuite {
  test("elem result should have passed width") {
    val ele = elem('x', 2, 3)
    assert(ele.width == 2)
  }
}
```

The assertions used in the previous two ScalaTest suites would only show an error that the assertion failed, without showing what the two values were. ScalaTest also provides a `===` operator that is more descriptive:

``` scala
assert(ele.width === 2) // "3 did not equal 2"
```

There is also an `expect` method that differentiates between the two values:

``` scala
expect(2) {
  ele.width
} // "Expected 2, but got 3"
```

Method `intercept` can verify that an exception is thrown:

``` scala
intercept[IllegalArgumentException] {
  elem('x', -2, 3)
}
```

## Behavior-Driven Development

The `FlatSpec` is one of many traits, such as [`FunSpec`](http://www.scalatest.org/getting_started_with_fun_spec), that allow for behavior-driven development (BDD). These traits can be mixed with other traits such as `ShouldMatchers` which allow the use of the `should` function to write tests very naturally as in RSpec:

*[BDD]: Behavior-Driven Development

``` scala
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import Element.elem

class ElementSpec extends FlatSpec with ShouldMatchers {
  "A UniformElement" should "have a width equal to the passed value" in {
      val ele = elem('x', 2, 3)
      ele.width should be (2)
    }

  it should "have a height equal to the passed value" in {
    val ele = elem('x', 2, 3)
    ele.height should be (3)
  }

  it should "throw an IAE if passed a negative width" in {
    evaluating {
      elem('x', 2, 3)
    } should produce [IllegalArgumentException]
  }
}
```

## Property Testing

[ScalaCheck](http://www.scalacheck.org/) is another testing tool that is similar to Haskell's [QuickCheck](http://hackage.haskell.org/package/QuickCheck), which essentially randomly generates test input data to test with. There is an implication operator `==>` that takes a function that places a constraint on the test data and a condition that must hold true for all data that fits that constraint:

``` scala
import org.scalatest.WordSpec
import org.scalatest.prop.Checkers
import org.scalacheck.Prop._
import Element.elem

class ElementSpec extends WordSpec with Checkers {
  "elem result" must {
    "have passed width" in {
      check((w: Int) => w > 0 ==> (elem('x', w, 3).width == w))
    }
  }
}
```

# Pattern Matching

_Case Classes_ are like stub classes which contain and are mainly about publicly accessible data. They're very similar to Haskell's algebraic data types (ADT). Consider the following domain specific language (DSL):

*[ADT]: Algebraic Data Type
*[DSL]: Domain Specific Language

``` scala
abstract class Expr
case class Var(name: String) extends Expr
case class Number(num: Double) extends Expr
case class UnOp(operator: String, arg: Expr) extends Expr
case class BinOp(operator: String, left: Expr, right: Expr) extends Expr
```

These case classes define factory methods to avoid having to explicitly write `new`, fields for the data, simple `toString` implementations, and a `copy` method that is very similar to Haskell's record syntax. For example, the following creates a copy of the `op` `BinOp` with the `operator` field changed to `"-"`:

``` scala
op.copy(operator = "-")
```

This is equivalent to Haskell's record syntax:

``` haskell
op { operator = "-" }
```

The main use of case classes is pattern matching. For example, a mathematical expression expressed using the `Expr` DSL constructed above can be simplified as follows:

``` scala
def simplifyTop(expr: Expr): Expr =
  expr match {
    case UnOp("-", UnOp("-", e))  => e
    case BinOp("+", e, Number(0)) => e
    case BinOp("*", e, Number(1)) => e
    case _ => expr
  }
```

This is very similar to other pattern matching features available in Haskell for example. In cases, we can use _constructor patterns_, _constant patterns_, and _variable patterns_. Variable patterns are simply those that begin with lower case characters, but we can force a lowercase identifier to be treated as a constant by surrounding it with backticks.

It's also to use _sequence patterns_. In this case, the `_*` operator can be used to specify "the rest of the sequence," like so:

``` scala
expr match {
  case List(0, _*) => println("found it")
  case _ =>
}
```

It's also possible to use _type patterns_ to match the type of the expression:

``` scala
def generalSize(x: Any) = x match {
  case s: String => s.length
  case m: Map[_, _] => m.size
  case _ => -1
}
```

_Type Erasure_ means that information about type arguments is not maintained at runtime. This is an artifact of the erasure model of generics that Java uses. As a result, it's not possible to pattern match on the type of `Map`, and the following code will return `true` for any `Map`:

``` scala
def isIntToIntMap(x: Any) = x match {
  case m: Map[Int, Int] => true
  case _ => false
}
```

It's possible to use `@` for _variable binding_ in pattern matching, just as in Haskell:

``` scala
expr match {
  case UnOp("abs", e @ UnOp("abs", _)) => e
  case _ =>
}
```

_Pattern guards_ are additional conditions placed on pattern matches. For example, to simplify an addition expression with identical operands to a multiplication by two, we can use:

``` scala
def simplifyAdd(e: Expr) = e match {
  case BinOp("+", x, y) if x == y =>
    BinOp("*", x, Number(2))
  case _ => e
}
```

A _sealed class_ is one that restricts subclassing of that class to the file it's defined in. This way the compiler can guarantee and enforce exhaustive pattern matching, which it couldn't do otherwise because the class could be extended in another file:

``` scala
sealed abstract class Expr
```

If there's a function that is known to only handle a specific subset of subtypes, a catchall case can be added, or the `@unchecked` annotation can be used:

``` scala
def describe(e: Expr): String = (e: @unchecked) match {
  case Number(_) => "a number"
  case Var(_) =>    "a variable"
}
```

The `Option` type is equivalent to Haskell's `Maybe` type. It can take on either a parameterized `Some` value or `None`.

<a name="case-sequence"></a>
A _case sequence_ is a function literal specific defined as a pattern match where each case is an entry point to the function:

``` scala
val withDefault: Option[Int] => Int = {
  case Some(x) => x
  case None => 0
}
```

This is why it's possible to pass a pattern match directly to the `react` function in the actors library:

``` scala
react {
  case (name: String, actor: Actor) => {
    actor ! getip(name)
    act()
  }
  case msg => {
    println("Unhandled message: " + msg)
    act()
  }
}
```

If a case sequence doesn't provide exhaustive patterns, it is considered a _partial function_ since it doesn't provide an output for every input. If it's applied to a value that it can't match, it throws a run-time exception.

The parameterized type `PartialFunction` can represent partial functions:

``` scala
val second: PartialFunction[List[Int],Int] = {
  case x :: y :: _ => y
}
```

The function `isDefinedAt` can then be used to determine if the function is defined for a particular value:

```
scala> second.isDefinedAt(List(5, 6, 7))
res0: Boolean = true
scala> second.isDefinedAt(List())
res0: Boolean = false
```

The partial function above gets translated to:

``` scala
new PartialFunction[List[Int],Int] {
  def apply(xs: List[Int]) = xs match {
    case x :: y :: _ => y
  }
  def isDefinedAt(xs: List[Int]) = xs match {
    case x :: y :: _ => true
    case _ => false
  }
}
```

The actors library's `react` function for example uses partial a partial argument function, since it's defined only for the messages teh caller wants to handle.

# Tuples

Tuples are the same as in Haskell, C++11, Python etc. They are indexed using `._n` where `n` it the nth tuple element. A difference from something like Python is that the following:

``` scala
val word, index = longest
```

Where `longest` is a tuple, ends up assigning the tuple to both `word` and `index`, contrary to what happens in Python. To assign the correct elements as in C++11's `std::tie`, use parentheses to pattern match as in Haskell.

# Stateful Objects

Scala has support for something similar to C# properties. When a non-private `var` is defined in a class, a pair of getter and setters is automatically generated for that variable. For example, given the following declaration:

``` scala
class Time {
  var hour   = 12
  var minute = 0
}
```

Changes the variable to be `private[this]` so that it's only accessible from the object itself, and the getters and setters take on the visibility of the original variable, this in effect restricts all external access to its generated getters and setters. The setter takes the form `x_=(arg)` [^ruby_setter].

[^ruby_setter]: This is of course very similar to setters in Ruby, which take the form `attr=(arg)`.

``` scala
class Time {
  private[this] var h = 12
  private[this] var m = 0

  def hour: Int = h
  def hour_=(x: Int) { h = x }

  def minute: Int = m
  def minute_=(x: Int) { m = x }
}
```

These getters and setters can be defined explicitly in order to encode things such as input validation:

``` scala
class Time {
  private[this] var h = 12
  private[this] var m = 0

  def hour: Int = h
  def hour_=(x: Int) {
    require(0 <= x && x < 24)
    h = x
  }

  def minute: Int = m
  def minute_=(x: Int) {
    require(0 <= x && x < 60)
    m = x
  }
}
```

It's also possible to define getters and setters that aren't backed by a variable, which is particularly useful for converter functions, for example. Note that in the following example, `_` is used to give the `celsius` variable a default value (0 for numeric types) [^monoid_default_value].

[^monoid_default_value]: This reminds me of a [Monoid]'s identity, `mempty` in the Haskell typeclass, but I doubt that `_` is backed by a Monoid typeclass because a Monoid would also require an associative binary operation. Perhaps it's simply more like the [default] package's default typeclass.

[Monoid]: http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-Monoid.html
[default]: http://hackage.haskell.org/package/data-default

``` scala
class Thermometer {
  var celsius: Float = _

  def fahrenheit = celsius * 9 / 5 + 32
  def fahrenheit_=(f: Float) {
    celsius = (f - 32) * 5 / 9
  }

  override def toString = fahrenheight + "F/" + celsius + "C"
}
```

# Type Parameterization

It's possible to hide the primary constructor by making it private. This makes its type usable but not its constructor, which can only be used from the class itself, such as through an auxiliary constructor, or its companion object.

``` scala
class Queue[T] private (
  private val leading:  List[T],
  private val trailing: List[T]
)
```

For example, a factory method can be created in the companion object:

``` scala
object Queue {
  def apply[T](xs: T*) = new Queue[T](xs.toList, Nil)
}
```

Another way is to define a _generic trait_, i.e. one that is parameterized, and hide the implementation inside the companion object, along with a factory method in the companion object:

``` scala
trait Queue[T] {
  def head: T
  def tail: Queue[T]
  def enqueue(x: T): Queue[T]
}

object Queue {
  def apply[T](xs: T*): Queue[T] = new QueueImpl[T](xs.toList, Nil)

  private class QueueImpl[T](
    private val leading:  List[T],
    private val trailing: List[T]
  ) extends Queue[T] {
    def mirror = ...
    def tail = ...
    def enqueue = ...
  }
}
```

## Variance

_Variance_ refers to inheritance relationships of parameterized types, such as whether `Set[String]` is a subtype of `Set[AnyRef]`.

For example, if `S` is a subtype of `T` and `Queue[S]` is considered a subtype of `Queue[T]`, then `Queue` is _covariant_ in its type parameter `T`. This would mean for example that we could pass `Queue[String]` to a method that accepted types `Queue[AnyRef]`.

However, generic types are _nonvariant_ by default, meaning that there would be no such subtype relationship. It's possible to annotate the type parameter as being _covariant_ by prepending the type parameter with `+`:

``` scala
trait Queue[+T] { ... }
```

_Contravariance_ would mean that if `T` is a subtype of `S`, then `Queue[S]` is a subtype of `Queue[T]`. It's also possible to annotate the type parameter to be _contravariant_ by using the `-` annotation:

``` scala
trait Queue[-T] { ... }
```

In order to make `Queue` covariant, it's necessary to specify a lower bound on the `enqueue` method and make it polymorphic as well. The lower bound enforces the requirement that `U` be a supertype of `T`.

``` scala
class Queue[+T] (
  private val leading:  List[T],
  private val trailing: List[T]
) {
  def enqueue[U :> T](x: U) = new Queue[U](leading, x :: trailing)
}
```

This means that given supertype `Fruit` and subtypes `Apple` and `Orange`, an `Orange` can be appended to a `Queue[Apple]`, yielding a `Queue[Fruit]` result.

There are also upper bounds which enforce the requirement that a type be a subtype of another, in the example below, it means that `T` must be a subtype of `Ordered[T]`:

``` scala
def orderedMergeSort[T <: Ordered[T]](xs: List[T]): List[T] = ...
```

# Abstract Members

A member of a class or trait is _abstract_ if it doesn't have a complete definition within the class. The implementations are meant to be defined in subclasses. Unlike other object-oriented languages, it's also possible to declare abstract fields and even abstract types. It's possible to declare abstract types, methods, `val`s and `var`s:

``` scala
trait Abstract {
  type T
  def transform(x: T): T
  val initial: T
  var current: T
}
```

This can then be implemented in a subclass:

``` scala
class Concrete extends Abstract {
  type T = String
  def transform(x: String) = x + x
  val initial = "hi"
  var current = initial
}
```

It's possible to override abstract methods with a concrete `val`, since the `val` will yield the same value every time, whereas the reverse can't be guaranteed.

Abstract `var`s provide implicit getters and setters for certain values.

Class parameter arguments are evaluated _before_ being passed to the class constructor, but concrete `val` definitions are evaluated _after_ the superclass is initialized. The following yields an error for failing to satisfy the requirement, since at the time the requirement is checked, the value of `denomArg` is still 0, since it's defined in the subclass.

``` scala
trait RationalTrait {
  val numerArg: Int
  val denomArg: Int
  require(denomArg != 0)
}

val x = 2

// defines an anonymous class that mixes in RationalTrait
// with its definition enclosed
new RationalTrait {
  val numerArg = 1 * x
  val denomArg = 2 * x
}

// yields java.lang.IllegalArgumentException: requirement failed
```

Pre-initialized fields are one way to solve this problem by defining certain subclass fields before the superclass is called:

``` scala
// anonymous class
new {
  val numerArg = 1 * x
  val denomArg = 2 * x
} with RationalTrait

// object definition
object twoThirds extends {
  val numerArg = 2
  val denomArg = 3
} with RationalTrait
```

The other way to solve this problem is using lazy `val`s, which defers the evaluation of the `val`'s expression until the first time it the `val` is used, much like in Haskell.

``` scala
trait LazyRationalTrait {
  val numerArg: Int
  val denomArg: Int
  lazy val numer = numerArg / g
  lazy val denom = denomArg / g
  private lazy val g = {
    require(denomArg != 0)
    gcd(numerArg, denomArg)
  }
}
```

Abstract types are useful when there are abstract methods which take on parameters of types specific to the subclass. For example, it wouldn't make sense to have the following because it would mean that we could pass any type of `Food` to any `Animal` subclass.

``` scala
class Food
abstract class Animal {
  def eat(food: Food)
}
```

Instead of defining the `eat` method as taking a `Food` parameter, we could define it to take some abstract type with an upper bound enforcing that it is a subclass of `Food`. This way, a subclass of `Animal` could explicitly specify the type of `Food` it eats.

``` scala
class Food
abstract class Animal {
  type SuitableFood <: Food
  def eat(food: SuitableFood)
}
```

A _path-dependent type_ is one that depends on its path, i.e. `some.object.Type`. Such a type can be instantiated using that syntax, since it implies a reference to `Type`'s outer object, particularly `object`. The syntax `Outer#Inner` can't be used to instantiate `Inner` since it doesn't refer to any instance of `Outer`.

## Structural Subtyping

_Structural subtyping_ is when two types get a subtyping relationship because they have the same members [^go_interfaces]. This is contrasted to the more traditional _nominal subtyping_, where each type has a name they have an explicitly declared subtyping relationship. Structural subtyping is achieved in Scala using _refinement types_.

[^go_interfaces]: This reminds me of [Go's interfaces].

[Go's interfaces]: /notes/go/#interfaces

For example, to create a `Pasture` class full of `Animal`s that eat `Grass`, we can define:

``` scala
Animal { type SuitableFood = Grass }
class Pasture {
  var animals: List[Animal { type SuitableFood = Grass }] = Nil
}
```

It can also be used to generalize a `using` method, such as Python's `with` syntax, so that it works on any object that has a `close` method. The first attempt at generalization wouldn't work because `T` could be any type, even one that _doesn't_ have a `close` method. This can be fixed by specifying an upper bound consisting of a refinement type. Note that if no base type is specified, like `Animal` preceding the braces above, then Scala uses `AnyRef`

``` scala
def using[T, S](obj: T)(operation: T => S) = {
  val result = operation(obj)
  obj.close() // type error
  result
}

def using[T, <: { def close(): Unit }, S](obj: T)(operation: T => S) = {
  val result = operation(obj)
  obj.close()
  result
}
```

## Enumerations

Enumerations in Scala aren't defined at the language-level. Instead there is a class `scala.Enumeration` that can be used to define enumerations, which works due to path-dependent types. This means that `Color.Value` would be different from `Direction.Value` becuase their parts differ:

``` scala
object Color extends Enumeration {
  val Red, Green, Blue = Value
}

object Direction extends Enumeration {
  val North = Value("North")
  val East  = Value("East")
  val South = Value("South")
  val West  = Value("West")
}

for (d <- Direction.values) print (d + " ")

Direction.East.id == 1
Direction(1)      == Direction.East
```

# Implicit Conversions and Parameters

Implicit conversions [^implicit_conversions_opinion] can be used to make two independent libraries interoperate in a simple manner. For example, using Swing in Scala would look something like this:

[^implicit_conversions_opinion]: Now that I've learned what implicit conversions are in Scala, I have formed an opinion about them. Implicit conversions are one of the parts that make C++ [very complex]. Scala's implicit conversions don't seem as complex as C++'s, here they're implicit at the site of use, but explicit at the site of definition. In C++ it feels that they're implicit on both ends, since you can have overloaded conversion operators and conversion constructors on either type, which is further made ambiguous with arithmetic type conversions.

[very complex]: /notes/cpp/#conversion-ambiguity

``` scala
val button = new JButton
button.addActionListener(
  new ActionListener {
    def actionPerformed(event: ActionEvent) {
      println("pressed!")
    }
  }
)
```

However, code written this way reflects Java's limitations and is therefore not idiomatic Scala. Using implicit conversions it can be possible to write it something like this:

``` scala
button.addActionListener(
  (_: ActionEvent) => println("pressed!")
)
```

This is done using an implicit conversion function like this one. What happens is that Scala compiles it normally and encounters a type error in the above code, so it checks if there's an implicit conversion function of the correct type, `ActionEvent => Unit`, and if it works then it continues compilation:

``` scala
implicit def function2ActionListener(f: ActionEvent => Unit) =
  new ActionListener {
    def actionPerformed(event: ActionEvent) = f(event)
  }
```

There are a variety of rules concerning implicit definitions.

* Only functions marked as `implicit` are tried.
* The implicit conversion must be in the scope as a single identifier, i.e. not `some.convert`. This is why some libraries include a `Preamble` object which often contains useful implicit conversions which can be imported with `import.Preamble._`
    * The _exception_ to this rule is that the compiler also looks inside the companion object of the source or target types of the conversion.
* The compiler only attempts one implicit conversion, i.e. it won't attempt converting `x + y` into `convert1(convert2(x)) + y`.

Implicit conversions are also used on the receiver of a selection. For example, `"abc".exists` is converted to `stringWrapper("abc").exists`.

Implicit conversions are often used for simulating new syntax, such as the `->` in a `Map`, which is defined as:

``` scala
package scala
object Predef {
  class ArrowAssoc[A](x: A) {
    def -> [B](y: B): Tuple2[A, B] = Tuple2(x, y)
  }

  implicit def any2ArrowAssoc[A](x: A): ArrowAssoc[A] =
    new ArrowAssoc(x)
}
```

## Implicit Parameters

Implicit parameters are those that can optionally be provided by the compiler. Note that the `implicit` applies to the entire last parameter list. Also, we didn't use a direct `String` since the compiler selects implicit parameters based on their types, so this should lower the chances that another type is used to fulfill the implicit parameter. Finally, implicit parameters must be available as single identifiers, which is why they are usually declared in an object which is imported.

``` scala
class PreferredPrompt(val preference: String)
class PreferredDrink(val preference: String)

object Greeter {
  def greet(name: String)(implicit prompt: PreferredPrompt, drink: PreferredDrink) {
    println("Welcome " + name + ", have some " + drink.preference)
    println(prompt.preference)
  }
}

object Preferences {
  implicit val prompt = new PreferredPrompt("$ ")
  implicit val drink  = new PreferredDrink("tea")
}

import Preferences._
Greeter.greet("Bob")(prompt, drink)
Greeter.greet("Bob") // or implicitly
```

Implicit parameters are often used to provide information about a type in a preceding, explicit parameter list. For example, it can be used to pass a compare function to a function that yields the largest element in a list. Scala actually provides many `orderer` functions in the standard library, which makes this function usable with many standard types without explicitly providing an `orderer`. Note that again, types are made as specific as possible to reduce ambiguity to the developer and to restrict the options available to the compiler:

``` scala
def maxList[T](elements: List[T])(implicit orderer: T => Ordered[T]): T =
  elements match {
    case List()    => throw new IllegalArgumentException("empty")
    case List(x)   => x
    case x :: rest =>
      val maxRest = maxList(rest) // (ordered)  is implicit
      if (x > maxRest) x          // ordered(x) is implicit
      else maxRest
  }
```

Also note that the implicit parameter can be used as an implicit parameter and conversion in the body, as a result, `ordered` doesn't appear anywhere in the function body. This is a very common thing to do, and since the name of the implicit parameter isn't used anywhere, it's possible to use a _view bound_.

For example, the following code essentially enforces the requirement that `T` can be _treated_ as an `Ordered[T]`, where _treated_ would mean that there is an implicit conversion available. If `T` is already an `Ordered[T]`, then an identity function is used as the implicit conversion:

``` scala
def maxList[T <% Ordered[T]](elements: List[T]): T = ...
```

If multiple conversions apply for an implicit conversion, Scala generally refuses to insert a conversion. However, since Scala 2.8, it now does something similar to C++ where it'll choose the most specific conversion available, where being _more specific_ entails:

* the argument type is a subtype of another conversion's argument type
* both conversions are methods and the enclosing class extends the other conversion's enclosing class

# Lists

The left fold is possible with `/:` and `foldLeft` and the right fold with `:\` and `foldRight`. The `/:` and `:\` names represent the way the fold tree leans.

``` scala
def sum(xs: List[Int]): Int = (0 /: xs) (_ + _)
def flattenRight[T](xss: List[List[T]]) =
  (xss :\ List[T]()) (_ ::: _)
```

An efficient way to append lists in constant time is to use `ListBuffers` [^difference_lists]:

[^difference_lists]: I wonder if these are similar to Haskell's [difference lists].

[difference lists]: http://hackage.haskell.org/package/dlist/docs/Data-DList.html

``` scala
import scala.collection.mutable.ListBuffer
val buf = new ListBuffer[Int]
buf += 1
3 +=: buf
buf.toList
```

An `ArrayBuffer` is similar to an `std::vector` in that it automatically resizes itself to fit its contents.

## Implementation {#list-implementation}

Lists are implemented as a covariant, abstract class `List` for which there are subclasses `::` and `Nil`. The covariant property allows a `List[Int]` to be assigned to a `List[Any]`:

``` scala
package scala
abstract class List[+T]

val xs: List[Any] = List(1, 2, 3)
```

The `Nil` object inherits from `List[Nothing]` so that `Nil` can be assigned to any `List`. The methods `isEmpty`, `head`, and `tail` are implemented for `Nil`, where the first returns `true` and the latter two throw an exception, as in Haskell.

``` scala
case object Nil extends List[Nothing]  {
  override def isEmpty = true
  def head: Nothing = throw ...
  def tail: List[Nothing] = throw ...
}
```

The `::`, "cons" class is defined so that `x :: xs` is treated as `::(x, xs)` where `::` is a case class defined as:

``` scala
final case class ::[T](head: T, tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false
}
```

List operations are defined so that the result type is "widened" to accommodate the types of all list elements. This is done by placing a lower bound

``` scala
def ::[U >: T](x: U): List[U] = new scala.::(x, this)

abstract class Fruit
class Apple extends Fruit
class Orange extends Fruit

val apples = new Apple  :: Nil
val fruits = new Orange :: apples
```

It turns out that the cons class is actually defined such that the tail is a mutable var, but only accessible within the `scala` package:

``` scala
final case class ::[U](hd: U, private[scala] var tl: List[U]) extends List[U] {
  def head = hd
  def tail = tl
  override def isEmpty: Boolean = false
}
```

A `ListBuffer` then works by directly modifying the tail of the last cons cell in the list. This means that the tails in the following two variables are shared to avoid copying [^mutability]:

[^mutability]: It seems like Scala uses mutability to leave optimization up to the developer. Contrast this with Haskell, where everything is immutable at the language level and the optimizations are done underneath at compile time or at the runtime level. The cons `:` in Haskell for example would also reuse the tail, creating a [persistent linked list], but the developer doesn't have to worry about how to best implement something like this for efficiency. Scala of course affords one more flexibility in how they implement something, but it expects that every developer be mindful of how best to implement things in the unusual functional and imperative environment.

[persistent linked list]: http://en.wikipedia.org/wiki/Persistent_data_structure#Linked_lists

``` scala
val ys = 1 :: xs
val zs = 2 :: xs
```

# For Expressions

All `for` expressions that `yield` are translated by the compiler into combinations of `map`, `flatMap`, and `withFilter`, and those that don't `yield` into combinations of `withFilter` and `foreach`. This means that the following are equivalent:

``` scala
persons withFilter (p => !p.isMale) flatMap (p =>
  (p.children map (c => (p.name, c.name))))

for (p <- persons; if !p.isMale; c <- p.children)
yield (p.name, c.name)
```

`for` expressions are of the following form, where _seq_ is a sequence of _generators_, _definitions_, and _filters_ delimited by semicolons:

``` scala
for (seq) yield expr

// for example: seq = generator; definition; filter
for (p <- persons; n = p.name; if (n startsWith "To"))
yield n
```

Generators are of the form `pat <- expr` where the pattern `pat` is matched for each element in the list. If the match succeeds, the variables are bound to the pattern components. If the match fails, the element is discarded from iteration.

## Translation {#for-expression-translation}

The following are examples of how `for` expressions are translated into combinations of `map`, `flatMap`, and `withFilter`.

``` scala
// case 1: one generator
for (x <- expr1) yield expr2
expr1.map(x => expr2)

// case 2: generator and filter
for (x <- expr1 if expr2) yield expr3
// transform into case 1
for (x <- expr1.withFilter(x => expr2)) yield expr3
expr1.withFilter(x => expr2).map(x => expr3)

// case 3: two generators
for (x <- expr1; y <- expr2; seq) yield expr3
expr1.flatMap(x => for (y <- expr2; seq) yield expr3)

// case 4: tuple pattern
for ((x1, ..., xn) <- expr1) yield expr2
expr1.map { case (x1, ..., xn) => expr2 }

// case 5: arbitrary pattern
//         first filter by successful match, then map.
//         guarantees no MatchError exception
for (pat <- expr1) yield expr2
expr1.withFilter {
  case pat => true
  case _   => false
}.map {
  case pat => expr2
}

// case 6: contains definitions
//         expr2 is evaluated each time x is generated
for (x <- expr1; y = expr2; seq) yield expr3
for ((x, y) <- for (x <- expr1) yield (x, expr2); seq)
yield expr3

// case 7: side-effect loop
for (x <- expr1) body
expr1.foreach(x => body)

for (x <- expr1; if expr2; y <- expr3) body
expr1.withFilter(x => expr2).foreach(x =>
  expr3.foreach(y => body))
```

## Generalized For Expressions

It's possible to add support for `for` expressions to any data type by defining `map`, `flatMap`, `withFilter`, and `foreach`. Depending on which of these functions are implemented, the following features of `for` expressions become available:

* `map`: expressions with a single generator
* `flatMap` and `map`: expressions with multiple generators
* `foreach`: loops with single/multiple generators
* `withFilter`: filter expressions

Type checking is performed after translation occurs. Given a parameterized type `C` denoting a collection, the following type signatures are generally used for the required methods. A standard technique to optimize `withFilter` is to not return an entire new object but to return a wrapper object that remembers that elements need to be filtered when they're processed later:

``` scala
abstract class C[A] {
  // monad methods
  def map[B](f: A => B): C[B]
  def flatMap[B](f: A => C[B]): C[B]
  def withFilter(p: A => Boolean): C[A]

  def foreach(b: A => Unit): Unit
}
```

# Collections API

The `Traversable` trait is at the top of the collections hierarchy and defines the following abstract operation, which is meant to traverse all elements of a collection and apply the operation `f` to each element:

``` scala
def foreach[U](f: Elem => U)
```

The `Iterable` trait defines an abstract `iterator` method, which `Iterable` uses to define `foreach` of `Traversable` it extends from:

``` scala
def foreach[U](f: Elem => U): Unit = {
  val it = iterator
  while (it.hasNext) f(it.next())
}
```

## Sequences

The sequence traits `Seq`, `IndexedSeq`, and `LinearSeq` represent iterables that have a length and whose elements have fixed indexed positions starting from 0.

Buffers are a sub-category of sequences that allow element insertions, removals, and appending operations. Common buffers are `ListBuffer` and `ArrayBuffer`.

## Sets

Sets are iterables with no duplicate elements. There are two subtraits `SortedSet` and `BitSet`. Ordering is preserved in `SortedSet`, and is backed by an ordered binary tree, with `immutable.TreeSet` being a [red-black tree] that keeps the tree balanced. `BitSet` uses an array of `Long` values to efficiently represent a set of packed bits, much like C++'s [bitset].

[red-black tree]: /notes/algorithms/#red-black-trees
[bitset]: http://en.cppreference.com/w/cpp/utility/bitset

## Maps

Maps' `get` method returns an `Option[Value]`, like `lookup` would return a `Maybe a` in Haskell. The `getOrElseUpdate` function facilitates the use of Maps as caches.

## Streams

Streams have elements that are computed lazily. The `#::` function is used to construct streams. Notice that only the head has been computed so far:

``` scala
val str = 1 #:: 2 #:: 3 #:: Stream.empty
// Stream(1, ?)

def fibonacciFrom(a: Int, b: Int): Stream[Int] =
  a #:: fibonacciFrom(b, a + b)

val fibs = fibonacciFrom(1, 1).take(7).toList
// List(1, 1, 2, 3, 5, 8, 13)
```

## Vectors

Vectors are effectively constant time, random access sequences represented as broad, shallow trees where every tree node contains up to 32 elements of the vector or 32 other tree nodes. The `updated` method can be used to update particular elements and is also effectively constant time, since only the node that contains the element and every node that points to it must be copied. These are currently the default implementation of immutable indexed sequences, `collection.immutable.IndexedSeq`.

## Ranges

Ranges can be defined as follows:

``` scala
1 to 3       // => Range(1, 2, 3)
5 to 14 by 3 // => Range(5, 8, 11, 14)
1 until 3    // => Range(1, 2,)
```

## Arrays

Scala arrays correspond to Java arrays such that `Array[T]` in Scala is a `T[]` in Java. Scala arrays are compatible with sequences, and provide all operations that sequences provide. This is facilitated through implicit conversions to `scala.collection.mutable.WrappedArray`. There's also an implicit conversion to `ArrayOps` which supports various methods available to sequences, without actually turning the array into a sequence.

Java doesn't allow generic arrays `T[]`, but this is made possible in Scala by creating an array of `Objects`. Creating generic arrays in Scala through `Array[T]` requires a run-time hint, since the information about the type `T` gets erased at runtime. This is done with a _class manifest_ of type `scala.reflect.ClassManifest`, which is a type descriptor object that describes the top-level class of a type. The compiler can be instructed to generate code to construct and pass a class manifest, this is done via an implicit parameter. This way, the compiler looks for an implicit value of type `ClassManifest[T]` so that the correct type of array can be constructed at run-time:

``` scala
def someMethod[T](xs: Vector[T])(implicit m: ClassManifest[T]): Array[T] = ...

// or with a context bound
def someMethod[T: ClassManifest](xs: Vector[T]): Array[T] = ...
```

## Views

Transformer methods are ones such as `map` and `filter` and come in strict and non-strict varieties. A strict transformer constructs a new collection on the spot. A non-strict transformer construct a proxy for the result collection such that its elements are constructed on demand.

``` scala
def lazyMap[T, U](coll: Iterable[T], f: T => U) =
  new Iterable[U] {
    def iterator = coll.iterator map f
  }
```

Most collections are strict by default in their transformers except for `Stream`. It's possible to turn a collection into a lazy one and vice versa through _collection views_, which represent a base collection with the difference that the transformers are lazy. The `view` method is used to make the transformers lazy, and `force` is used to go back to strict transformers.

``` scala
(v.view.map(_ + 1).map(_ * 2)).force
```

Views can provide a simple way to optimize otherwise costly operations [^lazy_haskell]:

[^lazy_haskell]: Something like this would be implicit in Haskell due to its non-strict nature.

``` scala
def isPalindrome(x: String) = x == x.reverse
def findPalindrome(s: Seq[String]) = s find isPalindrome

// creates 1000000 element sequence
findPalindrome(words take 1000000)

// creates no copy
findPalindrome(words.view take 1000000)
```

Views can also be used as subwindows into mutable sequences [^slices].

[^slices]: This is of course very much like [Go's slices] and [Rust's slices].

[Go's slices]: http://golang.org/doc/effective_go.html#slices
[Rust's slices]: http://static.rust-lang.org/doc/master/tutorial.html#vectors-and-strings

``` scala
val subarr = arr.view.slice(3, 6)

// can now perform operations on that slice only
// i.e. negate all elements in the slice
for (i <- 0 until subarr.length) subarr(i) = -subarr(i)
```

## Iterators

Iterators are affected by operations on them, such that for example a `map` called on an iterator leaves the iterator's position at the end of the sequence, so that an extra call to `next` will throw a `NoSuchElementException`. This is mitigated by duplicating the iterator with `duplicate` [^duplicate_fd].

[^duplicate_fd]: This reminds me of [open file descriptions] which record the file offset and status flags. Duplicate file descriptors [share this information]. To avoid this, it's necessary to perform a separate `open` call.

[open file descriptions]: http://man7.org/linux/man-pages/man2/open.2.html
[share this information]: http://man7.org/linux/man-pages/man2/dup.2.html

A `BufferedIterator` provides an extra method `head` that returns its first element without advancing the iterator.

## Java Interop {#java-interop-collections}

Scala provides implicit conversions for the major collection types in Java through the `JavaConversions` object.

## Architecture {#collections-architecture}

Most of the collection operations are implemented in terms of traversals and builders.

### Builders {#builders}

Builders are in charge of building new collections. The `result` method yields the collection that has been constructed thus far, and the builder can be reset to a clean slate to construct another collection with the `clear` method. The `mapResult` method can be used to return a result of a different type.

``` scala
package scala.collection.generic

class Builder[-Elem, +To] {
  def +=(elem: Elem): this.type
  def result(): To
  def clear()
  def mapResult(f: To => NewTo): Builder[Elem, NewTo] = ...
}
```

### Implementation Traits

The code is kept DRY by using _implementation traits_ which are named with a `Like` suffix, such as `TraversableLike`. These traits implement concrete methods and are parameterized using the collection's element type and its representation type, i.e. `Seq[I]` or `List[I]`. For example, `filter` is implemented here such that it creates a new builder for the representation type and appends elements to it if they satisfy the predicate, then the builder's result is returned.

*[DRY]: Don't Repeat Yourself

``` scala
package scala.collection

class TraversableLike[+Elem, +Repr] {
  def newBuilder: Builder[Elem, Repr]
  def foreach[U](f: Elem => U)
  ...
  def filder(p: Elem => Boolean): Repr = {
    val b = newBuilder
    foreach { elem => if (p(elem)) b += elem }
    b.result
  }
}
```

A problem presents itself when we want to return a different type of sequence from the one that is being operator one, for example:

``` scala
Map("a" -> 1, "b" -> 2) map { case (x, y) => (y, x) }
// Map(1 -> "a", 2 -> "b")

Map("a" -> 1, "b" -> 2) map { case (x, y) => y }
// List(1, 2)

BitSet(1, 2, 3) map (_.toFloat)
// Set(1.0, 2.0, 3.0)
```

This is mitigated with an implicit parameter on the function so that a builder factory may produce the correct type of builder by passing it the implicit parameter.

``` scala
def map[B, That](p: Elem => B)
  (implicit bf: CanBuildFrom[B, That, This]): That = {
  val b = bf(this)
  for (x <- this) b += f(x)
  b.result
}

package scala.collection.generic
trait CanBuildFrom[-From, -Elem, +To] {
  def apply(from: From): Builder[Elem, To]
}
```

For example, in the case of a `BitSet`, the companion object for `BitSet` would define a builder factory of type `CanBuildFrom[BitSet, Int, BitSet]`, with a more general fallback that converts to a regular `Set` with `CanBuildFrom[Set[_], A, Set[A]]`. Scala will then choose the more specific one when choosing the implicit instance.

Finally, collections are kept the same dynamic type. This is achieved through virtual dispatch by passing the source collection to the builder factory, which forwards the call to a `genericBuilder` method available on all generic, non-leaf classes, which itself calls the builder that belongs to the collection on which it is defined.

### Creating Collections

Creating a new collection type `T` can be done by using traits `IndexedSeq[Elem]` and `IndexedSeqLike[Elem, T]`. The latter requires the implementation of `newBuilder`. It's also necessary to implement appropriate `CanBuildFrom` implicits.

# Extractors

Extractors provide a way to define patterns that are decoupled from an object's representation. This is done using an `unapply` method that matches a value and deconstructs it:

``` scala
object EMail {
  def unapply(str: String): Option[(String, String)] = {
    val parts = str split "@"
    if (parts.length == 2) Some(parts(0), parts(1)) else None
  }
}
```

This is analogous to an `apply` method that constructs an object:

``` scala
object EMail {
  def apply(user: String, domain: String) = user + "@" + domain
}
```

This can be made more explicit by inheriting from the function type:

``` scala
// equivalent to extends Function2[String, String, String]
object EMail extends ((String, String) => String) { ... }
```

Pattern matching in Scala checks for an `unapply` method to deconstruct the object. If `unapply` returns `None`, then the pattern doesn't match and it moves on to the next pattern, throwing a `MatchError` if there are no other patterns that match. Remember that a single-tuple parameter can omit the set of parentheses, i.e. `func((a, b)) -> func(a, b)`.

``` scala
selectorString match { case EMail(user, domain) => ... }
```

It's generally a good idea to ensure that the following property holds:

``` scala
Obj.unapply(Obj.apply(a, b)) == Some(a, b)
```

In the case of a single pattern variable, that single variable is wrapped in an `Option` value.

It's also possible for the pattern to not bind any variables, in which case a `Boolean` is returned to indicate whether the pattern matched:

``` scala
object UpperCase {
  def unapply(s: String): Boolean = s.toUpperCase == s
}
```

It's also possible to define variable argument extractors, to be able to match on an arbitrary number of variables. This is done using the `unapplySeq` function:

``` scala
object Domain {
  def unapplySeq(whole: String): Option[Seq[String]] =
    Some(whole.split("\\.").reverse)
}
```

This allows matching like this:

``` scala
domain match {
  case Domain("org", "acm") => println("acm.org")
  case Domain("net", _*) => println("some .net domain")
}
```

## Compared to Case Classes

Modifying the case classes has an effect on client code, whereas extractors provide a layer of indirection between the data representation and the way it's viewed by clients. However, case classes to have advantages over extractors. They're easier and simpler to define. Case classes can also end up generating more efficient pattern matches because the compiler can optimize them since they're defined at the language level. Finally, case classes derived from a `sealed` base class can allow the compiler to check for pattern match exhaustiveness.

## Regular Expressions

Regular expressions can be constructed using the `Regex` constructor or with a `r` method on a string. It's possible to pattern match on a regular expression match using a predefined extractor. The matching is done by binding every matched group. If a group didn't match, it'll bind the variable to `null`:

``` scala
val Decimal = """(-)?(\d+)(\.\d*)?""".r
val Decimal(sign, integerpart, decimalpart) = "-1.23"
// sign = "-", integerpart = "1", decimalpart = ".23"
```

# Annotations

Annotations are like meaningful comments for the compiler. They can support generation of documentation (Scaladoc), for example. Annotations can be placed on any kind of declaration or definition, as well as expressions:

``` scala
@deprecated def oldMethod() = ...

(expr: @unchecked) match {
  // non-exhaustive cases
}
```

The general form of annotations is as follows. The `@` prefixed to an annotation can read as `new` since underneath the compiler is simply instantiating a class named after the annotation. Passing an annotation as argument to another has the consequence that it must use `new` instead of `@`.

``` scala
@annotation(exp1, exp2, ...)
```

The `@deprecated` annotation can mark something as deprecated, which elicits a compiler warning if some code uses it. It accepts an optional message:

``` scala
@deprecated("use newOne() instead")
def oldOne() = ...
```

The `@volatile` annotation informs the compiler that the variable in question will be used by multiple threads. This makes reads/writes slower, but accesses from multiple threads behave more predictably.

The `@serializable` annotation marks a class as serializable. The `@SerialVersionUID(id)` annotation marks the current version of a class. The `@transient` annotation marks fields that should not be serialized.

The `@scala.reflect.BeanProperty` annotation generates automatic get and set methods, i.e. `age` generates `getAge` and `setAge`. This is useful for Java-centric frameworks. The generated methods are available only after compilation, i.e. not within one's own code.

The `@tailrec` annotation designates that a method should be tail-recursion optimized. If the optimization cannot be performed, a warning is emitted.

The `@unchecked` annotation tells the compiler not to check for exhaustive match cases.

The `@native` annotation means that the method's implementation is supplied by the runtime, via the Java Native Interface (JNI), rather than in Scala.

``` scala
@native
def nativeMethod() { /* empty body required */ }
```

# XML

Scala allows XML literals anywhere that an expression is valid. The type of an XML literal is `Elem`. Class `Node` is the abstract superclass of all XML node classes. Class `Text` is a node holding just text. Class `NodeSeq` corresponds to a sequence of nodes, in fact, `Node` extends from `NodeSeq`, so a `Node` can be thought of as a one-element `NodeSeq`.

It's possible to interpolate Scala code within XML literals using braces `{}`. The interpolated code can itself contain XML literals, effectively allowing arbitrary nesting of XML code. Two braces in a row are used to print literal braces.

``` scala
var res = <a> {3 + 4} </a>
// scala.xml.Elem = <a> 7 </a>

var yearMade = 1955
var res2 = <a> { if (yearMade < 2000) <old>{yearMade}</old>
                 else xml.NodeSeq.Empty} </a>
// <a> <old>1955</old> </a>
```

Scala's XML support can be leveraged to provide object serialization:

``` scala
abstract class SomeClass {
  val description: String
  val year: Int

  def toXML =
    <someclass>
      <description>{description}</description>
      <year>{year}</year>
    </someclass>
}
```

There are various ways to deconstruct XML in Scala. The `text` method retrieves all of the text from the entire element tree, `flatMap` style. Scala also supports [XPath] through the methods `\` (for sub-elements) and `\\` (for recursive/deep search). Attributes can be extracted using the `@` sign before the attribute.

[XPath]: http://en.wikipedia.org/wiki/XPath

``` scala
var res  = <a><b><c>hello</c></b></a> \ "b"
// <b><c>hello</c></b>

var res2 = <a><b><c>hello</c></b></a> \\ "c"
// <c>hello</c>

val employee = <employee name="Joe" rank="officer" serial="IG-88"/>
val res3 = employee \ "@name"
// "Joe"
```

It's also possible to define a `fromXML` method to deserialize an object.

``` scala
object SomeClass {
  def fromXML(node: scala.xml.Node): SomeClass =
    new SomeClass {
      val description = (node \ "description").text
      val year        = (node \ "year").text.toInt
    }
}
```

It's possible to save a file from an XML node using, and conversely, load it:

``` scala
xml.XML.save("serialized.xml", node)
val loadednode = xml.XML.loadFile("serialized.xml")
```

It's possible to pattern match on XML. This is done by using XML literals where instead of interpolating expressions, variables are interpolated in order to bind to the matched data. In the context of XML, the `_*` is interpreted as matching any sequence of nodes down the XML tree.

``` scala
def proc(node: scala.xml.Node): String =
  node match {
    case <a>{contents @ _*}</a> => "It's an a: " + contents
    case <b>{contents @ _*}</b> => "It's a b: "  + contents
    case _ => "Something else."
  }

// proc(<a>a <em>b</em> c</a>) => "It's an a: ArrayBuffer(a, <em>b</em>, c)"
```

# Modular Programming

When a module grows too large for a single file, it may be useful to split the module up into separate traits defined in separate files, which can then be mixed into the original module.

A problem can arise when one such compartmentalized trait wants to refer to another trait that ultimately gets mixed into the same class. This can be circumvented by specifying the _self type_ which essentially defines the value of type of `this` for whenever it's referred to in the class. For example, assuming trait `Second` needs to refer to trait `First`'s method `count`, we can do:

``` scala
trait First {
  def count = 5
}

trait Second {
  this: First =>

  def printCount { println(count()) }
}
```

It's also possible to select types at runtime:

``` scala
val db: Database =
  if(args(0) == "student")
    StudentDatabase
  else
    SimpleDatabase
```

Sometimes the compiler won't be able to determine that two types are the same, as in the following case:

``` scala
object Obj {
  val db: Database = SomeDatabase

  object browser extends Browser {
    // compiler doesn't know `database` is
    // same type as `db`
    val database = db
  }

  // error: type mismatch
  // found: db.SomeCategory
  // required: browser.database.SomeCategory
  for (category <- db.allCategories)
    browser.displayCategory(category)
}
```

This can be resolved by using a singleton type through the `type` property, which is an extremely specific type that refers to the type of the specific object [^ruby_eigenclass]:

[^ruby_eigenclass]: This _definitely_ reminds me of Ruby's EigenClasses.

``` scala
object browser extends Browser {
  val database: db.type = db
}
```

# Object Equality

To recap, Scala's equality operators are equivalent to Java's `equals` methods, that is, unlike Java's equality operators which refer to object identity for reference types. Object identity is available through the `eq` method though it's rarely used.

There are four main pitfalls when defining equals methods:

1. wrong signature
2. changing it without also changing `hashCode`
3. defining it in terms of mutable fields
4. failing to define it as an equivalent relation

## Wrong Signature

Given a simple type like the following, the accompanying `equals` method is too naive. What happens is that when it's added to a collection, it's static type becomes `Any`, which would trigger the `equals` method for `Any` since overloading in Scala and Java is based on the static type. It turns out that the `equals` method in `Any` is simply object identity.

``` scala
class Point(val x: Int, val y: Int) {
  def equals(other: Point): Boolean =
    this.x == other.x && this.y == other.y
}
```

A more robust `equals` operator would assume that the parameter is of static type `Any`, and then perform a pattern match for its dynamic type:

``` scala
override def equals(other: Any) = other match {
  case that: Point => this.x == that.x && this.y == that.y
  case _ => false
}
```

It's also a common mistake to want to define the `==` operator directly, which is impossible since it's defined as `final`:

``` scala
final def == (that: Any): Boolean =
  if (null eq this) {null eq that} else {this equals that}
```

However, if this method is defined with a different parameter type, the compiler would regard it as an overloaded variant, and would allow the definition to occur, but the same problem would occur as above, where the parameter type isn't `Any`.

## Hash Code

When the `equals` method is redefined, it's also logically necessary to redefine the `hashCode` method, which by default is defined in `AnyRef` to be some transformation of the object's address. In fact, if two objects are determined to be equal as per the `equals` method, then they must return the same `hashCode`.

The `hashCode` method can be defined for `Point` so that it only references fields that were used in `equals` for equality determination.

``` scala
override def hashCode = 41 * (41 + x) + y
```

## Mutable Fields

Making the `hashCode` and as a result the `equals` method depend on mutable fields can have bad consequences. Adding an item to a collection and then changing that item's mutable field, which itself aids in equivalence determination, will mean that the collection will no longer find the object, since the collection (a `HashSet`) will keep looking in the wrong bucket, where it would expect to find the item based on its new data representation. Instead it may have been better to avoid redefining `hashCode` and create a separate comparison function such as `equalContents`.

## Equivalence Relation

The `equals` method should implement an equivalence relation on non-null objects, that is:

* it's _reflexive_: for any non-null value x

    `x.equals(x) == true`

* it's _symmetric_: for non-null values x and y

    `x.equals(y) == true == y.equals(x)`

* it's _transitive_: for non-null values x, y, and z

    `x.equals(y) && y.equals(z) == true == x.equals(z)`

* it's _consistent_: for non-null values x and y, multiple invocations of `x.equals(y)` should consistently return the same value provided no information used in equality determination is modified

* `x.equals(null)` should return false

## Subtypes {#subtype-equality}

There's a mistake that can be made given a subtype that redefines its `equals` method. If a supertype is compared to a subtype and appears on the left hand side, it would invoke the supertype's `equals` method which would _only_ compare those properties common to both types, meaning that the equality would not be symmetric.

``` scala
class ColoredPoint(x: Int, y: Int, val color: Color.Value)
  extends Point(x, y) {
  override def equals(other: Any) = other match {
    case that: ColoredPoint =>
      this.color == that.color && super.equals(that)
    case _ => false
  }
}

val p  = new Point(1, 2)
val cp = new ColoredPoint(1, 2, Color.Red)

p  equals cp == true
cp equals p  == false
```

A way to solve this is to define a `canEqual` method in classes that override `equals` and `hashCode`, which determines whether or not an object can be compared for equality with the given class. This allows subclasses of the class that redefined `canEqual` to continue to compare for equality with objects of the superclass. This is especially important for the anonymous class instantiation syntax:

``` scala
class Point(val x: Int, val y: Int) {
  override def hashCode = 41 * (41 + x) + y
  override def equals(other: Any) = other match {
    case that: Point =>
      (that canEqual this) &&
      (this.x == that.x) && (this.y == that.y)
    case _ =>
      false
  }
  def canEqual(other: Any) = other.isInstanceOf[Point]
}

class ColoredPoint(x: Int, y: Int, val color: Color.value)
  extends Point(x, y) {
  override def hashCode = 41 * super.hashCode + color.hashCode
  override def equals(other: Any) = other match {
    case that: ColoredPoint =>
      (that canEqual this) &&
      super.equals(that) && this.color == that.color
    case _ =>
      false
  }
  override def canEqual(other: Any) =
    other.isInstanceOf[ColoredPoint]
}
```

# Java Interop

The Scala compiler will attempt to use direct mappings to Java value types, such as an `Int`. However, sometimes this assumption can't be made, as in a collection, in which case it'll use wrapper classes like `java.lang.Integer`.

Singleton `objects` the compiler creates a Java class with suffix `$` which contains all of the methods and fields of the Scala singleton object as well as a `MODULE$` static field that holds the singleton instance created at runtime. If the singleton object has no companion class then a class without the `$` suffix is created which has a static forwarder method for each method in the singleton object.

Traits get compiled down to Java interfaces if they only contain abstract methods.

## Existential Types

Existential types in Scala are mainly used to facilitate Java's wildcard types and raw types. They take the form:

``` scala
type forSome { declarations }
```

For example, the following is equivalent to Java's `Iterator <?>` and reads as it being an `Iterator` of `T`'s for some type `T`. It's also possible to use _placeholder syntax_ which is similar to the one used in patterns. For each underscore present in the type a type parameter is added to the `forSome` clause:

``` scala
Iterator[T] forSome { type T }

// equivalently: placeholder syntax
Iterator[_]
```

Similarly, Java's `Iterator<? extends Component>` can be represented as follows in Scala, where it reads as being an `Iterator` of `T` for some type `T` that is a subtype of `Component`:

``` scala
Iterator[T] forSome { type T <: Component }

// equivalently: placeholder syntax
Iterator[_ <: Component]
```

Given a Java class with wildcards like this:

``` java
public class Wild {
  Collection<?> contents() {
    Collection<String> stuff = new Vector<String>();
    stuff.add("a");
    stuff.add("b");
    stuff.add("see");
    return stuff;
  }
}
```

If we want to create a new `Set` from the contents of this collection, we'll have trouble naming the type:

``` scala
import scala.collection.mutable.Set
val iter = (new Wild).contents.iterator
val set = Set.empty[???] // what type?
while (iter.hasMore)
  set += iter.next()
```

The solution to this is to create an abstract class with methods for each of the types in the `forSome` clause:

``` scala
abstract class SetAndType {
  type Elem // required for defining `set`
  val set: set[Elem]
}

def java2scala[T](jset: Collection[T]): SetAndType = {
  val sset = Set.empty[T] // T can be used, inferred from parameter
  val iter = jset.iterator
  while (iter.hasNext)
    sset += iter.next()

  return new SetAndType {
    type Elem = T
    val set = sset
  }
}
```

## Compiling Heterogenous Projects

Usually one would compile the Java code and add the output to the classpath when building the Scala code. However, this wouldn't work if the Java code references the Scala code, or vice versa. For this reason, Scala allows processing of Java source files as if they were Scala files. They won't be compiled, but they'll be scanned to determine what they contain. This is done with this sequence of commands:

``` bash
$ scalac -d bin FileAnalysis.scala FileItem.java File.java
$ javac -cp bin -d bin File.java FileItem.java FileManagement.java
$ scala -cp bin FileManagement
# program output
```

# Actors and Concurrency

Scala uses actors similar to Erlang's as its primary concurrency primitive. **Note** that this actors library is [deprecated]; [Akka] actors will be covered later and are the default actors library since Scala 2.10.

[deprecated]: http://docs.scala-lang.org/overviews/core/actors-migration-guide.html
[Akka]: http://akka.io/

``` scala
import scala.actors._

object PrintActor extends Actor {
  def act() {
    for (i <- 1 to 5) {
      println("acting")
      Thread.sleep(1000)
    }
  }
}

SillyActor.start() // prints "acting" five times

// equivalent: .start()s as soon as it's defined
val PrintActor2 = actor {
  for (i <- 1 to 5) {
    println("acting")
    Thread.sleep(1000)
  }
}
```

Actors can be sent messages using the `!` method. The following actor prints out the messages it receives:

``` scala
val echoActor = actor {
  while (true) {
    receive {
      case msg => println("received: " + msg)
    }
  }
}

echoActor ! "testing"
// received: testing
```

The way this works is that the `receive` method accepts a partial function, which in this case is specified as a partial function literal [case sequence]. The actor will block until a message is received that matches a pattern in the partial function, and non-matching messages are silently ignored.

[case sequence]: #case-sequence

It's also possible to treat "native" threads as actors through `Actor.self`, which yields an actor representing the current thread. This is useful for debugging, by specifically setting a `receive` function:

``` scala
import scala.actors.Actor._
self ! "hello"
self.receive { case x => x}
// res1: Any = hello

// 1 second timeout to avoid blocking repl
self.receiveWithin(1000) { case x => x }
```

Every actor is given its own thread so that they can each perform the `act` method. This is resource heavy, so a `react` method can be used that is similar to `receive`. The `react` method doesn't return after it finds and processes a message, instead it only evaluates the message handler. This means that the implementation doesn't need to preserve the call stack of the current thread, allowing it to reuse the thread for the next actor that wakes up.

Since `react` doesn't return, it's usually common to return the information (if any) by sending the message to another actor, then recursing the message handler `act` to continue to process messages. This recursion pattern is common enough that there's a method `loop` that can wrap around the `react` method so that it loops infinitely.

``` scala
object Resolver extends Actor {
  import java.net.{InetAddress, UnknownHostException}

  def act() {
    react {
      case (name: String, receiver: Actor) =>
        receiver ! getIp(name)
        act()
      case "EXIT" =>
        println("exiting") // doesn't recurse again
      case msg =>
        println("unhandled msg: " + msg)
        act()
    }
  }

  def getIp(name: String): Option[InetAddress] = {
    try {
      Some(InetAddress.getByName(name))
    } catch {
      case _:UnknownException => None
    }
  }
}

Resolver.start()

Resolver ! ("www.scala-lang.org", self)
self.receiveWithin(0) { case x => x }
// Some(ww.scala-lang.org/<some-ip>)
```

Actor's shouldn't block, such as while processing a message, since this can even cause deadlocks while actors wait on each other. If an actor needs to perform an operation that blocks in order to process a message, a separate actor should be created that performs the blocking operation and then notifies the original actor when its work is complete. In the following example, the actor will continue to accept requests even while it continues to perform blocking operations, which it delegates to a sub-actor.

``` scala
val someActor = actor {
  def emoteLater() {
    val mainActor = self
    actor {
      Thread.sleep(1000)
      mainActor ! "Emote"
    }
  }

  var emoted = 0
  emoteLater()

  loop {
    react {
      case "Emote" =>
        println("acting")
        emoted += 1
        if (emoted < 5)
          emoteLater()
      case msg =>
        println("received: " + msg)
    }
  }
}

/*
acting
acting
someActor ! "hello"
Received: hello
acting
acting
*/
```

It's very important to communicate with actors only via messages. However, unlike Erlang's actors, Scala allows mixing the traditional shared data & locks model with actors. For example, if multiple actors were to share a common mutable map, there are two approaches that can be taken to achieve this. The first would entail creating an actor that owns the map, defining messages for every kind of map operation that would be required such as getting and setting values. An alternative approach, however, would be to pass a thread-safe map such as `ConcurrentHashMap`.

It's also a good idea to keep actors self-contained. A good idea is to send contextual information about the request (if not the request itself) along with the response, to the original actor, since some time may have passed since the actor performed the request. It's also more readable to create case classes when possible:

``` scala
case class LookupIP(name: String, respondTo: Actor)
case class LookupResult(name: String, address: Option[InetAddress])

object Resolver extends Actor {
  def act() {
    loop {
      react {
        case LookupIP(name, actor) =>
          actor ! LookupResult(name, getIp(name))
      }
    }
  }

  def getIp = ...
}
```
