---
title: Scala
published: October 12, 2013
excerpt: Promising mixture of OO and FP
comments: off
toc: left
---

I've been meaning to learn Scala for some time, mainly because of it's alleged real-world applicability compared to Haskell [^real_world_haskell]. Haskell has really left me wanting to program in a very functional style. Scala seems to provide a decent compromise in a language that mixes object-oriented programming with functional programming characteristics. Furthermore, the JVM is battle-tested and probably the most robust virtual machine of any language out there at the moment.

*[JVM]: Java Virtual Machine

[^real_world_haskell]: Although lately I've come to realize just how applicable Haskell can be to real-world problems.

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

[^companion_ruby]: Reminds me of Ruby's 'EigenClasses', but I'm not quite sure yet if it's indeed similar, or if companion objects truly are just a separation for specifying class-wide values/methods.

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

**Auxiliary constructors** are additional constructors that simply directly or indirectly delegate object construction to the primary constructor. They are named after `this`, and their first action must be invoking another constructor:

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

[^cpp_partialapplication]: Reminds me of `std::bind` in C++11, which does the same thing by creating a functor, or [function object] --- not to be confused with the category theory [Functor] more common in [Haskell].

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

The assertions used in the previous two ScalaTest suites would only show an error that the assertion failed, without showing what the two values were. ScalaTest also provides a `===` operator that is more descriptived:

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

**Case Classes** are like stub classes which contain and are mainly about publicly accessible data. They're very similar to Haskell's algebraic data types (ADT). Consider the following domain specific language (DSL):

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

This is very similar to other pattern matching features available in Haskell for example. In cases, we can use **constructor patterns**, **constant patterns**, and **variable patterns**. Variable patterns are simply those that begin with lower case characters, but we can force a lowercase identifier to be treated as a constant by surrounding it with backticks.

It's also to use **sequence patterns**. In this case, the `_*` operator can be used to specify "the rest of the sequence," like so:

``` scala
expr match {
  case List(0, _*) => println("found it")
  case _ =>
}
```

It's also possible to use **type patterns** to match the type of the expression:

``` scala
def generalSize(x: Any) = x match {
  case s: String => s.length
  case m: Map[_, _] => m.size
  case _ => -1
}
```

**Type Erasure** means that information about type arguments is not maintained at runtime. This is an artifact of the erasure model of generics that Java uses. As a result, it's not possible to pattern match on the type of `Map`, and the following code will return `true` for any `Map`:

``` scala
def isIntToIntMap(x: Any) = x match {
  case m: Map[Int, Int] => true
  case _ => false
}
```

It's possible to use `@` for **variable binding** in pattern matching, just as in Haskell:

``` scala
expr match {
  case UnOp("abs", e @ UnOp("abs", _)) => e
  case _ =>
}
```

**Pattern guards** are additional conditions placed on pattern matches. For example, to simplify an addition expression with identical operands to a multiplication by two, we can use:

``` scala
def simplifyAdd(e: Expr) = e match {
  case BinOp("+", x, y) if x == y =>
    BinOp("*", x, Number(2))
  case _ => e
}
```

A **sealed class** is one that restricts subclassing of that class to the file it's defined in. This way the compiler can guarantee and enforce exhaustive pattern matching, which it couldn't do otherwise because the class could be extended in another file:

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

A **case sequence** is a function literal specific defined as a pattern match where each case is an entry point to the function:

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

If a case sequence doesn't provide exhaustive patterns, it is considered a **partial function** since it doesn't provide an output for every input. If it's applied to a value that it can't match, it throws a run-time exception.

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

# Lists

The left fold is possible with `/:` and `foldLeft` and the right fold with `:\` and `foldRight`. The `/:` and `:\` names represent the way the fold tree leans.

``` scala
def sum(xs: List[Int]): Int = (0 /: xs) (_ + _)
def flattenRight[T](xss: List[List[T]]) =
  (xss :\ List[T]()) (_ ::: _)
```

An efficient way to append lists in constant time is to use `ListBuffers` [^difference_lists]:

[^difference_lists]: I imagine these are similar to Haskell's [difference lists].

[difference lists]: http://hackage.haskell.org/package/dlist/docs/Data-DList.html

``` scala
import scala.collection.mutable.ListBuffer
val buf = new ListBuffer[Int]
buf += 1
3 +=: buf
buf.toList
```

An `ArrayBuffer` is similar to an `std::vector` in that it automatically resizes itself to fit its contents.

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

